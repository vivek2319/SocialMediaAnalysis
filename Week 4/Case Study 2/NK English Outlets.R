
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(tibble)
library(stringi)
library(stringr)
library(ggplot2)

prac_t = English_News_Outlets_for_Study
prac_t$Source = as.factor(prac_t$Source)

prac_t <- rowid_to_column(prac_t, "ID")

prac_t %>%
  group_by(Source) %>%
  summarize(Content = n_distinct(ID)) %>%
  ggplot(aes(Source, Content)) +
  geom_col() +
  coord_flip()

## Filter text

usenet_words = prac_t %>%
  unnest_tokens(word, Content) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word)


## Sort words by frequency
usenet_words %>%
  count(word, sort=TRUE)

## Sort words by newsgroup and highest frequency

words_by_newssource = usenet_words %>%
  count(Source, word, sort = TRUE) %>%
  ungroup()

words_by_newssource

## tf-idf

tf_idf = words_by_newssource %>%
  bind_tf_idf(word, Source, n) %>%
  arrange(desc(tf_idf))

tf_idf

tf_idf %>%
  group_by(Source) %>%
  top_n(12, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = Source)) +
  geom_col(show.legend =  FALSE) +
  facet_wrap(~ Source, scales = "free") +
  ylab("tf-idf") +
  coord_flip()


library(widyr)

newssource_cors = words_by_newssource %>%
  pairwise_cor(Source, word, n, sort = TRUE)

newssource_cors

library(ggraph)

install.packages("ggrepel")


library(igraph)

## Network Visualization -- 

set.seed(2017)

newssource_cors %>%
  filter(correlation > .75) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, width = correlation)) +
  geom_node_point(size = 6, color = "lightblue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

### Topic Modeling for Fun

#include only words that occur at least 50 times

word_sci_newssource = usenet_words %>%
  group_by(word) %>% 
  mutate(word_total = n()) %>%
  ungroup() %>%
  filter(word_total > 50)

## Convert into a DTM
## with document names such as sci.crypt_14147

nk_dtm = word_sci_newssource %>%
  unite(document, Source, ID) %>%
  count(document, word) %>%
  cast_dtm(document, word, n)

library(topicmodels)
nk_lda = LDA(nk_dtm, k = 4, control = list(seed = 2016))


nk_lda %>%
  tidy() %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ungroup() %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip()

### Distribution of gamma for each topic wihtin each Usenet newsgroup


nk_lda %>%
  tidy(matrix = "gamma") %>%
  separate(document, c("Source", "ID"), sep = "_") %>%
  mutate(Source = reorder(Source, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ Source) +
  labs(x = "Topic",
       y = "# of messages where this was the highest % topic")


# Sentiment Analysis

## Average AFINN score for posts within each newsgroup

newssource_sentiments = words_by_newssource %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(Source) %>%
  summarize(score =  sum(score * n) / sum(n))

newssource_sentiments %>%
  mutate(Source =  reorder(Source, score)) %>%
  ggplot(aes(Source, score, fill = score > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("Average Sentiment Score")



# Sentiment Analysis by Word
# Words with the greatest contributions to positive/negative sentiment
# scores in the Usenet text

contributions_nk = usenet_words %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(occurences = n(), 
            contribution = sum(score))

contributions_nk


## Which words have the most effect on sentiment scores overall?

contributions_nk %>%
  top_n(25, abs(contribution)) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip()


### which words contribute the most within each news group

top_sentiment_words = words_by_newssource %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  mutate(contribution = score * n / sum(n))

top_sentiment_words

top_sentiment_words %>%
  group_by(Source) %>%
  top_n(12, abs(contribution)) %>%
  ungroup() %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution, fill = Source)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Source, scales = "free") +
  coord_flip()

###  Sentiment Analysis by Message

sentiment_messages = usenet_words %>% 
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(Source, ID) %>%
  summarize(sentiment = mean(score),
            words = n()) %>%
  ungroup() %>%
  filter(words >= 5)


sentiment_messages

## As a simple measure to reduce the role of randomness
## we filtered out messages that had fewer than five words
## that contributed to sentiment

## What were the most positive messages?

sentiment_messages %>%
  arrange(desc(sentiment))

## Let us check out the most positive message of the dataset

print_message = function(group, message_id) {
  result = prac_t %>%
    filter(Source == group, ID == message_id, Content != "")
  cat(result$Content, sep = "\n")
}


print_message("chosonilbo", 15)

##

sentiment_messages %>%
  arrange(sentiment)

print_message("chosonilbo", 9)

##
### N-gram analysis

usenet_bigrams = prac_t %>%
  unnest_tokens(bigram, Content, token = "ngrams", n = 2)

usenet_bigrams_counts = usenet_bigrams %>%
  count(Source, bigram, sort = TRUE) %>%
  ungroup() %>%
  separate(bigram, c("word1", "word2"), sep = " ")

##removal of terms, in this case words of negation

negate_words = c("not", "without", "can't", "don't", "won't")

usenet_bigrams_counts %>%
  filter(word1 %in% negate_words) %>%
  count(word1, word2, wt  = n, sort = TRUE) %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  mutate(contribution = score * nn) %>%
  group_by(word1) %>%
  top_n(10, abs(contribution)) %>%
  ungroup() %>%
  mutate(word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>%
  ggplot(aes(word2, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free", nrow = 3) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  xlab("Words preceded by a negation") +
  ylab("Sentiment score * # of occurences") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()



## Write to CSV for Tableau
write.table(object_name, "nk_text_paper.csv", sep=",", col.names=T)
