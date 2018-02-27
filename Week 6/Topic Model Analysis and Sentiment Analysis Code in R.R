### Scraping Twitter Data in R and Text Analysis

library(twitteR)
library(ROAuth)
library(httr)
library(plyr)
library(tm)
library(rpart)
library(rpart.plot)
library(RColorBrewer)

### Sentiment Analysis with Twitter Data
library(syuzhet)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Set API Keys
api_key <- "BPBQ9S5KC5kqjU5QfiwpzPSFU"
api_secret <- "XkD4m3LTebSiWIVtvnqFdQIGfFCjkLnqDHLSoLkZsIjFa4Hxr7"
access_token <- "832060427852972033-nthFWKgvQg5PVacKqouyUtSAupgMthe"
access_token_secret <- "DrVf0KlZDiAwbTeefTmzeLLtDn3D6UA2DXx6a2MBXBrix"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# Collect Tweets
skip_tweets = searchTwitter("trump", n = 1000) 

# Transform Raw Twitter Data to Dataframe
skip_tweetsDF = twListToDF(skip_tweets) 

## Transformation of Tweets to DocumentTermMatrix
sk = skip_tweetsDF$text

TextPreprocessing = lapply(sk, function(x) {
  
  x = gsub('http\\S+\\s*', '', x) ## Remove URLs
  
  x = gsub('\\b+RT', '', x) ## Remove RT
  
  x = gsub('#\\S+', '', x) ## Remove Hashtags
  
  x = gsub('@\\S+', '', x) ## Remove Mentions
  
  x = gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
  
  x = gsub("\\d", '', x) ## Remove Controls and special characters
  
  x = gsub('[[:punct:]]', '', x) ## Remove Punctuations
  
  x = gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
  
  x = gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
 
})


# or as a vector
bd_list = as.vector(TextPreprocessing)

mycorpus <- Corpus(VectorSource(bd_list))

mycorpus = tm_map(mycorpus, content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')))

### Transformer all characters to lower case
mycorpus = tm_map(mycorpus, content_transformer(tolower))

### Remove all Punctuation
mycorpus = tm_map(mycorpus, removePunctuation)

### Remove all Numbers
mycorpus = tm_map(mycorpus, removeNumbers)

### Remove Stopwords
mycorpus = tm_map(mycorpus, removeWords, stopwords('english'))


#### transform to Document Term Matrix

skip.dtm = DocumentTermMatrix(mycorpus)



### Topic Model Analysis


rowTotals = apply(skip.dtm, 1, sum)
smtpmodel = skip.dtm[rowTotals>0, ]
smmodel_tweets = LDA(smtpmodel, 5)


terms(smmodel_tweets , 40)

### Empty rows identifier for Topic Model JSON Application 
empty.rows <- skip.dtm[rowTotals == 0, ]$dimnames[1][[1]]
mycorpus <- mycorpus[-as.numeric(empty.rows)]



### Preparation for Tableau 
## Create dataframe of discovered topics
topic_col=topics(smmodel_tweets)
topic_col2=as.data.frame(topic_col)

## Grab date from skip_tweetsDF$text
mine=as.data.frame(date=index(skip_tweetsDF$created), coredata(skip_tweetsDF$created))
mine = as.matrix(mine)
mine2 = mine[-as.numeric(empty.rows)]
topic_col2$date = mine2

## Write to CSV for Tableau
write.table(topic_col2, "topic_col2.csv", sep=",", col.names=T)

## Required packages
library(topicmodels)
library(dplyr)
library(stringi)
library(tm)
library(LDAvis)
## Find required quantities
phi <- posterior(smmodel_tweets)$terms %>% as.matrix
theta <- posterior(smmodel_tweets)$topics %>% as.matrix
vocab <- colnames(phi)
doc_length <- vector()
for (i in 1:length(mycorpus)) {
  temp <- paste(mycorpus[[i]]$content, collapse = ' ')
  doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
}
temp_frequency <- as.matrix(smtpmodel)
freq_matrix <- data.frame(ST = colnames(temp_frequency), Freq = colSums(temp_frequency))
rm(temp_frequency)

## Convert to json
json_lda <- createJSON(phi = phi, theta = theta,
                       vocab = vocab,
                       doc.length = doc_length,
                       term.frequency = freq_matrix$Freq)

serVis(json_lda)


# Get Sentiment Scores
#to remove emojis
skip_tweetsDF$text <- iconv(skip_tweetsDF$text, 'UTF-8', 'ASCII')

# Get Sentiment Analysis
skip_Sentiment = get_nrc_sentiment(skip_tweetsDF$text)

#### Visualization for Overall Sentiment for Skip

sentTotals = data.frame(colSums(skip_Sentiment))
names(sentTotals) = "count"
sentTotals = cbind("sentiment" = rownames(sentTotals), sentTotals)
rownames(sentTotals) = NULL
ggplot(data = sentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Trump Tweets")
