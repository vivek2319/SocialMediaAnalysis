


## Packages for Social Network Analysis
library(igraph)
library(graphTweets)

### Scraping Twitter Data in R and Text Analysis

library(twitteR)
library(ROAuth)
library(httr)
library(plyr)
library(tm)
library(topicmodels)


### Sentiment Analysis with Twitter Data
library(syuzhet)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Set API Keys
api_key <- "GlvclrB5Hx9Me1O7aJOnvL43B"
api_secret <- "CcQ065QNvG6S0YKZo8e7XMPaCKI0PAxSTNeOqoEUY4a5CJzzlC"
access_token <- "832060427852972033-xtulDmGMoE63eDiysHPmlatip6G2Uv8"
access_token_secret <- "xgpq9b4hsc1AqNd2YYdBrcfTsFGC4xUKiiYZLSpxKU95y"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)


tweets <- searchTwitter("anthem", n=300, lang="en")
tw_df <- twListToDF(tweets)

# Labeling our network
classified = cbind(tw_df, rep("anthem", nrow(tw_df)))
colnames(classified)[ncol(classified)]="classifier"

tw_df = classified

tweets2 <- searchTwitter("takeaknee", n=300, lang="en")
tw_df_2 <- twListToDF(tweets2)


# Labeling our network
classified = cbind(tw_df_2, rep("takeaknee", nrow(tw_df_2)))
colnames(classified)[ncol(classified)]="classifier"

tw_df_2 = classified


#combining Dataframes

total_en = rbind.fill(tw_df_2, tw_df)

str(total_en)



#### Text Preprocessing
sk = total_en$text

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

mycorpus = tm_map(mycorpus, content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')))

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


### Preparation for Tableau 
## Create dataframe of discovered topics
topic_col=topics(smmodel_tweets)
topic_col2=as.data.frame(topic_col)



### ONLY IF NEEDED (if rows are missing) Empty rows identifier for Topic Model JSON Application 
empty.rows <- skip.dtm[rowTotals == 0, ]$dimnames[1][[1]]

## Find out the missing rows
empty.rows

## Remove empty row identified as "951" from tw_df dataframe 
## For your analysis you may have more than one missing row.
## If you have more than one missing row, the code should look
## as follows
## For example   new.dtm.df = tw_df[-c(951, 224, 301, 501), ]
##Remember if you are not missing any rows, or rather your rows match
## for topic_col2   and tw_df   then you do not need to do this operation
## Just combine the two dataframes using line 14


ix = which(rownames(total_en) %in% c(empty.rows))
clean = total_en[-ix, ]

date_tab = clean$created

### Adding Topic Models to the Total dataframe
new_tw_df_7 = cbind(date_tab, topic_col2)

new_tw_df_8 = cbind(clean, topic_col2)


str(new_tw_df_8)


## Prepare for sentiment analysis
library(syuzhet)

clean$text <- iconv(clean$text, 'UTF-8', 'ASCII')

# Get Sentiment Analysis
comb_sent = get_nrc_sentiment(clean$text)

#combining Dataframes

total_en_final = cbind(new_tw_df_8, comb_sent)





edges <- getEdges(data = total_en_final, tweets = "text", source = "screenName", "retweetCount", "anger", "trust", "positive", "negative", "classifier", "topic_col", str.length = 20)
nodes <- getNodes(edges, source = "source", target="target")
g <- graph.data.frame(edges, directed = TRUE, vertices = nodes)


write.graph(g, file="comb_analysis2.graphml", format="graphml")

getwd()

write.csv(total_en_final, "filefortableau2.csv", row.names = FALSE)
