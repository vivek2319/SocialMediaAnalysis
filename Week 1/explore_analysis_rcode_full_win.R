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


tweets <- searchTwitter("thaad", n=3000, lang="en")
tw_df <- twListToDF(tweets)
edges <- getEdges(data = tw_df, tweets = "text", source = "screenName", "retweetCount", str.length = 20)
nodes <- getNodes(edges, source = "source", target="target")
g <- graph.data.frame(edges, directed = TRUE, vertices = nodes)


write.graph(g, file="exp_anal.graphml", format="graphml")

getwd()



#### Can we add topic models ?
sk = tw_df$text

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


ix = which(rownames(tw_df) %in% c(empty.rows))
clean = tw_df[-ix, ]

date_tab = clean$created

### Adding Topic Models to the Total dataframe
new_tw_df = cbind(date_tab, topic_col2)

new_tw_df_2 = cbind(clean, topic_col2)


str(clean)

head(new_tw_df)

head(new_tw_df_2)


### Add to the total dataframe and then the social network graph
edges <- getEdges(data = new_tw_df_2, tweets = "text", source = "screenName", "retweetCount", "favorited", "topic_col", str.length = 20)
nodes <- getNodes(edges)
g_2 <- graph.data.frame(edges, directed = TRUE, vertices = nodes)

write.graph(g_2, "gephi_topics2.graphml", format="graphml")


## Prepare for sentiment analysis
library(syuzhet)

tw_df$text <- iconv(tw_df$text, 'UTF-8', 'ASCII')

# Get Sentiment Analysis
thaad_Sentiment = get_nrc_sentiment(tw_df$text)

#### Visualization for Overall Sentiment for Thaad related tweets

sentTotals = data.frame(colSums(thaad_Sentiment))
names(sentTotals) = "count"
sentTotals = cbind("sentiment" = rownames(sentTotals), sentTotals)
rownames(sentTotals) = NULL
ggplot(data = sentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for Temer Tweets")


ix = which(rownames(thaad_Sentiment) %in% c(empty.rows))
clean_sent = thaad_Sentiment[-ix, ]


new_tw_df_3 = cbind(clean_sent, new_tw_df)

## Write to CSV for Tableau
write.table(new_tw_df_3, "thaad_df_tab.csv", sep=",", col.names=T)

getwd()

###Now go to excel and move the headers over one column.
###Don't forget to copy and paste over your topic model results

