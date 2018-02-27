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

####Preparation for tm
library(NLP) 

##Chinese text mining package
library(tmcn) 

###Chinese Words Segementation
library(jiebaR)
library(jiebaRD)

# Set API Keys
api_key <- "c5m5fB0UrcD7TcHIYLDjwSCQG"
api_secret <- "wopgSFUkFuJLAM4RFwULlrqCctZekC8eaM1xL6GtoxMqlFx03B"
access_token <- "2924732065-dCosb4RAZO6aEPhRSmGLq6LCkmrMforWeb9VYa7"
access_token_secret <- "He4h00r03o8MRMHkL473g0sKOIQArWh80RdiLyNgmCAIE"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

tweets <- searchTwitter("特朗普", n=500)
tw_df <- twListToDF(tweets)
edges <- getEdges(data = tw_df, tweets = "text", source = "screenName", "retweetCount", str.length = 20)
nodes <- getNodes(edges, source = "source", target="target")
g <- graph.data.frame(edges, directed = TRUE, vertices = nodes)


write.graph(g, file="trump_sim_chi_001_chineseAnal_exp_anal.graphml", format="graphml")

getwd()

#### Can we add topic models ?
pdchina = tw_df$text

###tweets cleaning
TextPreprocessing = lapply(pdchina, function(x) {
  
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
######Chinese language processing
##segmenting words and building corpus
jinping<-lapply(TextPreprocessing,function(x)unlist(segmentCN(x)))
temp<-Corpus(VectorSource(jinping),readerControl=list(reader = readPlain,language = "cn"))
###preprocess data, aka remove redundant stuff 
temp<-tm_map(temp,removePunctuation)
temp<-tm_map(temp,removeNumbers)
removeURL<-function(x) gsub("http[[:alnum:]]*","",x)
temp<-tm_map(temp, removeURL)
temp<-tm_map(temp,stripWhitespace)
###build up matrix for topic model
dtm.xi<-DocumentTermMatrix(temp)


### Topic Model Analysis
rowTotals = apply(dtm.xi, 1, sum)
smtpmodel = dtm.xi[rowTotals>0, ]
smmodel_tweets = LDA(smtpmodel, 5)
terms(smmodel_tweets , 40)

topic_col=topics(smmodel_tweets)
topic_col2=as.data.frame(topic_col)



### ONLY IF NEEDED (if rows are missing) Empty rows identifier for Topic Model JSON Application 
empty.rows <- dtm.xi[rowTotals == 0, ]$dimnames[1][[1]]

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

###IF empty.row = NULL

clean = tw_df


### Adding Topic Models to the Total dataframe


new_tw_df_2 = cbind(clean, topic_col2)


str(clean)


head(new_tw_df_2)


### Add to the total dataframe and then the social network graph
edges <- getEdges(data = new_tw_df_2, tweets = "text", source = "screenName", "retweetCount", "favorited", "topic_col", str.length = 20)
nodes <- getNodes(edges)
g_2 <- graph.data.frame(edges, directed = TRUE, vertices = nodes)

write.graph(g_2, "trump_simchi_001_gephi_topics3.graphml", format="graphml")
getwd()

###Prepare for Tableau
write.csv(new_tw_df_2, "xi_trump_simchi_001_tab.csv", row.names = FALSE)
getwd()

## Prepare for sentiment analysis
library(syuzhet)

translated.doc<-readDOC()

tw_df$text <- iconv(translated.doc$text, 'UTF-8', 'ASCII')

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
write.table(new_tw_df_3, "chi_xi_sentimentplus.csv", sep=",", col.names=T)

getwd()

#######In the future

###Dictionary Installation

#Go to http://pinyin.sogou.com/dict/ find and download dictionaries
#Political Science terminology dictionary from http://pinyin.sogou.com/dict/detail/index/1205
InstallDict("/Users/yangyuhao/Desktop/political.scel", dictname = "political_science") 
#"Political New Terms" CPC propaganda terms coined during Xi's presidency from http://pinyin.sogou.com/dict/detail/index/77392 
#E.g. "中华民族伟大复兴的中国梦"："the Chinese dream of great rejuvenation of Chinese nation"
InstallDict("/Users/yangyuhao/Download/Xi.scel", dictname = "Xi_Dict")

###Sentiment analysis package for Chinese is available, go try it!
