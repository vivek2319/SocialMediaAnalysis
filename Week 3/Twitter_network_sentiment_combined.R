


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


tweets <- searchTwitter("anthem", n=3000, lang="en")
tw_df <- twListToDF(tweets)

# Labeling our network
classified = cbind(tw_df, rep("anthem", nrow(tw_df)))
colnames(classified)[ncol(classified)]="classifier"

tw_df = classified

tweets2 <- searchTwitter("takeaknee", n=3000, lang="en")
tw_df_2 <- twListToDF(tweets2)


# Labeling our network
classified = cbind(tw_df_2, rep("takeaknee", nrow(tw_df_2)))
colnames(classified)[ncol(classified)]="classifier"

tw_df_2 = classified


#combining Dataframes

total_en = rbind.fill(tw_df_2, tw_df)


## Prepare for sentiment analysis
library(syuzhet)

total_en$text <- iconv(total_en$text, 'UTF-8', 'ASCII')

# Get Sentiment Analysis
comb_sent = get_nrc_sentiment(total_en$text)

#combining Dataframes

total_en_final = cbind(total_en, comb_sent)



edges <- getEdges(data = total_en_final, tweets = "text", source = "screenName", "retweetCount", "anger", "trust", "positive", "negative", "classifier", str.length = 20)
nodes <- getNodes(edges, source = "source", target="target")
g <- graph.data.frame(edges, directed = TRUE, vertices = nodes)


write.graph(g, file="comb_analysis.graphml", format="graphml")

getwd()

write.table(total_en_final, "total_en_final.csv", sep=",", col.names=T)

