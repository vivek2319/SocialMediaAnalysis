### Scraping Twitter Data in R and Text Analysis

library(twitteR)
library(ROAuth)
library(httr)
library(plyr)
library(tm)
library(rpart)
library(rpart.plot)


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



# top 3000 tweets that contain search term
skip_tweets = searchTwitter("nike", n = 1000) 
smith_tweets = searchTwitter("adidas", n = 1000) 

#convert raw tweet collection to dataframe
skip_tweetsDF = twListToDF(skip_tweets) 
smith_tweetsDF = twListToDF(smith_tweets) 


# 
sk = skip_tweetsDF$text
sm = smith_tweetsDF$text


###  Text Preprocessing for Twitter for Smith
TextPreprocessing = lapply(sm, function(x) {
  
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

smith.dtm = DocumentTermMatrix(mycorpus)


#### Labeling the Tweets as Smith 

dtm.m = as.matrix(smith.dtm)
dtm.df = as.data.frame(dtm.m, stringsAsFactors=FALSE)
classified = cbind(dtm.df, rep("smith", nrow(dtm.df)))
colnames(classified)[ncol(classified)]="classifier"
smith.df = classified


####
###  Text Preprocessing for Twitter for Smith
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


#### Labeling the Tweets as Smith 

dtm.m = as.matrix(skip.dtm)
dtm.df = as.data.frame(dtm.m, stringsAsFactors=FALSE)
classified = cbind(dtm.df, rep("skip", nrow(dtm.df)))
colnames(classified)[ncol(classified)]="classifier"
skip.df = classified

###Combine the two dataframes by row
combined = rbind.fill(skip.df, smith.df)


### Transform all NAs to Zero in Dataframe
dtm_total = combined
dtm_total[is.na(dtm_total)]=0

# Conditional Inference Tree for Mileage
library(party)
library(rpart)


## Create Training Data Set

trainmodel = sample(nrow(dtm_total), ceiling(nrow(dtm_total)*0.7))

testmodel = (1:nrow(dtm_total)) [-trainmodel]

traindata = dtm_total[trainmodel, ]
testdata = dtm_total[testmodel, ]

traindata = data.frame(traindata)
testdata = data.frame(testdata)
## Running a Supervised Machine Learning Analysis (Classification Tree)

mintest = rpart(classifier~., data = traindata, method = "class")

pred = predict(mintest, newdata=testdata, type = "class")
mc = table(pred, testdata$classifier)
err = 1.0 - (mc[1,1]+mc[2,2])/sum(mc)
mc
err

library(rpart.plot)
rpart.plot(mintest, type = 2, extra = 1, main="Smith vs. Skip without Sentiment Scores")

## Reading the Classification Tree or Decision Tree
## For the condition, YES is always to the left, NO is always to the right


printcp(mintest) # display the results 
plotcp(mintest) # visualize cross-validation results 
summary(mintest) # Summary



#to remove emojis
smith_tweetsDF$text <- iconv(smith_tweetsDF$text, 'UTF-8', 'ASCII')
skip_tweetsDF$text <- iconv(skip_tweetsDF$text, 'UTF-8', 'ASCII')

# Get Sentiment Analysis
smith_Sentiment = get_nrc_sentiment(smith_tweetsDF$text)
skip_Sentiment = get_nrc_sentiment(skip_tweetsDF$text)


skip_total = cbind(skip.df, skip_Sentiment)
smith_total = cbind(smith.df, smith_Sentiment)

### Return to run Classification Analysis above with mySentiment combined with 


#### Visualization for Overall Sentiment for Skip

sentTotals = data.frame(colSums(skip_Sentiment))
names(sentTotals) = "count"
sentTotals = cbind("sentiment" = rownames(sentTotals), sentTotals)
rownames(sentTotals) = NULL
ggplot(data = sentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Skip Tweets")

#### Visualization for Overall Sentiment for Smith

sentTotals = data.frame(colSums(smith_Sentiment))
names(sentTotals) = "count"
sentTotals = cbind("sentiment" = rownames(sentTotals), sentTotals)
rownames(sentTotals) = NULL
ggplot(data = sentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Smith Tweets")

###Combine the two total dataframes by row

combined_total = rbind.fill(skip_total, smith_total)


### Transform all NAs to Zero in Dataframe
dtm_total = combined_total
dtm_total[is.na(dtm_total)]=0



## Create Training Data Set

trainmodel = sample(nrow(dtm_total), ceiling(nrow(dtm_total)*0.7))

testmodel = (1:nrow(dtm_total)) [-trainmodel]

traindata = dtm_total[trainmodel, ]
testdata = dtm_total[testmodel, ]

traindata = data.frame(traindata)
testdata = data.frame(testdata)

## Running a Supervised Machine Learning Analysis (Classification Tree)

mintest = rpart(classifier~., data = traindata, method = "class")

pred = predict(mintest, newdata=testdata, type = "class")
mc = table(pred, testdata$classifier)
err = 1.0 - (mc[1,1]+mc[2,2])/sum(mc)
mc
err


### Plotting the Results
library(rpart.plot)
rpart.plot(mintest, type = 2, extra = 1, main="Smith vs. Skip")



printcp(mintest) # display the results 
plotcp(mintest) # visualize cross-validation results 
summary(mintest) # Summary



