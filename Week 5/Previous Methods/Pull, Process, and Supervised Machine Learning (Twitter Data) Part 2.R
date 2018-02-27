### Scraping Twitter Data in R and Text Analysis

library(twitteR)
library(ROAuth)
library(httr)
library(sentiment)
library(plyr)
library(tm)

# Set API Keys
api_key <- "BPBQ9S5KC5kqjU5QfiwpzPSFU"
api_secret <- "XkD4m3LTebSiWIVtvnqFdQIGfFCjkLnqDHLSoLkZsIjFa4Hxr7"
access_token <- "832060427852972033-nthFWKgvQg5PVacKqouyUtSAupgMthe"
access_token_secret <- "DrVf0KlZDiAwbTeefTmzeLLtDn3D6UA2DXx6a2MBXBrix"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)



### To get tweets from a particular user

trumptweets = userTimeline('realDonaldTrump', n=1000) # tweets from @realDonaldTrump

hillarytweets = userTimeline('HillaryClinton', n=1000) # tweets from @HillaryClinton


#### Get tweets from Home timeline, mentions, and favorites

HT = homeTimeline(n=15) # get tweets from home timeline

M = mentions(n=15) # get your tweets that were retweeted

favs = favorites("r_programming", n = 10) # tweets a user has favorited


###To extract all tweets with a particular hashtag or user mention

# top 25 tweets that contain search term
tweets <- searchTwitter("trump", n = 8000) 

# more info about tweets, convert to datafram
tweetsDF <- twListToDF(tweets) 
names(tweetsDF)

# conversion from list to data frame
trumptweets.df = do.call(rbind, lapply(trumptweets, as.data.frame))
hillarytweets.df = do.call(rbind, lapply(hillarytweets, as.data.frame))
bernietweets.df = do.call(rbind, lapply(bernietweets, as.data.frame))

# write to csv; fill in the … with a valid path
write.csv(trumptweets.df, "C:/…/trumptweets.csv")
write.csv(hillarytweets.df, "C:/…/hilllarytweets.csv")


### create an object for just the tweets
p = sapply(trumptweets, function(x) x$getText())

# or this code will also work
p = tweetsDF$text
p = trumptweets.df$text
p = hillarytweets.df$text
p = bernietweets.df$text

###  Text Preprocessing for Twitter
TextPreprocessing = lapply(p, function(x) {
  
  x = gsub('http\\S+\\s*', '', x) ## Remove URLs
  
  x = gsub('\\b+RT', '', x) ## Remove RT
  
  x = gsub('#\\S+', '', x) ## Remove Hashtags
  
  x = gsub('@\\S+', '', x) ## Remove Mentions
  
  x = gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
  
  x = gsub("\\d", '', x) ## Remove Controls and special characters
  
  x = gsub('[[:punct:]]', '', x) ## Remove Punctuations
  
  x = gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
  
  x = gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
  
  x = gsub(' +',' ',x) ## Remove extra whitespaces
  
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

### Transform to Term Document Matrix

trump.tdm = TermDocumentMatrix(mycorpus)

#### Or transform to Document Term Matrix

trump.dtm = DocumentTermMatrix(mycorpus)

# Remove sparse terms from the term-document matrix
trump.tdm.s <-removeSparseTerms(trump.tdm, sparse=0.99)

### Exploratory analysis of our Corpus

## Here we are sorting and counting the col sums of trump.dtm
trump.sums = sort(colSums(as.matrix(trump.dtm)), decreasing=TRUE)

# We will need to create a data frame with the words and their
# frequencies.
trump.csums = data.frame(word=names(trump.sums), freq=trump.sums)
colnames(trump.rsums)

# Install RColorBrewer for coloring our wordcloud
install.packages("RColorBrewer")
require(RColorBrewer)

install.packages("wordcloud")
library("wordcloud")
# RColorBrewer creates nice looking color palettes
# Create a palette, blue to green, and name it palette using brewer.pal

palette = brewer.pal(9, "BuGn")
palette = palette[-(1:2)]

trump_wordcloud <- wordcloud(trump.csums$word, trump.csums$freq,
                            scale=c(7,.2), min.freq=4, max.words=200,
                            random.order=FALSE, colors=palette)

####### Wordcloud finished.

### Further exploration of the corpus

findFreqTerms(trump.dtm, lowfreq=98)

findAssocs(trump.dtm, 'will', 0.15)

#### Labeling the Tweets as Clinton or Trump

dtm.m = as.matrix(trump.dtm)
dtm.df = as.data.frame(dtm.m, stringsAsFactors=FALSE)
classified = cbind(dtm.df, rep("hillary", nrow(dtm.df)))
colnames(classified)[ncol(classified)]="classifier"

str(classified)

trump_final = classified
clinton_final = classified
bernie_final = classified

#Check Clinton vs. Trump Observations


###  Accounting for  Oversampling
trump_final1 = trump_go[sample(1:nrow(trump_go), 205, replace = FALSE),]
obama_final1 = obama_final[sample(1:nrow(obama_final), 100, replace = FALSE),]


###Combine Clinton and Trump Dataframes by Row
combined = rbind.fill(hillary_go, trump_final1)
combined = rbind.fill(combined, bernie_final)

combined = rbind.fill(hillary_go, trump_go)
combined = rbind.fill(combined, bernie_go)

### Transform all NAs to Zero in Dataframe
dtm_total = combined
dtm_total[is.na(dtm_total)]=0

dtm_total$hillary = NULL

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

plot(mintest, uniform=TRUE, 
     main="Clinton vs. Trump")    
text(mintest, use.n=TRUE, all=TRUE, cex=.8)

## Reading the Classification Tree or Decision Tree
## For the condition, YES is always to the left, NO is always to the right


printcp(mintest) # display the results 
plotcp(mintest) # visualize cross-validation results 
summary(mintest) # Summary


## Conditional Inference Tree

fit = ctree(classifier~., data=traindata)
plot(fit, main="Conditional Inference Clinton vs. Trump")

pred = predict(fit, testdata)
mc = table(pred, testdata$classifier)
err = 1.0 - (mc[1,1]+mc[2,2])/sum(mc)
mc
err


### Sentiment Analysis with Twitter Data
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

#to remove emojis
trumptweets.df$text <- iconv(trumptweets.df$text, 'UTF-8', 'ASCII')
hillarytweets.df$text <- iconv(hillarytweets.df$text, 'UTF-8', 'ASCII')

# Get Sentiment Analysis
mySentiment = get_nrc_sentiment(hillarytweets.df$text)
mySentiment = get_nrc_sentiment(trumptweets.df$text)

head(mySentiment)

hillary_go = cbind(clinton_final, mySentiment)
trump_go = cbind(trump_final, mySentiment)

### Return to run Classification Analysis above with mySentiment combined with 


#### Visualization for Overall Sentiment
sentimentTotals = data.frame(colSums(tweets[,c(17:26)]))

sentTotals = data.frame(colSums(mySentiment))
names(sentTotals) = "count"
sentTotals = cbind("sentiment" = rownames(sentTotals), sentTotals)
rownames(sentTotals) = NULL
ggplot(data = sentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")


### Topic Model Analysis

rowTotals = apply(trump.dtm, 1, sum)
tpmodel = trump.dtm[rowTotals>0, ]
tpmodel_tweets = LDA(tpmodel, 8)



terms(tpmodel_tweets , 40)

### Sentiment Analysis of LDA Discovered Topic Models
sent_top=terms(tpmodel_tweets, 50)
sed = t(sent_top)
sed1=apply(sed, 1, paste, collapse=" ")
sent_topv = as.vector(sed1)
mySentiment = get_nrc_sentiment(sent_topv)

this=topics(tpmodel_results)
this2=as.data.frame(this)


## Print to Excel File
library(xlsx)

write.xlsx(dtm_total, "tweets_excel.xlsx")


### Empty rows identifier for Topic Model concerns 
empty.rows <- trump.dtm[rowTotals == 0, ]$dimnames[1][[1]]
mycorpus <- mycorpus[-as.numeric(empty.rows)]
mysen  = mySentiment[-as.numeric(empty.rows)]

### JSON application and Visualization
topicmodels_json_ldavis <- function(tpmodel_tweets, corpus, doc_term){
  ## Required packages
  library(topicmodels)
  library(dplyr)
  library(stringi)
  library(tm)
  library(LDAvis)
  ## Find required quantities
  phi <- posterior(tpmodel_tweets)$terms %>% as.matrix
  theta <- posterior(tpmodel_tweets)$topics %>% as.matrix
  vocab <- colnames(phi)
  doc_length <- vector()
  for (i in 1:length(mycorpus)) {
    temp <- paste(mycorpus[[i]]$content, collapse = ' ')
    doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
  }
  temp_frequency <- as.matrix(trump.dtm)
  freq_matrix <- data.frame(ST = colnames(temp_frequency), Freq = colSums(temp_frequency))
  rm(temp_frequency)
  
  ## Convert to json
  json_lda <- createJSON(phi = phi, theta = theta,
                         vocab = vocab,
                         doc.length = doc_length,
                         term.frequency = freq_matrix$Freq)
  
  return(json_lda)
}



llis.json <- topicmodels_json_ldavis(dtm_LDA_5, mycorpus, dtm3)
serVis(json_lda)
