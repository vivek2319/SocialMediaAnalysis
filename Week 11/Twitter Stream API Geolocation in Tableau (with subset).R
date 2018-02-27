

### Let Us Stream Some Data

##  Connect to Twitter Streaming API and return public statuses that
##  match one or more filter predicates.
#
## An example of an authenticated request using the ROAuth package,
## where consumerkey and consumer secret are fictitious.
## You can obtain your own at dev.twitter.com
library(streamR)
library(ROAuth)
library(RCurl)

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "Ce0kq7wWh5or4gqrx70bD7Rlr"
consumerSecret <- "h5VzwuVqRM0ISc50Xr8MaBLZPcvT0Ym3pdywPQUPQQkTGHnTBu"
my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret, requestURL=requestURL,
                             accessURL=accessURL, authURL=authURL)

my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

## not "http" make sure "https"

0961522

## capture tweets sent from New York City in English, searching the term "data" 

filterStream(file.name="refugees_files", track="refugees", follow=NULL,locations=c(-125, 25, -66, 50), timeout=1200, tweets=20000, oauth=my_oauth)


## good for lat long (all 42 variables)


tweets.df = parseTweets("refugees_files")

## Subset for term 'immigrants'

forfun = subset(tweets.df, grepl("refugees", tweets.df$text))

#### Can we add topic models ?
sk = forfun$text

library(plyr)
library(dplyr)


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


library(tm)
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
library(topicmodels)

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
## Just combine the two dataframes using line 144


ix = which(rownames(tweets.df) %in% c(empty.rows))
clean = tweets.df[-ix, ]

### Adding Topic Models to the Total dataframe
new_tw_df = cbind(clean, topic_col2)


head(new_tw_df)



## Prepare for sentiment analysis
library(syuzhet)

clean$text <- iconv(clean$text, 'UTF-8', 'ASCII')

# Get Sentiment Analysis
Sentiment = get_nrc_sentiment(clean$text)


new_tw_df = cbind(new_tw_df, Sentiment)


new_tw_df$place_lat = as.numeric(new_tw_df$place_lat)
new_tw_df$place_lon = as.numeric(new_tw_df$place_lon)

## Write to CSV for Tableau
write.csv(new_tw_df, "refugee_doc.csv", row.names = FALSE)



