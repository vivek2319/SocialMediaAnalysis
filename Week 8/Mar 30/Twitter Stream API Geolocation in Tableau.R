

### Let Us Stream Some Data

##  Connect to Twitter Streaming API and return public statuses that
##  match one or more filter predicates.

## An example of an authenticated request using the ROAuth package,
## where consumerkey and consumer secret are fictitious.
## You can obtain your own at dev.twitter.com
library(streamR)
library(ROAuth)
library(RCurl)

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "9IRidIHB3H3e4uOtXhMfUF9Nw"
consumerSecret <- "o1WF4ux2DHJgzl1V15Il6eKiVBsVLmpOJyAP4c9stOAouJUKgI"
my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret, requestURL=requestURL,
                             accessURL=accessURL, authURL=authURL)

my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

## not "http" make sure "https"

0961522

## capture tweets sent from New York City in Spanish only, and saving as an object in memory
tweets=filterStream(file.name="ny_english", language="en", track="#marchmadness",
                        locations=c(-74,40,-73,41), timeout=60, oauth=my_oauth)

## no geolocation - just searching for a term, location needs to be included (enabled)
tweets=filterStream(file.name="fun_englisht", language="en", track="#FinalFour", timeout=60, oauth=my_oauth)

## Parse tweets
tweets.df <- parseTweets("fun_english")

## Write to CSV for Tableau
write.table(tweets.df, "stream_tweets_class.csv", sep=",", col.names=T)

### Be sure to go into excel spreadsheet, open, and prepare to import into tableau




## capture tweets mentioning the "rstats" hashtag or sent from New York City

filterStream( file="tweets_rstats.json", locations=c(-74,40,-73,41), timeout=60, oauth=my_oauth )

track="help",

good = parseTweets(tweets, simplify = FALSE, verbose = TRUE)





## Grab date from tw_df$text
mine=as.data.frame(date=index(tw_df$created), coredata(tw_df$created))

mine = as.matrix(tw_df)

### Only use this if rows are missing
mine2 = mine[-as.numeric(empty.rows)]


topic_col2$date = mine

## Write to CSV for Tableau
write.table(topic_col2, "topic_col2.csv", sep=",", col.names=T)









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






## Examine TOpics and Dates Object

str(topic_col2)


### ONLY IF NEEDED (if rows are missing) Empty rows identifier for Topic Model JSON Application 
empty.rows <- skip.dtm[rowTotals == 0, ]$dimnames[1][[1]]
mycorpus <- mycorpus[-as.numeric(empty.rows)]