##Youtube Analysis  April 18th 2017 Tuesday


###

install_github("soodoku/tuber", build_vignettes = TRUE)

## remember to check the tuber box in your package library
## Create Youtube API and get credentials


yt_oauth("567621468460-ca309j3do4eno5k422km6i2fml7spf6q.apps.googleusercontent.com", "6O2hSzIdapTemvcBBIgLXVJY", token = '')



## get stats of a video

get_stats(video_id="_RvfzFv3c6Y")

##  Get Information About a Video

get_video_details(video_id="_RvfzFv3c6Y")


## Get Caption of a Video

get_captions(video_id="_RvfzFv3c6Y")



## Video Search with Keyword
res <- yt_search("Donald Trump")
head(res[, 1:3])


head(res$title)



### Get Sentiment Analysis of SNL Video

res$title <- iconv(res$title, 'UTF-8', 'ASCII')

# Get Sentiment Analysis
skip_Sentiment = get_nrc_sentiment(res$title)

#### Visualization for Overall Sentiment for Skip

sentTotals = data.frame(colSums(skip_Sentiment))
names(sentTotals) = "count"
sentTotals = cbind("sentiment" = rownames(sentTotals), sentTotals)
rownames(sentTotals) = NULL
ggplot(data = sentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for Obama Related Title Videos")









### Get the comments of video 
res <- get_comment_threads(c(video_id="_RvfzFv3c6Y"), max_results = 100)
head(res)





#### Can we add topic models ?
sk = res$textDisplay

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


## Find frequent terms

findFreqTerms(skip.dtm, 3)

## Find Associations
findAssocs(skip.dtm, "jew", 0.4)



### Get Sentiment Analysis of SNL Video

res$textDisplay <- iconv(res$textDisplay, 'UTF-8', 'ASCII')

# Get Sentiment Analysis
skip_Sentiment = get_nrc_sentiment(res$textDisplay)

#### Visualization for Overall Sentiment for Skip

sentTotals = data.frame(colSums(skip_Sentiment))
names(sentTotals) = "count"
sentTotals = cbind("sentiment" = rownames(sentTotals), sentTotals)
rownames(sentTotals) = NULL
ggplot(data = sentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for SNL Video")

######

####


chan_stat= get_channel_stats(channel_id="UCqFzWxSCi39LnW1JKFR3efg")


str(chan_stat)



###