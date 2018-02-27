
### Use this code in combination with "Twitter_network_sentiment_combined" from week 7

## Required packages
library(topicmodels)
library(dplyr)
library(stringi)
library(tm)
library(LDAvis)

mycorpus = mycorpus[-as.numeric(empty.rows)]

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