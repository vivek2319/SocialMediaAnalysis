### Exercise in Text Exploratory Analysis and Supervised Machine Learning 
# TM Package
# Plyr Package
## Examine the structure of the corpus
str(pres_speeches_1)

#Transform from character variable to factor variable
pres_speeches_1$president = as.factor(pres_speeches_1$president)

wash <- pres_speeches_1[1:6,]
madison <- pres_speeches_1[7:11,]
lincoln <- pres_speeches_1[12:19,]
obama <- pres_speeches_1[30:37,]

Sys.setlocale('LC_ALL','C') 


go = as.vector(lincoln$speech)

str(go)

persplit = unlist(strsplit(go, "[.]"))


mycorpus = Corpus(VectorSource(persplit))

mycorpus = tm_map(mycorpus,
                  content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                  mc.cores=1)

### Transformer all characters to lower case
mycorpus = tm_map(mycorpus, content_transformer(tolower))

### Remove all Punctuation
mycorpus = tm_map(mycorpus, removePunctuation)

### Remove all Numbers
mycorpus = tm_map(mycorpus, removeNumbers)

### Remove Stopwords
mycorpus = tm_map(mycorpus, removeWords, stopwords('english'))

### Transform to Document Term Matrix
dtm = DocumentTermMatrix(mycorpus)

### Print the Document Term Matrix
dtm

### Sparse the Matrix
dtm1 = removeSparseTerms(dtm, 0.99)



dtm.m = as.matrix(dtm1)
dtm.df = as.data.frame(dtm.m, stringsAsFactors=FALSE)
classified = cbind(dtm.df, rep("lincoln", nrow(dtm.df)))
colnames(classified)[ncol(classified)]="classifier"

str(classified)



obama_final = classified
lincoln_final = classified


### Text Visualization - Exploratory Analysis
# inspect frequent words

(freq.terms <- findFreqTerms(dtm1, lowfreq = 35))

#Term Frequency Plot
term.freq <- colSums(as.matrix(dtm1))
term.freq <- subset(term.freq, term.freq >= 20)
df <- data.frame(term = names(term.freq), freq = term.freq)

library(ggplot2)

ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + coord_flip()

# which words are associated with 'union'?
findAssocs(dtm1, "union", 0.01)


# which words are associated with 'will'?
findAssocs(dtm1, "will", 0.1)

# which words are associated with 'national'?
findAssocs(dtm1, "national", 0.1)



## Network Analysis of Content
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")

plot(tdm1, term = freq.terms, corThreshold = 0.01, weighting = T)



## Word Cloud
m <- as.matrix(dtm1)

# calculate the frequency of words and sort it by frequency
word.freq <- sort(colSums(m), decreasing = T)

# colors
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]

# plot word cloud
library(wordcloud)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 1, random.order = F, colors = pal)

# remove sparse terms
dtm2 <- removeSparseTerms(dtm1, sparse = 0.95)
m2 <- as.matrix(dtm1)
mt = t(m2)

# cluster terms
distMatrix <- dist(scale(mt))
fit <- hclust(distMatrix, method = "ward.D")

plot(fit)
rect.hclust(fit, k = 6) # cut tree into 6 clusters research position university analysis network social

### K Means Clustering
m3 <- t(m2) # transpose the matrix to cluster documents 
set.seed(122) # set a fixed random seed
k <- 6 # number of clusters

kmeansResult <- kmeans(m2, k)

round(kmeansResult$centers, digits = 3) # cluster centers

for (i in 1:k) {
  
  cat(paste("cluster ", i, ": ", sep = ""))
  
  s <- sort(kmeansResult$centers[i, ], decreasing = T)
  
  cat(names(s)[1:10], "\n")
  
  # print the tweets of every cluster
  
  # print(tweets[which(kmeansResult£cluster==i)])
}

#### Supervised Machine Learning - Classification Tree


###  Accounting for  Oversampling
lincoln_final1 = lincoln_final[sample(1:nrow(lincoln_final), 600, replace = FALSE),]
obama_final1 = obama_final[sample(1:nrow(obama_final), 600, replace = FALSE),]




combined = rbind.fill(obama_final, lincoln_final)


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

## Running a Supervised Machine Learning Analysis (Classidification Tree)

mintest = rpart(classifier~., data = traindata, method = "class")

pred = predict(mintest, newdata=testdata, type = "class")
mc = table(pred, testdata$classifier)
err = 1.0 - (mc[1,1]+mc[2,2])/sum(mc)
mc
err

plot(mintest, uniform=TRUE, 
     main="Lincoln vs. Obama")    
text(mintest, use.n=TRUE, all=TRUE, cex=.8)

## Reading the Classification Tree or Decision Tree
## For the condition, YES is always to the left, NO is always to the right


printcp(mintest) # display the results 
plotcp(mintest) # visualize cross-validation results 
summary(mintest) # Summary


library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(mytree)

## Conditional Inference Tree

fit = ctree(classifier~., data=traindata)
plot(fit, main="Conditional Inference Lincoln vs. Obama")

pred = predict(fit, testdata)
mc = table(pred, testdata$left)
err = 1.0 - (mc[1,1]+mc[2,2])/sum(mc)
mc
err



