### Exercise in Supervised Machine Learning
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



