### Week 5 | February 14-16 | Creating your own Network with your own Data

##  Import adjacency matrix (Excel .csv files) into R

library(igraph)

## Bring up the package and documentation

mydata = read.csv(file.choose(), header=TRUE, row.names=1)
mymatrix = as.matrix(mydata)
myNetwork = graph.adjacency(mymatrix, mode="undirected", diag=FALSE)
mode(mymatrix) ="numeric"
myNetwork
plot(myNetwork)

plot(myNetwork, layout=layout.random)

### Network Measurements in R

betweenness(myNetwork, directed=FALSE)
degree(myNetwork, mode="all")
plot(myNetwork, vertex.size=degree(myNetwork)*8)

plot(myNetwork, layout=layout.reingold.tilford(myNetwork, root="US"), vertex.size=betweenness(myNetwork)*5.5, vertex.color=degree(myNetwork))



#Directed Network in R

mydata = read.csv(file.choose(), header=TRUE, row.names=1)
mymatrix = as.matrix(mydata)
myNetwork = graph.adjacency(mymatrix, mode="directed", diag=FALSE)
mode(mymatrix) ="numeric"
myNetwork
plot(myNetwork)



### Import list of edges --> Excel list

mydata = read.csv(file.choose(), header=FALSE)
myedgenetwork = graph.data.frame(mydata, directed=TRUE)
myedgenetwork
plot(myedgenetwork)


#### Build network in R

my3Network = graph(edges=c("Mike","Jen", "Jen","Sherry", "David","Mike", "Mike","John", "Mike","Bill"), directed=TRUE)

### Network Measurements in R

betweenness(myNetwork, directed=FALSE)
degree(myNetwork, mode="all")
plot(myNetwork, vertex.size=degree(myNetwork)*8)

plot(myNetwork, layout=layout.reingold.tilford(myNetwork, root="US"), vertex.size=betweenness(myNetwork)*1.5, vertex.color=degree(myNetwork))




### Association Rules Package 

library("arulesViz")
library(arules)
# Bring up the Package and the Documentation
data("Groceries")


### Read transactions from desktop by calling and then setting the working directory
getwd()
setwd("/Users/lammbrau/Desktop")

groc = read.transactions("groceries.csv", sep=",")

summary(groc)

inspect(groc)
inspect(groc[1:3])




itemFrequency(groc[,1:6])

itemFrequencyPlot(groc, support=0.20)
itemFrequencyPlot(groc, support=0.10)

itemFrequencyPlot(groc, topN=10)
itemFrequencyPlot(groc, topN=20)

rules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.5, minlen=2))

rules

summary(rules)

inspect(rules[1:10])

inspect(sort(rules, by="lift")[1:10])

plot(rules, method="grouped")

plot(rules, method="grouped", control=list(k=50))

sel <- plot(rules, method="grouped", interactive=TRUE)
##plot sel  


subrules2 <- head(sort(rules, by="lift"), 20)

inspect(subrules2)

plot(subrules2, method="graph")

plot(subrules2, method="graph", control=list(type="itemsets"))

plot(subrules2, method="paracoord", control=list(reorder=TRUE))



detach(package:tm, unload=TRUE)
mike=(head(sort(rules, by = "support"), 125))
mike=(head(sort(rules, by = "confidence"), 75))
mike4=(head(sort(rules, by = "lift"), 75))
plot(mike, method="graph", control=list(type="items"))

plot(mike, method = "group")
plot(mike, method = "graph")
plot(mike, method = "graph", control = list(type="items"))
plot(mike, method = "paracoord", control=list(reorder=TRUE))


