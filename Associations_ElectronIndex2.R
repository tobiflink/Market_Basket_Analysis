##############################################
####Discover Associations Between Products####
##############################################


#Data Upload

library(arules)

library(arulesViz)

library(caTools)

library(lifecycle)

setwd("/Users/tobiadelokun/Documents/Ubiqum/Learning R")

read.transactions("/Users/tobiadelokun/Documents/Ubiqum/Learning R/DA_2_Task4/Dataset/ElectronidexTransactions2017_types.csv", sep = ",")

TransType<-read.transactions("/Users/tobiadelokun/Documents/Ubiqum/Learning R/DA_2_Task4/Dataset/ElectronidexTransactions2017_types.csv", sep = ",")

summary(TransType)

###Visualization

inspect(TransType[1:3])

itemFrequency(TransType[,1])

itemFrequencyPlot(TransType, topN=10)

itemFrequencyPlot(TransType,topN=10,type="relative",col=brewer.pal(8,'Pastel2'),main="Relative Product Type Frequency Plot")

itemFrequencyPlot(TransType,support=.10,type="relative",col=brewer.pal(8,'Pastel2'),main="Product Type Support Plot")


image(sample(TransType,20))

image(sample(TransType,100))




#Generating Rules

model1<-apriori(TransType,parameter = list(support=0.1, conf=0.8))

model1

inspect(model1)

#Tuning the parameters

model2<-apriori(TransType,parameter = list(support=0.01, conf=0.8))

model2

model2<-apriori(TransType,parameter = list(support=0.01, conf=0.8,minlen=2))

model2

apriori

summary(model2)

inspect(model2[1:10])

model3<-apriori(TransType,parameter = list(support=0.001, conf=0.8,minlen=2))

model3

inspect(sort(model2, by="lift")[1:10])

model2sorted<-sort(model2, by="lift")

model2sorted

summary(model2sorted)

inspect(model2[1:10])

inspect(model2sorted[1:10])

#Removing redundant rules

subset.rules <- which(colSums(is.subset(model2sorted,)) > 1) 

length(subset.rules)

which(redundant)

is.redundant(model2sorted)


redundant<- is.redundant(model2sorted)

model2.pruned<-model2sorted[!redundant]

summary(model2.pruned)

#Plot

plot(model2.pruned, jitter=0)

model2.ultimate <-sort(model2.pruned, by="lift")

model2.ultimate

inspect(model2.ultimate[1:10])

plot(model2.ultimate, engine = "plotly")

plot(model2.ultimate, method = "matrix", measure = "lift")

plot(model2.ultimate, method = "grouped", control = list(k = 50))

plot(model2.ultimate, method = "graph")

#Plot of 10 rules with highest confidence

top10RulesConf <- head(model2.ultimate, n = 10, by = "confidence")

plot(top10RulesConf, method = "graph",  engine = "htmlwidget")

plot(top10RulesConf, method = "graph")

#Plot of 10 rules with highest lift

top10RulesLift<- head(model2.ultimate, n = 10, by = "lift")

plot(top10RulesLift, method = "graph")

plot(top10RulesLift, method = "paracoord")


