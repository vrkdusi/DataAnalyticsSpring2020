library(dplyr)
library(ggplot2)
library(ggpubr)
library(MASS)
library(randomForest)
library(caret)
library(kernlab)
library(rpart)
library(rpart.plot)
library(class)
library(tidyverse)
library(tibble)

#Importing data
edges = read.csv('paradise_papers.edges.csv')
nodes.address = read.csv('paradise_papers.nodes.address.csv')
nodes.address$note = NULL
nodes.entity = read.csv('paradise_papers.nodes.entity.csv')
nodes.entity$inactivation_date = NULL
nodes.entity$node_id = NULL
nodes.intermediary = read.csv('paradise_papers.nodes.intermediary.csv')
nodes.officer = read.csv('paradise_papers.nodes.officer.csv')
nodes.other = read.csv('paradise_papers.nodes.other.csv')

#Initial Summary and data clean up 
summary(edges)
summary(nodes.address$countries)
summary(nodes.entity$company_type)
summary(nodes.intermediary$countries)
summary(nodes.officer$countries)
table(is.na(nodes.address))
nodes.address = nodes.address[complete.cases(nodes.address),]
table(is.na(nodes.entity))
nodes.entity = nodes.entity[complete.cases(nodes.entity),]
table(is.na(nodes.intermediary))
nodes.intermediary = nodes.intermediary[complete.cases(nodes.intermediary),]
table(is.na(nodes.officer))
nodes.officer = nodes.officer[complete.cases(nodes.officer),]
table(is.na(nodes.other))
nodes.other = nodes.other[complete.cases(nodes.other),]


#Observing which countries have most offshore companies registered
par(las=2) 
par(mar=c(5,8,4,6))
x1 = barplot(table(edges$sourceID), main='Paradise Papers Corporate Registries', ylab='# of Corporate Registries', ylim=range(pretty(c(0, table(edges$sourceID)))), names.arg=c('Unknown', 'Appleby', 'Aruba', 'Bahamas', 'Barbados','Cook Islands', 'Lebanon','Malta','Samoa'), axis.lty=1) #Displays where most companies (entities) are registered
y1 = as.matrix(table(edges$sourceID))
text(x1, y1+0.125e05, labels=as.character(y1))
table(edges$sourceID)

#Observing countries with most intermediary organizations registered
countries_intermediary = data.frame(stringsAsFactors=FALSE, nodes.intermediary$countries)
countries_intermediary = countries_intermediary %>% 
  mutate_all(~ifelse(. %in% c("N/A", "null", ""), NA, .)) %>% 
  na.omit()
countries_intermediary_names = names(table(nodes.intermediary$countries))[2:23]
y2 = barplot(table(countries_intermediary), names.arg=countries_intermediary_names, horiz=T, main = 'Countries with most Intermediary Orgs. Registered', xlim = range(0,300)) #Displays which countries has most intermediaries registered
x2 = as.matrix(table(countries_intermediary))
text(x2 + 10, y2, labels=as.character(x2))
  
#Observing nationalities of Officers involved in scams
countries_officer = data.frame(stringsAsFactors=FALSE, nodes.officer$countries)
countries_officer = countries_officer %>% 
  mutate_all(~ifelse(. %in% c("N/A", "null", ""), NA, .)) %>% 
  na.omit()
countries_officer_names = names(table(nodes.officer$countries))[2:3428]
countries_officer = data.frame(table(countries_officer), names.arg=countries_officer_names)
countries_officer = countries_officer[order(-countries_officer$Freq),]
y3 = barplot(countries_officer$Freq[1:10], names.arg=c('Malta', 'United States', 'United Kingdom', 'Italy', 'Samoa', 'China', 'Hong Kong', 'Bermuda', 'Germany', 'British Virgin Islands'), horiz=T, main='Top 10 Nationalities of Officers', xlim=range(0,50000))
x3 = as.matrix(countries_officer$Freq[1:10])
text(x3+1100, y3, labels=as.character(x3))

#Entity clean up
entity = nodes.entity[-1]
entity = entity[-14]
entity$country_codes = NULL #These rows dont offer anything towards predicting company type 
entity$jurisdiction = NULL
entity$ibcRUC = NULL
entity$incorporation_date = NULL
entity$struck_off_date = NULL
entity$status = NULL
entity$service_provider = NULL
entity$closed_date = NULL
entity = entity[order(entity$company_type),]
entity = entity[270816:290086,] #Remove all empty rows of company type
summary(entity)
entity$jurisdiction_description = as.numeric((factor(entity$jurisdiction_description)))
entity$countries = as.numeric((factor(entity$countries)))
entity$valid_until = as.numeric(factor(entity$valid_until))
entity$sourceID = as.numeric(factor(entity$sourceID))
summary(entity)

#Predicting Types of Companies
set.seed(100)
entity = droplevels(entity)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
entity_norm = as.data.frame(lapply(entity[,c(1,2,4,5)], normalize))
index = sample(1:nrow(entity), 0.025*nrow(entity), replace=FALSE)
entity_train = entity_norm[index,]
dim(entity_train)
entity_test = entity_norm[-index,]
dim(entity_test)
wss <- function(k) {
  kmeans(entity_norm, k, nstart = 10)$tot.withinss
}
k.values = 1:7
wss_values = map_dbl(k.values, wss)
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
KNNpred = knn(train=entity_train, test=entity_test, cl=entity[index,3], k=3)
table(entity$company_type)
table(KNNpred)
mean(KNNpred == entity[index,3])
pred_counts = rbind(table(KNNpred), table(entity$company_type))
y4 = barplot(pred_counts, beside=TRUE, xlab='Freq', xlim = range(0,20000), main='Predicted vs Actual types of Companies', col=c('darkblue', 'red'), horiz=T)
x4 = as.matrix(pred_counts)
text(x4+1000, y4, label=as.character(x4))
legend(10000,5, legend=c('KNN Predicts', 'Actual'), col=c('darkblue', 'red'), lty=1:2, cex=0.5, box.lty=0)

#Predicting Number of Entities per Country
summary(nodes.entity$countries)
entity_countries = nodes.entity[order(nodes.entity$countries),]
entity_countries = entity_countries[120342:290086, ]
entity$country_codes = NULL #These rows dont offer anything towards predicting company type 
entity_countries$jurisdiction = NULL
entity_countries$ibcRUC = NULL
entity_countries$incorporation_date = NULL
entity_countries$struck_off_date = NULL
entity_countries$status = NULL
entity_countries$service_provider = NULL
entity_countries$closed_date = NULL
entity_countries$name = NULL
entity_countries$note = NULL
entity_countries$jurisdiction_description = as.numeric((factor(entity_countries$jurisdiction_description)))
entity_countries$company_type = as.numeric((factor(entity_countries$company_type)))
entity_countries$valid_until = as.numeric(factor(entity_countries$valid_until))
entity_countries$sourceID = as.numeric(factor(entity_countries$sourceID))
entity_countries$country_codes = as.numeric(factor(entity_countries$country_codes))
head(entity_countries)

set.seed(100)
index2 = sample(1:nrow(entity), 0.00625*nrow(entity_countries), replace=FALSE)
entity_countries_norm = as.data.frame(lapply(entity_countries[,c(1,2,4,5,6)], normalize))
entity_countries_train = entity_countries_norm[index2,]
dim(entity_countries_train)
entity_countries_test = entity_countries_norm[-index2,]
dim(entity_countries_test)
entity_countries_train = entity_countries_train[complete.cases(entity_countries_train),]
entity_countries_test = entity_countries_test[complete.cases(entity_countries_test),]
k = sqrt(2/length(entity_countries))
wss <- function(k) {
  kmeans(entity_countries_norm, k, nstart = 10)$tot.withinss
}
k.values = 1:7
wss_values = map_dbl(k.values, wss)
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
KNNpred2 = knn(train=entity_countries_train, test=entity_countries_test, cl=entity_countries[index2,3], k=3)
table(entity_countries$countries)
mean(KNNpred2 == entity_countries[index2,3])
pred_counts2 = data.frame(table(KNNpred2),table(entity_countries$countries))
pred_counts2 = pred_counts2[order(-pred_counts2[2]),]
pred_counts2 = pred_counts2[1:7,]
pred_counts2_names = pred_counts2[,1]
pred_counts2 = rbind(pred_counts2[,2], pred_counts2[,4])
y5 = barplot(pred_counts2, beside=TRUE, xlab='Freq', xlim = range(0,200000), names.arg = pred_counts2_names, main='Predicted vs Actual # of Entities/Country', col=c('darkblue', 'red'), horiz=T)
x5 = as.matrix(pred_counts2)
text(x5+5000, y5, label=as.character(x5))
legend(15000,10, legend=c('KNN Predicts', 'Actual'), col=c('darkblue', 'red'), lty=1:2, cex=0.5, box.lty=0)