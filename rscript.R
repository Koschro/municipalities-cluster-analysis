setwd("C:/Users/User/Desktop/Stat II")
census<-read.csv2("census.csv")
census$Age<- gsub("^.*?/","",census$Age)
library(freqweights)
library(class)
library(tree)
library(cluster)
library(corrgram)
library(mclust)

hc1<-hclust(dist(census[,-1]),method="complete")
plot(hc1)

hc2<-hclustvfreq(census[,-1],method = "single", metric = "euclidean")
plot(hc2)

mc1$G	#The optimal number of mixture components.
mc1$BIC	#All BIC values.
mc1$bic	#Optimal BIC value.
mc1$loglik	#The loglikelihood corresponding to the optimal BIC.
mc1$df	 #The number of estimated parameters.
mc1$parameters	#A list with the following components:
mc1$pro #A vector whose kth component is the mixing proportion for the kth component 
mc1$mean #The mean for each component. 
mc1$variance  #A list of variance parameters for the model. 
mc1$z	 #posterior probabilities.
mc1$classification	 #map(z): The classification corresponding to z.
mc1$uncertainty	#The uncertainty associated with the classification.

summary(mc1)

cen_euc<-dist(census[,-1],method='euclidean')
cen_euc_ward<-hclust(cen_euc,method='ward.D')
cen_groups<-cutree(cen_euc_ward,k=3)
table(actual=census$class,predicted=cen_groups)

cen.km<-kmeans(census[,-1],5,nstart=25)
cen.km$size
cen.km$centers

cen_euc_ward<-hclust(cen_euc,method='ward.D')
plot(cen_euc_ward)
rect.hclust(cen_euc_ward,k=6,border="red")