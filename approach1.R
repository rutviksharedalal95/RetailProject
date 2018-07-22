#Approach-1#
retail=read.csv("TransactionData.csv",header=TRUE)
library(reshape2)

#Generating item-based dataframe
retail.items=dcast(retail,CustomerID~ItemType,value.var = "UnitsSold",fun.aggregate = sum)
colnames(retail.items)=c("CustomerID","bacon","bbq","butter","catfood","cereal","cleansers","coffee","cook","crackers","detergents","hotdogs","eggs","icecream","nuts","pill","pizza","snack","soap","soft","softdrinks","sugar","tissue","towel","yogurt")
retailfiltered.items=data.frame(retail.items$catfood,retail.items$cereal)
colnames(retailfiltered.items)=c("catfood","cereal")

#Visualizing customer purchasing behavior
library(ggplot2)
scatter.1=ggplot(retailfiltered.items,aes(x=catfood,y=cereal))+geom_point()
scatter.1

#log-transforming the attributes
log.catfood=log(retail.items$catfood+0.1)
log.cereal=log(retail.items$cereal+0.1)
retail.log=data.frame(log.catfood,log.cereal)
plot(log.cereal,log.catfood)
scatter.2=ggplot(retail.log,aes(x=log.catfood,y=log.cereal))+geom_point()
scatter.2

#scaling the log transformed attributes
scale.catfood=scale(log.catfood,center=TRUE,scale=TRUE)
scale.cereal=scale(log.cereal,center=TRUE,scale=TRUE)
plot(scale.catfood,scale.cereal)
retail.scale=data.frame(scale.catfood,scale.cereal)
scatter.3=ggplot(retail.scale,aes(x=scale.catfood,y=scale.cereal))+geom_point()
scatter.3

#Generating customer segments using k-means clustering
set.seed(123)
library(NbClust)
library("factoextra")

#Finding the optimum number of clusters
fviz_nbclust(retail.scale,kmeans, method = "wss")+ geom_vline(xintercept = 3, linetype = 2)

#Visualizing segmentation
retail.cluster=kmeans(retail.scale,3,nstart=20)
fviz_cluster(retail.cluster,data=retail.scale,ellipse.type="convex",palette="jco",ggtheme=theme_minimal())

#Demographics based evaluation
demo=read.csv("DemographicData.csv",header=TRUE)
d1.c=demo[which(retail.cluster$cluster==2),]
d2.c=demo[which(retail.cluster$cluster==3),]
d3.c=demo[which(retail.cluster$cluster==1),]
#Income
d1.income=data.frame(segment="cluster-2",Income=d1.c$Income)
d2.income=data.frame(segment="cluster-3",Income=d2.c$Income)
d3.income=data.frame(segment="cluster-1",Income=d3.c$Income)
plot.data=rbind(d1.income,d2.income,d3.income)
ggplot(plot.data, aes(x=segment, y=Income, fill=segment)) +geom_boxplot()
#MaleAge
d1.maleage=data.frame(segment="cluster-2",mage=d1.c[d1.c$Male.Age.!=7,]$Male.Age.)
d2.maleage=data.frame(segment="cluster-3",mage=d2.c[d2.c$Male.Age.!=7,]$Male.Age.)
d3.maleage=data.frame(segment="cluster-1",mage=d3.c[d3.c$Male.Age.!=7,]$Male.Age.)
plot.data=rbind(d1.maleage,d2.maleage,d3.maleage)
ggplot(plot.data, aes(x=segment, y=mage, fill=segment)) +geom_boxplot()
#FemaleAge
d1.femaleage=data.frame(segment="cluster-2",fmage=d1.c[d1.c$Female.Age.!=7,]$Female.Age.)
d2.femaleage=data.frame(segment="cluster-3",fmage=d2.c[d2.c$Female.Age.!=7,]$Female.Age.)
d3.femaleage=data.frame(segment="cluster-1",fmage=d3.c[d3.c$Female.Age.!=7,]$Female.Age.)
plot.data=rbind(d1.femaleage,d2.femaleage,d3.femaleage)
ggplot(plot.data, aes(x=segment, y=fmage, fill=segment)) +geom_boxplot()
#MaleOccupation
d1.maleocc=data.frame(segment="cluster-2",mocc=d1.c[d1.c$Male.Occupation!=11,]$Male.Occupation)
d2.maleocc=data.frame(segment="cluster-3",mocc=d2.c[d2.c$Male.Occupation!=11,]$Male.Occupation)
d3.maleocc=data.frame(segment="cluster-1",mocc=d3.c[d3.c$Male.Occupation!=11,]$Male.Occupation)
plot.data=rbind(d1.maleocc,d2.maleocc,d3.maleocc)
ggplot(plot.data, aes(x=segment, y=mocc, fill=segment)) +geom_boxplot()
#FemaleOccupation
d1.femaleocc=data.frame(segment="cluster-2",fmocc=d1.c$Female.Occupation)
d2.femaleocc=data.frame(segment="cluster-3",fmocc=d2.c$Female.Occupation)
d3.femaleocc=data.frame(segment="cluster-1",fmocc=d3.c$Female.Occupation)
plot.data=rbind(d1.femaleocc,d2.femaleocc,d3.femaleocc)
ggplot(plot.data, aes(x=segment, y=fmocc, fill=segment)) +geom_boxplot()
#Maleeducation
d1.maleed=data.frame(segment="cluster-2",med=d1.c$Male.Education)
d2.maleed=data.frame(segment="cluster-3",med=d2.c$Male.Education)
d3.maleed=data.frame(segment="cluster-1",med=d3.c$Male.Education)
plot.data=rbind(d1.maleed,d2.maleed,d3.maleed)
ggplot(plot.data, aes(x=segment, y=med, fill=segment)) +geom_boxplot()
#Femaleeducation
d1.femaleed=data.frame(segment="cluster-2",fmed=d1.c$Female.Education)
d2.femaleed=data.frame(segment="cluster-3",fmed=d2.c$Female.Education)
d3.femaleed=data.frame(segment="cluster-1",fmed=d3.c$Female.Education)
plot.data=rbind(d1.femaleed,d2.femaleed,d3.femaleed)
ggplot(plot.data, aes(x=segment, y=fmed, fill=segment)) +geom_boxplot()
#No. of cats
d1.cats=data.frame(segment="cluster-2",cats=d1.c[d1.c$Cats!=9,]$Cats)
d2.cats=data.frame(segment="cluster-3",cats=d2.c[d2.c$Cats!=9,]$Cats)
d3.cats=data.frame(segment="cluster-1",cats=d3.c[d3.c$Cats!=9,]$Cats)
plot.data=rbind(d1.cats,d2.cats,d3.cats)
ggplot(plot.data, aes(x=segment, y=cats, fill=segment)) +geom_boxplot()
#cabeltvsubscription
count1.ct=table(d1.c$Cable.TV)
count2.ct=table(d2.c$Cable.TV)
count3.ct=table(d3.c$Cable.TV)
count1.ct
count2.ct
count3.ct
par(mfrow=c(2,1))
barplot(count1.ct,xlab="cabel tv subscription",col="blue")
barplot(count2.ct,xlab="cabel tv subscription",col="blue")
barplot(count3.ct,xlab="cabel tv subscription",col="blue")
#newspapersubscription
count1.np=table(d1.c$Newspaper.subscriber)
count2.np=table(d2.c$Newspaper.subscriber)
count3.np=table(d3.c$Newspaper.subscriber)
count1.np
count2.np
count3.np
par(mfrow=c(2,1))
barplot(count1.np,xlab="newspaper subscription",col="blue")
barplot(count2.np,xlab="newspaper subscription",col="blue")
barplot(count3.np,xlab="newspaper subscription",col="blue")


