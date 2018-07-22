#Approach-2#
#subsetting transaction data according to 4 retail stores
retail=read.csv("TransactionData.csv",header=TRUE)
unique(retail$StoreID)
retail1=subset(retail,StoreID==1521)
retail2=subset(retail,StoreID==1522)
retail3=subset(retail,StoreID==1542)
retail4=subset(retail,StoreID==1558)

#Evaluating store No. 1521
retail1$purchase=grepl("\\d+", retail1$TransactionID, fixed=TRUE)
retail1$purchase.invoice=ifelse(retail1$purchase=="TRUE", 0, 1)
retail1$recency=retail1$Week-614
customer1.invoices=subset(retail1,select=c("TransactionID","CustomerID","purchase.invoice"))
customer1.invoices=customer1.invoices[!duplicated(customer1.invoices),]
total1.invoices=aggregate(purchase.invoice ~ CustomerID, data=customer1.invoices, FUN=sum, na.rm=TRUE)
recency1=aggregate(recency~CustomerID,data=retail1,FUN=max)
recency1=recency1$recency

#Evaluating store No. 1522
retail2$purchase=grepl("\\d+", retail2$TransactionID, fixed=TRUE)
retail2$purchase.invoice=ifelse(retail2$purchase=="TRUE", 0, 1)
retail2$recency=retail2$Week-614
customer2.invoices=subset(retail2,select=c("TransactionID","CustomerID","purchase.invoice"))
customer2.invoices=customer2.invoices[!duplicated(customer2.invoices),]
total2.invoices=aggregate(purchase.invoice ~ CustomerID, data=customer2.invoices, FUN=sum, na.rm=TRUE)
recency2=aggregate(recency~CustomerID,data=retail2,FUN=max)
recency2=recency2$recency

#Evaluating store No. 1542
retail3$purchase=grepl("\\d+", retail3$TransactionID, fixed=TRUE)
retail3$purchase.invoice=ifelse(retail3$purchase=="TRUE", 0, 1)
retail3$recency=retail3$Week-614
customer3.invoices=subset(retail3,select=c("TransactionID","CustomerID","purchase.invoice"))
customer3.invoices=customer3.invoices[!duplicated(customer3.invoices),]
total3.invoices=aggregate(purchase.invoice ~ CustomerID, data=customer3.invoices, FUN=sum, na.rm=TRUE)
recency3=aggregate(recency~CustomerID,data=retail3,FUN=max)
recency3=recency3$recency

#Evaluating store No. 1558
retail4$purchase=grepl("\\d+", retail4$TransactionID, fixed=TRUE)
retail4$purchase.invoice=ifelse(retail4$purchase=="TRUE", 0, 1)
retail4$recency=retail4$Week-614
customer4.invoices=subset(retail4,select=c("TransactionID","CustomerID","purchase.invoice"))
customer4.invoices=customer4.invoices[!duplicated(customer4.invoices),]
total4.invoices=aggregate(purchase.invoice ~ CustomerID, data=customer4.invoices, FUN=sum, na.rm=TRUE)
recency4=aggregate(recency~CustomerID,data=retail4,FUN=max)
recency4=recency4$recency


#Plotting data for each case
frequency1=total1.invoices$purchase.invoice
df1=as.data.frame(cbind(frequency1,recency1))
frequency2=total2.invoices$purchase.invoice
df2=as.data.frame(cbind(frequency2,recency2))
frequency3=total3.invoices$purchase.invoice
df3=as.data.frame(cbind(frequency3,recency3))
frequency4=total4.invoices$purchase.invoice
df4=as.data.frame(cbind(frequency4,recency4))
library(ggplot2)
library(grid)
library(gridExtra)
scatter.1=ggplot(df1,aes(x=frequency1,y=recency1))+geom_point()
scatter.2=ggplot(df2,aes(x=frequency2,y=recency2))+geom_point()
scatter.3=ggplot(df3,aes(x=frequency3,y=recency3))+geom_point()
scatter.4=ggplot(df4,aes(x=frequency4,y=recency4))+geom_point()
grid.arrange(scatter.1,scatter.2,scatter.3,scatter.4,ncol=2)


#log-transforming data
frequency1.log=log(frequency1)
recency1.log=log(recency1)
df1.log=as.data.frame(cbind(frequency1.log,recency1.log))
frequency2.log=log(frequency2)
recency2.log=log(recency2+0.1)
df2.log=as.data.frame(cbind(frequency2.log,recency2.log))
frequency3.log=log(frequency3)
recency3.log=log(recency3)
df3.log=as.data.frame(cbind(frequency3.log,recency3.log))
frequency4.log=log(frequency4)
recency4.log=log(recency4)
df4.log=as.data.frame(cbind(frequency4.log,recency4.log))
scatterlog.1=ggplot(df1.log,aes(x=frequency1.log,y=recency1.log))+geom_point()
scatterlog.2=ggplot(df2.log,aes(x=frequency2.log,y=recency2.log))+geom_point()
scatterlog.3=ggplot(df3.log,aes(x=frequency3.log,y=recency3.log))+geom_point()
scatterlog.4=ggplot(df4.log,aes(x=frequency4.log,y=recency4.log))+geom_point()
grid.arrange(scatterlog.1,scatterlog.2,scatterlog.3,scatterlog.4,ncol=2)

#scaling the log-transformed data
frequency1.scale=scale(frequency1.log,center=TRUE,scale=TRUE)
recency1.scale=scale(recency1.log,center=TRUE,scale=TRUE)
df1.scale=as.data.frame(cbind(frequency1.scale,recency1.scale))
colnames(df1.scale)=c("scaled frequency","scaled recency")
frequency2.scale=scale(frequency2.log,center=TRUE,scale=TRUE)
recency2.scale=scale(recency2.log,center=TRUE,scale=TRUE)
df2.scale=as.data.frame(cbind(frequency2.scale,recency2.scale))
colnames(df2.scale)=c("scaled frequency","scaled recency")
frequency3.scale=scale(frequency3.log,center=TRUE,scale=TRUE)
recency3.scale=scale(recency3.log,center=TRUE,scale=TRUE)
df3.scale=as.data.frame(cbind(frequency3.scale,recency3.scale))
colnames(df3.scale)=c("scaled frequency","scaled recency")
frequency4.scale=scale(frequency4.log,center=TRUE,scale=TRUE)
recency4.scale=scale(recency4.log,center=TRUE,scale=TRUE)
df4.scale=as.data.frame(cbind(frequency4.scale,recency4.scale))
colnames(df4.scale)=c("scaled frequency","scaled recency")
scatterscale.1=ggplot(df1.scale,aes(x=frequency1.scale,y=recency1.scale))+geom_point()
scatterscale.2=ggplot(df2.scale,aes(x=frequency2.scale,y=recency2.scale))+geom_point()
scatterscale.3=ggplot(df3.scale,aes(x=frequency3.scale,y=recency3.scale))+geom_point()
scatterscale.4=ggplot(df4.scale,aes(x=frequency4.scale,y=recency4.scale))+geom_point()
grid.arrange(scatterscale.1,scatterscale.2,scatterscale.3,scatterscale.4,ncol=2)

#customer segmentation for each of the retail stores
library(dplyr)
set.seed(123)
kmax=15
twss1=sapply(2:kmax,function(k){kmeans(df1.scale,k,nstart=20, iter.max=1000000)$tot.withinss})
twss1
km.res1=kmeans(df1.scale,4,nstart=25)
library("factoextra")
fviz_cluster(km.res1,data=df1.scale,ellipse.type="convex",palette="jco",ggtheme=theme_minimal())


set.seed(123)
kmax=15
twss2=sapply(2:kmax,function(k){kmeans(df2.scale,k,nstart=20, iter.max=1000000)$tot.withinss})
twss2
km.res2=kmeans(df2.scale,4,nstart=25)
fviz_cluster(km.res2,data=df2.scale,ellipse.type="convex",palette="jco",ggtheme=theme_minimal())

set.seed(123)
kmax=15
twss3=sapply(2:kmax,function(k){kmeans(df3.scale,k,nstart=20, iter.max=1000000)$tot.withinss})
twss3
km.res3=kmeans(df3.scale,4,nstart=25)
fviz_cluster(km.res3,data=df3.scale,ellipse.type="convex",palette="jco",ggtheme=theme_minimal())


set.seed(123)
kmax=15
twss4=sapply(2:kmax,function(k){kmeans(df4.scale,k,nstart=20, iter.max=1000000)$tot.withinss})
twss4
km.res4=kmeans(df4.scale,4,nstart=25)
fviz_cluster(km.res4,data=df4.scale,ellipse.type="convex",palette="jco",ggtheme=theme_minimal())

par(mfrow=c(2,2))
plot(2:kmax,twss1,type="b", pch = 19, xlab="Number of clusters K", ylab="Total within-clusters sum of squares")
plot(2:kmax,twss2,type="b", pch = 19, xlab="Number of clusters K", ylab="Total within-clusters sum of squares")
plot(2:kmax,twss3,type="b", pch = 19, xlab="Number of clusters K", ylab="Total within-clusters sum of squares")
plot(2:kmax,twss4,type="b", pch = 19, xlab="Number of clusters K", ylab="Total within-clusters sum of squares")

grid.arrange(
  fviz_cluster(km.res1,data=df1.scale,ellipse.type="convex",palette="jco",ggtheme=theme_minimal()),
  fviz_cluster(km.res2,data=df2.scale,ellipse.type="convex",palette="jco",ggtheme=theme_minimal()),
  fviz_cluster(km.res3,data=df3.scale,ellipse.type="convex",palette="jco",ggtheme=theme_minimal()),
  fviz_cluster(km.res4,data=df4.scale,ellipse.type="convex",palette="jco",ggtheme=theme_minimal()),ncol=2)

#identifying cluster members
which(km.res1$cluster==2)
which(km.res2$cluster==3)
which(km.res3$cluster==1)
which(km.res4$cluster==3)

#Generating association rules
library(plyr)
itemList1=ddply(retail1,c("TransactionID"), function(t1)paste(t1$ItemType, collapse = ","))
itemList1$TransactionID=NULL
colnames(itemList1)=NULL
write.csv(itemList1,"marketbasket1.csv", quote = FALSE, row.names =FALSE)

library(tidyverse)
library(readxl)
library(knitr)
library(lubridate)
library(arules)
library(arulesViz)

tr1=read.transactions('marketbasket1.csv',format='basket',sep=',')
tr1
summary(tr1)
itemFrequencyPlot(tr1, topN=20, type='absolute')
rules1=apriori(tr1, parameter = list(supp=0.001, conf=0.80))
rules1=sort(rules1, by='lift', decreasing = TRUE)
summary(rules1)
inspect(rules1[1:10])
toprules1=rules1[1:10]
plot(toprules1,method="graph")


itemList2=ddply(retail2,c("TransactionID"), function(t1)paste(t1$ItemType, collapse = ","))
itemList2$TransactionID=NULL
colnames(itemList2)=NULL
write.csv(itemList2,"marketbasket2.csv", quote = FALSE, row.names =FALSE)

tr2=read.transactions('marketbasket2.csv',format='basket',sep=',')
tr2
summary(tr2)
itemFrequencyPlot(tr2, topN=20, type='absolute')
rules2=apriori(tr2, parameter = list(supp=0.001, conf=0.80))
rules2=sort(rules2, by='lift', decreasing = TRUE)
summary(rules2)
inspect(rules2[1:10])
toprules2=rules2[1:10]
plot(toprules2,method="graph")


itemList3=ddply(retail3,c("TransactionID"), function(t1)paste(t1$ItemType, collapse = ","))
itemList3$TransactionID=NULL
colnames(itemList3)=NULL
write.csv(itemList3,"marketbasket3.csv", quote = FALSE, row.names =FALSE)

tr3=read.transactions('marketbasket3.csv',format='basket',sep=',')
tr3
summary(tr3)
itemFrequencyPlot(tr3, topN=20, type='absolute')
rules3=apriori(tr3, parameter = list(supp=0.001, conf=0.80))
rules3=sort(rules3, by='lift', decreasing = TRUE)
summary(rules3)
inspect(rules3[1:10])
toprules3=rules3[1:10]
plot(toprules3,method="graph")


itemList4=ddply(retail4,c("TransactionID"), function(t1)paste(t1$ItemType, collapse = ","))
itemList4$TransactionID=NULL
colnames(itemList4)=NULL
write.csv(itemList4,"marketbasket4.csv", quote = FALSE, row.names =FALSE)

tr4=read.transactions('marketbasket4.csv',format='basket',sep=',')
tr4
summary(tr4)
itemFrequencyPlot(tr4, topN=20, type='absolute')
rules4=apriori(tr4, parameter = list(supp=0.001, conf=0.80))
rules4=sort(rules4, by='lift', decreasing = TRUE)
summary(rules4)
inspect(rules4[1:10])
toprules4=rules4[1:10]
plot(toprules4,method="graph")

par(mfrow=c(2,2))
itemFrequencyPlot(tr1, topN=20, type='absolute',col="blue")
itemFrequencyPlot(tr2, topN=20, type='absolute',col="blue")
itemFrequencyPlot(tr3, topN=20, type='absolute',col="blue")
itemFrequencyPlot(tr4, topN=20, type='absolute',col="blue")

par(mfrow=c(2,2))
plot(toprules1,method="graph")
plot(toprules2,method="graph")
plot(toprules3,method="graph")
plot(toprules4,method="graph")

#Finding coupons and demographics
coupon1=retail1$CouponOrigin
coupon1=coupon1[coupon1!=0]
count1=table(coupon1)
count1
coupon2=retail2$CouponOrigin
coupon2=coupon2[coupon2!=0]
count2=table(coupon2)
count2
coupon3=retail3$CouponOrigin
coupon3=coupon3[coupon3!=0]
count3=table(coupon3)
count3
coupon4=retail4$CouponOrigin
coupon4=coupon4[coupon4!=0]
count4=table(coupon4)
count4
par(mfrow=c(2,2))
barplot(count1,xlab="Coupon Origin",col="blue")
barplot(count2,xlab="Coupon Origin",col="blue")
barplot(count3,xlab="Coupon Origin",col="blue")
barplot(count4,xlab="Coupon Origin",col="blue")

