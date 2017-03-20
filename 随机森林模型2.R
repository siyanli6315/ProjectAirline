library(randomForest)
library(ggplot2)
library(showtext)
library(scales)
library("latex2exp")
library(plyr)
library(reshape2)
library(grid)
library(grDevices)
showtext.auto(enable=T)

setwd("~/Desktop")
jfkair=read.csv("JFK.csv",header=T,sep=",")
jfkair=jfkair[,-c(8,9,10,28,33,34)]

jfkair$rate1=jfkair$ArrCount/jfkair$OriginCount
jfkair$rate2=jfkair$DepCount/jfkair$sumDepDelay
jfkair$rate12=cut(jfkair$rate1,breaks = c(-1,0.4,0.6,1))
jfkair$rate22=cut(jfkair$rate2,breaks = c(-1,0.4,0.6,1))

jfkair=jfkair[!is.na(jfkair$Maxtemp),]
jfkair=jfkair[!is.na(jfkair$Meantemp),]
jfkair$Maxvisibility[is.na(jfkair$Maxvisibility)]=median(jfkair$Maxvisibility,na.rm=T)
jfkair$Meanvisibility[is.na(jfkair$Meanvisibility)]=median(jfkair$Meanvisibility,na.rm=T)
jfkair$Minvisibility[is.na(jfkair$Minvisibility)]=median(jfkair$Minvisibility,na.rm=T)
jfkair$Rainfall=as.character(jfkair$Rainfall)
jfkair$Rainfall[jfkair$Rainfall=="T"]="100"
jfkair$Rainfall=as.numeric(jfkair$Rainfall)
jfkair$Events=as.character(jfkair$Events)
jfkair$Events[jfkair$Events==""]="orther"
jfkair$Winddirdegrees[is.na(jfkair$Winddirdegrees)]=median(jfkair$Winddirdegrees,na.rm=T)
jfkair$Events=as.factor(jfkair$Events)
jfkair$Year=as.factor(jfkair$Year)
summary(jfkair)
names(jfkair)

jfkair_o=jfkair[,-c(4:7,29,30,32)]
jfkair_d=jfkair[,-c(4:7,29,30,31)]

#对出发的航班建模
set.seed(100)
n=nrow(jfkair_o)
index=sample(1:n,n%/%4)
train=jfkair_o[-index,]
test=jfkair_o[index,]

mod1=randomForest(rate12~.,data=train,importance=T,ntree=300)
#print(mod1)
pdf("起飞模型2.pdf",width = 5,height = 5)
plot(mod1,main="随机森林模型") #ntree=300
dev.off()
rate12_pre=predict(mod1,newdata=test)
mod1_table=table(test$rate12,rate12_pre)
mod1_acc=sum(rate12_pre==test$rate12)/nrow(test)
write.table(rbind(mod1_table,mod1_acc),file="起飞模型2.txt")

mod1_importance=data.frame(importance(mod1))
mod1_importance$v=row.names(mod1_importance)
pfun1=function(){
  tmp1=data.frame(x=mod1_importance$v,y=mod1_importance$MeanDecreaseAccuracy)
  tmp1=tmp1[order(tmp1$y,decreasing=T),]
  tmp1$x=factor(tmp1$x,levels = tmp1$x)
  ph5.1=ggplot(tmp1)+
    geom_bar(aes(x=x,y=y),fill="darkred",stat="identity")+
    labs(y="MeanDecreaseAccuracy")+
    theme(axis.title.x = element_text(size=18),
          axis.title.y = element_blank(),
          axis.text = element_text(size=12))+
    coord_flip()
  
  tmp2=data.frame(x=mod1_importance$v,y=mod1_importance$MeanDecreaseGini)
  tmp2=tmp2[order(tmp2$y,decreasing=T),]
  tmp2$x=factor(tmp2$x,levels = tmp2$x)
  ph5.2=ggplot(tmp2)+
    geom_bar(aes(x=x,y=y),fill="darkred",stat="identity")+
    labs(y="MeanDecreaseGini")+
    theme(axis.title.x = element_text(size=18),
          axis.title.y = element_blank(),
          axis.text = element_text(size=12))+
    coord_flip()
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1,2)))
  vplayout <- function(x,y){
    viewport(layout.pos.row = x, layout.pos.col = y)
  }
  print(ph5.1, vp = vplayout(1,1))
  print(ph5.2, vp = vplayout(1,2))
}
pdf("起飞模型2imp.pdf",width=9,height=5)
pfun1()
dev.off()

##重抽样
set.seed(200)
target=which(jfkair_o$rate12=="(0.6,1]")
sam=jfkair_o[sample(target,400),]
train=rbind(train,sam)

mod1=randomForest(rate12~.,data=train,importance=T,ntree=300)
print(mod1)
pdf("起飞模型2.1.pdf",width = 5,height = 5)
plot(mod1,main="随机森林模型") #ntree=300
dev.off()
rate12_pre=predict(mod1,newdata=test)
mod1_table=table(test$rate12,rate12_pre)
mod1_acc=sum(rate12_pre==test$rate12)/nrow(test)
write.table(rbind(mod1_table,mod1_acc),file="起飞模型2.1.txt")

mod1_importance=data.frame(importance(mod1))
mod1_importance$v=row.names(mod1_importance)
pfun1=function(){
  tmp1=data.frame(x=mod1_importance$v,y=mod1_importance$MeanDecreaseAccuracy)
  tmp1=tmp1[order(tmp1$y,decreasing=T),]
  tmp1$x=factor(tmp1$x,levels = tmp1$x)
  ph5.1=ggplot(tmp1)+
    geom_bar(aes(x=x,y=y),fill="darkred",stat="identity")+
    labs(y="MeanDecreaseAccuracy")+
    theme(axis.title.x = element_text(size=18),
          axis.title.y = element_blank(),
          axis.text = element_text(size=12))+
    coord_flip()
  
  tmp2=data.frame(x=mod1_importance$v,y=mod1_importance$MeanDecreaseGini)
  tmp2=tmp2[order(tmp2$y,decreasing=T),]
  tmp2$x=factor(tmp2$x,levels = tmp2$x)
  ph5.2=ggplot(tmp2)+
    geom_bar(aes(x=x,y=y),fill="darkred",stat="identity")+
    labs(y="MeanDecreaseGini")+
    theme(axis.title.x = element_text(size=18),
          axis.title.y = element_blank(),
          axis.text = element_text(size=12))+
    coord_flip()
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1,2)))
  vplayout <- function(x,y){
    viewport(layout.pos.row = x, layout.pos.col = y)
  }
  print(ph5.1, vp = vplayout(1,1))
  print(ph5.2, vp = vplayout(1,2))
}
pdf("起飞模型2.1imp.pdf",width=9,height=5)
pfun1()
dev.off()

#对到达的航班建模
set.seed(100)
n=nrow(jfkair_d)
index=sample(1:n,n%/%4)
train=jfkair_d[-index,]
test=jfkair_d[index,]
mod1=randomForest(rate22~.,data=train,importance=T,ntree=300)
print(mod1)
pdf("到达模型2.pdf",width = 5,height = 5)
plot(mod1,main="随机森林模型") #ntree=300
dev.off()
rate22_pre=predict(mod1,newdata=test)
mod1_table=table(test$rate22,rate22_pre)
mod1_acc=sum(rate22_pre==test$rate22)/nrow(test)
write.table(rbind(mod1_table,mod1_acc),file="到达模型2.txt")

mod1_importance=data.frame(importance(mod1))
mod1_importance$v=row.names(mod1_importance)
pfun1=function(){
  tmp1=data.frame(x=mod1_importance$v,y=mod1_importance$MeanDecreaseAccuracy)
  tmp1=tmp1[order(tmp1$y,decreasing=T),]
  tmp1$x=factor(tmp1$x,levels = tmp1$x)
  ph5.1=ggplot(tmp1)+
    geom_bar(aes(x=x,y=y),fill="darkred",stat="identity")+
    labs(y="MeanDecreaseAccuracy")+
    theme(axis.title.x = element_text(size=18),
          axis.title.y = element_blank(),
          axis.text = element_text(size=12))+
    coord_flip()
  
  tmp2=data.frame(x=mod1_importance$v,y=mod1_importance$MeanDecreaseGini)
  tmp2=tmp2[order(tmp2$y,decreasing=T),]
  tmp2$x=factor(tmp2$x,levels = tmp2$x)
  ph5.2=ggplot(tmp2)+
    geom_bar(aes(x=x,y=y),fill="darkred",stat="identity")+
    labs(y="MeanDecreaseGini")+
    theme(axis.title.x = element_text(size=18),
          axis.title.y = element_blank(),
          axis.text = element_text(size=12))+
    coord_flip()
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1,2)))
  vplayout <- function(x,y){
    viewport(layout.pos.row = x, layout.pos.col = y)
  }
  print(ph5.1, vp = vplayout(1,1))
  print(ph5.2, vp = vplayout(1,2))
}
pdf("到达模型2imp.pdf",width=9,height=5)
pfun1()
dev.off()

##重抽样
set.seed(200)
target=which(jfkair_d$rate22=="(0.6,1]")
sam=jfkair_d[sample(target,1500,replace = T),]
train=rbind(train,sam)

mod1=randomForest(rate22~.,data=train,importance=T,ntree=300)
print(mod1)
pdf("到达模型2.1.pdf",width = 5,height = 5)
plot(mod1,main="随机森林模型") #ntree=300
dev.off()
rate22_pre=predict(mod1,newdata=test)
mod1_table=table(test$rate22,rate22_pre)
mod1_acc=sum(rate22_pre==test$rate22)/nrow(test)
write.table(rbind(mod1_table,mod1_acc),file="到达模型2.1.txt")

mod1_importance=data.frame(importance(mod1))
mod1_importance$v=row.names(mod1_importance)
pfun1=function(){
  tmp1=data.frame(x=mod1_importance$v,y=mod1_importance$MeanDecreaseAccuracy)
  tmp1=tmp1[order(tmp1$y,decreasing=T),]
  tmp1$x=factor(tmp1$x,levels = tmp1$x)
  ph5.1=ggplot(tmp1)+
    geom_bar(aes(x=x,y=y),fill="darkred",stat="identity")+
    labs(y="MeanDecreaseAccuracy")+
    theme(axis.title.x = element_text(size=18),
          axis.title.y = element_blank(),
          axis.text = element_text(size=12))+
    coord_flip()
  
  tmp2=data.frame(x=mod1_importance$v,y=mod1_importance$MeanDecreaseGini)
  tmp2=tmp2[order(tmp2$y,decreasing=T),]
  tmp2$x=factor(tmp2$x,levels = tmp2$x)
  ph5.2=ggplot(tmp2)+
    geom_bar(aes(x=x,y=y),fill="darkred",stat="identity")+
    labs(y="MeanDecreaseGini")+
    theme(axis.title.x = element_text(size=18),
          axis.title.y = element_blank(),
          axis.text = element_text(size=12))+
    coord_flip()
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1,2)))
  vplayout <- function(x,y){
    viewport(layout.pos.row = x, layout.pos.col = y)
  }
  print(ph5.1, vp = vplayout(1,1))
  print(ph5.2, vp = vplayout(1,2))
}
pdf("到达模型2.1imp.pdf",width=9,height=5)
pfun1()
dev.off()