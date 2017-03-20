#JFK2 可视化
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
jfkair=jfkair[,c(1:7)]
names(jfkair)=c("year","month","day","o_count","o_delay","d_delay","d_count")

fun1=function(year){
  index=jfkair$year%in%year
  o_count=sum(jfkair$o_count[index])
  d_count=sum(jfkair$d_count[index])
  o_delay=sum(jfkair$o_delay[index])
  d_delay=sum(jfkair$d_delay[index])
  return(c(year,o_count,d_count,o_delay,d_delay))
}
year=unique(jfkair$year)
airyear=data.frame(t(sapply(year,fun1)))
names(airyear)=c("year","o_count","d_count",
                 "o_delay","d_delay")
airyear$o_rate=airyear$o_delay/airyear$o_count
airyear$d_rate=airyear$d_delay/airyear$d_count

pfun=function(){
  pdata=data.frame(x=airyear$year,y=c(airyear$o_count,airyear$d_count,airyear$o_delay,airyear$d_delay,airyear$o_rate,airyear$d_rate),fill=rep(c("总出发航班数","总到达航班数","总出发延迟航班数","总到达延迟航班数","出发延迟率","到达延迟率"),each=nrow(airyear)))
  pdata$fill=factor(pdata$fill,levels = c("总出发航班数","总到达航班数","总出发延迟航班数","总到达延迟航班数","出发延迟率","到达延迟率"))
  ph1=ggplot(data=pdata,aes(x=x,y=y))+
    geom_line(linetype=1,size=1,color="darkred")+
    geom_point(size=3,shape=15,color="darkred")+
    labs(x="年份",y="")+
    scale_x_continuous(limits=c(1988,2008),breaks=1988:2008,
                       labels=as.character(1988:2008))+
    facet_wrap(~fill,ncol=2,scale="free_y")+
    theme(legend.text = element_text(size=10),
          axis.title.x=element_text(size=18), 
          axis.text.x=element_text(size=10,angle=45,
                                   vjust=1,hjust=0.9),
          axis.title.y=element_blank(),
          axis.text.y=element_text(size=10))
  print(ph1)
}

pdf("图5_肯尼迪机场描述统计.pdf",width=9,height=5)
pfun()
dev.off()
