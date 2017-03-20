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
airdelay=read.csv("allair.csv",header=T,sep=",")
airdelay$sumairdelay=airdelay$MeanArrDelay*airdelay$ArrdelayCount

#图1:航班总数和延迟航班总数随年份变换图、延迟平均时间随年份变化图、延迟率随时间变化图
fun1=function(year){
  index=airdelay$Year%in%year
  aircount=sum(airdelay$AirCount[index])
  airdelaycount=sum(airdelay$ArrdelayCount[index])
  sumairdelay=sum(airdelay$sumairdelay[index])
  return(c(year,aircount,airdelaycount,sumairdelay))
}
year=unique(airdelay$Year)
airyear=data.frame(t(sapply(year,fun1)))
names(airyear)=c("year","aircount","airdelaycount","sumairdelay")
airyear$meanairdelay=airyear$sumairdelay/airyear$airdelaycount
airyear$delayrate=airyear$airdelaycount/airyear$aircount
airyear$delayrate2=paste(round(airyear$delayrate*100,2),"%",sep="")

pfun1=function(){
  pdata=data.frame(x=airyear$year,y=c(airyear$aircount,airyear$airdelaycount),fill=rep(c("总航班数","延迟航班数"),each=nrow(airyear)))
  ph1=ggplot(data=pdata,aes(x=x,y=y,color=fill))+
    geom_line(linetype=1,size=1)+
    geom_point(size=3,shape=15)+
    scale_color_manual(values=c("darkred","darkblue"))+
    scale_y_continuous(limits=c(1e6,8e6),
                       breaks=seq(1e6,8e6,by=2e6),
                       labels=c(TeX("$\\1.0\\times 10^{6}$"),
                                TeX("$\\3.0\\times 10^{6}$"),
                                TeX("$\\5.0\\times 10^{6}$"),
                                TeX("$\\7.0\\times 10^{6}$")))+
    labs(x="年份",y="数量")+
    scale_x_continuous(limits=c(1988,2008),breaks=1988:2008,
                       labels=as.character(1988:2008))+
    theme(legend.position=c(0.2,0.9),
          legend.title = element_blank(),
          legend.text = element_text(size=10),
          axis.title=element_text(size=18),
          axis.text.y=element_text(size=10),
          axis.text.x=element_text(size=10,angle=45,
                                   vjust=1,hjust=0.9))
  ph2=ggplot(data=airyear,aes(x=year))+
    geom_line(aes(y=delayrate),linetype=1,size=1,color="darkred")+
    geom_point(aes(y=delayrate),size=3,shape=15,color="darkred")+
    labs(x="",y="延误率")+
    scale_x_continuous(limits=c(1988,2008),breaks=1988:2008,
                       labels=as.character(1988:2008))+
    scale_y_continuous(limits=c(0.35,0.6),
                       breaks=seq(0.35,0.6,by=0.05),
                       labels=paste(seq(0.35,0.6,by=0.05)*100,"%",sep=""))+
    theme(axis.title.y=element_text(size=18),
          axis.text.y=element_text(size=10),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  ph3=ggplot(data=airyear,aes(x=year))+
    geom_line(aes(y=meanairdelay),linetype=1,
              size=1,color="darkred")+
    geom_point(aes(y=meanairdelay),size=3,
               shape=15,color="darkred")+
    labs(x="年份",y="平均延误时间")+
    scale_x_continuous(limits=c(1988,2008),breaks=1988:2008,
                       labels=as.character(1988:2008))+
    theme(axis.title=element_text(size=18),
          axis.text.y=element_text(size=16),
          axis.text.x=element_text(size=10,angle=45,
                                   vjust=1,hjust=0.9))
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(7,2)))
  vplayout <- function(x,y){
    viewport(layout.pos.row = x, layout.pos.col = y)
  }
  print(ph1, vp = vplayout(1:7,1))
  print(ph2, vp = vplayout(1:3,2))
  print(ph3, vp = vplayout(4:7,2))
}
pdf("图1.pdf",width=9,height=5)
pfun1()
dev.off()

#图2:航班总数和延迟航班总数随月份变化图
fun2=function(month){
  index=airdelay$Month%in%month
  aircount=sum(airdelay$AirCount[index])
  airdelaycount=sum(airdelay$ArrdelayCount[index])
  sumairdelay=sum(airdelay$sumairdelay[index])
  return(c(month,aircount,airdelaycount,sumairdelay))
}
month=unique(airdelay$Month)
airmonth=data.frame(t(sapply(month,fun2)))
names(airmonth)=c("month","aircount",
                  "airdelaycount","sumairdelay")
airmonth$meanairdelay=airmonth$sumairdelay/airmonth$airdelaycount
airmonth$delayrate=airmonth$airdelaycount/airmonth$aircount
airmonth$delayrate2=paste(round(airmonth$delayrate*100,2),"%",sep="")

pfun2=function(){
  pdata=data.frame(x=airmonth$month,y=c(airmonth$aircount,airmonth$airdelaycount),fill=rep(c("总航班数","延迟航班数"),each=nrow(airmonth)))
  ph1=ggplot(data=pdata,aes(x=x,y=y,color=fill))+
    geom_line(linetype=1,size=1)+
    geom_point(size=3,shape=15)+
    scale_color_manual(values=c("darkred","darkblue"))+
    scale_y_continuous(limits=c(3e6,1.2e7),
                       breaks=seq(3e6,1.2e7,by=2e6),
                       labels=c(TeX("$\\3.0\\times 10^{6}$"),
                                TeX("$\\5.0\\times 10^{6}$"),
                                TeX("$\\7.0\\times 10^{6}$"),
                                TeX("$\\9.0\\times 10^{6}$"),
                                TeX("$\\1.1\\times 10^{7}$")))+
    labs(x="月份",y="数量")+
    scale_x_continuous(limits=c(1,12),breaks=1:12,
                       labels=as.character(1:12))+
    theme(legend.position=c(0.2,0.9),
          legend.title = element_blank(),
          legend.text = element_text(size=10),
          axis.title=element_text(size=18),
          axis.text.y=element_text(size=10),
          axis.text.x=element_text(size=10))
  ph2=ggplot(data=airmonth,aes(x=month))+
    geom_line(aes(y=delayrate),linetype=1,size=1,color="darkred")+
    geom_point(aes(y=delayrate),size=3,shape=15,color="darkred")+
    labs(x="",y="延误率")+
    scale_x_continuous(limits=c(1,12),breaks=1:12,
                       labels=as.character(1:12))+
    scale_y_continuous(limits=c(0.4,0.55),
                       breaks=seq(0.4,0.55,by=0.05),
                       labels=paste(seq(0.4,0.55,by=0.05)*100,"%",sep=""))+
    theme(axis.title.y=element_text(size=18),
          axis.text.y=element_text(size=10),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  ph3=ggplot(data=airmonth,aes(x=month))+
    geom_line(aes(y=meanairdelay),linetype=1,
              size=1,color="darkred")+
    geom_point(aes(y=meanairdelay),size=3,
               shape=15,color="darkred")+
    labs(x="月份",y="平均延误时间")+
    scale_x_continuous(limits=c(1,12),breaks=1:12,
                       labels=as.character(1:12))+
    theme(axis.title=element_text(size=18),
          axis.text.y=element_text(size=16),
          axis.text.x=element_text(size=10))
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(7,2)))
  vplayout <- function(x,y){
    viewport(layout.pos.row = x, layout.pos.col = y)
  }
  print(ph1, vp = vplayout(1:7,1))
  print(ph2, vp = vplayout(1:3,2))
  print(ph3, vp = vplayout(4:7,2))
}
pdf("图2.pdf",width=9,height=5)
pfun2()
dev.off()

#地图部分
#library(maps)
alaska=map_data("world","usa:alaska")
hawaii=map_data("world","usa:hawaii")
state=map_data("state")
data(state.fips)
data(us.cities)
state.fips$polyname2=NA
for(i in 1:nrow(state.fips)){
  tmp=strsplit(as.character(state.fips$polyname[i]),":")
  state.fips$polyname2[i]=tmp[[1]][1]
}

ggplot()+
  geom_polygon(data=state,aes(x=long, y=lat, group=group))+
  #geom_polygon(data=hawaii,aes(x=long, y=lat, group=group))+
  #geom_polygon(data=alaska,aes(x=long, y=lat, group=group))+
  #geom_point(data=us.cities,aes(x=long,y=lat),color="white",size=0.1)+
  scale_x_continuous(limits=c(-125,-67))+
  scale_y_continuous(limits=c(25,50))+
  coord_map()+
  theme_classic()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())

#图3:2008年各州总航班数量，延迟航班数量、航班延迟比例
library(plyr)
tmp=data.frame(region=state.fips$polyname2,abb=state.fips$abb)
state=join(state,tmp,by="region")
airdelay$state=as.character(airdelay$state)

pfun3=function(year){
  tmp1=airdelay[airdelay$Year==year,]
  tmp1=tmp1[!is.na(tmp1$state),]
  abb=unique(tmp1$state)
  fun1=function(x){
    index=tmp1$state%in%x
    aircount=sum(airdelay$AirCount[index])
    airdelaycount=sum(airdelay$ArrdelayCount[index])
    return(c(aircount,airdelaycount))
  }
  tmp2=data.frame(t(sapply(abb,fun1)))
  names(tmp2)=c("aircount","airdelaycount")
  tmp2$abb=abb
  tmp2$delayrate=tmp2$airdelaycount/tmp2$aircount
  tmp2$delayrate2=paste(round(tmp2$delayrate*100,2),"%",sep="")
  state=join(state,tmp2,by="abb")
  
  ph1=ggplot()+
    geom_polygon(data=state,aes(x=long, y=lat, group=group,fill=aircount))+
    scale_x_continuous(limits=c(-125,-67))+
    scale_y_continuous(limits=c(25,50))+
    scale_fill_gradient(low="skyblue3",high="darkblue")+
    labs(title="航班总数")+
    coord_map()+
    theme_classic()+
    theme(title=element_text(size=15),
          legend.position = "None",
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank())
  
  ph2=ggplot()+
    geom_polygon(data=state,aes(x=long,y=lat,group=group,fill=airdelaycount))+
    scale_x_continuous(limits=c(-125,-67))+
    scale_y_continuous(limits=c(25,50))+
    scale_fill_gradient(low="gray",high="darkgreen")+
    labs(title="延迟航班数总数")+
    coord_map()+
    theme_classic()+
    theme(legend.position = "None",
          title=element_text(size=15),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank())
  
  ph3=ggplot()+
    geom_polygon(data=state,aes(x=long,y=lat,group=group,fill=delayrate))+
    scale_x_continuous(limits=c(-125,-67))+
    scale_y_continuous(limits=c(25,50))+
    scale_fill_gradient(low="gray",high="darkred")+
    labs(title="航班延迟率")+
    coord_map()+
    theme_classic()+
    theme(title=element_text(size=15),
          legend.position = "None",
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank())
  
  tmp3=tmp2[order(tmp2$delayrate,decreasing = T)[1:15],]
  tmp3$x=1:15
  ph4=ggplot(data=tmp3,aes(x=x,y=delayrate))+
    geom_line(linetype=1,size=1)+
    geom_point(size=2,shape=15)+
    #geom_text(aes(label=delayrate2),size=3,vjust=-1)+
    scale_x_continuous(breaks=c(1:15),labels=tmp3$abb)+
    theme(axis.title=element_blank(),
          axis.text=element_text(size=12))
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(18,14)))
  vplayout <- function(x,y){
    viewport(layout.pos.row = x, layout.pos.col = y)
  }
  print(ph1, vp = vplayout(1:9,1:6))
  print(ph2, vp = vplayout(10:18,1:6))
  print(ph3, vp = vplayout(1:12,7:14))
  print(ph4, vp = vplayout(12:18,7:13))
}

pdf("图3_2008地图.pdf",width=9,height=5)
pfun3(2008)
dev.off()

#图4:2008年各城市总航班数量，延迟航班数量、航班延迟比例
us.cities$city=NA
for(i in 1:nrow(us.cities)){
  tmp=strsplit(us.cities$name[i]," ")[[1]]
  n=length(tmp)
  if(n==2){
    tmp2=tmp[1]
  }else{
    tmp2=paste(tmp[1],tmp[2])
    if(n>3){
      for(j in 3:(n-1)){
        tmp2=paste(tmp2,tmp[j])
      }
    }
  }
  us.cities$city[i]=tmp2
}
airdelay=join(airdelay,us.cities,by="city")

pfun4=function(year){
  options(warn=-1)
  tmp=airdelay[airdelay$Year==year,]
  tmp=tmp[!is.na(tmp$city),]
  tmp=tmp[!is.na(tmp$long),]
  city=unique(tmp$city)
  fun1=function(x){
    index=tmp$city%in%x
    aircount=sum(tmp$AirCount[index],na.rm=T)
    airdelaycount=sum(tmp$ArrdelayCount[index],na.rm=T)
    long=tmp$long[index][1]
    lat=tmp$lat[index][1]
    return(c(aircount,airdelaycount,long,lat))
  }
  tmp2=data.frame(t(sapply(city,fun1)))
  names(tmp2)=c("aircount","airdelaycount","long","lat")
  tmp2$city=city
  tmp2$delayrate=tmp2$airdelaycount/tmp2$aircount
  tmp2$delayrate2=paste(round(tmp2$delayrate*100,2),"%",sep="")
  tmp2$aircount2=cut(tmp2$aircount,breaks =c(0,quantile(tmp2$aircount)[2:5]))
  levels(tmp2$aircount2)=c("少","较少",'较多',"多")
  tmp2$airdelaycount2=cut(tmp2$airdelaycount,breaks =c(0,quantile(tmp2$airdelaycount)[2:5]))
  levels(tmp2$airdelaycount2)=c("少","较少",'较多',"多")
  tmp2$delayrate3=cut(tmp2$delayrate,breaks =c(0,quantile(tmp2$delayrate)[2:5]))
  levels(tmp2$delayrate3)=c("<35.8%","35.8%~39.5%",'39.5%~43.1%',">43.1%")

  ph1=ggplot()+
    geom_polygon(data=state,aes(x=long, y=lat, group=group),fill="white",color="black")+
    geom_point(data=tmp2,aes(x=long,y=lat,color=aircount2),size=1.5)+
    scale_color_brewer(palette="Blues")+
    scale_x_continuous(limits=c(-125,-67))+
    scale_y_continuous(limits=c(25,50))+
    labs(title="航班总数")+
    coord_map()+
    theme_classic()+
    theme(legend.title=element_blank(),
          legend.text = element_text(size=12),
          title=element_text(size=15),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank())
  #print(ph1)
  
  ph2=ggplot()+
    geom_polygon(data=state,aes(x=long, y=lat, group=group),fill="white",color="black")+
    geom_point(data=tmp2,aes(x=long,y=lat,color=airdelaycount2),size=1.5)+
    scale_color_brewer(palette="Greens")+
    scale_x_continuous(limits=c(-125,-67))+
    scale_y_continuous(limits=c(25,50))+
    labs(title="延迟航班总数")+
    coord_map()+
    theme_classic()+
    theme(legend.title=element_blank(),
          legend.text = element_text(size=12),
          title=element_text(size=15),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank())
  #print(ph2)
  
  ph3=ggplot()+
    geom_polygon(data=state,aes(x=long, y=lat, group=group),fill="white",color="black")+
    geom_point(data=tmp2,aes(x=long,y=lat,color=delayrate3),size=1.5)+
    scale_color_brewer(palette="Reds")+
    scale_x_continuous(limits=c(-125,-67))+
    scale_y_continuous(limits=c(25,50))+
    labs(title="航班延迟率")+
    coord_map()+
    theme_classic()+
    theme(legend.title=element_blank(),
          legend.text = element_text(size=12),
          title=element_text(size=15),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank())
  #print(ph3)
  
  tmp3=tmp2[order(tmp2$delayrate,decreasing = T)[1:15],]
  tmp3$x=1:15
  ph4=ggplot(data=tmp3,aes(x=x,y=delayrate))+
    geom_line(linetype=1,size=1)+
    geom_point(size=2,shape=15)+
    #geom_text(aes(label=delayrate2),size=3,vjust=-1)+
    scale_x_continuous(breaks=c(1:15),labels=tmp3$city)+
    theme(axis.title=element_blank(),
          axis.text.x=element_text(size=12,angle=45,
                                 vjust=1,hjust=0.9),
          axis.text.y=element_text(size=12))
  #print(ph4)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(18,14)))
  vplayout <- function(x,y){
    viewport(layout.pos.row = x, layout.pos.col = y)
  }
  print(ph1, vp = vplayout(1:9,1:6))
  print(ph2, vp = vplayout(10:18,1:6))
  print(ph3, vp = vplayout(1:12,7:14))
  print(ph4, vp = vplayout(12:18,7:13))
}

pdf("图4_2008地图.pdf",width=10,height=5)
pfun4(2008)
dev.off()
