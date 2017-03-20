library(shiny)
library(ggplot2)
library(showtext)
library(scales)
library("latex2exp")
library(plyr)
library(grid)
library(grDevices)
showtext.auto(enable=T)

load("us.cities.rda")
load("airdelay.rda")
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

shinyServer(function(input,output){
  output$plot=renderPlot(pfun4(input$year))
})
