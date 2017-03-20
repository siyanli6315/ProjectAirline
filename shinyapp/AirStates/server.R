library(shiny)
library(ggplot2)
library(showtext)
library(scales)
library("latex2exp")
library(plyr)
library(grid)
library(grDevices)
showtext.auto(enable=T)

load("state.rda")
load("airdelay.rda")

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

shinyServer(function(input,output){
  output$plot=renderPlot(pfun3(input$year))
})
