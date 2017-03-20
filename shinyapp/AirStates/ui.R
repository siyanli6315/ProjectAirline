library(shiny)

shinyUI(fluidPage(
  titlePanel("美国各州航班情况汇总"),
  sliderInput("year","年份",
              min=1998,max=2008,value=19),
  plotOutput("plot",height=500,width=900)
))
