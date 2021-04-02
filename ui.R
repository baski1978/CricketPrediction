#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyWidgets)
#library(DT)
#library(shinydashboard)
#twlookup<- if_else(input$CityName!='','',bind('', teams$Team_Name))

#switch(paste(input$cityname,input$tossname,input$weekday),""=rbind('', teams$Team_Name),'')

loadselect<-function(a){
  if(a=='1'){
    ldcn-choice<- rbind('',unique(matches$City_Name))
    ldtn-choice<-c('')
    ldwd-choice<- c('')
    selectInput
  }else if (a=='2'){
    ldcn-choice<- c('')
    ldtn-choice<-rbind('',unique(matches$Toss_Name))    
    ldwd-choice<- c('')
  }else {
    ldtn-choice<-c('')
    ldcn-choice<- c('')
    ldwd-choice<-rbind('',unique(matches$weekday))
    
    conditionalPanel(
      condition = "input.smooth == true",
      selectInput("smoothMethod", "Method",
                  list("lm", "glm", "gam", "loess", "rlm"))
    )
    
  }
}


print(getwd())
source('prework.R')

#print(colnames(teams))

# Define UI for application that draws a histogram
shinyUI(fluidPage(
                   
                   setBackgroundColor("green")  ,
                   #tag$style("#text1{color: red;}"),
                   h4("Cricket Prediction" , style='text-align:center;color:white' ),
                   fluidRow(column(3,selectInput("teamname", "TossWinner", choices = teams$Team_Name  , selected = 1)),
                            column(3, selectInput("cityname", "CityName", choices = rbind('',unique(matches$City_Name)) ,   selected = 'Bangalore')),
                            column(3, selectInput("tossname", "TossName", choices =rbind('',unique(matches$Toss_Name))  ,   selected = 0)),
                            column(3, selectInput("weekday", "Weekday", choices = rbind('',unique(matches$weekday)),   selected = 0),),
                            style='background-color: black;color:white;margin:0px 0px 0px 0px ;padding-top=0px;padding-bottom=0px;font-size:80%'),
                   fluidRow(column(2, sliderInput("over", "Current Over:", min = 0, max = 20, value = 5)),
                            column(1, textInput('cscore',"current score")),
                            column(3),
                            column(3,h6("Include City Name"),checkboxInput("somevalue",'' , TRUE,)),
                            column(1, textInput('fscore',"score to win") ),
                            column(2),
                            style='background-color: lightgreen;color:black;margin:0px 0px 0px 0px  ;padding-top=0px;padding-bottom=0px;font-size:80%'),
                   fluidRow(column( 6, h4('Loss Prob')),column( 6, h4('Win Prob')),
                            style='text-align:center;background-color: green;color:white;margin:0px 0px 0px 0px  ;padding-top=0px;padding-bottom=0px;font-size:120%'),
                   fluidRow(column( 6, verbatimTextOutput('lp',),style='font-size:200%'),
                            column( 6, verbatimTextOutput('wp'),style='font-size:200%'),
                            style='text-align:center;background-color: lightgreen;margin:0px 0px 0px 0px  ;color:black;padding-top=0px;padding-bottom=0px;font-size:200%'),
                   fluidRow(column(9,
                                     tabsetPanel(
                                       tabPanel("Team Statistics", tableOutput("mytable12"),style="font-size:60%;font-weight:bold" ),
                                       tabPanel("Predict Score", plotlyOutput("plot2",width = "700", height="280")),
                                       tabPanel("Pitch Type Graph", plotlyOutput("plot1",width = "800", height="280"))
                                     
                                   )),
                            column(3 , verbatimTextOutput('pitchtype',),style='font-size:200%'),
                            style='background-color: black;color:white;margin:0px 0px 0px 0px ;border-style: dashed ;padding-top=0px;padding-bottom=0px'
                            )
)
)
