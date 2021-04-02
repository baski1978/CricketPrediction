#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
load('PredectGameBy-twfbcnwd.RData')
load('PredectGameBy-cn.RData')
load('PredectGameBy-tn.RData')
load('PredectGameBy-wd.RData')
load('PredectGameBy-cntn.RData')
load('predict_score.RData')

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  

  modelcall <- function(a,b,c,d) {
  print(paste("a=",a,"b=",b,"c=",c,"d=",d))
   if( ( d!='' ) && ( b=='' ) && ( c =='' ) ){
     test<-data.frame(Toss_Winner=a,weekday=d)
     pred<-predict(tree3,test)
     return(pred)
   }else if(( c!='' ) && ( b=='' ) && ( d =='' )){
     
     test<-data.frame(Toss_Winner=a,Toss_Name=c)
     pred<-predict(tree2,test)
     return(pred)
     
   } else if( ( c!='' ) && ( b!='' ) && ( d =='' ) ){
     
     print('inside two vairab dt')
     test<-data.frame(Toss_Winner=a,Toss_Name=c,City_Name=b)
     print(test)
     pred<-predict(tree4,test)
     print(pred)
     print('ok ok ok ')
     return(pred)
     
   }
    else {
     test<-data.frame(Toss_Winner=a,City_Name=b)
     pred<-predict(tree1,test)
     return(pred)
   }
  }
  
  modelcall2 <- function(a,b){
    testdata<-data.frame( teambattingid=a, Over_id=b )
    pred2<-predict(sampleforscore_tree,testdata)
    return(pred2)
  }
    
  observeEvent(input$weekday ,
    if(input$weekday!='' ){
      print(session$clientData)
      updateSelectInput(session , "tossname", selected=0)
      updateSelectInput(session , "cityname", selected=0)
      updateCheckboxInput(session,"somevalue", value = FALSE)
      
    }
  )
  observeEvent(input$over,
               if(input$over!='' ){
                 xs=input$over
                 y <- with(teams,Team_Id[match(input$teamname,Team_Name)])
                 if(input$cscore==''){z=0}else{z=input$cscore}
                 inline <- function(m,n1){
                       ro=1
                       print(as.character(n1))
                       print("llllllllll")
                       z1=0
                       j=0
                       k=0
                       print("llllllllll")
                       for(i in m:20 ){
                          j[ro]<- floor(modelcall2(as.character(n1),i))
                          t=j
                          k[ro]<- i
                          if(i==m && z!=0 ){
                            z1=z-j[ro]
                           } else {z1=z1}
                          j[ro]=j[ro]+z1
                          ro=ro+1
                       }
                       print(ro)
                       return(data.frame(cbind(j,k)))
                 }
                 predxy= inline(xs,y) 
                 predxy$j <- unlist(predxy$j)
                 predxy$k <- unlist(predxy$k)
                 print(predxy)
                 output$plot2 <- renderPlotly({
                   p<- ggplot(predxy,aes(k,j) )+
                   geom_point( colour='red') +
                     geom_segment(inherit.aes = TRUE,xend=k,yend=j)+
                     labs(x="Match Overs" , y="Match Score" )
                   p <- ggplotly(p)
                   dev.off()
                   p
                   
                 })
               }
  )
  
  observeEvent(input$teamname ,{
        print('########')
    
         show("pitchtype")
        
         output$mytable12=renderTable(idx_allrates)

         output$plot1 <- renderPlotly({
         p <-   ggplot(city_tosstype_twmw, aes(x = City_Name, y = count, fill = Toss_Name)) +   # Fill column
           geom_bar(stat = "identity", width = .6) +   # draw the bars
           theme(axis.text.x = element_text(angle=90, vjust=0.6)) + 
           labs(
             subtitle="City Pitch : Batting or Fielding ",
             caption="City_Name: count",
             x="City Name",
             y="Count of Matches Won by Toss Type") 
           p <-ggplotly(p)
           dev.off()
           p
         })
         
         
      })
  
  observeEvent(input$somevalue ,
               if(input$somevalue ){
                 print(session$clientData)
                 updateSelectInput(session , "weekday", selected=0)
               } else {
                 updateSelectInput(session , "cityname", selected=0)
               }
               
  )
  
  observeEvent(input$tossname ,
               if(input$tossname!='' ){
                 print(session$clientData)
                 print('asdads bhaskar adadasd')
                 print(input$somevalue)
                 print('asdads bhaskar adadasd')
                 updateSelectInput(session , "weekday", selected=0)
                 if(!input$somevalue ){
                    updateSelectInput(session , "cityname", selected=0) }
                 
               }
  )
  
  observeEvent(input$cityname ,
               if(input$cityname!='' ){
                 print(session$clientData)
                 if(!input$somevalue ){
                 updateSelectInput(session , "tossname", selected=0)}
                 updateSelectInput(session , "weekday", selected=0)
                 output$pitchtype <- renderText({
                   y<- with(ctm,pitch[match(input$cityname,City_Name)])
                   y
                 })
                 
               }
  )
 
  
  output$lp<-renderText({
    #print(paste(input$teamname , input$cityname,input$tossname,input$weekday ))
    x<-tryCatch(x<-modelcall({input$teamname},{input$cityname},{input$tossname},{input$weekday}),warning=function(w){return()},error=function(e){return()},finally={}
                )

    return(round(100*x[1],2))   
  })
  
  output$wp<-renderText({
    x<-tryCatch(x<-modelcall({input$teamname},{input$cityname},{input$tossname},{input$weekday}),warning=function(w){return('')},error=function(e){return('')},finally={})
    return(round(100*x[2],2))    
  })
})
