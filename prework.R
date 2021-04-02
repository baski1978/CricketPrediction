library(tableHTML)
library('plotly')
library(MASS)
library(caTools)
library(rpart)
library("FSelector")
library("rpart")
library("caret")
library("rpart.plot")
library("xlsx")
library("data.tree")
library("caTools")
library("lattice")
library("ggplot2")
library(dplyr)
setwd('D:/story/shinycricket/circketpredection')
teams = read.csv(file='Team.csv')
matches <- read.csv(file='Match.csv')
bbyball <-read.csv(file='Ball_By_Ball.csv')

matches$City_Name <- ifelse(matches$City_Name =='Bengaluru' , 'Bangalore', matches$City_Name)
matches$City_Name <- ifelse(matches$City_Name =='Hyderabad (Deccan)' , 'Hyderabad', matches$City_Name)
matches$Toss_Name <-ifelse(matches$Toss_Name =='Bat','bat',matches$Toss_Name)
matches$Toss_Name <- ifelse(matches$Toss_Name=='Field','field',matches$Toss_Name)
matches$mwtwsame<-ifelse(matches$Toss_Winner==matches$match_winner,1,0)
matches$Toss_Winner <- ifelse(matches$Toss_Winner=='Rising Pune Supergiant','Rising Pune Supergiants',matches$Toss_Winner)
matchcountsbyteam <- data.frame(table(matches$Toss_Winner))

matches$weekday<-weekdays(as.Date(matches$match_date,'%m/%d/%y'))


teamruns <- (data.frame(Team_Batting=bbyball$Team_Batting, 
                        Runs_Scored=bbyball$Runs_Scored,
                        Extra_runs=bbyball$Extra_runs,
                        MatcH_id=bbyball$MatcH_id,
                        Over_id=bbyball$Over_id,
                        Ball_id=bbyball$Ball_id,
                        Innings_No=bbyball$Innings_No,
                        outtype=bbyball$Out_type
                        )
             )

teamruns$teambattingid <- with(teams,Team_Id[match(teamruns$Team_Batting,Team_Name)])
teamruns$teambattingid<-ifelse( is.na(teamruns$teambattingid)  ,teamruns$Team_Batting, teamruns$teambattingid)
teamruns$teambattingname <- with(teams,Team_Name[match(teamruns$teambattingid,Team_Id)])


#aggtab1 <- aggregate(teamruns[2]+teamruns[3], by=cbind(teamruns[4],teamruns[5],teamruns[7],teamruns[10]),FUN=sum) 
#aggregate(aggtab1$Runs_Scored  ,by=list(aggtab1$teambattingname),FUN=max)


lbwouttypes<-filter(teamruns ,teamruns[8]=='lbw')
caughtouttypes<-filter(teamruns ,teamruns[8]=='caught')
runoutouttypes<-filter(teamruns ,teamruns[8]=='run out')

idx_runsinmatch <- aggregate(teamruns[2]+teamruns[3], by=cbind(teamruns[4],teamruns[10]),FUN=sum) 
idx_maxrunsinmatch <- aggregate(idx_runsinmatch$Runs_Scored  ,by=list(idx_runsinmatch$teambattingname),FUN=max)
idx_minrunsinmatch <- aggregate(idx_runsinmatch$Runs_Scored  ,by=list(idx_runsinmatch$teambattingname),FUN=min)
idx_strikerate<-aggregate(teamruns[2]+teamruns[3], by=teamruns[10] ,FUN=mean)

idx_lbwindex<-lbwouttypes %>%
  group_by(lbwouttypes[10] ,lbwouttypes[4] ) %>%
  summarise(counts=n()) %>%
  group_by( teambattingname  ) %>%
  summarise(average= mean(counts))
 
idx_caughtindex<-caughtouttypes %>%
  group_by(caughtouttypes[10] ,caughtouttypes[4] ) %>%
  summarise(counts=n()) %>%
  group_by( teambattingname  ) %>%
  summarise(average= mean(counts))

idx_runoutindex<-runoutouttypes %>%
  group_by(runoutouttypes[10] ,runoutouttypes[4] ) %>%
  summarise(counts=n()) %>%
  group_by( teambattingname  ) %>%
  summarise(average= mean(counts))


colnames(idx_maxrunsinmatch)[1] = 'teambattingname'
colnames(idx_minrunsinmatch)[1] = 'teambattingname'
colnames(idx_maxrunsinmatch)[2] = 'maxrunsinmatch'
colnames(idx_minrunsinmatch)[2] = 'minrunsinmatch'
colnames(idx_runoutindex)[2] = 'rounoutrate'
colnames(idx_lbwindex)[2] = 'caughtoutrate'
colnames(idx_caughtindex)[2] = 'lbwrate'

idx_allrates <- merge(merge( merge( merge( merge(idx_maxrunsinmatch,idx_minrunsinmatch , by.x='teambattingname', all=TRUE ),
                                           idx_strikerate ,by.x='teambattingname', all=TRUE ),
                                    idx_runoutindex, by.x='teambattingname', all=TRUE  ),
                             idx_lbwindex ,by.x='teambattingname', all=TRUE ),
                      idx_caughtindex ,by.x='teambattingname', all=TRUE )


chasing <- bbyball %>%
  select(Over_id,Ball_id , MatcH_id ,Innings_No, Team_Batting ,Extra_runs,Runs_Scored)


chasing$teambattingid <- with(teams,Team_Id[match(chasing$Team_Batting,Team_Name)])
chasing$teambattingid<-ifelse( is.na(chasing$teambattingid)  ,chasing$Team_Batting, chasing$teambattingid)
chasing$teambattingname <- with(teams,Team_Name[match(chasing$teambattingid,Team_Id)])


  runstotal <-chasing %>%
     select(MatcH_id,teambattingname,Over_id,Innings_No,Runs_Scored,Extra_runs,teambattingid) %>%
     group_by(MatcH_id,teambattingname,teambattingid,Over_id,Innings_No) %>%
     summarise(runs=sum(Runs_Scored),extras=sum(Extra_runs)) %>%
        group_by(MatcH_id,teambattingname,teambattingid)  %>% 
        mutate(cumruns=cumsum(runs), cumextras=cumsum(extras)) %>%
        mutate(totruns=sum(runs),totextras=sum(extras))
  
  runstotal$cumrunsextras <- runstotal$cumruns+runstotal$cumextras



city_tosstype_twmw <-  filter(matches, matches$mwtwsame==1 ) 
city_tosstype_twmw <- (city_tosstype_twmw %>%
  group_by(City_Name,Toss_Name) %>%
  summarise(count=n()))

city_tosstype_twmw_bat <- city_tosstype_twmw %>% 
  select(City_Name,Toss_Name,count) %>%
  filter(Toss_Name == "bat")

city_tosstype_twmw_field <- city_tosstype_twmw %>% 
  select(City_Name,Toss_Name,count) %>%
  filter(Toss_Name == "field")

ctm <- merge(city_tosstype_twmw_bat,city_tosstype_twmw_field, by = c('City_Name'), all = TRUE )

colnames(ctm)[2]='Toss_Name_bat'
colnames(ctm)[4]='Toss_Name_field'
colnames(ctm)[3]='batcounts'
colnames(ctm)[5]='fieldcounts'

ctm$Toss_Name_bat <- ifelse(is.na(ctm$Toss_Name_bat),'bat',ctm$Toss_Name_bat)
ctm$Toss_Name_field <- ifelse(is.na(ctm$Toss_Name_field),'field',ctm$Toss_Name_field)

ctm$batcounts <- ifelse(is.na(ctm$batcounts),0,ctm$batcounts)
ctm$fieldcounts <- ifelse(is.na(ctm$fieldcounts),0,ctm$fieldcounts)
ctm$pitch <- ifelse(ctm$fieldcounts-ctm$batcounts>0,'Bowling Pitch' , 'Batting Pitch')


colnames(city_tosstype_twmw_field)

fig<-  plot_ly() %>%
  add_trace(x=city_tosstype_twmw_bat$City_Name,y=city_tosstype_twmw_bat$count,type='bar', name='batting',
            text=city_tosstype_twmw_bat$count,textposition='auto',
            marker = list(color = 'rgb(158,200,225)',
                          line = list(color = 'rgb(8,48,10)', width = 1.5))) %>%
  add_trace(x=city_tosstype_twmw_field$City_Name,y=city_tosstype_twmw_field$count,type='bar',name='fielding',
            text=city_tosstype_twmw_field$count,textposition='auto',
            marker = list(color = 'rgb(158,202,22)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) 
  
fig

#countby_tt_tw_mw <-data.frame(table(matches$Toss_Winner,matches$Toss_Name,matches$mwtwsame))
#countby_tt_tw_mw$group2 <- paste(countby_tt_tw_mw$Var2,countby_tt_tw_mw$Var3)
#countby_tt_tw_mw_na <- filter( countby_tt_tw_mw , countby_tt_tw_mw$Var1!='' )

dtdf<-data.frame(matches$Toss_Winner,matches$Toss_Name,matches$City_Name,matches$mwtwsame,matches$weekday)
set.seed(1000)

colnames(dtdf)[1]='Toss_Winner'
colnames(dtdf)[2]='Toss_Name'
colnames(dtdf)[3]='City_Name'
colnames(dtdf)[4]='mwtwsame'
colnames(dtdf)[5]='weekday'


dtdf$twmwsame=factor(dtdf$mwtwsame,levels=c(0,1))

dtdf$Toss_Winner=as.factor(dtdf$Toss_Winner)
dtdf$Toss_Name=as.factor(dtdf$Toss_Name)
dtdf$City_Name=as.factor(dtdf$City_Name)
dtdf$weekday=as.factor(dtdf$weekday)

sample = sample.split(dtdf$twmwsame,SplitRatio = .999)

train=subset(dtdf,sample==TRUE)
test=subset(dtdf,sample==FALSE)

tree <- rpart(twmwsame~Toss_Winner+Toss_Name+City_Name+weekday ,data = train)
pred <-predict(tree,test,type='class')
save(tree,file='PredectGameBy-twfbcnwd.RData')

tree1 <- rpart(twmwsame~Toss_Winner+City_Name ,data = train)
tree2 <- rpart(twmwsame~Toss_Winner+Toss_Name ,data = train)
tree3 <- rpart(twmwsame~Toss_Winner+weekday ,data = train)
tree4 <- rpart(twmwsame~Toss_Winner+City_Name+Toss_Name ,data = train)

pred1 <-predict(tree1,test,type='class')
pred2 <-predict(tree2,test,type='class')
pred3 <-predict(tree3,test,type='class')
pred4 <-predict(tree4,test,type='class')

save(tree1,file='PredectGameBy-cn.RData')
save(tree2,file='PredectGameBy-tn.RData')
save(tree3,file='PredectGameBy-wd.RData')
save(tree4,file='PredectGameBy-cntn.RData')

sample2 = sample.split(runstotal$cumrunsextras,SplitRatio = .999)
sampleforscore_train = subset(runstotal, sample2==TRUE)
sampleforscore_test = subset(runstotal, sample2==FALSE)


sampleforscore_tree = lm(formula =  cumrunsextras~ teambattingid +Over_id,data=sampleforscore_train)
predict_score = predict(sampleforscore_tree,sampleforscore_test)                        

save(predict_score,file='predict_score.RData')
#plot(y=sampleforscore_test$cumrunsextras,x=sampleforscore_test$Over_id  ,type='l', col='pink' )      
#plot(y=predict_score,x=sampleforscore_test$Over_id ,type='l', col='green' )   
#colnames(runstotal)











                         