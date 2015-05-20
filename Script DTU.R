rm(list=ls())
require(rvest)
source('GetUrlData.R')
setwd('C:/Users/Dimitrios/Documents/Dimitris_general/R programming my projects/DTUprject')
##link
url<-'http://www.kurser.dtu.dk/search.aspx?lstTeachingPeriod=E1;E2;E3;E4;E5;E1A;E2A;E3A;E4A
;E5A;E1B;E2B;E3B;E4B;E5B;E;F1;F2;F3;F4;F5;F1A;F2A;F3A;F4A;F5A;F1B;F2B;F3B;F4B;F5B;F&YearGroup
=2014-2015,2015-2016&btnSearch=Search&menulanguage=en-GB'
## Get courses

state1<-'#ctl00_PlaceHolderMain_PageHtml td div a'
categories2<-GetUrlData(urlPath=url,state=state1)
## Get Departments
state12<-'#lstDepartment option'
depatments<-GetUrlData(urlPath=url,state=state12)

##Initialazing the data frame, CHECK TO MAKE OVERALL AND NOT FOR WINTER ONLY
Period <- 'Winter-2014'
df <- data.frame(matrix(nrow=length(categories),ncol=0))
CourseNum <- substring(categories,1,5)
GeneralLinks <- paste('http://www.kurser.dtu.dk/',CourseNum,'.aspx?menulanguage=en-gb',sep='')
GradeLinks <- paste('http://www.kurser.dtu.dk/courses/',CourseNum,'/info/default.aspx',sep='')
GradeLinks2 <- paste('http://karakterer.dtu.dk/Histogram/1/',CourseNum,'/',Period,sep='')
jj <- 1
CourseType <- rep(0,length(GeneralLinks))
ECTS <- rep(0,length(GeneralLinks))
Department <- rep(0,length(GeneralLinks))
SecondaryDepartment <- rep(0,length(GeneralLinks))
AverageGrade <- rep(0,length(GeneralLinks))
#TempHist <- rep(0,c(100,100))
for (ii in GeneralLinks){
    res<-try(ECTS[jj]<-ii %>%
                 html() %>%
                 html_nodes('h2+ table tr:nth-child(4) .value') %>%
                 html_text,silent=TRUE)
    if (inherits(res,'try-error')){
        ECTS[jj]<-ii %>%
            html() %>%
            html_nodes('.normal+ table tr:nth-child(4) .value') %>%
            html_text
    }else{
        ECTS[jj]<-ii %>%
            html() %>%
            html_nodes('h2+ table tr:nth-child(4) .value') %>%
            html_text
    }
    CourseTypeTemp <- ii %>%
        html() %>%
        html_nodes('.value div:nth-child(1)') %>%
        html_text
    CourseType[jj] <- CourseTypeTemp[1]
    
    Department[jj] <- ii %>%
        html() %>%
        html_nodes('.SubTableLevel2 tr:nth-child(1) .page+ td') %>%
        html_text
    
    res2<-try(SecondaryDepartment[jj]<-ii %>%
                  html() %>%
                  html_nodes('#pagecontents tr:nth-child(2) div') %>%
                  html_text,silent=TRUE)
    if (inherits(res2,'try-error')){
        SecondaryDepartment[jj] <- 'No'
    }else{
        SecondaryDepartment[jj]<-ii %>%
            html() %>%
            html_nodes('#pagecontents tr:nth-child(2) div') %>%
            html_text
    }
    
    AverageGrade[jj] <- GradeLinks2[jj] %>%
        html() %>%
        html_nodes('h2+ table tr:nth-child(4) td+ td') %>%
        html_text
    
    #for (kk in TempHist){
    res3<-try(SecondaryDepartment[jj]<-ii %>%
                  html() %>%
                  html_nodes('#pagecontents tr:nth-child(2) div') %>%
                  html_text,silent=TRUE)
    if (inherits(res3,'try-error')){
        SecondaryDepartment[jj] <- 'No'
    }else{
        SecondaryDepartment[jj]<-ii %>%
            html() %>%
            html_nodes('#pagecontents tr:nth-child(2) div') %>%
            html_text
    }
    #}
    jj<-jj+1
}
rownames(df)<-categories
df$ECTS <- ECTS
df$CourseType <- CourseType
df$Department <- gsub('[\r\n]','',Department)
df$SecondaryDepartment <- gsub('[\r\n]','',SecondaryDepartment)
AverageGradeTemp <- gsub('[\r\n (Efter7 trinsskalaen) ]','',AverageGrade)
AverageGradeTemp <- gsub(',','.',AverageGradeTemp)
df$AverageGrade <- gsub('-','',AverageGradeTemp)