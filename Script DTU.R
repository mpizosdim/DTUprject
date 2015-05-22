##packages and so on...
rm(list=ls())
require(rvest)
library(stringi)
library(RCurl)
library(rlist)
setwd('C:/Users/Dimitrios/Documents/Dimitris_general/R programming my projects/DTUprject')
source('GetUrlData.R')
##link
url<-paste0('http://www.kurser.dtu.dk/search.aspx?lstTeachingPeriod=E1;E2;E3;E4;E5;E1A;',
            'E2A;E3A;E4A;E5A;E1B;E2B;E3B;E4B;E5B;E;F1;F2;F3;F4;F5;F1A;F2A;F3A;F4A;F5A;F1B;',
            'F2B;F3B;F4B;F5B;F&YearGroup=2014-2015,2015-2016&btnSearch=Search&menulanguage=en-GB')
## States
#first page(number)
state_GetCourse<-'#ctl00_PlaceHolderMain_PageHtml td div a'
state_GetDepartments <- '#lstDepartment option'
state_GetECTS <- 'h2+ table tr:nth-child(4) .value'
state_GetECTS2 <- '.normal+ table tr:nth-child(4) .value'
state_CourseTypr <- '.value div:nth-child(1)'
state_CourseTypr2 <-'.normal+ table tr:nth-child(5) .value'
state_GetDepartment <- '.SubTableLevel2 tr:nth-child(1) .page+ td'
state_GetDepartmentSecondary <- '#pagecontents tr:nth-child(2) div'
#second page(information)
state_PeriodNumber <- 'span td+ td'

##Third page
state_GetGradeTable <- 'td tr+ tr td:nth-child(2)'
state_Attendants <- 'h2+ table td+ td'
## Get courses
categories<-GetUrlData(urlPath=url,state=state_GetCourse)
categories<-unique(categories)

## Get Departments

depatments<-GetUrlData(urlPath=url,state=state_GetDepartments)

## get courses number
CourseNum <- substring(categories,1,5)

##Initialazing the data frame, CHECK TO MAKE OVERALL AND NOT FOR WINTER ONLY

df <- data.frame(matrix(nrow=length(categories),ncol=0))
## Get Links
CourseInfoLinks <- paste('http://www.kurser.dtu.dk/',CourseNum,'.aspx?menulanguage=en-gb',sep='')
GradeLinks <- paste('http://www.kurser.dtu.dk/courses/',CourseNum,'/info/default.aspx',sep='')


CourseType <- rep(0,length(CourseInfoLinks))
ECTS <- rep(0,length(CourseInfoLinks))
Department <- rep(0,length(CourseInfoLinks))
SecondaryDepartment <- rep(0,length(CourseInfoLinks))

jj <- 1
for (ii in CourseInfoLinks[1:10]){
    
    if (url.exists(ii)){
        #get ECTS credits
        res<-try(ECTS[jj]<-GetUrlData(urlPath=ii,state_GetECTS),silent=TRUE)
        resB<-try(ECTS[jj]<-GetUrlData(urlPath=ii,state_GetECTS2),silent=TRUE)
        if (inherits(res,'try-error')){
            ECTS[jj]<- GetUrlData(urlPath=ii,state_GetECTS2)
        }else if(inherits(resB,'try-error')){
            ECTS[jj]<-GetUrlData(urlPath=ii,state_GetECTS)
        }else{
            ECTS[jj]<-NA
        }
        #get CourseType credits
        res2 <- try(CourseType[jj]<-GetUrlData(urlPath=ii,state_CourseTypr),silent=TRUE)
        if (inherits(res2,'try-error')){
            CourseType[jj] <- NA
        }else{
            CourseTypeTemp <- GetUrlData(urlPath=ii,state_CourseTypr)
            CourseType[jj] <- CourseTypeTemp[1]
        }
        # get Deparment and secondary
        res3 <- try(Department[jj]<-GetUrlData(urlPath=ii,state_GetDepartment),silent=TRUE)
        if (inherits(res3,'try-error')){
            Department[jj] <- NA
        }else{
            Department[jj] <- GetUrlData(urlPath=ii,state_GetDepartment)
        }
        res4<-try(SecondaryDepartment[jj]<-GetUrlData(urlPath=ii,state_GetDepartmentSecondary),silent=TRUE)
        if (inherits(res4,'try-error')){
            SecondaryDepartment[jj] <- 'No'
        }else{
            SecondaryDepartment[jj]<-GetUrlData(urlPath=ii,state_GetDepartmentSecondary)
        }
        
    }else{
        ECTS[jj]<- NA
        CourseType[jj]<- NA
        Department[jj]<- NA
        SecondaryDepartment[jj]<- NA
    }    
    
    jj<-jj+1
}

NumberListCourse<-list()
jj<-1
for (ii in GradeLinks[1:10]){
    if (url.exists(ii)){
        res5<-try(NumberListCourseTemp<-GetUrlData(urlPath=ii,state_PeriodNumber),silent=TRUE)
        if (inherits(res5,'try-error')){
            NumberListCourse <- list.append(NumberListCourse,NA)
        }else{
            NumberListCourseTemp <- GetUrlData(urlPath=ii,state_PeriodNumber)
            NumberListCourseTemp <- substr(NumberListCourseTemp, 1, nchar(NumberListCourseTemp)-1) 
            NumberListCourse <- list.append(NumberListCourse,NumberListCourseTemp)
        }
    }else{
        NumberListCourse <- list.append(NumberListCourse,NA)
    }
    
}
NumberListCourse<-stri_split(as.character(NumberListCourse),regex="\\s+")
f1 <- function(s) gsub('s','Summer-20',s)
f2 <- function(s) gsub('v','Winter-20',s)
f3 <- function(s,x) paste('http://karakterer.dtu.dk/Histogram/1/',s,'/',x,sep='')
NumberListCourse <- sapply(NumberListCourse,f1)
NumberListCourse <- sapply(NumberListCourse,f2)


TotalData <- list()
DATA<-list()
browser()
for (ii in seq_along(CourseNum[1:10])){
    DATA[ii]<-list()
    for (jj in seq_along(NumberListCourse[[ii]])){
        NameTable <- NumberListCourse[[ii]][jj]
        NumberListCourse[[ii]][jj] <- paste('http://karakterer.dtu.dk/Histogram/1/',CourseNum[ii],'/',NumberListCourse[[ii]][jj],sep='')
        
        
        if (url.exists(NumberListCourse[[ii]][jj])){
            res6<-try(NumberListCourseTemp<-GetUrlData(urlPath=NumberListCourse[[ii]][jj],state_GetGradeTable),silent=TRUE)
            if (inherits(res6,'try-error')){
                Grades<-NA
            }else{
                Grades<-GetUrlData(urlPath=NumberListCourse[[ii]][jj],state_GetGradeTable)
                Grades<-gsub('[\r\n ]','',Grades)     
            }
            
            res7<-try(NumberListCourseTemp<-GetUrlData(urlPath=NumberListCourse[[ii]][jj],state_Attendants),silent=TRUE)
            if (inherits(res7,'try-error')){
                Attendants<-NA
            }else{
                Attendants <- GetUrlData(urlPath=NumberListCourse[[ii]][jj],state_Attendants)
                Attendants <- gsub('[\r\n ]','',Attendants)
                Attendants <- gsub('\\(.*?\\)', '', Attendants)
            }
            
        }else{
            Grades <- NA
            Attendants <- NA
        }
        DATA[ii]<-list.append(DATA[ii],list(Grades=Grades,GeneralInfo=Attendants))
        
    }   
    
}










rownames(df)<-categories
df$ECTS <- ECTS
df$CourseType <- CourseType
df$Department <- gsub('[\r\n]','',Department)
df$SecondaryDepartment <- gsub('[\r\n]','',SecondaryDepartment)
AverageGradeTemp <- gsub('[\r\n (Efter7 trinsskalaen) ]','',AverageGrade)
AverageGradeTemp <- gsub(',','.',AverageGradeTemp)
df$AverageGrade <- gsub('-','',AverageGradeTemp)