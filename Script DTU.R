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
url2 <-'http://www.kurser.dtu.dk/search.aspx?lstTeachingPeriod=E1;E2;E3;E4;E5;E1A;E2A;E3A;E4A;E5A;E1B;E2B;E3B;E4B;E5B;E,E1;E1A;E1B,E2;E2A;E2B,E3;E3A;E3B,E4;E4A;E4B,E5;E5A;E5B,F1;F2;F3;F4;F5;F1A;F2A;F3A;F4A;F5A;F1B;F2B;F3B;F4B;F5B;F,F1;F1A;F1B,F2;F2A;F2B,F3;F3A;F3B,F4;F4A;F4B,F5;F5A;F5B,January,August;July;SummerSchool;June,August,July,June,SummerSchool&YearGroup=2014-2015,2015-2016&btnSearch=Search'
## States
#first page(number)
state_GetCourse<-'#ctl00_PlaceHolderMain_PageHtml td div a'
state_GetDepartments <- '#lstDepartment option'
state_GetECTS <- 'h2+ table tr:nth-child(4) .value'
state_GetECTS2 <- '.normal+ table tr:nth-child(4) .value'
state_CourseTypr <- '.value div:nth-child(1)'
state_CourseTypr2 <-'.normal+ table tr:nth-child(5) .value'
state_GetDepartment <- '.SubTableLevel2 tr:nth-child(1) .page+ td'
state_GetDepartmentSecondary <- 'tr:nth-child(2) div'
#second page(information)
state_PeriodNumber <- 'span td+ td'

##Third page
state_GetGradeTable <- 'td tr+ tr td:nth-child(2)'
#state_Attendants <- 'h2+ table td+ td'
state_Attendants <-'h2+ table tr:nth-child(4) td+ td , h2+ table tr:nth-child(3) td+ td , h2+ table tr:nth-child(2) td+ td , h2+ table tr:nth-child(1) td+ td'
## Get courses
categories<-GetUrlData(urlPath=url2,state=state_GetCourse)
categories<-unique(categories)

## Get Departments

depatments<-GetUrlData(urlPath=url,state=state_GetDepartments)

## get courses number
CourseNum <- substring(categories,1,5)
CourseNum <- unique(CourseNum)
##Initialazing the data frame

df <- data.frame(matrix(nrow=length(categories),ncol=0))
## Get Links
CourseInfoLinks <- paste('http://www.kurser.dtu.dk/',CourseNum,'.aspx?menulanguage=en-gb',sep='')
GradeLinks <- paste('http://www.kurser.dtu.dk/courses/',CourseNum,'/info/default.aspx',sep='')


CourseType <- rep(0,length(CourseInfoLinks))
ECTS <- rep(0,length(CourseInfoLinks))
Department <- rep(0,length(CourseInfoLinks))
SecondaryDepartment <- rep(0,length(CourseInfoLinks))

jj <- 1
for (ii in CourseInfoLinks){
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
    ECTS[jj] <- gsub(',','.',ECTS[jj])
    Department[jj] <- gsub('[\r\n ]',' ',Department[jj])  
    SecondaryDepartment[jj] <- gsub('[\r\n ]',' ',SecondaryDepartment[jj])  
    jj<-jj+1
}




NumberListCourse<-list()
jj<-1
for (ii in GradeLinks){
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
#browser()
NumberListCourse2<-stri_split(as.character(NumberListCourse2),regex="\\s+")
f1 <- function(s) gsub('s','Summer-20',s)
f2 <- function(s) gsub('v','Winter-20',s)
f3 <- function(s) gsub('c','',s)
f4 <- function(s) gsub('[[:punct:]]','',s)
NumberListCourse2 <- sapply(NumberListCourse2,f3)
NumberListCourse2 <- sapply(NumberListCourse2,f4)
NumberListCourse2 <- sapply(NumberListCourse2,f1)
NumberListCourse2 <- sapply(NumberListCourse2,f2)


TotalData <- list()
DATA<-list()
for (ii in seq_along(CourseNum)){
    DATA<-list()
    for (jj in seq_along(NumberListCourse[[ii]])){
        NameTable <- NumberListCourse[[ii]][jj]
        NumberListCourse[[ii]][jj] <- paste('http://karakterer.dtu.dk/Histogram/1/',CourseNum[ii],'/',NumberListCourse[[ii]][jj],sep='')
        NumberListCourse2 <- paste('http://karakterer.dtu.dk/Histogram/1/',CourseNum[ii],'-1/',NameTable,sep='')
        NumberListCourse3 <- paste('http://karakterer.dtu.dk/Histogram/1/',CourseNum[ii],'-2/',NameTable,sep='')
        
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
                Attendants <- gsub(',','.',Attendants)
            }
        }else if(url.exists(NumberListCourse2)){
            res8<-try(NumberListCourseTemp<-GetUrlData(urlPath=NumberListCourse2,state_GetGradeTable),silent=TRUE)
            if (inherits(res8,'try-error')){
                Grades<-NA
            }else{
                Grades<-GetUrlData(urlPath=NumberListCourse2,state_GetGradeTable)
                Grades<-gsub('[\r\n ]','',Grades)     
            }
            
            res9<-try(NumberListCourseTemp<-GetUrlData(urlPath=NumberListCourse2,state_Attendants),silent=TRUE)
            if (inherits(res9,'try-error')){
                Attendants<-NA
            }else{
                Attendants <- GetUrlData(urlPath=NumberListCourse2,state_Attendants)
                Attendants <- gsub('[\r\n ]','',Attendants)
                Attendants <- gsub('\\(.*?\\)', '', Attendants)
                Attendants <- gsub(',','.',Attendants)
            }
            
        }else if(url.exists(NumberListCourse3)){
            res8<-try(NumberListCourseTemp<-GetUrlData(urlPath=NumberListCourse3,state_GetGradeTable),silent=TRUE)
            if (inherits(res8,'try-error')){
                Grades<-NA
            }else{
                Grades<-GetUrlData(urlPath=NumberListCourse3,state_GetGradeTable)
                Grades<-gsub('[\r\n ]','',Grades)     
            }
            
            res9<-try(NumberListCourseTemp<-GetUrlData(urlPath=NumberListCourse3,state_Attendants),silent=TRUE)
            if (inherits(res9,'try-error')){
                Attendants<-NA
            }else{
                Attendants <- GetUrlData(urlPath=NumberListCourse3,state_Attendants)
                Attendants <- gsub('[\r\n ]','',Attendants)
                Attendants <- gsub('\\(.*?\\)', '', Attendants)
                Attendants <- gsub(',','.',Attendants)
            }
        }else{
            Grades <- NA
            Attendants <- NA
        }
        if (length(Grades)==0){
            Grades<-NA
        }
        if (length(Attendants)==0){
            Attendants<-NA
        }
        if (length(Grades)>8){
            Grades <- Grades[1:8]
        }
        tmp <- list(Grades=as.numeric(Grades),GeneralInfo=as.numeric(Attendants))
        DATA[[NameTable]] <- tmp
        
    }
    TotalData[[CourseNum[ii]]]<-DATA
    
}

### add different code for less memory
#1:100,101:200...
load('DATA2')
CourseNumSplit <- split(CourseNum,ceiling(seq_along(CourseNum)/100))
NumberListCourseSplit <- split(NumberListCourse2,ceiling(seq_along(NumberListCourse2)/100))
TotalData <- list()
DATA<-list()
kk<-3
for (ii in seq_along(CourseNumSplit[[kk]])){
    DATA<-list()
    for (jj in seq_along(NumberListCourseSplit[[kk]][[ii]])){
        NameTable <- NumberListCourseSplit[[kk]][[ii]][jj]
        NumberListCourse1 <- paste('http://karakterer.dtu.dk/Histogram/1/',CourseNumSplit[[kk]][ii],'/',NameTable,sep='')
        NumberListCourse2 <- paste('http://karakterer.dtu.dk/Histogram/1/',CourseNumSplit[[kk]][ii],'-1/',NameTable,sep='')
        NumberListCourse3 <- paste('http://karakterer.dtu.dk/Histogram/1/',CourseNumSplit[[kk]][ii],'-2/',NameTable,sep='')
        
        if (url.exists(NumberListCourse1)){
            
            res7<-try(NumberListCourseTemp <- GetUrlData(urlPath=NumberListCourse1,state_Attendants),silent=TRUE)
            if (inherits(res7,'try-error')){
                Attendants<-NA
            }else{
                Attendants <- GetUrlData(urlPath=NumberListCourse1,state_Attendants)
                Attendants <- gsub('[\r\n ]','',Attendants)
                Attendants <- gsub('\\(.*?\\)', '', Attendants)
                Attendants <- gsub(',','.',Attendants)
            }
        }else if(url.exists(NumberListCourse2)){
            
            res9<-try(NumberListCourseTemp<-GetUrlData(urlPath=NumberListCourse2,state_Attendants),silent=TRUE)
            if (inherits(res9,'try-error')){
                Attendants<-NA
            }else{
                Attendants <- GetUrlData(urlPath=NumberListCourse2,state_Attendants)
                Attendants <- gsub('[\r\n ]','',Attendants)
                Attendants <- gsub('\\(.*?\\)', '', Attendants)
                Attendants <- gsub(',','.',Attendants)
            }
            
        }else if(url.exists(NumberListCourse3)){
            
            res9<-try(NumberListCourseTemp<-GetUrlData(urlPath=NumberListCourse3,state_Attendants),silent=TRUE)
            if (inherits(res9,'try-error')){
                Attendants<-NA
            }else{
                Attendants <- GetUrlData(urlPath=NumberListCourse3,state_Attendants)
                Attendants <- gsub('[\r\n ]','',Attendants)
                Attendants <- gsub('\\(.*?\\)', '', Attendants)
                Attendants <- gsub(',','.',Attendants)
            }
        }else{
            Attendants <- NA
        }
        
        if (length(Attendants)==0){
            Attendants<-NA
        }
        
        tmp <- list(GeneralInfo=as.numeric(Attendants))
        DATA[[NameTable]] <- tmp
        
    }
    TotalData[[CourseNumSplit[[kk]][ii]]]<-DATA
    closeAllConnections()
}


save('TotalData',file='C:/Users/Dimitrios/Documents/Dimitris_general/R programming my projects/DTUprject/DATA3_3')
remove('TotalData')

lapply(TotalData, function(x) x[names(x)=='Summer-2006'])





#rownames(df)<-categories
#df$ECTS <- ECTS
#df$CourseType <- CourseType
Department <- gsub('[\r\n]','',Department)
SecondaryDepartment <- gsub('[\r\n]','',SecondaryDepartment)
#AverageGradeTemp <- gsub('[\r\n (Efter7 trinsskalaen) ]','',AverageGrade)
#AverageGradeTemp <- gsub(',','.',AverageGradeTemp)
#df$AverageGrade <- gsub('-','',AverageGradeTemp)


TotalData
ECTS
CourseType
Department
SecondaryDepartment

CourseType <- unique()
UniqueECTS <- unique(ECTS)
sUM5ECTS <- sum(ECTS=='5',na.rm=TRUE)

#dataExtracted <- data.frame(row.names=CourseNum,CourseType = CourseType, ECTS = ECTS, Department = Department, SecondaryDepartment = SecondaryDepartment)

