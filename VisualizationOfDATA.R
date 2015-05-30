rm(list=ls())
library(dplyr)
library(ggplot2)
library(reshape2)
library('doBy')
setwd('C:/Users/Dimitrios/Documents/Dimitris_general/R programming my projects/DTUprject')

load('DATA4')
Filenames <- paste0('DATA3_',as.character(1:18))

Data <- lapply(Filenames,function(fn) {
    load(fn)
    return (TotalData)
})
Data <- unlist(Data,recursive=FALSE)


lapply(Data, function(x) x[names(x)=='Winter-2012'])


a<-data.frame(DATA4)


## Result 1
Result1 <- summaryBy(CourseType~ECTS+CourseType,a,FUN=length)
Result1 <- Result1[order(as.character(Result1$CourseType),as.numeric(as.character(Result1$ECTS))),]
a$ECTS2 <- 'Other'
a$ECTS2[a$ECTS %in%'5'] <- '5'
a$ECTS2[a$ECTS %in%'10'] <- '10'
a$ECTS2 <- factor(a$ECTS2)
ggplot(data=a, aes(x=CourseType,stat='bin', fill=ECTS2))+geom_bar(width=1)
ggplot(data=a, aes(x=Department))+geom_bar(width=1)

## Result 2
substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
}
YearStarted <- vector(mode="character", length=length(Data))
for (ii in seq_along(Data)){
    YearsOfCourse <- unique(substrRight(names(Data[[ii]])[order(names(Data[[ii]]))],4))
    YearStarted[ii] <- YearsOfCourse[order(YearsOfCourse)][1]
}

YearStarted <- factor(YearStarted)
YearStarted <- YearStarted[YearStarted!='ter0' & YearStarted!='NA' & YearStarted!='2092'& YearStarted!='2093'& YearStarted!='2087']
YearStarted <- factor(YearStarted)
Years <- unique(YearStarted)
Years <- Years[order(Years)]
NumberOfCourses <- vector(mode='numeric',length=length(Years))
for (ii in seq_along(Years)){
    jj<-ii+1
    NumberOfCourses[jj]<- NumberOfCourses[jj-1]+sum(YearStarted==Years[ii])
}
NumberOfCourses<-NumberOfCourses[2:17]
Toplot <- data.frame(Year= Years,Count =NumberOfCourses)

ggplot(Toplot,aes(x=Year,y=Count,group='Count'))+geom_point()+geom_line()


## Result 3

AverageGrade <- vector(mode='numeric',length=length(Data))
for (jj in seq_along(Data)){
    Temp<-vector(mode='numeric',length=length(Data[[jj]]))
    for (ii in seq_along(Data[[jj]])){
        Temp[ii] <- Data[[jj]][[ii]][[1]][4] 
    }
    if (all(is.na(Temp))){
        AverageGrade[jj] <- NA
    }else{
        AverageGrade[jj]<-mean(Temp,na.rm=TRUE)    
    }
    
}

a$AverageGrade <- AverageGrade





DataCourse <- tbl_df(a)
UniqueCourseType <- unique(Results$CourseType)
UniqueECTS <- unique(Results$ECTS)
sUM5ECTS <- sum(ECTS=='5',na.rm=TRUE)
numberOfBScCourses <- nrow(filter(DataCourse,CourseType=='BSc'))
numberOfMScCourses <- nrow(filter(Data,CourseType=='MSc'))
numberOfPhDCourses <- nrow(filter(Data,CourseType=='Ph.D.'))
numberOfDiplomaCourses <- nrow(filter(Data,CourseType=='Diplomingeniør'))
numberOfDeltiCourses <- nrow(filter(Data,CourseType=='Deltidsdiplom'))
numberOfPartTimeCourses <- nrow(filter(Data,CourseType=='Parttime master'))
height <- c(numberOfBScCourses,numberOfDeltiCourses,numberOfDiplomaCourses,numberOfMScCourses,numberOfPartTimeCourses,numberOfPhDCourses)
# tbl_df
ListConsentraded <- list(Course = CourseNum,CourseType= CourseType, ECTS=ECTS, Department = Department, SecondaryDepartment = SecondaryDepartment)

ggplot(Data, aes(x=ECTS,y=CourseType,fill=Cultivar))+
    geom_bar(stat='identity')+
    guides(fill=guide_legend(reverse=TRUE))