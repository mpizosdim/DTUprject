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

Result1 <- summaryBy(CourseType~ECTS+CourseType,a,FUN=length)
Result1 <- Result1[order(as.character(Result1$CourseType),as.numeric(as.character(Result1$ECTS))),]





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