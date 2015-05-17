## Fall semester
url<- 'http://www.kurser.dtu.dk/search.aspx?lstTeachingPeriod=E1;E2;E3;E4;E5;E1A;E2A;E3A;E4A;E5A;E1B;E2B;E3B;E4B;E5B;E&YearGroup=2014-2015,2015-2016&btnSearch=Search'
categories <- url %>% 
    html() %>% 
    html_nodes('#ctl00_PlaceHolderMain_PageHtml td div a') %>% 
    html_text()
categories<-unique(categories)
CourseNum <- substring(categories,1,5)
GeneralLinks <- paste('http://www.kurser.dtu.dk/',CourseNum,'.aspx?menulanguage=en-gb',sep='')
GradeLinks <- paste('http://www.kurser.dtu.dk/courses/',CourseNum,'/info/default.aspx',sep='')
jj <- 1
Name <- rep(0,length(Links))
ECTS <- rep(0,length(Links))
TempHist <- rep(0,c(100,100))
for (ii in GeneralLinks){
    Name[jj] <- ii %>%
        html() %>%
        html_nodes('h2') %>%
        html_text    
    ECTS[jj]<-ii %>%
        html() %>%
        html_nodes('h2+ table tr:nth-child(4) .value') %>%
        html_text
    CourseType[jj] <- ii %>%
        html() %>%
        html_nodes('.value div:nth-child(1)') %>%
        html_text
    TempHist <- GradeLinks[jj] %>%
        html() %>%
        html_nodes('.ms-WPBody td+ td a') %>%
        html_attr('href')
    
    for (kk in TempHist){
        
    }
    jj<-jj+1
}









## Spring Semester
url2 <- 'http://www.kurser.dtu.dk/search.aspx?lstTeachingPeriod=F1;F2;F3;F4;F5;F1A;F2A;F3A;F4A;F5A;F1B;F2B;F3B;F4B;F5B;F&YearGroup=2014-2015,2015-2016&btnSearch=Search'
categories2 <- url2 %>% 
    html() %>% 
    html_nodes('#ctl00_PlaceHolderMain_PageHtml td div a') %>% 
    html_text()

## january
url3 <- 'http://www.kurser.dtu.dk/search.aspx?lstTeachingPeriod=January&YearGroup=2014-2015,2015-2016&btnSearch=Search'
categories <- url3 %>% 
    html() %>% 
    html_nodes('#ctl00_PlaceHolderMain_PageHtml td div a') %>% 
    html_text()

## July

url4 <- 'http://www.kurser.dtu.dk/search.aspx?lstTeachingPeriod=July&YearGroup=2014-2015,2015-2016&btnSearch=Search'
categories <- url4 %>% 
    html() %>% 
    html_nodes('#ctl00_PlaceHolderMain_PageHtml td div a') %>% 
    html_text()

## June

url5 <- 'http://www.kurser.dtu.dk/search.aspx?lstTeachingPeriod=June&YearGroup=2014-2015,2015-2016&btnSearch=Search'
categories <- url5 %>% 
    html() %>% 
    html_nodes('#ctl00_PlaceHolderMain_PageHtml td div a') %>% 
    html_text()

##August

url6 <- 'http://www.kurser.dtu.dk/search.aspx?lstTeachingPeriod=August&YearGroup=2014-2015,2015-2016&btnSearch=Search'
categories <- url6 %>% 
    html() %>% 
    html_nodes('#ctl00_PlaceHolderMain_PageHtml td div a') %>% 
    html_text()
