GetUrlData <- function(urlPath,state,check=FALSE){
    if (check==FALSE){
        categories <- urlPath %>% 
            html() %>% 
            html_nodes(state) %>% 
            html_text()
        categories    
    }else{
        if (url.exists(urlPath)){
            checker<-try(NumberListCourseTemp<-GetUrlData(urlPath=urlPath,state))
            if (inherits(checker,'try-error')){
                categories <- NA
            }else{
                categories <- GetUrlData(urlPath=urlPath,state)
            }
        }else{
            categories <- NA
        }
    }
    
}