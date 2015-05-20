GetUrlData <- function(urlPath,state){
    categories <- urlPath %>% 
        html() %>% 
        html_nodes(state) %>% 
        html_text()
    categories
}