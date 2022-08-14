library(tidyverse)
library(rvest)

#scrape personality details
get_personality<-function(url){
  html = url%>%read_html()
  
  character = html%>%
    html_elements("h3")%>%
    head(1)%>%
    html_text()
  
  data= html%>%
    html_elements("table.zui-table")%>%
    html_table()%>%
    .[[1]]
  
  names(data)=c("item","avg_rating","rank","rating_sd","number_ratings")
  data$character = str_replace(character," Descriptive Personality Statistics","")
  
  data
}

base_url<-'https://openpsychometrics.org/tests/characters/stats/WSW/'

ww_profiles<-data.frame()
#create a loop to scrape all characters, there are a total of 15 characters profiled, use range 1:16
for(i in 1:16){
  url<-paste0(base_url, i)
  temp_data<-get_personality(url)
  ww_profiles<-rbind(ww_profiles,temp_data)
}




