library(tidyverse)
library(ggpattern)
library(usdata)
library(usmapdata)
library(showtext)
library(geosphere)
library(data.table)

#get state map coordinates
state_map<-map_data("state")

#default font from showtext
font_add_google("Noto Serif", "Noto Serif")
font_add_google("Roboto", "roboto")
showtext_auto()

#recreate dataset from New York Times
prohib<-data.frame(
  state = c("Texas","Oklahoma","South Dakota","Wisconsin","Montana","West Virginia","Alabama","Louisiana","Mississippi","Arkansas",
            "Idaho","North Dakota","Wyoming","Tennessee",
            "Utah","Arizona","Kentucky"),
  status = c(rep("Ban in effect",10),rep("Ban expected soon",4), rep("Ban blocked",3)),
  pattern = c(rep("weave",14),rep("stripe",3))
)

#merge dataset with original map data 
map_data<-state_map|>mutate(region=str_to_title(region))|>left_join(prohib, by=c("region"="state"))|>mutate(pattern=case_when(is.na(status)~"weave",TRUE~pattern))
#create factor with levels for status to arrange the status in different order. defaults to alphabetical
map_data$status<-factor(map_data$status, levels=c("Ban in effect","Ban expected soon","Ban blocked"))
#use usdata library function state2abbr to get state abbreviations for our labels
map_data$state_abbr <- state2abbr(map_data$region)
#

#centroid labels - https://stackoverflow.com/questions/38699761/getting-the-centroids-of-lat-and-longitude-in-a-data-frame
findCentroid <- function(Lon, Lat, ...){
  centroid(cbind(Lon, Lat), ...)
}
setDT(map_data)
map_data[, c("cent_long", "cent_lat") := as.list(findCentroid(long, lat)), by = region]
map_data

#create labels
labels<-map_data|>
  distinct(region, state_abbr, cent_long, cent_lat)|>
  mutate(color=case_when(region %in% c("Texas","Oklahoma","South Dakota","Wisconsin","Montana","West Virginia","Alabama","Louisiana","Mississippi","Arkansas") ~ "white",
                         TRUE ~ "black"))

#plot
ggplot(map_data,aes(x=long, y=lat, group=group))+
  #plot base map
  geom_polygon(fill='#DDDDDD', size=0.3, color="white")+
  #plot states with statuses
  geom_polygon_pattern(data=map_data|>filter(!is.na(status)), inherit.aes=FALSE,
                       mapping=aes(x=long, y=lat, group=group, pattern=status, fill=status),
                       pattern_density=0.1, pattern_colour=NA, pattern_fill="white", pattern_spacing=0.02,
                       color="white", size=0.2)+
  #select colors manually
  scale_fill_manual(values=c("#86171B","#C17264","#C17264"))+
  scale_pattern_manual(values=c("none","none","stripe"))+
  #overlay text labels for each state
  geom_text(data=labels|>filter(region %in% prohib$state), inherit.aes=FALSE, 
            mapping=aes(x=cent_long,y=cent_lat, label=state_abbr, color=color), fontface="bold", family="roboto", size=3.25)+
  #add scale identity for color to map colors for each labels to actual value, e.g. white or black
  scale_color_identity()+
  coord_map()+
  labs(title="Where Abortion Is Prohibited",
       caption=" Data as of July 8, 2022 (New York Times) \nRecreated by @tanya_shapiro")+
  theme_void()+
  theme(legend.position = "top",
        legend.title=element_blank(),
        legend.key.size = unit(0.4, 'cm'),
        plot.caption=element_text(hjust=0.05, color="grey50", vjust=-12, size=8),
        plot.title=element_text(hjust=0.5, face="bold", size=20, margin=margin(b=25), family="Noto Serif"))

ggsave("nyt_abortion_map.jpeg", height=6, width=6)
