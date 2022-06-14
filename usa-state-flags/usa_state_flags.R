library(tidyverse)
library(ggpattern)
library(usdata)
library(showtext)

#get state map coordinates
state_map<-map_data("state")

#add image links from flagpedia
state_flag_map<-state_map|>
  mutate(state_abbr=state2abbr(region),
         flag_link = paste0('https://flagpedia.net/data/us/w1160/',tolower(state_abbr),".webp"))

#default font from showtext
font_add_google("Chivo", "chivo")
showtext_auto()

#plot
ggplot()+
  geom_polygon_pattern(data=state_flag_map%>%filter(state_abbr != "DC"), 
                       mapping=aes(x=long,y=lat,group=group, pattern_filename=I(flag_link)),
                       pattern="image", pattern_type="expand",
                       color="white", size=0.4)+
  coord_map()+
  theme_void()+
  labs(title="United States of Flags", 
       subtitle="Official State Flags. Images from flagpedia.net.", caption="Graphic by @tanya_shapiro")+
  theme(plot.title=element_text(hjust=0.5, family="chivo", face="bold", color="white"),
        plot.subtitle = element_text(hjust=0.5, color="white"),
        plot.caption = element_text(hjust=0.95, color="white"),
        plot.margin=margin(t=10, b=10),
        plot.background = element_rect(fill="#1C2023"))

ggsave("us_flags.jpeg", width=8, height=6)
  