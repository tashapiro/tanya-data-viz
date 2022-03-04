library(maps)
library(tidyverse)

#import map dataframe from maps library
world_map <-map_data("world")

#https://data2.unhcr.org/en/situations/ukraine
countries<-c("Ukraine","Poland","Hungary","Moldova","Slovakia","Romania","Russia","Belarus")
refugees <-c(0,649903,144738,103254, 90329,57192,53300,384)
data<-data.frame(countries,refugees)

#set fill colors per region. Ukraine = Blue, Russia & Belarus = red, Neighboring countries= green, other = grey
world_map<-world_map%>%
  mutate(fill=case_when(region=="Ukraine"~"#90BFEE",
                        region %in% c("Russia","Belarus") ~"#CB9DA7",
                         region %in% countries ~ "#79BD9D",
                         TRUE ~ "grey85"
                         ))

#create dataframe for points of information
point_data<-world_map%>%
  group_by(region)%>%
  #create centroids for lat long per region
  filter(region %in% countries & region != "Ukraine")%>%
  summarise(lat = mean(lat),
            long = mean(long))%>%
  #append refugee data
  left_join(data, by=c("region"="countries"))%>%
  mutate(
         #override Russia centroid, map cut-off
         lat = replace(lat, region=="Russia", 52),
         long = replace(long, region=="Russia",39),
         #include ukranian label coordinates as starting point for migration lines
         lat_ukr = 49.25, 
         long_ukr = 31.5,
         #add vjust parameters to adjust text data in plots
         region_vjust = case_when(refugees>60000 ~ -.8, TRUE ~ -4),
         stat_vjust = case_when(refugees>60000 ~1.1, TRUE~0.1),
         name_color = case_when(refugees>60000 ~ '#1A5A9A', TRUE ~"white")
  )

#Ukraine label
text_data<-data.frame(
  text= "UKRAINE", 
  lat = 49.25, 
  long = 31.5
)

#import city coordinates from map library  
data(world.cities)
#filter world cities for Ukranian cities, use top 5 most populous cities
ukr_cities<-world.cities%>%filter(country.etc =='Ukraine')%>%arrange(-pop)%>%head(5)

ggplot(world_map, aes(long, lat, group=group))+
  geom_polygon(aes(fill=fill), color="white", size=0.3)+
  #migration lines
  geom_segment(inherit.aes=FALSE, data=point_data, aes(x=long_ukr, xend=long, y=lat_ukr, yend=lat), size=0.8, linetype="dotted", color='#1A5A9A')+
  #overlay Ukrainian country map
  geom_polygon(data = world_map%>%filter(region=="Ukraine"), aes(fill=fill), color="white", size=0.3)+
  #plot refugee bubbles
  geom_point(inherit.aes=FALSE, data = point_data, aes(long, lat, size=refugees/1000), fill='#90BFEE', color='#579DE2', shape=21)+
  #overlay refugee numbers
  geom_text(inherit.aes=FALSE, data=point_data, aes(long, lat,label=scales::comma(refugees), vjust=stat_vjust),size=2.5, family="Gill Sans", color='#1A5A9A')+
  #overlay country names
  geom_text(inherit.aes=FALSE, data= point_data, aes(long, lat, label=toupper(region), vjust=region_vjust, color=name_color), size=2.8, family="Gill Sans")+
  #Ukraine label
  geom_text(inherit.aes=FALSE, data=text_data, aes(long, lat, label=text), size=5, family="Gill Sans Bold", color='#1A5A9A')+
  #Ukrainian cities
  geom_point(inherit.aes=FALSE, data=ukr_cities, aes(long,lat), color='#1A5A9A')+
  geom_text(inherit.aes=FALSE, data=ukr_cities, aes(long,lat,label=name), size=4, vjust=1.7, family="Gill Sans", color='#1A5A9A')+
  #set the fill and color color to match the inputs from the dataframe with "scale_identity
  scale_fill_identity()+
  scale_color_identity()+
  scale_size(range=c(9,37), breaks=c(100,150, 200, 250,500), guide="none")+
  #adjust section of world map to focus on Ukraine and neighboring countries with xlim & ylim
  coord_map(xlim=c(15,42),
            ylim=c(44,55))+
  #add title and themes
  labs(title="NUMBER OF UKRAINIAN REFUGEES IN NEIGHBORING EUROPEAN COUNTRIES",subtitle="Number of Ukrainian refugees in neighboring countries. Graphic does not depict additional 110K refugees in other additional European countries.",
       caption="Data from UNHCR as of Mar 3rd, 2022 | Chart @tanya_shapiro")+
  theme_void()+
  theme(text=element_text(family="Gill Sans"),
        plot.margin = margin(r=15,l=15),
        plot.title=element_text(size=16, family="Gill Sans Bold", margin=margin(b=10)),
        plot.subtitle=element_text(size=14, margin=margin(b=12)),
        plot.caption=element_text(size=10)
)

ggsave("ukranian_refugees.jpeg", height=8.5, width=12)
