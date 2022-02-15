library(tidyverse)
library(maps)
library(viridis)
library(patchwork)

#import data set with median age of marriage for women (data scraped from Wikipedia - https://en.wikipedia.org/wiki/List_of_countries_by_age_at_first_marriage)
df <- read.csv("marriage_median_age_women.csv")
#get world map data (lat longs per country)
map<-map_data("world")
#combine map data with marriage df, left join (marriage dataset might not include all countries!)
map_data<-left_join(map, df, by=c("region"="Country"))

#MAP GLOBE PLOTS

#Americas - use coord_map with ortho and set orientation params to show respective section of globe
map_plot_amer<-ggplot(map_data, aes(x=long, y= lat, group=group, fill=Women))+
  geom_polygon()+
  scale_fill_viridis(guide="none")+
  coord_map("ortho",orientation=c(10, -90, 0))+
  theme_void()

#Africa - use coord_map with ortho and set orientation params to show respective section of globe
map_plot_afr<-ggplot(map_data, aes(x=long, y= lat, group=group, fill=Women))+
  geom_polygon()+
  scale_fill_viridis(guide="none")+
  coord_map("ortho",orientation=c(0, 20, 0))+
  theme_void()


#PREP DATA FOR BAR CIRCLE PLOTS, taken from R Graph Gallery Tutorial: https://www.r-graph-gallery.com/circular-barplot.html 

#Americas Bar Plot Data Prep
bar_amer<-df%>%filter(Continent %in% c('Americas'))%>%arrange(Continent, -Women)
bar_amer$id <- seq(1, nrow(bar_amer))
number_of_bar_amer <- nrow(bar_amer)
angle_amer <- 90 - 360 * (bar_amer$id-0.5) /number_of_bar_amer 
bar_amer$hjust <- ifelse( angle_amer < -90, 1, 0)
bar_amer$angle <- ifelse(angle_amer < -90, angle_amer+180, angle_amer)

#Africa Bar Plot Data Prep
bar_afr<-df%>%filter(Continent %in% c('Africa'))%>%arrange(Continent, -Women)
bar_afr$id <- seq(1, nrow(bar_afr))
number_of_bar_afr <- nrow(bar_afr)
angle2 <- 90 - 360 * (bar_afr$id-0.5) /number_of_bar_afr
bar_afr$hjust <- ifelse( angle2 < -90, 1, 0)
bar_afr$angle <- ifelse(angle2 < -90, angle2+180, angle2)

#CREATE BAR CIRCLE PLOTS

#Americas Bar Circle Plot
bar_plot_amer<-ggplot(bar_amer, aes(x=as.factor(id), y=Women, fill=Women)) +       
  geom_bar(stat="identity") +
  geom_text(aes(x=as.factor(id), y=Women+3, label=Country, hjust=hjust, angle=angle), color="black", family="Gill Sans",alpha=0.6, size=3, inherit.aes = FALSE )+ 
  scale_fill_viridis(limits= c(15,35), 
                     guide=guide_colorbar(title.position = "top",
                                          barwidth = 10,
                                          title.hjust = 0.5))+
  ylim(-50,40) +
  labs(title="AMERICAS",
       subtitle= "Median Age of Marriage for Women by Country", 
       caption="Data from Wikipedia | Chart by @tanya_shapiro",
       fill="Median Age")+
  theme_void() + 
  coord_polar()+
  theme(
    legend.position="top",
    text = element_text(family="Gill Sans"),
    plot.title= element_text(hjust=0.5, size = 15, margin=margin(b=10)),
    plot.subtitle=element_text(hjust=0.5, margin=margin(b=15)),
    plot.margin= margin(t=25,b=10)
  )

#Africa Bar Circles Plot
bar_plot_afr<-ggplot(bar_afr, aes(x=as.factor(id), y=Women, fill=Women)) +       
  geom_bar(stat="identity") +
  geom_text(aes(x=as.factor(id), y=Women+3, label=Country, hjust=hjust, angle=angle), color="black", family="Gill Sans",alpha=0.6, size=3, inherit.aes = FALSE )+ 
  scale_fill_viridis(limits= c(15,35), 
                     guide=guide_colorbar(title.position = "top",
                                          barwidth = 10,
                                          title.hjust = 0.5))+
  ylim(-50,40) +
  labs(title="AFRICA",
       subtitle= "Median Age of Marriage for Women by Country", 
       caption="Data from Wikipedia | Chart by @tanya_shapiro",
       fill="Median Age")+
  theme_void() + 
  coord_polar()+
  theme(
    legend.position="top",
    text = element_text(family="Gill Sans"),
    plot.title= element_text(hjust=0.5, size = 15, margin=margin(b=10)),
    plot.subtitle=element_text(hjust=0.5, margin=margin(b=15)),
    plot.margin= margin(t=25,b=10)
  )



#combine bar circle plot with globe choropleth maps, use inset_element from patchwork library
americas<-bar_plot_amer + inset_element(map_plot_amer, left = 0.3, bottom = 0.28, right = 0.7, top = 0.72)
africa<-bar_plot_afr + inset_element(map_plot_afr, left = 0.3, bottom = 0.28, right = 0.7, top = 0.72)

americas

#uncoment below to save
#ggsave("americas_marriage.jpeg", height=9, width=6)
