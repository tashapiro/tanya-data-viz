library(tidyverse)
library(sf)
library(ggpattern)
library(ggimage)
library(ggtext)
library(gtrendsR)
library(patchwork)
library(showtext)
library(sysfonts)


#add in fonts
sysfonts::font_add_google("roboto slab","roboto slab")
sysfonts::font_add_google("roboto","roboto")

#set showtext to render fonts
showtext::showtext_auto()
showtext::showtext_opts(dpi=300)

main_font = 'roboto slab'

#get google trends info
google_results<-gtrendsR::gtrends(
  #insert key words as list
  keyword= c("Trump","Taylor Swift"),
  #geography takes iso code 2
  geo = 'US',
  #time frame - 1 month 
  time = "today 12-m"
)


#grab data frame from list of results
by_region<-google_results$interest_by_region

#pivot data to see info by state, create outcome column
df_outcome<-by_region|>
  dplyr::rename(state = location)|>
  tidyr::pivot_wider(id_cols = state, names_from = keyword, values_from = hits)|>
  janitor::clean_names()|>
  dplyr::mutate(
    outcome = case_when(trump>taylor_swift ~ 'Trump',
                        taylor_swift>trump ~ 'Taylor Swift',
                        trump == taylor_swift ~ 'Tie'),
    region = tolower(state)
  )


#get base map 
map_us<-ggplot2::map_data("state")

#map files for alaska and hawaii
map_ak<-st_read('https://raw.githubusercontent.com/glynnbird/usstatesgeojson/master/alaska.geojson')
map_hi<-st_read('https://raw.githubusercontent.com/glynnbird/usstatesgeojson/master/hawaii.geojson')


#map alaska - default color for trump
plot_alaska<-ggplot(map_ak)+
  geom_sf(fill = pal[2], color=NA)+
  scale_x_continuous(limits=c(-170, -129))+
  theme_void()

#map alaska - default color for trump
plot_hawaii<-ggplot(map_hi)+
  geom_sf(fill = pal[2], color=NA)+
  theme_void()


#combine map data with outcome data
df_map<-map_us|>left_join(df_outcome, by="region")

#colors
pal<-c("#0077FF","#F23838")

#map plot for all states except hawaii, alaska
plot_map<-ggplot()+
#state map, fill by outcome if it's not a tie
geom_polygon(
  data = df_map|>filter(outcome!='Tie'),
  mapping = aes(group=group, fill=outcome, x=long, y=lat),
  color = "white", linewidth=0.1)+
#add in states where there is a tie 
ggpattern::geom_polygon_pattern(
  data = df_map|>filter(outcome=='Tie'),
  mapping = aes(group=group, x=long, y=lat),
  pattern_density=0.1, pattern_colour=pal[1], fill= pal[2], pattern_spacing=0.02,
  color="white", linewidth=0.1
  )+
scale_fill_manual(values = pal)+
#add in projection
coord_map("albers",  lat0 = 45.5, lat1 = 29.5)+
#labels and titles 
#default blank theme for map
theme_void()+
theme(
  legend.position = "none"
 # plot.margin = margin(r=5)
)



#set up legend for plot
df_legend<-data.frame(
  image = c("images/legend-trump.png","images/legend-taylor.png"),
  text = c("Trump","Swift"),
  x_pos = c(1,1),
  y_pos = c(1,2)
)

plot_legend<-ggplot(df_legend)+
  ggimage::geom_image(
    mapping = aes(x= x_pos, y=y_pos, image = image),
    size=0.24
  )+
  geom_text(
    mapping=aes(x = x_pos, y= y_pos - 0.45, label=text),
    family = main_font, size = 3
  )+
  scale_y_continuous(limits=c(0.5,3))+
  scale_x_continuous(limits = c(0.7, 1.2))+
  theme_void()



#combine plots together
plot_final<-plot_map + 
    #add alaska 
    inset_element(plot_alaska, left=0, right=0.3, bottom = 0.05, top=0.3, align_to='full')+
    #add hawaii
    inset_element(plot_hawaii, left=0.2, right = 0.4, bottom = 0.07, top=0.2)+
    #add legend
    inset_element(plot_legend2, left = 0.77, bottom= 0.15, top= 0.85,  right= 0.97, align_to = 'full')+
    plot_annotation(title = 'Popularity Contest: Trump vs. Swift',
                    subtitle = "Is Trump really more popular than Taylor Swift? Map shows the more popular Google search by state. Data from Google Trends over the last 12 months.",
                    caption = "Source: Google Trends",
                    theme = theme(
                    plot.margin = margin(t=10, r=10, l=10, b=5),
                    plot.title = ggtext::element_textbox_simple(size = 16, family=main_font, face='bold', margin=margin(b=5)),
                    plot.subtitle = ggtext::element_textbox_simple(size=10, family='roboto', color='#6F6F6F'),
                    plot.caption = ggtext::element_textbox_simple(size=8, family='roboto slab', color="#6F6F6F", hjust=0)))


ggsave("trump_swift.png", plot_final)
