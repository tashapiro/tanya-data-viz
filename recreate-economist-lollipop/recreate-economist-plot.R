library(tidyverse)
library(ggtext)
library(htmltools)
library(glue)

#---- CREATE DATA
data <- data.frame(
  rank = 1:5,
  country = c("Iceland", "Sweden","Finland","Norway","Portugal"),
  value = c(83, 81,78,77,75)
)|>
  #create country label - rank and country
  #add spans with white . to create space effect, Iceland should be bolded (**)
  mutate(country_label = case_when(rank==1 ~ glue("**{rank}<span style='color:white;'>.....</span>{country}**"),
                                   TRUE ~ glue("{rank}<span style='color:white;'>.....</span>{country}")))

#factor to order by value
data$country<-factor(data$country, levels=rev(c("Iceland", "Sweden","Finland","Norway","Portugal")))

#data to recreate grid lines
data_gridlines<-data.frame(
  x = seq(30,90, by=10)
  )

#---- SET UP LABELS, COLORS, AND FONTS
#colors
pal_red<-"#E3120B"
pal_grey<-"#B3B4AF"
#download font https://fonts.google.com/specimen/Barlow?category=Sans+Serif
font<-"Barlow"


#text to pass thru ggtext geoms + elements
title<-tags$span(style="color:black;font-weight:bold;","**Iceland tops *The Economist*'s glass-ceiling index for 2023**")
subtitle<-tagList(
  tags$span("**Top five countries,** 100 = maximum"),
  tags$br(), tags$br(),
  ##download font awesome locally first! https://fontawesome.com/download
  tags$span(HTML(enc2utf8("&#xf063;")), style='font-family:"Font Awesome 6 Free Solid\"'),
  tags$span("Rank out of 29")
)
source<-tags$p(style="font-size:10pt;color:grey40;font-weight:normal;", "NOTE: The is plot is a recreation of a plot by The Economist. This is an example of how to produce it in ggplot2.")





#---- PUTTING TOGETHER THE PLOT
ggplot(data=data)+
  #fake x grid lines
  geom_segment(data=data_gridlines, 
               mapping=aes(x=x, xend=x, y=Inf, yend=-Inf),
               color="grey90")+
  #fake y grid lines
  geom_segment(mapping=aes(x=30, xend=90, y=country, yend=country),
               color="grey90")+
  #stem of lollipop
  geom_segment(mapping=aes(x=24, xend=value, y=country, yend=country), linewidth=0.8, color=pal_grey)+
  #add point
  geom_point(mapping=aes(x=value, y=country), shape=21, fill=pal_red, color="white", size=5)+
  #fake y axis text (adjusting text to align with title)
  ggtext::geom_textbox(mapping=aes(x=0, y=country, label=country_label),
                        family=font, hjust=0, halign=0, box.size=NA, fill=NA, box.padding=unit(rep(0,4),"pt"))+
  scale_x_continuous(
    #position x axis labels at the top
    position="top",
    #adjust limits - add some room at beginning for plot labels
    limits=c(0,95),
    #adjust the value breaks, show values for 30 to 90, intervals of 10
    breaks = c(25,seq(30,90,by=10)),
    labels = c("~",seq(30,90,by=10)),
    expand=c(0,0))+
  scale_y_discrete(
    #remove trip from y axis
    expand=c(0,0.25)
  )+
  labs(x="",y="", 
       title=title,
       subtitle=subtitle,
       caption = source)+
  theme(text = element_text(family=font), 
        panel.background = element_blank(), 
        axis.ticks=element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_textbox_simple(size=12),
        plot.title = element_textbox_simple(margin=margin(b=20), 
                                            width = unit(3.7, "in"), 
                                            size=18,
                                            hjust=0),
        plot.subtitle = element_textbox_simple(),
        plot.caption  = element_textbox_simple(margin=margin(t=30)),
        plot.margin = margin(b=20, t=20, r=15, l=5))


ggplot2::ggsave("recreate-economist.png", height=4.75, width=4.75)
