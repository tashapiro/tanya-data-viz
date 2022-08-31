library(tidyverse)
library(ggtext)
library(ggimage)
library(sysfonts)
library(showtext)

#import fonts
font_add_google("roboto", "roboto")
font_add_google("open sans", "open sans")
showtext_auto()

#create main dataframe 
df<-data.frame(
  year = 1999:2019,
  age_leo = 24:44,
  gf = c(
    rep("Gisele Bundchen",6),
    rep("Bar Refaeli",6),
    "Blake Lively",
    "Erin Heatherton",
    rep("Toni Garrn",2),
    "Kelly Rohrbach",
    rep("Nina Agdal",2),
    rep("Camila Morrone",2)
  ),
  age_gf = c(
    18:23,
    20:25, 
    23, 
    22, 
    20:21,
    25,
    24:25, 
    20:21
  )
)

#data for annotations about max age limit
max_points<-data.frame(x=c(2010, 2015, 2017), y=rep(25,3))

#data for year segments by girlfriend
by_gf<-df|>
  group_by(gf)|>
  summarise(min_year = min(year),
            max_year = max(year))


#color palette for plot
pal_leo<-'#FD7600'
pal_gf <-'#24C4C4'
pal_bg<-'#030623'
pal_annotate<-'#B6B6B6'

#data for images and respective positions on x axis
images<-data.frame(name=c("Leonardo DiCaprio", unique(df$gf)),
                   pos = seq(from=1999, to=2019, length.out = 9),
                   pal_label<-c(pal_leo, rep(pal_gf,8)))|>
  mutate(path = paste0("images/",str_replace_all(tolower(name)," ","_"),".png"))


#data frame for connector segments from images to years
connectors<-data.frame(
  x= c(2001.5,2004,2005.5,2006.5, 2006.5,2011,2009,2009,2012,2011.5,2013.5,2014,2015,2016.5,2019,2018.5),
  xend = c(2001.5, 2005.5,2005.5,2006.5,2011,2011,2009,2012,2012,2013.5,2013.5,2015,2015,2016.5,2018.5,2018.5),
  y= c(-10,-10,-10,-10,-6,-6,-10,-8,-8,-10,-10,-10,-10,-10,-10,-10),
  yend= c(-4,-10,-4,-6,-6,-4,-8,-8,-4,-10,-4,-10,-4,-4,-10,-4)
)

#create custom title to use with ggtext::element_textbox_simple
title<-'<span style="color:#FD7600;">LEONARDO DICAPRIO</span><span style="color:white;"> REFUSES TO DATE </span><span style="color:#24C4C4;font-weight: bold;">A WOMAN OVER 25 </span>'

#plot
ggplot(data=df, aes(x=year))+
  geom_segment(data = data.frame(y = seq(from=0, to=50, by=5)),
               mapping=aes(x=1999, xend=2019, y=y, yend=y), color="white", size=0.1, alpha=0.2)+
  #leos age data
  geom_line(mapping=aes(y=age_leo), color=pal_leo)+
  geom_point(mapping=aes(y=age_leo), shape=21, fill=pal_bg, color=pal_leo, size=4)+
  geom_text(mapping=aes(y=age_leo+1.75, label=age_leo), color=pal_leo)+
  #girlfriend age data
  geom_segment(mapping=aes(x=year, xend=year, y=0, yend=age_gf), color=pal_gf, size=5)+
  geom_text(mapping=aes(y=age_gf+1.5, label=age_gf), color=pal_gf)+
  #adjust scales to allow for pictures
  scale_y_continuous(limits=c(-20,50), breaks=seq(from=0, to=50, by=5))+
  #create new x axis labels
  geom_text(mapping=aes(x=year, label=paste0("'",substr(year,3,4)), y=-1.5), color="white")+
  #max age limit annotations
  annotate(geom="text", label="Leo's Age Limit", x=2013.5, y=32, color=pal_annotate)+
  geom_segment(mapping=aes(x=2010, xend=2017, y=30, yend=30), color=pal_annotate, size=0.15)+
  geom_segment(data=max_points, mapping=aes(x=x, xend=x, y=30, yend=26), color=pal_annotate, size=0.3)+
  geom_point(data=max_points, mapping=aes(x=x, y=y+1.5),
             shape=21, fill=pal_bg, color=pal_annotate, size=11)+
  geom_text(data=max_points, mapping=aes(x=x, y=y+1.5, label=y), color=pal_gf)+
  #custom legend
  geom_segment(data=data.frame(x=rep(2000,2), xend=rep(2002,2), y=c(40,45), color=c(pal_leo,pal_gf), size=c(0.25,1.5)),
               mapping=aes(x=x,xend=xend,y=y, yend=y, color=color, size=size))+
  geom_point(mapping=aes(x=2001, y=40), shape=21, color=pal_leo, fill=pal_bg, size=4)+
  annotate(geom="text", y=45, x=2002.5, label="Leo's Girlfriend's Age", color=pal_gf, hjust=0)+
  annotate(geom="text", y=40, x=2002.5, label="Leo's Age", color=pal_leo, hjust=0)+
  scale_size_identity()+
  #x axis girlfriend groupings
  geom_segment(data=by_gf, mapping=aes(x=min_year, xend=max_year, y=-4, yend=-4), color=pal_gf)+
  geom_segment(data=by_gf, mapping=aes(x=min_year, xend=min_year, y=-4, yend=-3), color=pal_gf)+
  geom_segment(data=by_gf, mapping=aes(x=max_year, xend=max_year, y=-4, yend=-3), color=pal_gf)+
  #segments to connect images to groupings
  geom_segment(data=images|>filter(name!="Leonardo DiCaprio"),
               mapping=aes(x=pos, xend=pos, y=-13, yend=-10), color=pal_gf)+
  geom_segment(data=connectors, mapping=aes(x=x,xend=xend,y=y,yend=yend), color=pal_gf)+
  #plot images
  geom_image(data=images, mapping=aes(y=-14, x=pos, image=path), size=0.07)+
  geom_richtext(data=images, 
                mapping=aes(y=-19.5, x=pos, color=pal_label, label=str_replace(name," ","<br>")),
                fill = NA, label.color = NA, hjust=0.4,
                show.legend = FALSE, fontface="bold")+
  scale_color_identity()+
  labs(title=title, x="", y="")+
  theme(
    panel.background = element_rect(fill=pal_bg, color=NA),
    plot.background = element_rect(fill=pal_bg),
    plot.title = element_textbox_simple(size=17, halign=0.5),
    text = element_text(color="white"),
    plot.margin = margin(t=30, l=10, r=10),
    panel.grid = element_blank(),
    axis.text.y=element_text(color="white"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x=element_blank()
  )

#save plot
ggsave("plot/dicaprio-gfs.png", height=8, width=8)
