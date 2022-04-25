library(tidyverse)
library(ggimage)
library(showtext)
library(sysfonts)

#set font
font_add_google("Roboto", "roboto")
showtext_auto()

#data frame sketch based on tweet from @bananapeele https://twitter.com/bananapeele/status/1517987473837674501/photo/1
ss<-data.frame(
  name = c("Chrishell","Emma","Chelsea",
           "Christine","Maya","Amanza",
           "Heather","Mary","Vanessa",
           "Davina","Jason","Brett"),
  x = c(0,0.5,0.7,
        1,-1,-0.5,
        -0.3,-0.2,-1,
        1,0.4,0.4),
  y = c(0,0.2,0.7,
        1,1,0.3,
        -0.2,-0.8,-1,
        -1,-0.4,-0.75)
)

#line segements
segments<-data.frame(x=c(-1,0), xend=c(1,0), y=c(0,-1), yend=c(0,1))

#append image paths to dataset
ss$image<-paste0("images/",ss$name,".png")

#color palette
pal_plot<-"black"
pal_font<-"white"
pal_line<-"white"
pink<-'#FF558E'

#plot
ggplot(data=ss)+
  geom_segment(data=segments, mapping=aes(x=x, xend=xend, y=y, yend=yend), color=pink)+
  geom_point(aes(x=x,y=y), color=pal_line, size=10.8)+
  geom_image(aes(x=x,y=y,image=image), asp=1.15)+
  geom_label(aes(x=x,y=y-0.15,label=name), size=2.5, fill="black", color=pal_font)+
  annotate("text",label="CONFIDENT",y=1.1, x=0, color=pal_font, fontface="bold")+
  annotate("text",label="ANXIOUS",y=-1.1, x=0, color=pal_font, fontface="bold")+
  annotate("text",label="MALICIOUS",x=1.1, y=0, angle=-90, color=pal_font, fontface="bold")+
  annotate("text",label="BENEVOLENT",x=-1.1, y=0, angle=90, color=pal_font, fontface="bold")+
  lims(x=c(-1.2,1.2), y=c(-1.2,1.2))+
  labs(x="", y="",title="Selling Sunset Vibes", subtitle="Personality analysis of stars from Netflix's Selling Sunset. \n Based on chart by @bananapeele",
       caption="@tanya_shapiro")+
  theme_void()+
  theme(text=element_text(color=pal_font, family="roboto"),
        plot.title=element_text(hjust=0.5, face="bold"),
        plot.subtitle=element_text(hjust=0.5, size=10),
        plot.background = element_rect(fill=pal_plot),
        plot.margin = margin(t=10,b=10,l=10,r=10))

ggsave("selling_sunset.png",width=6.5, height=6.5)
