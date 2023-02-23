library(tidyverse)
library(ggh4x)
library(ggtext)

#generate data
#data based on graphic from wikipedia: https://en.wikipedia.org/wiki/Bullet_graph
data <- data.frame(
  category = c("Revenue","Profit","Avg Order Size","New Customers","Customer Satisfcation"),
  units = c("US $ (thousands)","%","US $","Count","Rating out of 5"),
  low = c(150,.2,350,1200,3.5),
  med = c(225, .25, 500, 2000, 4.5),
  high = c(300, .3, 600, 2500, 5),
  target = c(250, .27, 550, 2100, 4.6),
  actual = c(275,.22,320,1700,4.7)
)

#factor categories to display in specific order
data$category<-factor(data$category, levels= c("Revenue","Profit","Avg Order Size","New Customers","Customer Satisfcation"))
#create new y axis labels with units for ggtext (pass in HTML/CSS format)
data$label <- paste0("<span style='font-size:11pt;color:black;'>**",data$category,'**</span><br><span style="font-size:8pt;">',data$units,'</span>')

#PLOT
ggplot(data = data)+
  #bars for low, satisfactory, and good levels
  geom_col(mapping=aes(y=label, x=high, fill="3"), width=0.8)+
  geom_col(mapping=aes(y=label, x=med, fill="2"), width=0.8)+
  geom_col(mapping=aes(y=label, x=low, fill="1"), width=0.8)+
  #thinner bar for actual
  geom_col(mapping=aes(y=label, x=actual), width=0.4, fill="black")+
  #target tick 
  geom_vline(mapping=aes(xintercept=target), linewidth=0.5)+
  #adjust x scale to remove padding
  scale_x_continuous(expand=c(0,0))+
  #manually override colors for legend
  scale_fill_manual(values=c("grey50","grey70","grey90"), labels=c("Low","Satisfactory","Good"))+
  #wrap by category to create unique x axis per category
  facet_wrap(~category, ncol=1, scales="free")+
  #customize x axis for each category so ticks line up
  ggh4x::facetted_pos_scales(x = list(
    category == "Profit" ~ scale_x_continuous(breaks=c(0,.1,.2,.3), labels = scales::percent, expand=c(0,0))
  ))+
  #adjust titles and labels
  labs(title="EXAMPLE OF A BULLET CHART", 
       subtitle = "Plot created with ggplot2, ggh4x, and ggtext.",
       x="", y="", fill="Levels")+
  #theme
  theme_minimal()+
  theme(text=element_text(family="Chivo"),
        legend.position="top",
        legend.justification='left',
        legend.margin = margin(l=0, t=5),
        plot.margin = margin(r=40,t=10),
        #use element textbox to align text to left hand side
        axis.text.y=element_textbox(hjust=0, width=grid::unit(1.4, "inch")),
        panel.grid.major.y=element_blank(),
        plot.title = element_text(face="bold", margin=margin(b=5, l=0), size=18),
        plot.subtitle = element_text(margin=margin(l=0)),
        strip.text = element_blank())

