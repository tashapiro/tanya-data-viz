library(tidyverse)
library(gtrendsR)
library(geomtextpath)
library(ggtext)
library(htmltools)
library(sysfonts)
library(showtext)

#get data with gtrendsR
google_results<-gtrendsR::gtrends(
  keyword= c("Lensa","ChatGPT"),
  time = "today 1-m"
)

#grab data frame from list of results
df_time<-google_results$interest_over_time

#format data
df_plot<-df_time|>
  mutate(date=as.Date(date),
         hits = replace_na(as.numeric(hits),0))|>
  filter(date >= as.Date(max(df_time$date))-14)


#import fonts 
#download font awesome locally first! https://fontawesome.com/download
sysfonts::font_add_google("Barlow","Barlow")
sysfonts::font_add_google("Roboto","Roboto")
sysfonts::font_add('fb', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
showtext::showtext_auto()
showtext::showtext_opts(dpi=300)

#use htmltools to create title
title=tagList(p("Google Trends:",
                span("ChatGPT",style='color:#20A39E;'),
                "vs.",
                span("Lensa", style="color:#FFAC05;")
))

#create subtitle
subtitle = span("Keyword search comparison of popular AI tools, ChatGPT and Lensa, over a two week period from Nov 22, 2022 to Dec 6, 2022. Results are measured by **interest over time**, a scale based on the number of searches for a given term relative to its highest peak in a given period of time.")

#create caption (not using htmltools - issue with Font Awesome Brands)
caption = paste0("<span style='font-family:Barlow;'>Source: Google Trends</span><br>",
                "<span style='font-family:fb;'>&#xf099;</span>",
                 "<span style='font-family:Barlow;color:white;'>.</span>",
                 "<span style='font-family:Barlow;'>@tanya_shapiro</span>",
                 "<span style='font-family:Barlow;color:white;'>....</span>",
                 "<span style='font-family:fb;'>&#xf09b;</span>",
                 "<span style='font-family:Barlow;color:white;'>.</span>",
                 "<span style='font-family:Barlow;'>tashapiro</span>"
                 )


#plot
ggplot(data=df_plot,
       mapping=aes(x=date, y=hits, color=keyword))+
  geomtextpath::geom_textline(mapping=aes(label=keyword),
                              family="Roboto",
                              linewidth=1, hjust=1, vjust=-.5, fontface="bold")+
  scale_color_manual(values=c("#20A39E","#FFAC05"))+
  scale_x_date(breaks="3 days", date_labels ="%b-%d")+
  labs(y="Interest Over Time", 
       x="", 
       title=title,
       subtitle = subtitle,
       caption = caption)+
  theme(text=element_text(family="rc"), 
        panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.margin  = ggplot2::margin(t=20, r=40, l=15, b=10),
        axis.title.y = element_text(margin=margin(r=10)),
        plot.title = ggtext::element_textbox_simple(face="bold", size=18, margin=margin(b=5)),
        plot.caption = ggtext::element_textbox_simple(color="#444444"),
        plot.subtitle = ggtext::element_textbox_simple(color="#444444", family="rc", margin=margin(b=10)),
        legend.position="none",
        panel.grid.major.y = element_line(color="#C6C6C6", size=.2),
        axis.ticks = element_blank())


#save
ggsave("ai-trends.png", bg="white", units="in", width=9, height=6)
