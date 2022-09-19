library(nflfastR)
library(tidyverse)
library(ggimage)
library(RColorBrewer)
library(ggtext)
library(sysfonts)
library(showtext)

#import font from sysfonts
sysfonts::font_add_google("chivo","chivo")
showtext_auto()


#load data
nfl_stats <- load_player_stats()
nfl_qb <- nfl_stats|>filter(position=="QB" & week==1)
nfl_team<- teams_colors_logos

#reshape data
nfl_plot_data<-nfl_qb|>
  filter(passing_yards>70)|>
  select(headshot_url, player_name, player_display_name, recent_team, completions, attempts, passing_yards, passing_tds, interceptions, sacks, rushing_yards, rushing_tds)|>
  inner_join(teams_colors_logos|>select(team_abbr, team_nick, team_division), by=c("recent_team"="team_abbr"))|>
  separate(team_division, into=c("conference","division"), sep=" ")|>
  mutate(comp_perc = completions/attempts)|>
  arrange(conference, division, -comp_perc, -passing_yards)|>
  group_by(conference, division)|>
  mutate(rank = row_number(), 
         pos = -rank,
         yd_color = case_when(passing_yards>330 | passing_yards<125 ~ "white", TRUE ~"black"))

#create new dataframe with positions for line divisions to create table
nfl_plot_lines<-data.frame(lines = seq(0.5,-4.5))

#table headers
headers<-data.frame(name = c("Player","Team","YDS","CMP %","TD"),
           pos = c(0, 1.5, 2.8,3.5,4.1),
           hjust= c(0,0,0.5,0.5,0.5))

#create title with css/html formatting, this will be applied with ggtext::element_textbox_simple in ggplot
title<-"<span style='font-size:18pt;'>**NFL QB Passing Stats**<br></span><span style='font-size:14pt'>Data from nflfastR for Week 1 of 2022 Season. Metrics related to passing performance.<br></span>"


#plot
ggplot(nfl_plot_data)+
  geom_image(mapping=aes(y=pos, image=headshot_url, x=0), size=0.1, asp=1.5)+
  geom_rect(mapping=aes(xmin=2.5, xmax=3.1, ymin=pos-0.5, ymax=pos+0.5, fill=passing_yards), show.legend = FALSE)+
  scale_fill_gradientn(colours=brewer.pal(7,"PuOr"))+
  geom_text(data=headers|>filter(hjust==0), mapping=aes(label=name, x=pos, y=-0.1), hjust=0, fontface="bold")+
  geom_text(data=headers|>filter(hjust==0.5), mapping=aes(label=name, x=pos, y=-0.1), fontface="bold")+
  geom_text(mapping=aes(y=pos, x=0.3, label=player_name), hjust=0)+
  geom_text(mapping=aes(y=pos, x=1.5, label=team_nick), hjust=0)+
  geom_text(mapping=aes(y=pos, x=2.8, label=passing_yards, color=yd_color))+
  scale_color_identity()+
  geom_text(mapping=aes(y=pos+0.1, x=3.5, label=round(comp_perc*100,1)), size=3.2)+
  geom_text(mapping=aes(y=pos-0.2, x=3.5, label=paste0(completions,"/",attempts)), size=2, color="grey30")+
  geom_text(mapping=aes(y=pos, x=4.1, label=passing_tds))+
  geom_hline(data=nfl_plot_lines, mapping=aes(yintercept=lines), color="grey50", size=0.3)+
  labs(title=title, caption="Data: nflfastR | Graphic: @tanya_shapiro")+
  scale_x_continuous(limits=c(0,4.1))+
  scale_y_continuous(limits=c(-5,0.2), expand=c(0,0))+
  facet_grid(conference~toupper(division))+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        plot.title = element_textbox_simple(halign=0.5),
        strip.background.x = element_rect(fill="black"),
        strip.text.x=element_text(face="bold", size=9, color="white"),
        strip.text.y=element_text(face="bold",size=9),
        plot.margin = margin(t=20, b=20, r=10, l=10),
        axis.text = element_blank(),
        axis.title=element_blank())