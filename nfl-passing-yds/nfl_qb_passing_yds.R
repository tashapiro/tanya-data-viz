library(tidyverse)
library(nflfastR)
library(ggimage)
library(geomtextpath)
library(ggtext)
library(glue)

#download fonts locally
#font awesome uses 6.4.2
#https://fontawesome.com/download
#Chivo
#https://fonts.google.com/specimen/Chivo

pbp = nflfastR::load_pbp(season=2023)
teams = nflfastR::teams_colors_logos
players = nflreadr::load_players()

social_caption<-function(twitter="@tanya_shapiro",
                         github = "tashapiro",
                         threads = "tshapiro",
                         linkedin=NA,
                         mastodon=NA,
                         icon_color="black",
                         font_color="black",
                         bg_color="white",
                         font_family="Roboto"){
  
  icons = list(
    twitter = "&#xe61b",
    github = "&#xf09b",
    linkedin = "&#xf08c",
    mastodon = "&#xf4f6",
    threads = "&#xe618"
  )  
  
  social = list(threads= threads, linkedin =linkedin, twitter =twitter, mastodon =mastodon, github =github)
  social = social[!is.na(social)]
  
  caption = ""
  for (name in names(social)){
    icon = icons[name]
    info = social[name]
    html = glue("<span style='font-family:\"Font Awesome 6 Brands\";color:{icon_color};'>{icon};</span><span style='color:{bg_color};'>.</span><span style='font-family:{font_family};color:{font_color};'>{info}</span><span style='color:{bg_color};'>..</span>")
    caption = paste0(caption,html)
  }
  
  caption
}

#create social caption
my_socials<-social_caption(twitter="@tanya_shapiro",
          github = "tashapiro",
          linkedin="shapirotanya",
          mastodon=NA,
          icon_color="#464F51",
          font_color="#464F51",
          bg_color="white",
          font_family="Chivo")


caption<-paste0("<span style='fony-family:Chivo;color:#464F51'>Source: {nflfastR} </span><span style='color:white;'>..</span>",my_socials)

weekly<-pbp|>
  group_by(passer_id, week)|>
  summarise(passing_yards = sum(passing_yards, na.rm=T))|>
  ungroup()|>
  group_by(passer_id)|>
  mutate(games_played = n_distinct(week))|>
  left_join(players|>select(gsis_id, display_name, team_abbr, position), by=c("passer_id"="gsis_id"))|>
  ungroup()|>
  filter(games_played>=4 & position=='QB')|>
  left_join(teams, by="team_abbr")


weekly_avg<-weekly|>
  group_by(week)|>
  summarise(avg = mean(passing_yards))


totals<-weekly|>
  group_by(passer_id, display_name, team_abbr, team_logo_espn)|>
  summarise(total = sum(passing_yards))|>
  arrange(desc(total))|>
  ungroup()|>
  mutate(rank = row_number())

weekly<-weekly|>left_join(totals|>select(passer_id, total,rank), by="passer_id")

weekly_subset<-weekly|>filter(rank<26 & rank!=1)
number_one<-weekly|>filter(rank==1)|>left_join(weekly_avg, by="week")
totals<-totals|>filter(rank<26)|>mutate(text_x = case_when(rank<10 ~ 1.32, TRUE ~ 1.42))


#factor player desc order of total passing yards
totals$display_name<-factor(totals$display_name, levels = totals$display_name)
number_one$display_name<-factor(number_one$display_name, levels = totals$display_name)
weekly_subset$display_name<-factor(weekly_subset$display_name, levels = totals$display_name)


ggplot()+
  #plot team logo
  geom_image(data=totals, mapping=aes(y=600, x=3.7, image=team_logo_espn), by="height", size=0.18)+
  #plot player rank
  geom_text(data = totals, mapping=aes(y=600, x=text_x, label=display_name), hjust=0, family="Chivo", size=3)+
  #plot player name
  geom_text(data = totals, mapping=aes(y=600, x=1.02, label=rank), hjust=0, family="Chivo", size=3, fontface="bold")+
  #craete x axis per plot
  geom_hline(data=weekly_avg, mapping=aes(yintercept=0), color="black", linewidth=1)+
  geom_line(data = weekly_avg, mapping=aes(x=week, y=avg, group=1), color="grey60", linewidth=0.8)+
  #player line for all except #1
  geom_line(data = weekly_subset, mapping=aes(x=week, y=passing_yards, group=1, color=I(team_color)),
            show.legend = F, linewidth=.8)+
  #add in player reference lines for #1 with text labels per line
  geomtextpath::geom_textline(
    data=number_one, mapping=aes(x=week, y=passing_yards, color=I(team_color), label="QB Stat"),
    vjust=-.7, hjust=0.99, size=2.25, linewidth=.8
  )+
  geomtextpath::geom_textline(
    data=number_one, mapping=aes(x=week, y=avg, label="Weekly Avg"),
    vjust=1.9, hjust=0.99, size=2.25, linewidth=.8, color="grey50"
  )+
  facet_wrap(~display_name)+
  scale_x_continuous(expand=c(0,0), labels=paste0("W",1:4))+
  scale_y_continuous(limits=c(0,700), 
                     breaks = c(0,100,200,300,400,500),
                     labels=c('',100,'',300,'',500), expand=c(0,0))+
  labs(title="NFL QB Weekly Passing Yds 2023",
       subtitle = "Top 25 quarterbacks show by season passing yards. Compared to weekly average for all quarterbacks.",
       caption = caption)+
  theme_minimal()+
  theme(text = element_text(family="Chivo"),
        plot.title = element_text(face='bold', size=20),
        plot.margin = margin(t=20, l=20, r=30, b=10),
        plot.subtitle = ggtext::element_markdown(color='#464F51', size=11),
        plot.caption = ggtext::element_markdown(margin=margin(t=20), size=8, hjust=0),
        axis.text = element_text(size=7.5),
        axis.text.y=element_text(size=7),
        strip.text = element_blank(),
        panel.spacing.x = unit(1.5, "lines"),
        axis.title = element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y = element_line(linewidth=.4))


ggsave("nfl_passing_yds.png", height=9.5, width=9.5, bg="white")
        axis.title.y=element_blank())
