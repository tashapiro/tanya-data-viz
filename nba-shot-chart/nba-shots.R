library(tidyverse)
library(BasketballAnalyzeR)
library(nbastatR)
library(ggimage)
library(cropcircles)
library(ggtext)
library(glue)
library(janitor)
library(htmltools)

grey<-'#818990'
#must download Chivo locally
font<-"Chivo"

#create bigger lag time for connection to import nbastatdata
Sys.setenv(VROOM_CONNECTION_SIZE=500072)

#get shot data to pull back for specific teams
shots<-nbastatR::teams_shots(team_ids=c('1610612744','1610612747','1610612755','1610612742','1610612760','1610612757'), seasons=2023)

shots_player<-shots|>filter(namePlayer %in% c("LeBron James","Stephen Curry","Joel Embiid","Luka Doncic",
                                              "Damian Lillard","Shai Gilgeous-Alexander"))|>
  #scale x and y coords to fit court (eyeballed it)
  mutate(x = (locationX/10)-0,
         y = (locationY/10)-41.75)|>
  #remove anyshots beyond half court range
  filter(y<0)

#get accuracy stat per player
player_stats<-shots_player|>
  group_by(namePlayer, nameTeam, typeEvent)|>
  summarise(shots =n())|>
  pivot_wider(names_from=typeEvent, values_from=shots)|>
  janitor::clean_names()|>
  mutate(total_shots = made_shot+missed_shot,
         accuracy = made_shot/total_shots)|>
  ungroup()|>
  arrange(-accuracy)

#custom data frame with images (from espn) and labels to pass into ggtext
images <- data.frame(
  namePlayer = c("Joel Embiid","Luka Doncic","LeBron James","Stephen Curry","Damian Lillard","Shai Gilgeous-Alexander"),
  label = c("Joel Embiid","Luka Doncic","LeBron James","Stephen Curry","Damian Lillard","Shai G-Alexander"),
  image = c("https://a.espncdn.com/combiner/i?img=/i/headshots/nba/players/full/3059318.png",
            "https://a.espncdn.com/combiner/i?img=/i/headshots/nba/players/full/3945274.png",
            "https://a.espncdn.com/combiner/i?img=/i/headshots/nba/players/full/1966.png",
            "https://a.espncdn.com/combiner/i?img=/i/headshots/nba/players/full/3975.png",
            "https://a.espncdn.com/combiner/i?img=/i/headshots/nba/players/full/6606.png",
            "https://a.espncdn.com/combiner/i?img=/i/headshots/nba/players/full/4278073.png")
)|>
  left_join(player_stats, by=c("namePlayer"="name_player"))|>
  left_join(df_dict_nba_teams|>select(nameTeam,slugTeam), by=c("name_team"="nameTeam"))|>
  mutate(text_label = glue("<span style='font-size:14px;'>**{toupper(label)}**</span><br><span style='font-size:12.5px;color:grey40;'>{slugTeam} · {total_shots} shots · </span><span style='color:#ED254E;font-size:12.5px;'>{round(accuracy*100,0)}% made</span>"))

#circle crop images
images$cropped<-cropcircles::circle_crop(images=images$image, border_size = 1, border_colour = "whitesmoke")
#factor players by accuracy
shots_player$namePlayer<-factor(shots_player$namePlayer, levels = player_stats$name_player)
images$namePlayer<-factor(images$namePlayer, levels = player_stats$name_player)


#create caption with html tools to pass into plot.caption theme with ggtext::element_textbox_simple
#must download font awesome brands locally to work: https://fontawesome.com/download
caption<-tagList(
    tags$span("Source: nbastatR"),
    tags$br(),
    tags$span(HTML(enc2utf8("&#xf099;")), style='color:#ED254E;font-family:"Font Awesome 6 Brands\"'),
    tags$span("@tanya_shapiro"),
    tags$span(HTML(enc2utf8("&#xf09b;")), style='color:#ED254E;font-family:"Font Awesome 6 Brands\"'),
    tags$span("tashapiro"),
    tags$span(HTML(enc2utf8("&#xf08c;")), style='color:#ED254E;font-family:"Font Awesome 6 Brands\"'),
    tags$span("shapirotanya")
  )

#add court drawings
BasketballAnalyzeR::drawNBAcourt(ggplot(data=shots_player), 
                                 size=0.5, col="grey20")+
  #shot data
   geom_point(data=shots_player,
              mapping=aes(x=x,y=y, fill=typeEvent),
              shape=21, color="white", size=2.5, alpha=0.8)+
  #backdrop image with fill to create border
  geom_image(data=images, mapping=aes(x=-20, y=6, image=cropped), color="#818990", size=0.16, asp=1/1.2)+
  #player image
  geom_image(data=images, mapping=aes(x=-20, y=6, image=cropped), size=0.15, asp=1/1.2)+
  #text per player with name, team, and stats
  ggtext::geom_textbox(data=images, mapping=aes(x=5, y=6, label=text_label), 
                        fill=NA, box.size=NA, 
                        family=font)+
  scale_fill_manual(values=rev(c("grey70","#ED254E")))+
  scale_y_continuous(expand=c(0.1,0.2))+
  facet_wrap(~namePlayer, ncol=3)+
  coord_equal()+
  guides(fill = guide_legend(override.aes=list(size=5)))+
  labs( fill="Type",
        title="NBA Player Shot Chart", 
       subtitle = "2022-23 Regular Season",
       caption = caption)+
  theme(legend.position = "top",
        legend.title = element_text(face="bold", size=12),
        plot.margin = margin(t=20),
        legend.text = element_text(size=12),
        legend.margin = margin(rep(0,4)),
        strip.text = element_blank(),
        strip.background = element_blank(),
        legend.key = element_blank(),
        axis.text = element_blank(),
        legend.justification = "left",
        text = element_text(family=font),
        panel.background = element_blank(),
        plot.title = element_text(face="bold", size=18),
        plot.subtitle = element_text(color="#818990", size=16, margin=margin(b=5)),
        panel.grid.minor=element_blank(),
        plot.caption = element_textbox_simple(hjust=0.01, color="#818990", margin=margin(b=10), size=10),
       # panel.grid.major = element_line(color="grey90", linewidth=0.3),
        axis.title=element_blank(),
        axis.ticks = element_blank())


