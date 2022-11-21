library(rvest)
library(tidyverse)
library(RColorBrewer)
library(viridis)
library(sysfonts)
library(showtext)
library(geomtextpath)
library(ggimage)
library(ggtext)

#font set up 

#Main Text Font - Roboto 
sysfonts::font_add_google("Roboto", "Roboto")

#Title Font - Cocon, similar to FIFA logo
#https://www.cufonfonts.com/font/cocon#google_vignette
sysfonts::font_add("Cocon", "fonts/CoconRegularFont.otf")

#Caption Font - Font Awesome Branded Icons for Twitter/Github logos
#download font awesome locally first! https://fontawesome.com/download
sysfonts::font_add('fb', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
#use showtext to render font, set dpi to 300 to adjust graphic for ggsave


showtext_auto()
showtext::showtext_opts(dpi = 300)

#DATA SCRAPING
#data frame by year with references to table index per wikipedia page
ref = data.frame(
    year= c(1982, 1986, 1990, 1994, 1998, 2002, 2006, 2010, 2014, 2018),
    index = c(15, 14, 15, 13, 25, 24, 23, 14, 19, 20),
    host = c("Spain","Mexico","Italy","United States","France","South Korea","Germany","South Africa","Brazil","Russia")
  )

#create links to all FIFA pages by year
ref$link = paste0("https://en.wikipedia.org/wiki/",ref$year,"_FIFA_World_Cup")

#helper function to scrape data
get_table<-function(year){
  url = ref$link[ref$year==year]
  index = ref$index[ref$year==year]
  data = url%>%
    read_html()%>%
    html_elements("table.wikitable")%>%
    .[index]%>%
    html_table()%>%
    .[[1]]|>
    rename(Pos=1)%>%
    select(Pos, Team,L, W)
  
  data$year = year
  data
}

#for loop to scrape and merge all years into one dataframe
data = data.frame()
for(i in ref$year){
  data = rbind(data, get_table(i))
}

#relabel West Germany as Germany (division of 1945 - split to East & West until 1994)
data<-data|>mutate(Team=case_when(Team=="West Germany"~"Germany",TRUE ~Team))

#get list of teams that competed in more than 3 competitions
teams <- unique(data$Team)
teams<-data|>group_by(Team)|>summarise(wc=n())|>filter(wc>3)

#Geographic Groupings
geo_group<-data.frame(
  country = c("Italy","Germany","England","Norway","Sweden","Switzerland","Netherlands","France","Belgium",
              "Spain","Croatia","Poland","Portugal","Denmark","Russia","Scotland",
              "United States","Argentina","Brazil","Paraguay","Uruguay","Costa Rica","Colombia","Chile","Mexico",
              "Nigeria","Cameroon","Saudi Arabia","South Korea","Japan","Iran","Morocco","Algeria"
  ),
  geo = c(rep("Europe",16), rep("Americas",9), rep("Other",8))
)

#Create Factor for Continent to arrange in order
geo_group$geo<-factor(geo_group$geo, levels=c("Europe","Americas","Other"))


#create dataset for plotting
df_plot<-data|>
  filter(!grepl("Eliminated",Pos) & Team %in% teams$Team)|>
  left_join(teams, by="Team")%>%
  left_join(geo_group, by=c("Team"="country"))%>%
  left_join(ref, by="year")%>%
  group_by(year)%>%
  complete(Team = teams$Team)%>%
  filter(!grepl("Eliminated",Team))|>
  mutate(counter=1, Pos = as.integer(Pos),
         group = case_when(is.na(Pos) ~ "Not Present", 
                           Pos==1 ~ "Win", Pos<=4 ~ "Semi Finals",
                           Pos<=8 ~ "Quarter Finals",
                           Pos<=16 ~ "Round of 16", 
                           Pos<=32 ~ "Group Stage"))|>
  ungroup()|>
  group_by(Team)|>
  arrange(Team, year)|>
  mutate(wc = sum(counter[group!="Not Present"]),
         win  = sum(counter[group=="Win"]))|>
  arrange(geo, -wc, -win, Team)|>
  ungroup()

#create unique y positions in separate df
df_pos<-df_plot|>distinct(Team)|>mutate(y_pos=row_number())

#merge the unique y positions to original data set 
df_plot<-df_plot|>left_join(df_pos, by="Team")

#introduce spacings for y position to break up geo groups
df_plot$y_pos<-ifelse(df_plot$y_pos<16,df_plot$y_pos, df_plot$y_pos+1)
df_plot$y_pos<-ifelse(df_plot$y_pos<26,df_plot$y_pos, df_plot$y_pos+1)

#create factor for top achievment status to order them properly 
df_plot$group = factor(df_plot$group, levels=c("Not Present","Group Stage","Round of 16", "Quarter Finals","Semi Finals","Win"))



#color palette for fill 
pal<-viridis(n=5, option="viridis")[1:5]

#2002 unusual year, South Korean AND Japan hosted, manually add Japan in as host
df_plot$host[df_plot$Team=="Japan" & df_plot$year==2002]<-"Japan"


#create heatmap by team and year
plot<-ggplot(data=df_plot, mapping=aes(x=as.character(year), y=y_pos, fill=group))+
  #tile heatmap
  geom_tile(color="white", width=.9, height=.9)+
  #circle points overlayed to represente Teams that Hosted in a given year
  geom_point(inherit.aes=FALSE, 
             data=df_plot|>filter(Team==host), 
             mapping=aes(x=as.character(year), y=y_pos), shape=21, fill="white", color="grey20", size=3)+
  #Annotation for "Hosted"
  annotate(geom="text", x="2006", y=-0.5, label="Hosted", family="Roboto", size=2.75)+
  geom_segment(mapping=aes(x="2006", xend="2006", y=-0.1, yend=.85), size=0.1, arrow=arrow(length=unit(.05, "in")))+
  #custom y axis labels for Teams/countries
  geom_text(mapping=aes(label=Team, y=y_pos, x=-5), size=3, hjust=0, color="#909090", family="Roboto")+
  #Geographic groupings with text on lines
  geomtextpath::geom_textsegment(mapping=aes(label="Europe",x=-6, xend=-6, y=15, yend=1), family="Roboto", fontface="bold")+
  geomtextpath::geom_textsegment(mapping=aes(label="Americas", x=-6, xend=-6, y=25, yend=17), family="Roboto", fontface="bold")+
  geomtextpath::geom_textsegment(mapping=aes(label="Other", x=-6, xend=-6, y=36, yend=27), family="Roboto", fontface="bold")+
  #scales to resize -- experimental, might be more efficient way....
  scale_x_discrete(breaks=as.character(unique(df_plot$year)), expand=expansion(mult=c(1.5,0.1), add=c(1,1)))+
  scale_y_reverse(breaks =seq(0,36, by=1), limits=c(37.2,-2), expand=c(0,0))+
  scale_fill_manual(values=c("grey90",pal),guide = guide_legend(ncol=1, override.aes=list(width=0.6, height=0.6)))+
  coord_equal()+
  labs(x="", y="", fill="Top Achievement")+
  theme_void()+
  theme(text=element_text(family="Roboto"),
        plot.title=element_text(face="bold"),
        legend.position = "none",
        legend.title =  element_text(size=8),
        legend.text=element_text(size=6),
        axis.text.y=element_blank(),
        axis.text.x = element_text(size=8, angle=90, family="Roboto"),
        axis.ticks=element_blank(),
        panel.background = element_blank())



#create title text - used with ggtext::geom_textbox
fifa_text<- "The FIFA World Cup, started in 1930, is an international football competition that takes place every four years. The analysis looks at the span of FIFA competitions between 1978 and 2018, tracking teams that participated in a minimum of 4 tournaments."
fifa_text2<-"It is important to note that the format of the competition and number of teams admitted has changed over time. In 1982, a total of 24 teams participated, and in subsequent tournaments, FIFA expanded the tournament to admit 32 teams. The Round of 16 was not introduced until 1986 - to standardize comparison, teams that achieved a ranking of at least 16 in competitions before 1986 were grouped as Round of 16. "
fifa_text3<-"Graphic groups teams by geographic location. The other grouping combines teams from Asia, Africa, and Australia. Between 1950 and 1990, Germany competed as two entities, West Germany and East Germany (result of the 1945 division). East Germany participated once in 1974. To standardize analysis, West Germany is relabeled as Germany for both 1982 and 1990 tournaments."

title<-paste0("<span style='font-family:Cocon;font-size:28pt;'>**FIFA WORLD CUP**</span><br><br>",
              "<span style='font-face:CA;font-size:11pt;color:#545454;'>",fifa_text,"<span><br><br>",
              "<span style='font-face:Roboto;font-size:11pt;color:#545454;'>",fifa_text2,"<span><br><br>",
              "<span style='font-face:Roboto;font-size:11pt;color:#545454;'>",fifa_text3,"<span>")

#create caption text - used with ggtext::geom_textbox
caption<-paste0(
  "<span style='font-family:Roboto;padding-right:10px;'>Source: Wikipedia",
  "<span style = 'color:#ffffff;'>.....</span>",
  "<span style='font-family:fb;'>&#xf099;</span>",
  "<span style='font-family:Roboto;'>@tanya_shapiro</span>",
  "<span style = 'color:#ffffff;'>...</span>",
  "<span style='font-family:fb;'  >&#xf09b;</span>",
  "<span style='font-family:Roboto;'> tashapiro </span>")


#create df for custom legend (positions relative to new plot creating in next step)
df_legend<-data.frame(x= rep(seq(3.38, 5, by=.6),2),
                      y= c(rep(2.1, 3),rep(1.95,3)),
                      label=c("Not Present","Group Stage", "Round of 16","Quarter Finals","Semi Finals","Winner"),
                      color = c("grey90",pal))


#Adding alpha function to geom_image
#solution from: https://stackoverflow.com/questions/60820565/is-there-a-way-to-add-an-alpha-value-within-geom-image-in-ggplot
transparent <- function(img) {
  magick::image_fx(img, expression = "0.1*a", channel = "alpha")
}

#Final Plot (note: positioning of layers was very experimental, hindsight would set scales from 0 to n)
ggplot()+
  #add in tile heatmap
  annotation_custom(ggplotGrob(plot), xmin=3, xmax=8.5, ymin=1.25, ymax=4.75)+
  #add ball image 
  geom_image(mapping=aes(x=3.3, y=4.5, image="https://raw.githubusercontent.com/tashapiro/tanya-data-viz/main/fifa-world-cup/ball.png"), image_fun = transparent, size=.4)+
  #add plot title
  geom_textbox(mapping=aes(x=3.3, y=4.5, label=title), hjust=0, vjust=1, box.size=NA, fill=NA,  width = unit(4, "inch"))+
  #add caption
  geom_textbox(mapping=aes(x=3.3, y=1.25, label=caption), size=3, color="#6E6E6E", hjust=0, vjust=1, box.size=NA, fill=NA,  width = unit(3.5, "inch"))+
  #add custom legend
  geom_text(mapping=aes(x=3.35, y=2.25, label="Best Achievement"), hjust=0, family="Roboto", fontface="bold")+
  geom_text(data=df_legend, mapping=aes(x=x+.08, y=y, label=label), hjust=0, family="Roboto", size=3.5)+
  geom_point(data=df_legend, mapping=aes(x=x, y=y, color=I(color)), shape=15, size=6)+
  scale_x_continuous(limits=c(3,7), expand=c(0,0))+
  scale_y_continuous(limits=c(1,5), expand=c(0,0))+
  coord_equal()+
  theme_void()

#save
ggsave("fifa.png", bg="white" , height=9, width=9, units="in")
