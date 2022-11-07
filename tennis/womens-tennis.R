library(tidyverse)
library(rvest)
library(reactablefmtr)
library(reactable)
library(htmltools)
library(htmlwidgets)
library(sysfonts)
library(showtext)
library(webshot2)


#wikipedia data
url_women<-'https://en.wikipedia.org/wiki/List_of_Grand_Slam_women%27s_singles_champions'

#scrape data with rvest
raw<-url_women%>%
  rvest::read_html()%>%
  rvest::html_elements(".wikitable")%>%
  .[3]%>%
  html_table()%>%
  .[[1]]%>%
  rename(titles=1, player=2, ae=3, oe=4, australian_open = 5, french_open = 6, wimbledon=7,
         us_open=8, years=9)

#clean-up and reshape data with dplyr
df<-raw%>%
  mutate(player = trimws(str_replace_all(player,"\\/","")),
         ae = as.integer(str_replace(ae, "N/A",NA_character_)),
         oe = as.integer(str_replace(oe, "N/A",NA_character_)),
         rank = dense_rank(desc(titles)),
         )%>%
  select(rank, player, years, australian_open, french_open, us_open, wimbledon, titles)

#get first 11 records
df<-df%>%head(11)

#add in region manually
df$region<-c("AU","US","DE",rep("US",6),"FR","US")
df$region<-paste0("https://raw.githubusercontent.com/catamphetamine/country-flag-icons/master/flags/1x1/",df$region,".svg")

#create custom color palette for scale fill
pal_scale<-c("#F4FFFD","#E9DAEC","#A270E5","#43009A")

#main body of reactable - note, I downloaded the Chivo font locally from Google Fonts first!
table<-reactable(df%>%select(rank, player, region, australian_open, french_open, us_open, wimbledon, titles),
                 theme = reactableTheme(
                   style=list(fontFamily="Chivo"),
                   borderColor="#DADADA"
                   ),
                 defaultPageSize = 11,
          defaultColDef = colDef(vAlign="center",
                                 align="center",
                                 headerVAlign="center",
                                 style = color_scales(df, span = 4:7, colors=pal_scale),
                                 headerStyle = list(fontFamily="Chivo"),
                                 width=90
          ),
          columnGroups = list(
            colGroup(name="", columns=c("player","region","titles"), headerStyle = list(fontFamily="Chivo"), align="left"),
            colGroup(name="Event", columns=c("australian_open","us_open","french_open","wimbledon"), headerStyle = list(fontFamily="Roboto"))
          ),
          columns = list(
            rank = colDef(show=FALSE),
            player = colDef(name= "Player (First Title - Last Title)", 
                            align="left", width=250,
                            cell=function(value){
                                image <- img(src = paste0("https://raw.githubusercontent.com/tashapiro/tanya-data-viz/main/tennis/images/",str_replace_all(tolower(value)," ","_"),".png"), style = "height: 33px;", alt = value)
                                tagList(
                                  div(style = "display: inline-block;vertical-align:middle;width:50px", image),
                                  div(style="display: inline-block;vertical-align:middle;",
                                    div(style = "vertical-align:middle;", value),
                                    div(style = "vertical-align:middle;font-size:8pt;color:#8C8C8C;", paste0("(",df[df$player==value,]$years),")"))
                                )}
                              ),
            region = colDef(name="Region", 
                            align="left",
                            cell=function(value, index){
                              image <- img(src = value, style = "width:60px;height:20px;", alt = value)
                              player <- df$player[index]
                              if(player %in% c("Monica Seles","Molla Bjurstedt Mallory")){
                                tagList(div(style = "display:inline-block;vertical-align:middle;width:80px", image,"*")
                                      # div(style = "display:inline-block;", "*")
                                        )
                              }
                              else{
                                tagList(div(style = "display:inline-block;vertical-align:middle;width:50px", image))
                            }
                              },
                            width=120),
            australian_open = colDef(name="AU Open"),
            french_open = colDef(name="FR Open"),
            us_open = colDef(name="US Open"),
            wimbledon = colDef(name="Wmbl"),
            titles = colDef(name="Total Titles",
                            width=180,
                            class = "border-left",
                            align="left",
                            cell = data_bars(df,
                                             fill_color="#7814ff",
                                             text_position = "outside-end", 
                                             bar_height = 10,
                                             text_size = 12,
                                             min_value=5,
                                             max_value =32,
                                             background = "transparent"))
          )
          )



#add title, subtitle, footnote and source
#note, I downloaded fonts locally - Chivo & Font Awesome Branded Icons
table_final<-table%>%
  #title & subtitle
  htmlwidgets::prependContent(
    tagList(
      tags$img(src = "https://pngimg.com/uploads/tennis/tennis_PNG10416.png", style = "width:50px;height:34px;display:inline-block;vertical-align:middle;"),
      #tags$h1("trophy  ",style="font-family:'Font Awesome 6 Free';margin-bottom:0;display:inline-block;vertical-align:middle;padding-right:10px;"),
      tags$div("Grand Slam Legends", style="font-size:32px;font-weight:bold;font-family:Chivo;margin-bottom:0;display:inline-block;vertical-align:middle;"), 
      tags$h3("Top Women's Tennis Players by Singles Championship Titles", style="font-family:Chivo;margin-bottom:0;margin-top:0;font-weight:400;color:#8C8C8C;padding-left:10px;")
  )
  )%>%
  #footnote and source
  htmlwidgets::appendContent(
    tags$div("* Player represented more than one country during career. Most recent country shown.", style="font-family:Roboto;color:black;font-size:9pt;border-bottom-style:solid;border-top-style:solid;width:910px;padding-bottom:8px;padding-top:8px;border-color:#DADADA;"),
    tags$div(
      tags$div("Data: Wikipedia as of November 2022 | Graphic: ", style="display:inline-block;vertical-align:middle;"),
      tags$div("twitter", style="font-family:'Font Awesome 6 Brands';display:inline-block;vertical-align:middle;"),
      tags$div("tanya_shapiro", style="display:inline-block;vertical-align:middle;"),
      tags$div("github", style="font-family:'Font Awesome 6 Brands';display:inline-block;vertical-align:middle;"),
      tags$div("tashapiro", style="display:inline-block;vertical-align:middle;"),
             style="font-family:Chivo;color:#8C8C8C;font-size:10pt;width:910px;padding-top:8px;display:inline-block;vertical-align:middle;")
  )

#preview table
table_final


#option to save via webshot (you can also crop manually in browser)
html <- "womens_tennis.html"
#save html file
saveWidget(table_final, html)
#save as png static image
webshot(html, "womens_tennis.png", zoom=3, vwidth = 980)
