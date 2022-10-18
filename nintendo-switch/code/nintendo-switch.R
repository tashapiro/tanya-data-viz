library(tidyverse)
library(ggchicklet)
library(ggimage)
library(ggtext)
library(sysfonts)
library(showtext)

#import texts
sysfonts::font_add_google("Lato","Lato")
sysfonts::font_add_google("Roboto","Roboto", regular.wt=300)
#must download font awesome locally first! https://fontawesome.com/download
font_add('fs', 'fonts/Font Awesome 6 Free-Solid-900.otf')
showtext_auto()

#data frame for buttons
buttons<-data.frame(
  x = c(-1.1,-0.4, -0.75, -0.75,
        -0.75,7.5,
        7.15,7.85,7.5,7.5,
        7.2),
  y= c(2,2, 2.3, 1.7,
       3.2,2,
       3.2,3.2,3.5,2.9,
       1.2),
  size = c(rep(7,4),
           11,11,
           rep(7,4),
           7),
  type = c("left_arrow","right_arrow","up_arrow","down_arrow","joystick_left","joystick_right",
           "Y","A","X","B", "home"),
  icon = c("<span style='font-family:fs'>&#xf0d9;</span>",
           "<span style='font-family:fs'>&#xf0da;</span>",
           "<span style='font-family:fs'>&#xf0d8;</span>",
           "<span style='font-family:fs'>&#xf0d7;</span>",
           rep("",6),
           "<span style='font-family:fs'>&#xf015;</span>")
)

#games data from wikipedia: https://en.wikipedia.org/wiki/List_of_best-selling_Nintendo_Switch_video_games
games <- data.frame(
  rank = seq(1,5, by=1),
  title = c("Mario Kart 8 Deluxe","Animal Crossing: New Horizons","Super Smash Bros. Ultimate", "Legend of Zelda: Breath of the Wild",
            "Pokemon Sword and Shield"),
  copies = c(46.82, 39.38, 28.82, 27.14, 24.5),
  pos = rev(seq(1,3.325, length.out =5)),
  local = c("https://raw.githubusercontent.com/tashapiro/tanya-data-viz/main/nintendo-switch/icons/mario-kart.png",
            "https://raw.githubusercontent.com/tashapiro/tanya-data-viz/main/nintendo-switch/icons/animal-crossing.png",
            "https://raw.githubusercontent.com/tashapiro/tanya-data-viz/main/nintendo-switch/icons/super-smash.png",
            "https://raw.githubusercontent.com/tashapiro/tanya-data-viz/main/nintendo-switch/icons/zelda.png",
            "https://raw.githubusercontent.com/tashapiro/tanya-data-viz/main/nintendo-switch/icons/pokemon.png")
)


#title to plot with geom_textbox
title<-paste0(
  "<span style='font-size:22pt;color:white;font-family:Lato;'>**TOP NINTENTDO SWITCH GAMES**</span><br>",
  "<span style='font-family:Roboto;font-size:12pt;color:#ECECEC;'>Based on total number of copies sold. Copies sold expressed in millions. Graphic does not consider impact of varying game release dates. Data as of Oct 2022.</span>"
)



#taken from StackOverflow from Kamil Slowikowski https://stackoverflow.com/questions/30136725/plot-background-colour-in-gradient
make_gradient <- function(deg = 45, n = 100, cols = blues9) {
  cols <- colorRampPalette(cols)(n + 1)
  rad <- deg / (180 / pi)
  mat <- matrix(
    data = rep(seq(0, 1, length.out = n) * cos(rad), n),
    byrow = TRUE,
    ncol = n
  ) +
    matrix(
      data = rep(seq(0, 1, length.out = n) * sin(rad), n),
      byrow = FALSE,
      ncol = n
    )
  mat <- mat - min(mat)
  mat <- mat / max(mat)
  mat <- 1 + mat * n
  mat <- matrix(data = cols[round(mat)], ncol = n)
  grid::rasterGrob(
    image = mat,
    width = unit(1, "npc"),
    height = unit(1, "npc"), 
    interpolate = TRUE
  )
}

g <- make_gradient(
  deg = 230, n = 500, cols = c("black","#020024","#020024","#530979","#8c0d5e")
)



pal_screen = "white"
pal_s_text = "black"
pal_bar = '#099DFF'
pal_main<-'#323436'
pal_red <-'#EE3E35'
pal_blue <- '#00B1D2'
pal_button<-'#1E1E1E'
anchor<-1.4

ggplot()+
  annotation_custom(
    grob = g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  )+
  #red controller
  geom_rrect(mapping=aes(xmin=0-1.5, xmax=0.75, ymin=0, ymax=4),
             r = unit(0.35, 'npc'), fill=pal_red, color="black", size=0.3)+
  #blue controller
  geom_rrect(mapping=aes(xmin=6.75-0.75, xmax=6.75+1.5, ymin=0, ymax=4),
             r = unit(0.35, 'npc'), fill=pal_blue, color="black", size=0.3)+
  #main panel
  geom_rect(mapping=aes(xmin=0, xmax=6.75, ymin=0, ymax=4), fill=pal_main, color="black", size=0.3)+
  #border panel
  geom_rrect(mapping=aes(xmin=0.2, xmax=6.75-0.2, ymin=0.1, ymax=3.9),
             r = unit(0.05, 'npc'), fill="black")+
  #screen
  geom_rect(mapping=aes(xmin=0.625, xmax=6.75-0.625, ymin=0.375, ymax=4-0.375), fill=pal_screen)+
  #joystick
  geom_point(data= buttons|>filter(type %in% c("joystick_left","joystick_right")), mapping=aes(x=x, y=y), size=13, color="#4C4C4C")+
  geom_point(data= buttons|>filter(type %in% c("joystick_left","joystick_right")), mapping=aes(x=x, y=y), stroke=0.8, shape=3, size=8.7, color=pal_button)+
  #round buttons
  geom_point(data= buttons|>filter(type!="home"), mapping=aes(x=x, y=y, size=size), color=pal_button)+
  #text for buttons
  geom_text(data= buttons|>filter(type %in% c("A","X","Y","B")), mapping=aes(x=x, y=y, label=type), color="white")+
  #misc button - home, square, plus, and line
  geom_point(data=buttons|>filter(type=="home"), mapping=aes(x=x,y=y,size=size), shape=21, stroke=1.5, fill=pal_button, color="#808080")+
  geom_point(mapping=aes(x=-0.45,y=1.2), size=6, shape=15, color=pal_button)+
  geom_point(mapping=aes(x=-0.45,y=1.2), size=4, color="black")+
  geom_point(mapping=aes(y=3.7, x=7), shape=3, size=2, stroke=2, color=pal_button)+
  geom_segment(mapping=aes(y=3.7, yend=3.7, x=-0.15,xend=-0.35), color=pal_button, size=1.5)+
  #icons for buttons
  geom_richtext(data=buttons|>filter(icon!=""), mapping=aes(x=x, y=y, label=icon), color="black", size=3, fill = NA, label.color = NA,hjust=0.5)+
  scale_size_identity()+
  #tile
  geom_textbox(mapping=aes(x=3.75, y=4.75), label=title, box.size=NA, fill=NA, width=unit(6.5, "in"), halign=0.5)+
  geom_text(mapping=aes(x=8.2, y=-0.65), label="Source: Wikipedia | Graphic: @tanya_shapiro ", color="#ECECEC", size=3, hjust=1, family="Roboto")+
  scale_y_continuous(limits=c(-0.75,5.5))+
  #plot game data
  geom_text(data=games, mapping=aes(y=pos+0.02, x=anchor, label=title), color=pal_s_text, hjust=0, size=3, family="Roboto")+
  geom_segment(data=games, mapping=aes(y=pos-0.24, yend=pos-0.24, x=anchor, xend=anchor+(0.088*copies)), size=7, color=pal_bar)+
  geom_text(data=games, mapping=aes(y=pos-0.24, x=anchor+0.1, label=paste0(round(copies,1),"M copies sold")), color="white", hjust=0, size=2.7, family="Roboto")+
  geom_image(data=games, mapping=aes(y=pos-0.18, x=anchor-0.35, image=local), asp=1.6, size=0.04)+
  theme_void()


ggsave(filename="switch.png", height=5, width=7.5)
