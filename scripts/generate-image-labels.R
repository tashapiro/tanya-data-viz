library(tidyverse)
library(geomtextpath)
library(ggimage)
library(cropcircles)
library(magick)
library(glue)

df_artists = read.csv("../data/df_artists.csv")

#get first 100
df_sub = df_artists%>%
  arrange(-popularity)%>%
  head(100)%>%
  mutate(circle = cropcircles::circle_crop(image_url))

#custom function to apply border to circle image with magick
border <- function(im) {
  ii <- magick::image_info(im)
  ii_min <- min(ii$width, ii$height)
  
  img <- image_blank(width = ii_min, height = ii_min, color = "none")
  drawing <- image_draw(img)
  symbols(ii_min/2, ii_min/2, circles = ii_min/2, bg = 'white', inches = FALSE, add = TRUE)
  dev.off()
  
  x = image_composite(image_scale(drawing, "x430"), image_scale(im, "x400"), offset = "+15+15")
  
  x
}

#custom function to wrap text around image
plot_image_label<-function(image,
                           label,
                           font_color="black", 
                           top_bottom="top",
                           hjust=0.2){
  
  t = seq(0, 1, length.out = 100) * pi
  
  #set up data
  if(top_bottom=="top"){data = data.frame(x = cos(t),y = sin(t))}
  else if(top_bottom=="bottom"){data=data.frame(x = cos(t),y = sin(t)*-1)}
  
  #set up data
  if(top_bottom=="top"){vjust=1.1}
  else if(top_bottom=="bottom"){vjust=-0.1}
  
  #set up data
  if(top_bottom=="top"){ymax=1.2}
  else if(top_bottom=="bottom"){ymax=0.9}
  
  #set up data
  if(top_bottom=="top"){ymin=-0.9}
  else if(top_bottom=="bottom"){ymin=-1.2}
  
  #plot
  ggplot() +
    geom_image(aes(x=0, y=0, image = image), asp=2.4/2.1, size=.7, image_fun=border) +
                 scale_x_continuous(limits = c(-1.2, 1.2))+
                 scale_y_continuous(limits=c(ymin, ymax))+
                 geom_textpath(data = data, aes(x,y,label = toupper(label)), linecolor=NA, color=font_color,
                               size = 14.5,  fontface="bold", vjust = vjust, hjust=hjust)+
                 coord_equal()+
                 theme_void()
}

#test function
plot_image_label(image=df_sub$circle[1],
                 label= df_sub$name[1],
                 font_color="#fada4b",
                 top_bottom = "bottom", 
                 hjust=hjust)

#craete new file path for images
df_sub = df_sub%>% mutate(new_image_path = paste0(tolower(str_replace_all(name," ","_")),".png"))

#list of hjust values by .11
list_hjust = seq(0,1, by=0.1)


#create loop to generate and save all images with labels
for(i in 1:nrow(df_sub)){
  pos = sample(c("top","bottom"),1)
  hjust = sample(list_hjust,1)
  path = df_sub$new_image_path[i]
  plot = plot_image_label(image=df_sub$circle[i],
                          label= df_sub$name[i],
                          font_color="#fada4b",
                          top_bottom = pos, 
                          hjust=hjust)
  ggsave(filename=glue("../images/circle-labels/{path}"), plot=plot, height=3.95, width=4.5)
}

#create loop to save all image files without the text
for(i in 1:nrow(df_sub)){
  img = border(image_read(df_sub$circle[i]))
  path = df_sub$new_image_path[i]
  image_write(img, path = glue("../images/circles/{path}"), format = "png")
}

