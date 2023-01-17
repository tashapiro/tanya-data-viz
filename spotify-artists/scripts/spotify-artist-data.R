library(tidyverse)
library(spotifyr)
library(glue)


#set up spotify access: https://developer.spotify.com/dashboard/login
Sys.setenv(SPOTIFY_CLIENT_ID = 'XXXXX')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'XXXXX')
access_token <- get_spotify_access_token()


#get genre artists - uses spotify search. limit is 50, uses offset to collect more than 50
offsets = seq(0, by=51, length.out=20)
pop_artists = data.frame()
for(i in offsets){
  temp = spotifyr::get_genre_artists(genre="Pop", limit=50, market="US", offset=i)
  pop_artists = rbind(pop_artists, temp)
}

#grab pop artists
pop_artists = pop_artists%>%
  unnest(images)%>%
  group_by(id)%>%
  mutate(max_height=max(height),
         genres = as.character(genres))%>%
  filter(height==max_height)%>%
  rename(image_url = url, image_width=width, image_height=height)%>%
  ungroup()

#write to csv
write.csv(pop_artists, "../data/df_artists.csv", row.names=FALSE)
