library(glue)

social_caption<-function(twitter="@tanya_shapiro",
                         github = "tashapiro",
                         linkedin=NA,
                         mastodon=NA,
                         icon_color="black",
                         font_color="black",
                         bg_color="white",
                         font_family="Roboto"){
  
  icons = list(
    twitter = "&#xf099",
    github = "&#xf09b",
    linkedin = "&#xf08c",
    mastodon = "&#xf4f6"
  )  
  
  social = list(twitter =twitter, github =github, linkedin =linkedin, mastodon =mastodon)
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
