# Personal Data Visualization Projects


## Objective
This repo is dedicated to sharing my adventures with miscellaneous data visualization projects with others.

Most of my visualization work is coded with R, directories will include datasets and code files. Code files will include comments to walk through different steps of generating the visualiztion.

## Summary

| **Topic**                                    | **Main Packages**                     | **Source**                |
|:---------------------------------------------|:--------------------------------------|:--------------------------|
| [Popular Spotify Artists](./spotify-artists) | ggplot, ggtext, ggimage.              | spotifyr                  |
| [Speaker Election](./soth-election)          | ggplot, ggtext, ggimage, ggsankey     | voteogram                 |
| [FIFA World Cup](./fifa-world-cup)           | ggplot, ggtext, ggimage, geomtextpath | Wikipedia                 |
| [ChatGPT vs. Lensa](./chat-gpt)              | ggplot, geomtextpath, ggtext          | Google Trends             |
| [Grand Slam Tennis Legends](./tennis)        | htmltools, htmlwidgets, reactablefmtr | Wikipedia                 |
| [Selling Sunset Vibes](./selling-sunset)     | ggplot, ggimage                       | A Tweet                   |
| [The Leo Chart](./dicaprio-gfs)              | ggplot, ggimage                       | Reddit                    |
| [Nintendo Switch Games](./nintendo-switch)   | ggplot, ggimage, ggchicklet           | Wikipedia                 |
| [WestWorld Attribute Matrix](./westworld)    | ggplot, ggtext, ggimage               | Open-Source Psychometrics |

# Gallery

## [Most Popular Artists on Spotify](./spotify-artists)

![plot](./spotify-artists/plots/popular-artists.png)

## [Speaker of The House Election: GOP Flip-Floppers](./soth-election)

![plot](./soth-election/plot/soth-election.png)

## [FIFA World Cup](./fifa-world-cup/fifa-world-cup.R)

![plot](./fifa-world-cup/fifa.png)

## [Grand Slam Legends](./tennis/womens-tennis.R)

![plot](./tennis/womens-tennis.png)

## [ChatGPT vs. Lensa](./chatgpt-lensa/chatgpt-lensa.R)

![plot](./chatgpt-lensa/chatgpt-lensa.png)

## [Nintendo Switch: Top Games](https://github.com/tashapiro/tanya-data-viz/blob/main/nintendo-switch/code/nintendo-switch.R)

Top 5 Nintendo Switch Games based on copies sold. Data from [Wikipedia](https://en.wikipedia.org/wiki/List_of_best-selling_Nintendo_Switch_video_games). Graphic rendered in the style of a Nintendo Switch, screen used as plot background for bar chart. Graphic created with **ggplot**, **ggtext**, **ggimage**, and **ggchicklet**.

![plot](./nintendo-switch/plot/switch.png)

## [The Leo Chart](https://github.com/tashapiro/tanya-data-viz/blob/main/dicaprio-gfs/dicaprio-gfs.R)

Based on a chart found on [Reddit circa 2019](https://www.insider.com/leonardo-dicaprio-girlfriends-reddit-chart-2019-3) by **TrustLittleBrother**. I wanted to recreate the infamous graphic using ggplot and related ggplot libarires in R. Images of Leo and his party of girlfriends taken from the original image. Code uses ggplot, ggtext, and ggimage.

![plot](./dicaprio-gfs/plot/dicaprio-gfs.png)

## [Westworld Attribute Matrix](https://github.com/tashapiro/tanya-data-viz/blob/main/westworld/code/ww-radar-plot.R)
Inspired by the ["Attribute Matrix"](https://wwrp.fandom.com/wiki/Attribute_Matrix) show on the HBO series, Westworld. Data from the [Open-Source Psychometrics Project](https://openpsychometrics.org/). Plot created using **ggplot**, **ggtext**, and **ggimage**. 

![plot](./westworld/plots/westworld-radar-plot.png)

## [NYT Times - Where Abortion Is Prohibited](https://github.com/tashapiro/tanya-data-viz/tree/main/nyt-abortion-map)
Attempt to recreate New York Times' abortion ban choropleth map using R (ggplot + ggpatern). The [original image](https://www.instagram.com/p/Cf1-6ifuGfR/) is from NYT's Instagram.
![plot](./nyt-abortion-map/recreated-nyt-map.jpeg)

## [Selling Sunset](https://github.com/tashapiro/tanya-data-viz/blob/main/selling-sunset/selling-sunset.R)
Recreated the funny (and totally fictious) plot created by [@bananapeele](https://twitter.com/bananapeele/status/1517987473837674501?s=20&t=nIAvx3gUHxyEAMogmJUOdg).
&nbsp;

![plot](./selling-sunset/selling_sunset.png)

## [Globe Bar Plots](https://github.com/tashapiro/tanya-data-viz/tree/main/globe-bar-plot)

Inspiration for the Globe Bar plot came from [R Graph Gallery's tutorial](https://www.r-graph-gallery.com/circular-barplot.html) on circular bar plots. Used the tutorial to generate the bar plot, then overlayed a choropleth map (in the styel of a globe) in the center. Created using ggplot2.

![plot](./globe-bar-plot/africa_marriage.jpeg)


