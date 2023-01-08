library(voteogram)
library(tidyverse)
library(ggsankey)
library(ggtext)
library(systemfonts)
library(showtext)

#import font
font_add_google("Roboto Slab", family = "Roboto Slab")
font_add_google("Roboto", family = "Roboto")

#must install font awesome brands locally
sysfonts::font_add('Font Awesome 6 Brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
showtext_auto()
showtext::showtext_opts(dpi=300)

#import script to create caption with social icons
source("social-caption.R")

#roll call ids for speaker elections
rc_ids = c(2:7,9:13,15,16,18,20)

#loop through ids and use voteogram to pull relevant roll call data
df_rcs = data.frame()
for (rc in rc_ids){
  temp = voteogram::roll_call("house",118, 1, rc)
  temp = temp$votes
  temp$roll_call = rc
  df_rcs = rbind(df_rcs, temp)
}

#get list of republicans who did not vote for McCarthy consistently
df_changers = df_rcs%>%
  filter(party=="R")%>%
  group_by(bioguide_id, member_name)%>%
  summarise(positions = paste(unique(position), collapse=", "))%>%
  filter(positions!="McCarthy")

changer_ids = df_changers$bioguide_id

pivot = df_rcs%>%
  filter(bioguide_id %in% changer_ids)%>%
  select(member_name, state_abbrev, roll_call, position)%>%
  pivot_wider(names_from=roll_call, values_from = position)

#create data frame to show votes by member in wide format (row per member, column per roll call)
votes=df_rcs%>%
  filter(party=="R" & bioguide_id %in% changer_ids)%>%
  mutate(new_pos = case_when(position %in% c("Present","Not Voting")~"Abstain",
                             position %in% c("Banks","Zeldin","Donald J.","Biggs")~"Other Candidate",
                             TRUE ~ position))%>%
  select(member_name, roll_call, new_pos)%>%
  pivot_wider(names_from=roll_call, values_from=new_pos)

#relabel columns
names(votes) = c("member","rc2","rc3","rc4","rc5","rc6","rc7","rc9","rc10","rc11","rc12","rc13","rc15","rc16","rc18")

#votes for the 15th (roll call 20) round not out yet, manually added to data set
vote20 = data.frame(
  rc20 = c("Abstain","McCarthy","Abstain","McCarthy","McCarthy",
         "McCarthy","McCarthy","Abstain","McCarthy","Abstain",
         "Abstain","McCarthy","McCarthy","McCarthy","McCarthy",
         "McCarthy","McCarthy","McCarthy","McCarthy","Abstain",
         "McCarthy","McCarthy","McCarthy")
)

#combine rc 20 vote with original votes data set
votes = cbind(votes, vote20)

#create sankey df with ggsankey::make_long
df_sankey = votes %>%
  make_long(names(votes)[-1])


#ref list for numerical ordering of roll calls
ref = list(rc2=1, rc3=2, rc4=3, rc5=4, rc6=5, rc7=6, rc9=7, rc10=8, rc11=9, rc12=10, rc13=11, rc15=12,
           rc16=13, rc18=14, rc20=15)

#format data - add info for # of vote labels, new column for label position + color
df_sankey = df_sankey|>
  group_by(x, node)|>
  mutate(votes = n())|>
  ungroup()|>
  mutate(xpos = unlist(ref[x]),
         xpos = case_when(is.na(next_node)~xpos+.15,
                          TRUE ~ xpos+.4),
         votes = case_when(votes>2 ~ as.character(votes),
                           is.na(next_x) ~ as.character(votes),
                           x=="rc2"~ as.character(votes),
                           TRUE ~ ""),
         label_color = case_when(node=="Other Candidate" & !is.na(next_node) ~ "white",
                                 TRUE ~ "grey20")
  )

#factor for ordering of candidates
order = c("McCarthy","Donalds","Jordan","Hern","Other Candidate","Abstain")
#factor nodes - change order of positions
df_sankey$node = factor(df_sankey$node, levels=order)
df_sankey$next_node = factor(df_sankey$next_node, levels=order)

#social caption 
caption = paste0("<span>Source: {voteogram}<span><br>",social_caption(mastodon="fosstodon/@tanya_shapiro", linkedin="shapirotanya", font_family="Roboto Slab"))

#custom labels for legend - include image + label - works with ggtext::element_markdown
mccarthy = "<img src='https://raw.githubusercontent.com/tashapiro/tanya-data-viz/main/soth-election/images/legend/McCarthy.png' width='40'><br><span>McCarthy</span>"
donalds = "<img src='https://raw.githubusercontent.com/tashapiro/tanya-data-viz/main/soth-election/images/legend/Donalds.png' width='40'><br><span>Donalds</span>"
jordan = "<img src='https://raw.githubusercontent.com/tashapiro/tanya-data-viz/main/soth-election/images/legend/Jordan.png' width='40'><br><span>Jordan</span>"
hern = "<img src='https://raw.githubusercontent.com/tashapiro/tanya-data-viz/main/soth-election/images/legend/Hern.png' width='40'><br><span>Hern</span>"
other = "<img src='https://raw.githubusercontent.com/tashapiro/tanya-data-viz/main/soth-election/images/legend/Other.png' width='40'><br><span>Other</span>"
abstain = "<img src='https://raw.githubusercontent.com/tashapiro/tanya-data-viz/main/soth-election/images/legend/Abstain.png' width='40'><br><span>Abstain</span>"


#plot
ggplot(df_sankey, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               label=votes,   
               fill = node)) +
  #annotations with ggtext::geom_textbox
  geom_textbox(
    fill="white", box.size=NA, hjust=0,
    width=unit(4,"in"),
    mapping=aes(x=2.2, y=17.5, label="<p style='font-family:Roboto;font-size:6.5pt;'><span style='color:#303036;'><b>Other</b></span> combines votes for less popular candidates<br>First vote includes Biggs, Banks, and Zeldin</p>")
  )+
  geom_textbox(
    fill="white", box.size=NA, halign=0.5,
    mapping=aes(x=13.5, y=-18.2, label="<p style='font-family:Roboto;font-size:6.5pt;'>Flip-floppers coalesce behind<br><span style='color:#FC5130;'><b>McCarthy</b><span></p>")
  )+
  #add in sankey
  geom_sankey(flow.alpha = 0.45) +
  geom_sankey_text(mapping=aes(x=xpos, color=node), 
                   hjust=0,
                   size=3,
                   family="Roboto",
                   show.legend = FALSE)+
  #annotation arrow
  geom_curve(mapping=aes(x=2.2, xend=1.5, y=16.8, yend=11), 
             color="grey20", curvature=0.2, linewidth=0.1,
             arrow = arrow(length=unit("0.05","in")))+
  scale_color_manual(values = c("#FC5130","#DAA829","#51B36A","#45A1BF","#303036","#A5A5A5"))+
  scale_x_discrete(labels=c("1st","2nd","3rd","4th","5th","6th","7th","8th","9th","10th","11th","12th","13th","14th","15th"))+
  scale_fill_manual(
    values = c("#FC5130","#FAC748","#6FCB86","#5FBBDA","#303036","#CCCCCC"),
    labels = c(mccarthy, donalds, jordan, hern, other, abstain),
    guide = guide_legend(nrow=1, override.aes=c(color=NA, fill=NA))
  )+
  labs(fill="Position", x="Vote", y="",
       title="Speaker of The House Election: GOP Flip-Floppers", 
       caption = caption,
       subtitle="Over the course of the 2023  election, **23 Republicans** switched their positions at different stages. Graphic follows their collective voting behavior over all 15 votes.")+
  theme_sankey(base_size = 12)+
  theme(
        legend.position="top",
        legend.title=element_text(size=12, face="bold"),
       # legend.text=element_text(size=10),
        legend.text = element_markdown(halign=0.5),
        text=element_text(family="Roboto Slab"),
        plot.margin = margin(t=20, b=10, l=15, r=15),
        axis.title.x  = element_text(margin=margin(t=10), size=10),
        axis.text.x = element_text(size=11),
        plot.title = element_text(face="bold", family="Roboto Slab"),
        plot.caption = element_textbox_simple(size=8),
        plot.subtitle = element_textbox_simple(margin=margin(b=10), size=12))

#save image
ggsave("../plot/soth-election.png", width=8, height=6)
