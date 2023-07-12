install.packages("tidyverse")
install.packages("ggplot2")
install.packages("devtools")
library(tidyverse)
library(ggplot2)
library(devtools)
install_github("JaseZiv/worldfootballR")
library(worldfootballR)


df2 <- fb_player_scouting_report("https://fbref.com/en/players/b34c63a5/Rayan-Cherki", pos_versus = "primary")

df3 <- fb_player_scouting_report("https://fbref.com/en/players/35a6e5c7/Gabriel-Veiga", pos_versus = "primary")


player_df2 <- df2[c(10, 11, 17, 18, 49, 50, 57, 103, 105, 124),]

player_df3 <- df3[c(10, 11, 17, 18, 49, 50, 57, 103, 105, 124),]



player_df2$index <- 1:10

player_df3$index <- 1:10



player_df2 <- player_df2 %>% 
  mutate(type = case_when(
    index %in% 1:4 ~ "Attacking",
    index %in% 5:10 ~ "Possession"
  ))

player_df3 <- player_df3 %>% 
  mutate(type = case_when(
    index %in% 1:4 ~ "Attacking",
    index %in% 5:10 ~ "Possession"
  ))



temp <- (360/(length(player_df2$index))/2)

myAng <- seq(-temp, -360+temp, length.out = length(player_df2$index))

ang <- ifelse(myAng < -90, myAng+180, myAng)

ang <- ifelse(ang < -90, ang+180, ang)


temp <- (360/(length(player_df3$index))/2)

myAng <- seq(-temp, -360+temp, length.out = length(player_df3$index))

ang <- ifelse(myAng < -90, myAng+180, myAng)

ang <- ifelse(ang < -90, ang+180, ang)


color1 <- "blue"
color2 <- "lightgreen"



ggplot(data = player_df, aes(x = reorder(Statistic, index), y = Percentile, label= Percentile, fill = type))+
  geom_bar(data = player_df, width = 1,
           color = "oldlace",
           stat =  "identity")+
  coord_polar()+
  geom_bar(aes(y=100, fill=type), stat = "identity", width = 1, alpha = 0.5)+
  geom_hline(yintercept = seq(0, 100, by = 100), 
             color = "grey",
             size = 1)+
  geom_vline(xintercept = seq(0.5, 12, by = 1),
             color = "grey",
             size = 0.5)+
  geom_label(color = "grey20", fill = "oldlace", size = 2.5, fontface = "bold", family = "Calibri", show.legend = FALSE)+
  scale_fill_manual(values = c(color1, color2, color3))+
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "oldlace", color = "oldlace"),
        legend.title = element_blank(),
        legend.text = element_text(color = "grey20", family = "Calibri", face = "bold"),
        legend.key.size = unit(0.5, "cm"),
        legend.box.spacing = unit(0, "mm"),
        plot.title = element_text(hjust = 0.5, color = "grey20", face = "bold", size = 16, family = "Calibri"),
        plot.subtitle = element_text(hjust = 0.5, color = "grey20", size = 8, family = "Calibri"),
        plot.background = element_rect(fill = "oldlace", color = "oldlace"),
        panel.background = element_rect(fill = "oldlace", color = "oldlace"),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold", size = 6.8, color = "grey20"),
        axis.title = element_blank(),
        axis.text.x = element_text(face = "bold", size = 7, family = "Calibri"))+
  labs(title = "Dominik Szoboszlai Scouting Report",
       subtitle = "@thetrickyblues // Last 365 Days // via: worldfootballR", x = NULL, y= NULL)


ggsave("Szoboszlai_radar.png", height = 6, width = 6, dpi = "retina")







































