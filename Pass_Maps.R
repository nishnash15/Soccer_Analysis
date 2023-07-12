install.packages("devtools")
devtools::install_github("statsbomb/SDMtools")
devtools::install_github("statsbomb/StatsBombR")
install.packages("tidyverse")
install.packages("ggsoccer")
install.packages("ggplot2")
library(ggplot2)


library(tidyverse)
library(StatsBombR)
library(ggsoccer)

Comp <- FreeCompetitions()

ucl_german <- Comp %>% 
  filter(competition_id==16 & season_name=="2012/2013")

matches <- FreeMatches(ucl_german)

events_df <- get.matchFree(matches)

clean_df <- allclean(events_df)

muller_pass1 <- clean_df %>%
  filter(player.name == 'Thomas Müller') %>%
  filter(type.name == 'Pass')



ggplot(muller_pass1) +
  annotate_pitch(dimensions = pitch_statsbomb) +
  geom_segment(aes(x=location.x, y=location.y, xend=pass.end_location.x, yend=pass.end_location.y),
               colour = "coral",
               arrow = arrow(length = unit(0.15, "cm"),
                             type = "closed")) +
  labs(title="Thomas Muller's Passing Map",
       subtitle="UEFA Champions League Final 12/13",
       caption="Data Source: StatsBomb") +
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    text = element_text(family="Geneva", color='white'),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  )

ggsave("Thomas Muller Passing UCL Final.png")



                  
                   





