install.packages("cricketdata", dependencies = TRUE)
install.packages("tidyverse")
install.packages("ggimage")
install.packages("dplyr")
install.packages("gt")
install.packages("ggplot2")
install.packages("ggrepel")
install.packages("png")

library(cricketdata)
library(tidyverse)
library(ggimage)
library(gt)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(png)

pbp <- fetch_cricsheet("bbb", "male", "ipl") 
pbp %>% head()

names(pbp)
nrow(pbp)

ipl_1 <- pbp %>%
  filter(season == "2023") %>%
  group_by(batsman = striker) %>%
  summarize(runs = sum(runs_off_bat), balls = n() - sum(!is.na(wides)), strikerate = 100 * (runs/balls), boundary = sum(runs_off_bat %in% c(4, 6)) * 100 / balls, dot = sum(runs_off_bat == 0) * 100 / balls, boundary_over_dot = boundary/dot)


ipl_2 <- pbp %>%
  filter(season == "2023") %>%
  group_by(batsman = player_dismissed) %>%
  filter(wicket == "TRUE") %>%
  summarize(outs = n())

ipl <- left_join(ipl_1, ipl_2, by = "batsman")

ipl <- ipl %>%
  filter(!is.na(outs)) %>%
  filter(runs >= 100) %>%
  mutate(average = runs/outs) 

ipl %>%
  ggplot(aes(x = average, y = strikerate, size = boundary_over_dot)) + geom_point(alpha=0.7) +
  scale_size(name="Boundary Over Dot Percentage") +
  geom_hline(yintercept = mean(ipl$strikerate), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(ipl$average), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_text_repel(aes(label=batsman), size = 3.5) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm') +
  labs(x = "Batting Average",
       y = "Strike Rate", title = "Batting Average v.s Strike Rate in IPL 2023 (100+ Runs)",
       caption = "Amrit Vignesh") + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20))
