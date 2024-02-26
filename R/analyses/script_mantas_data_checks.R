library(tidyverse); library(here); library(easystats);
library(kableExtra); library(gtsummary); library(ggthemes)
library(patchwork); library(gt)

## Data import ----
df.tot = read.csv(here("data/data-raw/Manta Data_Annie.csv"), 
                    header=TRUE, sep=",", na.strings="NA", dec=".",
                    strip.white=TRUE)

## Data summaries ----

# Only group foraging with non unknown individuals
df.group.2 = df.tot %>% 
  filter(group == 1)

# Nb of individual by group
df.group.2 %>% 
  group_by(clip)
  

hist(df.group$group_size)
hist(df.group.2$group_size)
