library(tidyverse); library(here); library(easystats);
library(kableExtra); library(gtsummary); library(ggthemes)
library(patchwork); library(gt)

## Data import ----
df.tot = read.csv(here("data/data-raw/Manta Data_Annie.csv"), 
                    header=TRUE, sep=",", na.strings="NA", dec=".",
                    strip.white=TRUE)
df.group = read.csv(here("data/data-raw/group_rp.csv"), 
                  header=TRUE, sep=",", na.strings="NA", dec=".",
                  strip.white=TRUE)

## Data summaries ----

# Number of foraging events
length(unique(df.tot$npid))
length(unique(df.tot$id))


# Only group foraging with non unknown individuals
df.group.2 = df.tot %>% 
  filter(group == 1)

# Nb of individual by group
df.group.2 %>% 
  group_by(clip)
  

hist(df.group$group_size)
hist(df.group.2$group_size)

boxplot(group_size ~ plankton, df.group)

ggplot(df.group, aes(x = plankton, y = group)) +
  geom_bar(stat = "identity", width = .2) 

ggplot(df.group, aes(x = plankton, y = group)) +
  geom_bar(stat = "identity", width = .2) +
  facet_wrap(~site)

ggplot(df.tot, aes(x = plankton, y = group)) +
  geom_bar(stat = "identity", width = .2) 

ggplot(df.tot, aes(x = plankton, y = group)) +
  geom_bar(stat = "identity", width = .2) +
  facet_wrap(~site)

ggplot(df.tot, aes(x = as.factor(site), y = current)) +
  geom_boxplot()

df.tot %>% ggplot(aes(y = no_mantas, x = current)) +
  geom_point()

df.tot %>% ggplot(aes(y = group, x = plankton)) +
  geom_point(alpha = .02)
