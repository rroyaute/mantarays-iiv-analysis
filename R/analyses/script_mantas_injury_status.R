library(tidyverse); library(here); library(easystats);
library(kableExtra); library(lme4); library(effects)
library(marginaleffects);  library(ggeffects);  library(rptR)
library(gtsummary); library(ggthemes); library(patchwork)
library(tidybayes); library(gt); library(AICcmodavg)

# TODO:
# verify time to ht column

## Data import ----
df.group = read.csv(here("data/data-raw/group_rp.csv"), 
                    header=TRUE, sep=",", na.strings="NA", dec=".",
                    strip.white=TRUE)
df.group$id = as.factor(df.group$id)
df.group$site = as.factor(df.group$site)
df.group$sex_f = as.factor(ifelse(df.group$sex=="1","F","M"))
df.group$size = as.factor(df.group$size)
df.group$maturity = as.factor(df.group$maturity)
df.group$plankton = as.factor(df.group$plankton)
df.group$shark_bite = as.factor(df.group$shark_bite)
df.group$anthropogenic = as.factor(df.group$anthropogenic)

# Transform Time columns centered aroun 12:00pm expressed in hours
df.group$time_cen = hm(df.group$time)
df.group$time_cen = as.numeric(df.group$time_cen-hours(12))/3600

# Transform Number of mantas columns centered mean
df.group$no_mantas_sc = as.numeric(scale(df.group$no_mantas))

# Transform Number of mantas columns centered mean
df.group$time_ht_sc = as.numeric(scale(df.group$time_ht))




## Effect of shark bites on position in group and group size ----

glmm.injury = glmer(injury ~  position + (1|id), 
                        family = "binomial",
                        df.group)
summary(glmm.injury)

glmm.injury.a = glmer(anthropogenic ~  position + (1|id), 
                    family = "binomial",
                    df.group)
summary(glmm.injury.a)

glmm.injury.s = glmer(shark_bite ~  position + (1|id), 
                      family = "binomial",
                      df.group)
summary(glmm.injury.s)

plot(allEffects(glmm.injury.a))
plot(allEffects(glmm.injury.s))

plot(ggpredict(glmm.injury.a))
plot(ggpredict(glmm.injury.s))

# Position
df.group$injury_f = as.factor(df.group$injury)
df.group$anthropogenic_f = as.factor(df.group$anthropogenic)
df.group$shark_bite_f = as.factor(df.group$shark_bite)

glmm.injury = glmer(position ~  injury_f + (1|id), 
                    family = "poisson",
                    df.group)
summary(glmm.injury)

glmm.injury.a = glmer(position ~  anthropogenic_f + (1|id), 
                      family = "poisson",
                      df.group)
summary(glmm.injury.a)

glmm.injury.s = glmer(position ~  shark_bite_f + (1|id), 
                      family = "poisson",
                      df.group)
summary(glmm.injury.s)

plot(allEffects(glmm.injury))
plot(allEffects(glmm.injury.a))
plot(allEffects(glmm.injury.s))

plot(ggpredict(glmm.injury))
plot(ggpredict(glmm.injury.a))
plot(ggpredict(glmm.injury.s))
