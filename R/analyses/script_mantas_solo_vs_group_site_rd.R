library(tidyverse); library(here); library(easystats);
library(kableExtra); library(lme4); library(effects)
library(marginaleffects);  library(ggeffects);  library(rptR)
library(gtsummary); library(ggthemes); library(patchwork)
library(tidybayes); library(gt); library(AICcmodavg)

# TODO:
# verify time to ht column
# verify current column
# Injury columns

## Data import ----
df.tot = read.csv(here("data/data-raw/Manta Data_Annie.csv"), 
                  header=TRUE, sep=",", na.strings="NA", dec=".",
                  strip.white=TRUE)
df.tot$id = as.factor(df.tot$id)
df.tot$site = as.factor(df.tot$site)
df.tot$sex_f = as.factor(ifelse(df.tot$sex=="1","F","M"))
df.tot$size = as.factor(df.tot$size)
df.tot$maturity = as.factor(df.tot$maturity)
df.tot$plankton = as.factor(df.tot$plankton)
df.tot$current = as.factor(df.tot$current)
# df.tot$shark_bite = as.factor(df.tot$shark_bite)
# df.tot$anthropogenic = as.factor(df.tot$anthropogenic)

# Transform Time columns centered aroun 12:00pm expressed in hours
df.tot$time_cen = hm(df.tot$time)
df.tot$time_cen = as.numeric(df.tot$time_cen-hours(12))/3600

# Transform Number of mantas columns centered mean
df.tot$no_mantas_sc = as.numeric(scale(df.tot$no_mantas))

# Transform Number of mantas columns centered mean
df.tot$time_ht_sc = as.numeric(scale(df.tot$time_ht))

df.tot$current_n = as.numeric(df.tot$current)

## What explains variation in foraging strategies (solo vs. group foraging)? ----
### Model fitting ----

glmm.solo.v.g.null = glmer(group ~  1 + (1|id) + (1|site), 
                           family = "binomial",
                           df.tot)

glmm.solo.v.g.abio = glmer(group ~  
                             nyear + 
                             current_n + 
                             time_ht_sc + 
                             (1|id) + (1|site), 
                           family = "binomial",
                           df.tot)

glmm.solo.v.g.bio.ext = glmer(group ~  
                                plankton +
                                no_mantas_sc + 
                                (1|id), 
                              family = "binomial",
                              df.tot)

glmm.solo.v.g.full = glmer(group ~  
        nyear +
        current_n +
        time_ht_sc +
        plankton +
        no_mantas_sc +
        (1|id) + (1|site),
      family = "binomial",
      df.tot)

summary(glmm.solo.v.g.full)
plot(allEffects(glmm.solo.v.g.full))

# Save models 
saveRDS(glmm.solo.v.g.null, file = here("outputs/mods/glmm.solo.v.g.null_site_rd.rds"))
saveRDS(glmm.solo.v.g.abio, file = here("outputs/mods/glmm.solo.v.g.abio_site_rd.rds"))
saveRDS(glmm.solo.v.g.bio.ext, file = here("outputs/mods/glmm.solo.v.g.bio_site_rd.ext.rds"))
saveRDS(glmm.solo.v.g.full, file = here("outputs/mods/glmm.solo.v.g.full_site_rd.rds"))


Model.list=list()
Model.list[[1]]=glmm.solo.v.g.null
Model.list[[2]]=glmm.solo.v.g.abio
Model.list[[3]]=glmm.solo.v.g.bio.ext
Model.list[[4]]=glmm.solo.v.g.full


names(Model.list)=c("Null", "Abiotic","Biotic (external)", "Abiotic + Biotic (external)")
# Calculate AIC values and delta_AIC by setting second.ord = F 
#(returns AICc otherwise, which are mostly used with limited samples sizes)
aictab.solo.v.g = data.frame(aictab(Model.list, second.ord = F)) %>% 
  select(1:4) %>% 
  rename("Model" = Modnames)

aictab.solo.v.g

R2.null = r2_nakagawa(glmm.solo.v.g.null)
R2.abio = r2_nakagawa(glmm.solo.v.g.abio)
R2.bio.ext = r2_nakagawa(glmm.solo.v.g.bio.ext)
R2.full = r2_nakagawa(glmm.solo.v.g.full)

### Variance explained by individuals ----
# Import rptR model
rpt.V.solo.v.g = rpt(group ~  nyear + current_n + time_ht_sc +
                       plankton + no_mantas_sc + (1|id) + (1|site), 
                     grname = c("id", "site", "Fixed", "Residual"), 
                     datatype = c("Binary"), 
                     npermut = 1000,
                     parallel = T, 
                     data = df.tot,
                     ratio = F)

rpt.r2.solo.v.g = rpt(group ~  nyear + current_n + time_ht_sc +
                        plankton + no_mantas_sc + (1|id) + (1|site),
                      grname = c("id", "site", "Fixed", "Residual"), 
                      datatype = c("Binary"), 
                      npermut = 1000, 
                      parallel = T, 
                      data = df.tot,
                      ratio = T)
rpt.V.solo.v.g
rpt.r2.solo.v.g
saveRDS(rpt.V.solo.v.g, file = here("outputs/mods/rpt.V.solo.v.g_site_rd.rds"))
saveRDS(rpt.r2.solo.v.g, file = here("outputs/mods/rpt.r2.solo.v.g_site_rd.rds"))

