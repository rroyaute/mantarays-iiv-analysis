library(tidyverse); library(here); library(easystats);
library(kableExtra); library(lme4); library(effects)
library(marginaleffects);  library(ggeffects);  library(rptR)
library(gtsummary); library(ggthemes); library(patchwork)
library(tidybayes); library(gt); library(AICcmodavg)

# Set seed for reproducibility
set.seed(42)

# What explains variation in foraging strategies (solo vs. group foraging)? 
# Data import ----
df.tot = read.csv(here("data/data-raw/Manta Data_Annie.csv"), 
                    header=TRUE, sep=",", na.strings="NA", dec=".",
                    strip.white=TRUE)
df.tot$id = as.factor(df.tot$id)
df.tot$site = as.factor(df.tot$site)
df.tot$sex_f = as.factor(ifelse(df.tot$sex=="1","F","M"))
df.tot$size = as.factor(df.tot$size)
df.tot$maturity = as.factor(df.tot$maturity)
df.tot$plankton = as.factor(df.tot$plankton)

# Transform Number of mantas columns centered mean
df.tot$no_mantas_sc = as.numeric(scale(df.tot$no_mantas))

# Transform Number of mantas columns centered mean
df.tot$time_ht_sc = as.numeric(scale(df.tot$time_ht))


# Model fitting ----
glmm.solo.v.g.null = glmer(group ~  1 + (1|id) + (1|site), 
                           family = "binomial",
                           df.tot)

glmm.solo.v.g.abio = glmer(group ~  
                             nyear + 
                             current + 
                             time_ht_sc + 
                             (1|id) + (1|site), 
                           family = "binomial",
                           df.tot)

glmm.solo.v.g.bio = glmer(group ~  
                                plankton +
                                no_mantas_sc + 
                                (1|id), 
                              family = "binomial",
                              df.tot)

glmm.solo.v.g.full = glmer(group ~  
                             nyear +
                             current +
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
saveRDS(glmm.solo.v.g.bio, file = here("outputs/mods/glmm.solo.v.g.bio_site_rd.rds"))
saveRDS(glmm.solo.v.g.full, file = here("outputs/mods/glmm.solo.v.g.full_site_rd.rds"))



## Full model checks ----
summary(glmm.solo.v.g.full)
plot(allEffects(glmm.solo.v.g.full))
check_model(glmm.solo.v.g.full)

# Variance explained by individuals ----
## Store model formulas ----
f.solo.v.g_site.null = as.formula(group ~  1 + (1|id) + (1|site))
f.solo.v.g_site.abio = as.formula(group ~  nyear + current +
                            time_ht_sc + (1|id) + (1|site))
f.solo.v.g_site.bio = as.formula(group ~  plankton + no_mantas_sc + 
                           (1|id) + (1|site))
f.solo.v.g_site.full = as.formula(group ~  nyear + current + time_ht_sc +
                            plankton + no_mantas_sc + (1|id) + (1|site))

## abio model ----
rpt.V.solo.v.g_site.abio = rpt(f.solo.v.g_site.abio, 
                       grname = c("id", "site", "Fixed", "Residual"), 
                       datatype = c("Binary"), 
                       npermut = 1000,
                       parallel = T, 
                       data = df.tot,
                       ratio = F)
saveRDS(rpt.V.solo.v.g_site.abio, file = here("outputs/mods/rpt.V.solo.v.g_site.abio.rds"))

rpt.r2.solo.v.g_site.abio = rpt(f.solo.v.g_site.abio, 
                        grname = c("id", "site", "Fixed", "Residual"), 
                        datatype = c("Binary"), 
                        npermut = 1000, 
                        parallel = T, 
                        data = df.tot,
                        ratio = T)
saveRDS(rpt.r2.solo.v.g_site.abio, file = here("outputs/mods/rpt.r2.solo.v.g_site.abio.rds"))

## bio model ----
rpt.V.solo.v.g_site.bio = rpt(f.solo.v.g_site.bio, 
                      grname = c("id", "site", "Fixed", "Residual"), 
                      datatype = c("Binary"), 
                      npermut = 1000,
                      parallel = T, 
                      data = df.tot,
                      ratio = F)
saveRDS(rpt.V.solo.v.g_site.bio, file = here("outputs/mods/rpt.V.solo.v.g_site.bio.rds"))

rpt.r2.solo.v.g_site.bio = rpt(f.solo.v.g_site.bio, 
                       grname = c("id", "site", "Fixed", "Residual"), 
                       datatype = c("Binary"), 
                       npermut = 1000, 
                       parallel = T, 
                       data = df.tot,
                       ratio = T)
saveRDS(rpt.r2.solo.v.g_site.bio, file = here("outputs/mods/rpt.r2.solo.v.g_site.bio.rds"))

## full model ----
rpt.V.solo.v.g_site.full = rpt(f.solo.v.g_site.full, 
                       grname = c("id", "site", "Fixed", "Residual"), 
                       datatype = c("Binary"), 
                       npermut = 1000,
                       parallel = T, 
                       data = df.tot,
                       ratio = F)
saveRDS(rpt.V.solo.v.g_site.full, file = here("outputs/mods/rpt.V.solo.v.g_site.full.rds"))

rpt.r2.solo.v.g_site.full = rpt(f.solo.v.g_site.full, 
                        grname = c("id", "site", "Fixed", "Residual"), 
                        datatype = c("Binary"), 
                        npermut = 1000, 
                        parallel = T, 
                        data = df.tot,
                        ratio = T)
saveRDS(rpt.r2.solo.v.g_site.full, file = here("outputs/mods/rpt.r2.solo.v.g_site.full.rds"))









## Old model versions (not run because not converging) ----
# glmm.solo.v.g.null = glmer(group ~  1 + (1|id), 
#                         family = "binomial",
#                         df.tot)
# 
# glmm.solo.v.g.abio = glmer(group ~  nyear + site + current + time_ht_sc + (1|id), 
#                         family = "binomial",
#                         df.tot)
# 
# glmm.solo.v.g.bio.ext = glmer(group ~  plankton + no_mantas_sc + (1|id), 
#                            family = "binomial",
#                            df.tot)
# 
# glmm.solo.v.g.full = glmer(group ~  nyear + 
#                              site + 
#                              current + 
#                              time_ht_sc +
#                              plankton + 
#                              no_mantas_sc + 
#                              (1|id), 
#                            family = "binomial",
#                            df.tot)
# 
# # Save models 
# saveRDS(glmm.solo.v.g.null, file = here("outputs/mods/glmm.solo.v.g.null.rds"))
# saveRDS(glmm.solo.v.g.abio, file = here("outputs/mods/glmm.solo.v.g.abio.rds"))
# saveRDS(glmm.solo.v.g.bio.ext, file = here("outputs/mods/glmm.solo.v.g.bio.ext.rds"))
# saveRDS(glmm.solo.v.g.full, file = here("outputs/mods/glmm.solo.v.g.full.rds"))

# rpt.V.solo.v.g = rpt(group ~  nyear + site + current + time_ht_sc +
#                        plankton + no_mantas_sc + (1|id), 
#                   grname = c("id", "Fixed", "Residual"), 
#                   datatype = c("Binary"), 
#                   # npermut = 1000,
#                   parallel = T, 
#                   data = df.tot,
#                   ratio = F)
# saveRDS(rpt.V.solo.v.g, file = here("outputs/mods/rpt.V.solo.v.g.rds"))
# 
# rpt.r2.solo.v.g = rpt(group ~  nyear + site + current + time_ht_sc +
#                         plankton + no_mantas_sc + (1|id), 
#                    grname = c("id", "Fixed", "Residual"), 
#                    datatype = c("Binary"), 
#                    # npermut = 1000, 
#                    parallel = T, 
#                    data = df.tot,
#                    ratio = T)
# saveRDS(rpt.r2.solo.v.g, file = here("outputs/mods/rpt.r2.solo.v.g.rds"))
