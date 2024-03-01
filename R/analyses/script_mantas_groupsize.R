library(tidyverse); library(here); library(easystats);
library(kableExtra); library(lme4); library(effects)
library(marginaleffects);  library(ggeffects);  library(rptR)
library(gtsummary); library(ggthemes); library(patchwork)
library(tidybayes); library(gt); library(AICcmodavg)

# What explains variation in group leadership?
# Data import ----
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

# Transform Number of mantas columns scaled to sd units
df.group$no_mantas_sc = as.numeric(scale(df.group$no_mantas))

# Transform time to high scaled to sd units
df.group$time_ht_sc = as.numeric(scale(df.group$time_ht))

# Transform current into numeric variable
df.group$current = as.numeric(df.group$current.VU.AO)

# Model fitting ----
glmm.gsize.null = glmer(group_size ~  1 + (1|id) + (1|site), 
                        family = "poisson",
                        df.group)

glmm.gsize.abio = glmer(group_size ~  nyear + current + time_ht_sc + 
                          (1|id) + (1|site), 
                        family = "poisson",
                        df.group)

glmm.gsize.bio = glmer(group_size ~  plankton + no_mantas_sc + 
                         (1|id) + (1|site),  
                       family = "poisson",
                       df.group)
glmm.gsize.full = glmer(group_size ~  nyear + current + time_ht_sc +
                          plankton + no_mantas_sc + (1|id) + (1|site),  
                        family = "poisson",
                        df.group)

# Save models 
saveRDS(glmm.gsize.null, file = here("outputs/mods/glmm.gsize.null.rds"))
saveRDS(glmm.gsize.abio, file = here("outputs/mods/glmm.gsize.abio.rds"))
saveRDS(glmm.gsize.bio, file = here("outputs/mods/glmm.gsize.bio.rds"))
saveRDS(glmm.gsize.full, file = here("outputs/mods/glmm.gsize.full.rds"))


## Full model checks ----
summary(glmm.gsize.full)
plot(allEffects(glmm.gsize.full))
check_model(glmm.gsize.full)

# Variance explained ----
## Store model formulas ----
f.gsize.null = as.formula(group_size ~  1 + (1|id) + (1|site))
f.gsize.abio = as.formula(group_size ~  nyear + current +
                            time_ht_sc + (1|id) + (1|site))
f.gsize.bio = as.formula(group_size ~  plankton + no_mantas_sc + 
                           (1|id) + (1|site))
f.gsize.full = as.formula(group_size ~  nyear + current + time_ht_sc +
                            plankton + no_mantas_sc + (1|id) + (1|site))

## abio model ----
rpt.V.gsize.abio = rpt(f.gsize.abio, 
                       grname = c("id", "site", "Fixed", "Residual"), 
                       datatype = c("Poisson"), 
                       npermut = 1000,
                       parallel = T, 
                       data = df.group,
                       ratio = F)
saveRDS(rpt.V.gsize.abio, file = here("outputs/mods/rpt.V.gsize.abio.rds"))

rpt.r2.gsize.abio = rpt(f.gsize.abio, 
                        grname = c("id", "site", "Fixed", "Residual"), 
                        datatype = c("Poisson"), 
                        npermut = 1000, 
                        parallel = T, 
                        data = df.group,
                        ratio = T)
saveRDS(rpt.r2.gsize.abio, file = here("outputs/mods/rpt.r2.gsize.abio.rds"))

## bio model ----
rpt.V.gsize.bio = rpt(f.gsize.bio, 
                      grname = c("id", "site", "Fixed", "Residual"), 
                      datatype = c("Poisson"), 
                      npermut = 1000,
                      parallel = T, 
                      data = df.group,
                      ratio = F)
saveRDS(rpt.V.gsize.bio, file = here("outputs/mods/rpt.V.gsize.bio.rds"))

rpt.r2.gsize.bio = rpt(f.gsize.bio, 
                       grname = c("id", "site", "Fixed", "Residual"), 
                       datatype = c("Poisson"), 
                       npermut = 1000, 
                       parallel = T, 
                       data = df.group,
                       ratio = T)
saveRDS(rpt.r2.gsize.bio, file = here("outputs/mods/rpt.r2.gsize.bio.rds"))

## full model ----
rpt.V.gsize.full = rpt(f.gsize.full, 
                       grname = c("id", "site", "Fixed", "Residual"), 
                       datatype = c("Poisson"), 
                       npermut = 1000,
                       parallel = T, 
                       data = df.group,
                       ratio = F)
saveRDS(rpt.V.gsize.full, file = here("outputs/mods/rpt.V.gsize.full.rds"))

rpt.r2.gsize.full = rpt(f.gsize.full, 
                        grname = c("id", "site", "Fixed", "Residual"), 
                        datatype = c("Poisson"), 
                        npermut = 1000, 
                        parallel = T, 
                        data = df.group,
                        ratio = T)
saveRDS(rpt.r2.gsize.full, file = here("outputs/mods/rpt.r2.gsize.full.rds"))

