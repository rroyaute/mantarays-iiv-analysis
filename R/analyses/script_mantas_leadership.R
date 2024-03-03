library(tidyverse); library(here); library(easystats)
library(kableExtra); library(lme4); library(effects)
library(marginaleffects);  library(ggeffects);  library(rptR)
library(gtsummary); library(ggthemes); library(patchwork)
library(tidybayes); library(AICcmodavg); library(gt)

# Set seed for reproducibility
set.seed(42)

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


# Model fitting ----
glmm.lead.null = glmer(leader ~  1 + (1|id), 
                       family = "binomial",
                       df.group)

glmm.lead.status = glmer(leader ~  sex_f + maturity + (1|id), 
                            family = "binomial",
                            df.group)

glmm.lead.inj.state = glmer(leader ~  shark_bite + anthropogenic + (1|id), 
                            family = "binomial",
                            df.group)

glmm.lead.full = glmer(leader ~  sex_f + maturity + 
                    shark_bite + anthropogenic + (1|id), 
                  family = "binomial",
                  df.group)

# Save models 
saveRDS(glmm.lead.null, file = here("outputs/mods/glmm.lead.null.rds"))
saveRDS(glmm.lead.status, file = here("outputs/mods/glmm.lead.status.rds"))
saveRDS(glmm.lead.inj.state, file = here("outputs/mods/glmm.lead.inj.state.rds"))
saveRDS(glmm.lead.full, file = here("outputs/mods/glmm.lead.full.rds"))


# Full model checks ----
summary(glmm.lead.full)
plot(allEffects(glmm.lead.full))


# Variance explained ----
## Store model formulas ----
f.gsize.null = as.formula(leader ~  1 + (1|id))
f.lead.status = as.formula(leader ~  sex_f + maturity + (1|id))
f.lead.inj.state = as.formula(leader ~  shark_bite + anthropogenic + (1|id))
f.lead.full = as.formula(leader ~  sex_f + maturity + 
                            shark_bite + anthropogenic + (1|id))

## status model ----
rpt.V.lead.status = rpt(f.lead.status, 
                       grname = c("id", "Fixed", "Residual"), 
                       datatype = c("Binary"), 
                       npermut = 1000,
                       parallel = T, 
                       data = df.group,
                       ratio = F)
saveRDS(rpt.V.lead.status, file = here("outputs/mods/rpt.V.lead.status.rds"))

rpt.r2.lead.status = rpt(f.lead.status, 
                        grname = c("id", "Fixed", "Residual"), 
                        datatype = c("Binary"), 
                        npermut = 1000, 
                        parallel = T, 
                        data = df.group,
                        ratio = T)
saveRDS(rpt.r2.lead.status, file = here("outputs/mods/rpt.r2.lead.status.rds"))

## injury state model ----
rpt.V.lead.inj.state = rpt(f.lead.inj.state, 
                      grname = c("id", "Fixed", "Residual"), 
                      datatype = c("Binary"), 
                      npermut = 1000,
                      parallel = T, 
                      data = df.group,
                      ratio = F)
saveRDS(rpt.V.lead.inj.state, file = here("outputs/mods/rpt.V.lead.inj.state.rds"))

rpt.r2.lead.inj.state = rpt(f.lead.inj.state, 
                       grname = c("id", "Fixed", "Residual"), 
                       datatype = c("Binary"), 
                       npermut = 1000, 
                       parallel = T, 
                       data = df.group,
                       ratio = T)
saveRDS(rpt.r2.lead.inj.state, file = here("outputs/mods/rpt.r2.lead.inj.state.rds"))

## full model ----
rpt.V.lead.full = rpt(f.lead.full, 
                       grname = c("id", "Fixed", "Residual"), 
                       datatype = c("Binary"), 
                       npermut = 1000,
                       parallel = T, 
                       data = df.group,
                       ratio = F)
saveRDS(rpt.V.lead.full, file = here("outputs/mods/rpt.V.lead.full.rds"))

rpt.r2.lead.full = rpt(f.lead.full, 
                        grname = c("id", "Fixed", "Residual"), 
                        datatype = c("Binary"), 
                        npermut = 1000, 
                        parallel = T, 
                        data = df.group,
                        ratio = T)
saveRDS(rpt.r2.lead.full, file = here("outputs/mods/rpt.r2.lead.full.rds"))

