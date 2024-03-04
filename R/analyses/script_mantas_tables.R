library(tidyverse); library(here); library(easystats)
library(ggthemes); library(patchwork)
library(tidybayes); library(gt); library(gtsummary)
library(broom.mixed); library(AICcmodavg); library(kableExtra)
library(rptR)

# Load functions ----
source(here("R/funs/R2_funs.R"))
# Import model objects ----
## glmer objects ----
# Group vs solo
glmm.solo.v.g.null = read_rds(file = here("outputs/mods/glmm.solo.v.g.null_site_rd.rds"))
glmm.solo.v.g.abio = read_rds(file = here("outputs/mods/glmm.solo.v.g.abio_site_rd.rds"))
glmm.solo.v.g.bio = read_rds(file = here("outputs/mods/glmm.solo.v.g.bio_site_rd.rds"))
glmm.solo.v.g.full = read_rds(file = here("outputs/mods/glmm.solo.v.g.full_site_rd.rds"))

# Group size
glmm.gsize.null = read_rds(file = here("outputs/mods/glmm.gsize.null.rds"))
glmm.gsize.abio = read_rds(file = here("outputs/mods/glmm.gsize.abio.rds"))
glmm.gsize.bio = read_rds(file = here("outputs/mods/glmm.gsize.bio.rds"))
glmm.gsize.full = read_rds(file = here("outputs/mods/glmm.gsize.full.rds"))

# Group leadership
glmm.lead.null = read_rds(file = here("outputs/mods/glmm.lead.null.rds"))
glmm.lead.status = read_rds(file = here("outputs/mods/glmm.lead.status.rds"))
glmm.lead.state = read_rds(file = here("outputs/mods/glmm.lead.inj.state.rds"))
glmm.lead.full = read_rds(file = here("outputs/mods/glmm.lead.full.rds"))

## rptR objects ----
# Import foraging strategy 
rpt.V.solo.v.g.full = read_rds(file = here("outputs/mods/rpt.V.solo.v.g_site.full.rds"))
rpt.V.solo.v.g.abio = read_rds(file = here("outputs/mods/rpt.V.solo.v.g_site.abio.rds"))
rpt.V.solo.v.g.bio = read_rds(file = here("outputs/mods/rpt.V.solo.v.g_site.bio.rds"))

rpt.r2.solo.v.g.full = read_rds(file = here("outputs/mods/rpt.r2.solo.v.g_site.full.rds"))
rpt.r2.solo.v.g.abio = read_rds(file = here("outputs/mods/rpt.r2.solo.v.g_site.abio.rds"))
rpt.r2.solo.v.g.bio = read_rds(file = here("outputs/mods/rpt.r2.solo.v.g_site.bio.rds"))

# Import group size 
rpt.V.gsize.full = read_rds(file = here("outputs/mods/rpt.V.gsize.full.rds"))
rpt.V.gsize.abio = read_rds(file = here("outputs/mods/rpt.V.gsize.abio.rds"))
rpt.V.gsize.bio = read_rds(file = here("outputs/mods/rpt.V.gsize.bio.rds"))

rpt.r2.gsize.full = read_rds(file = here("outputs/mods/rpt.r2.gsize.full.rds"))
rpt.r2.gsize.abio = read_rds(file = here("outputs/mods/rpt.r2.gsize.abio.rds"))
rpt.r2.gsize.bio = read_rds(file = here("outputs/mods/rpt.r2.gsize.bio.rds"))

# Import group leadership 
rpt.r2.lead.full = read_rds(file = here("outputs/mods/rpt.r2.lead.full.rds"))
rpt.r2.lead.status = read_rds(file = here("outputs/mods/rpt.r2.lead.status.rds"))
rpt.r2.lead.inj.state = read_rds(file = here("outputs/mods/rpt.r2.lead.inj.state.rds"))

rpt.V.lead.full = read_rds(file = here("outputs/mods/rpt.V.lead.full.rds"))
rpt.V.lead.status = read_rds(file = here("outputs/mods/rpt.V.lead.status.rds"))
rpt.V.lead.inj.state = read_rds(file = here("outputs/mods/rpt.V.lead.inj.state.rds"))

# Calculate conditional and marginal R2 ----
# Only need full model here
# Using the point estimates for variances from the rpt.V objects to calculate
# marginal repeatability (variance explained by fixed effects)

## Group vs solo foraging ----
R2_cond.solo.v.g.full = R2_cond_fun.point.est(Vi = rpt.V.solo.v.g.full$R$id[2],
                          Vsite = rpt.V.solo.v.g.full$R$site[2],
                          Vfe = rpt.V.solo.v.g.full$R$Fixed[2],
                          VR = rpt.V.solo.v.g.full$R$Residual[2])

R2_marg.solo.v.g.full = R2_marg_fun.point.est(Vi = rpt.V.solo.v.g.full$R$id[2],
                          Vsite = rpt.V.solo.v.g.full$R$site[2],
                          Vfe = rpt.V.solo.v.g.full$R$Fixed[2],
                          VR = rpt.V.solo.v.g.full$R$Residual[2])
R2_marg.solo.v.g.abio = R2_marg_fun.point.est(Vi = rpt.V.solo.v.g.abio$R$id[2],
                                    Vsite = rpt.V.solo.v.g.abio$R$site[2],
                                    Vfe = rpt.V.solo.v.g.abio$R$Fixed[2],
                                    VR = rpt.V.solo.v.g.abio$R$Residual[2])
R2_marg.solo.v.g.bio = R2_marg_fun.point.est(Vi = rpt.V.solo.v.g.bio$R$id[2],
                                    Vsite = rpt.V.solo.v.g.bio$R$site[2],
                                    Vfe = rpt.V.solo.v.g.bio$R$Fixed[2],
                                    VR = rpt.V.solo.v.g.bio$R$Residual[2])

## Group size ----
# Only need full model here
R2_cond.gsize.full = R2_cond_fun.point.est(Vi = rpt.V.gsize.full$R$id[2],
                                    Vsite = rpt.V.gsize.full$R$site[2],
                                    Vfe = rpt.V.gsize.full$R$Fixed[2],
                                    VR = rpt.V.gsize.full$R$Residual[2])

R2_marg.gsize.full = R2_marg_fun.point.est(Vi = rpt.V.gsize.full$R$id[2],
                                    Vsite = rpt.V.gsize.full$R$site[2],
                                    Vfe = rpt.V.gsize.full$R$Fixed[2],
                                    VR = rpt.V.gsize.full$R$Residual[2])
R2_marg.gsize.abio = R2_marg_fun.point.est(Vi = rpt.V.gsize.abio$R$id[2],
                                    Vsite = rpt.V.gsize.abio$R$site[2],
                                    Vfe = rpt.V.gsize.abio$R$Fixed[2],
                                    VR = rpt.V.gsize.abio$R$Residual[2])
R2_marg.gsize.bio = R2_marg_fun.point.est(Vi = rpt.V.gsize.bio$R$id[2],
                                   Vsite = rpt.V.gsize.bio$R$site[2],
                                   Vfe = rpt.V.gsize.bio$R$Fixed[2],
                                   VR = rpt.V.gsize.bio$R$Residual[2])

## Group leadership ----
# Only need full model here
R2_cond.lead.full = R2_cond_fun.point.est(Vi = rpt.V.lead.full$R$id[2],
                                    Vsite = NA,
                                    Vfe = rpt.V.lead.full$R$Fixed[2],
                                    VR = rpt.V.lead.full$R$Residual[2])

R2_marg.lead.full = R2_marg_fun.point.est(Vi = rpt.V.lead.full$R$id[2],
                                    Vsite = NA,
                                    Vfe = rpt.V.lead.full$R$Fixed[2],
                                    VR = rpt.V.lead.full$R$Residual[2])
R2_marg.lead.status = R2_marg_fun.point.est(Vi = rpt.V.lead.status$R$id[2],
                                    Vsite = NA,
                                    Vfe = rpt.V.lead.status$R$Fixed[2],
                                    VR = rpt.V.lead.status$R$Residual[2])
R2_marg.lead.inj.state = R2_marg_fun.point.est(Vi = rpt.V.lead.inj.state$R$id[2],
                                   Vsite = NA,
                                   Vfe = rpt.V.lead.inj.state$R$Fixed[2],
                                   VR = rpt.V.lead.inj.state$R$Residual[2])

# Reproduce model tables ----
## Group vs. solo ----
labs = list(
  nyear ~ "Year",
  current ~ "Current",
  time_ht_sc ~ "Time to high tide",
  plankton ~ "Plankton",
  no_mantas_sc ~ "Number of mantas",
  `id.sd__(Intercept)` ~ "sdi",
  `site.sd__(Intercept)` ~ "sdsite")

tbl.glmm.solo.v.g.full = glmm.solo.v.g.full %>% 
  tbl_regression(
    intercept = T,
    exponentiate = F,
    pvalue_fun = ~ style_pvalue(.x, digits = 2), 
    tidy_fun = broom.mixed::tidy,
    label = labs) %>%
  add_global_p() %>%
  bold_p(t = .05) %>%
  bold_labels() %>%
  italicize_levels() %>% 
  modify_header(label = "**Term**", 
                p.value = "**P**",
                estimate = "**Estimate**") %>% 
  add_n(location = "level")
# Note footnotes and random effect p-values to be added manually
# Report random effect p-value
rpt.V.solo.v.g.full$P$P_permut_link


## Group size ----
labs = list(
  nyear ~ "Year",
  plankton ~ "Plankton",
  current ~ "Current",
  time_ht_sc ~ "Time to high tide",
  no_mantas_sc ~ "Number of mantas",
  `id.sd__(Intercept)` ~ "sdi",
  `site.sd__(Intercept)` ~ "sdsite")

tbl.glmm.gsize.full = glmm.gsize.full %>% 
  tbl_regression(
    intercept = T,
    exponentiate = F,
    pvalue_fun = ~ style_pvalue(.x, digits = 2), 
    tidy_fun = broom.mixed::tidy,
    label = labs) %>%
  add_global_p() %>%
  bold_p(t = .05) %>%
  bold_labels() %>%
  italicize_levels() %>% 
  modify_header(label = "**Term**", 
                p.value = "**P**",
                estimate = "**Estimate**") %>% 
  add_n(location = "level")
# Note footnotes and random effect p-values to be added manually
# Report random effect p-value
rpt.V.gsize.full$P$P_permut_link

## Group leadership ----
labs = list(
  # nyear ~ "Year",
  sex_f ~ "Sex",
  maturity ~ "Maturity",
  shark_bite ~ "Injury status (shark bites)",
  anthropogenic ~ "Injury status (anthropogenic injury)",
  `id.sd__(Intercept)` ~ "sdi")

tbl.glmm.lead.full = glmm.lead.full %>% 
  tbl_regression(
    intercept = T,
    exponentiate = F,
    pvalue_fun = ~ style_pvalue(.x, digits = 2), 
    tidy_fun = broom.mixed::tidy,
    label = labs) %>%
  add_global_p() %>%
  bold_p(t = .05) %>%
  bold_labels() %>%
  italicize_levels() %>% 
  modify_header(label = "**Term**", 
                p.value = "**P**",
                estimate = "**Estimate**") %>% 
  add_n(location = "level")
# Note footnotes and random effect p-values to be added manually
# Report random effect p-value
rpt.V.lead.full$P$P_permut_link


## Combine and export in 1 table ----
tbl.fullmod.all.traits = tbl_stack(list(tbl.glmm.solo.v.g.full, 
               tbl.glmm.gsize.full,
               tbl.glmm.lead.full), 
          group_header = c("Group vs. solo foraging",
                           "Group size",
                           "Group leadership"))
tbl.fullmod.all.traits %>% 
  as_gt() %>% 
  gtsave(filename = here("outputs/tables/tbl.fullmod.all.traits.html"))
tbl.fullmod.all.traits %>% 
  as_gt() %>% 
  gtsave(filename = here("outputs/tables/tbl.fullmod.all.traits.docx"))

# Additional info to fill in manually
# variance component R2
R2_fun.point.est.all(Vi = rpt.V.solo.v.g.full$R$id[2],
                     Vsite = rpt.V.solo.v.g.full$R$site[2],
                     Vfe = rpt.V.solo.v.g.full$R$Fixed[2],
                     VR = rpt.V.solo.v.g.full$R$Residual[2]) %>% 
  kable(digits = 2)
R2_fun.point.est.all(Vi = rpt.V.gsize.full$R$id[2],
                     Vsite = rpt.V.gsize.full$R$site[2],
                     Vfe = rpt.V.gsize.full$R$Fixed[2],
                     VR = rpt.V.gsize.full$R$Residual[2]) %>% 
  kable(digits = 2)

R2_fun.point.est.all(Vi = rpt.V.lead.full$R$id[2],
                     Vsite = NA,
                     Vfe = rpt.V.lead.full$R$Fixed[2],
                     VR = rpt.V.lead.full$R$Residual[2]) %>% 
  kable(digits = 2)

# Random effect P-values
rpt.V.solo.v.g.full$P
rpt.V.gsize.full$P
rpt.V.lead.full$P

# Reproduce AIC tables ----
## Group vs solo ----
### AIC ----
Model.list=list()
Model.list[[1]]=glmm.solo.v.g.null
Model.list[[2]]=glmm.solo.v.g.abio
Model.list[[3]]=glmm.solo.v.g.bio
Model.list[[4]]=glmm.solo.v.g.full


names(Model.list) = c("Null", "Abiotic","Biotic", "Abiotic + Biotic")
# Calculate AIC values and delta_AIC by setting second.ord = F 
#(returns AICc otherwise, which are mostly used with limited samples sizes)
aictab.solo.v.g = data.frame(aictab(Model.list, second.ord = F)) %>% 
  select(1:4) %>% 
  rename("Model" = Modnames)

### R2 ----
R2.table.solo.v.g = data.frame(
  Model = c("Null", "Abiotic","Biotic", "Abiotic + Biotic"),
  R2 = c(0,
         R2_marg.solo.v.g.abio,
         R2_marg.solo.v.g.bio,
         R2_marg.solo.v.g.full))

## Group size ----
### AIC ----
Model.list=list()
Model.list[[1]]=glmm.gsize.null
Model.list[[2]]=glmm.gsize.abio
Model.list[[3]]=glmm.gsize.bio
Model.list[[4]]=glmm.gsize.full

names(Model.list) = c("Null", "Abiotic","Biotic", "Abiotic + Biotic")
# Calculate AIC values and delta_AIC by setting second.ord = F 
#(returns AICc otherwise, which are mostly used with limited samples sizes)
aictab.gsize = data.frame(aictab(Model.list, second.ord = F)) %>% 
  select(1:4) %>% 
  rename("Model" = Modnames)

### R2 ----
R2.table.gsize = data.frame(
  Model = c("Null", "Abiotic","Biotic", "Abiotic + Biotic"),
  R2 = c(0,
         R2_marg.gsize.abio,
         R2_marg.gsize.bio,
         R2_marg.gsize.full))

## Group leadership ----
### AIC table ----
Model.list=list()
Model.list[[1]]=glmm.lead.null
Model.list[[2]]=glmm.lead.status
Model.list[[3]]=glmm.lead.state
Model.list[[4]]=glmm.lead.full

names(Model.list) = c("Null", "Status","Injury", "Status + Injury")
# Calculate AIC values and delta_AIC by setting second.ord = F 
#(returns AICc otherwise, which are mostly used with limited samples sizes)
aictab.lead = data.frame(aictab(Model.list, second.ord = F)) %>% 
  select(1:4) %>% rename("Model" = Modnames)

### R2 ----
R2.table.lead = data.frame(
  Model = c("Null", "Status","Injury", "Status + Injury"),
  R2 = c(0,
         R2_marg.lead.status,
         R2_marg.lead.inj.state,
         R2_marg.lead.full))


## Export in 1 table ----
aictab.solo.v.g = full_join(aictab.solo.v.g, R2.table.solo.v.g) %>% 
  mutate(Trait = "Group vs. solo foraging")

aictab.gsize = full_join(aictab.gsize, R2.table.gsize) %>% 
  mutate(Trait = "Group size")

aictab.lead = full_join(aictab.lead, R2.table.lead) %>%
  mutate(Trait = "Group leadership")

aictab.all = rbind(aictab.solo.v.g, aictab.gsize, aictab.lead)

aictab.all = aictab.all %>%
  gt(groupname_col = "Trait") %>%
  fmt_number(decimals = 2) %>% 
  cols_label(Delta_AIC = "∆AIC",
             R2 = "R\U00B2")

aictab.all %>% 
  gtsave(filename = here("outputs/tables/aictab.all.html"))
aictab.all %>% 
  gtsave(filename = here("outputs/tables/aictab.all.docx"))


# Reproduce variance and R2 tables for all traits ----
# Variances
V.summary.svg = V.dist(Vi = rpt.V.solo.v.g.full$R_boot_link$id,
                       Vsite = rpt.V.solo.v.g.full$R_boot_link$site,
                       Vfe = rpt.V.solo.v.g.full$R_boot_link$Fixed,
                       VR = rpt.V.solo.v.g.full$R_boot_link$Residual) %>% 
  data.frame() %>% 
  select(1, 2, 4, 5) %>% 
  mutate(Trait = "Group vs. solo foraging")
V.summary.gsize = V.dist(Vi = rpt.V.gsize.full$R_boot_link$id,
                       Vsite = rpt.V.gsize.full$R_boot_link$site,
                       Vfe = rpt.V.gsize.full$R_boot_link$Fixed,
                       VR = rpt.V.gsize.full$R_boot_link$Residual) %>% 
  data.frame() %>% 
  select(1, 2, 4, 5) %>% 
  mutate(Trait = "Group size")

V.summary.lead = V.dist(Vi = rpt.V.lead.full$R_boot_link$id,
                         Vsite = NA,
                         Vfe = rpt.V.lead.full$R_boot_link$Fixed,
                        VR = rpt.V.lead.full$R_boot_link$Residual) %>% 
  data.frame() %>% 
  select(1, 2, 4, 5) %>% 
  mutate(Trait = "Group leadership")

V.summary = rbind(V.summary.svg, V.summary.gsize, V.summary.lead) %>% 
  gt(groupname_col = "Trait") %>%
  fmt_number(decimals = 2) %>% 
  cols_label(Parameter = "Variance component")

V.summary %>% 
  gtsave(filename = here("outputs/tables/V.summary.html"))

V.summary %>% 
  gtsave(filename = here("outputs/tables/V.summary.docx"))

# R2
r2.summary.svg = R2.dist(Vi = rpt.r2.solo.v.g.full$R_boot_link$id,
                       Vsite = rpt.r2.solo.v.g.full$R_boot_link$site,
                       Vfe = rpt.r2.solo.v.g.full$R_boot_link$Fixed,
                       VR = rpt.r2.solo.v.g.full$R_boot_link$Residual) %>% 
  data.frame() %>% 
  select(1, 2, 4, 5) %>% 
  mutate(Trait = "Group vs. solo foraging")
r2.summary.gsize = R2.dist(Vi = rpt.r2.gsize.full$R_boot_link$id,
                         Vsite = rpt.r2.gsize.full$R_boot_link$site,
                         Vfe = rpt.r2.gsize.full$R_boot_link$Fixed,
                         VR = rpt.r2.gsize.full$R_boot_link$Residual) %>% 
  data.frame() %>% 
  select(1, 2, 4, 5) %>% 
  mutate(Trait = "Group size")

r2.summary.lead = R2.dist(Vi = rpt.r2.lead.full$R_boot_link$id,
                        Vsite = NA,
                        Vfe = rpt.r2.lead.full$R_boot_link$Fixed,
                        VR = rpt.r2.lead.full$R_boot_link$Residual) %>% 
  data.frame() %>% 
  select(1, 2, 4, 5) %>% 
  mutate(Trait = "Group leadership")

r2.summary = rbind(r2.summary.svg, r2.summary.gsize, r2.summary.lead) %>% 
  gt(groupname_col = "Trait") %>%
  fmt_number(decimals = 2) %>% 
  cols_label(Parameter = "Variance component")

r2.summary %>% 
  gtsave(filename = here("outputs/tables/r2.summary.html"))

r2.summary %>% 
  gtsave(filename = here("outputs/tables/r2.summary.docx"))

# Reproduce deltaVariance and deltaR2 tables ----
## Import all rptR models ----
# Sex
rpt.R.f = read_rds(file = here("outputs/mods/rpt.R.f.rds"))
rpt.R.m = read_rds(file = here("outputs/mods/rpt.R.m.rds"))
rpt.V.f = read_rds(file = here("outputs/mods/rpt.V.f.rds"))
rpt.V.m = read_rds(file = here("outputs/mods/rpt.V.m.rds"))
rpt.r2.f = read_rds(file = here("outputs/mods/rpt.r2.f.rds"))
rpt.r2.m = read_rds(file = here("outputs/mods/rpt.r2.m.rds"))

# maturity
rpt.R.j = read_rds(file = here("outputs/mods/rpt.R.j.rds"))
rpt.R.a = read_rds(file = here("outputs/mods/rpt.R.a.rds"))
rpt.V.j = read_rds(file = here("outputs/mods/rpt.V.j.rds"))
rpt.V.a = read_rds(file = here("outputs/mods/rpt.V.a.rds"))
rpt.r2.j = read_rds(file = here("outputs/mods/rpt.r2.j.rds"))
rpt.r2.a = read_rds(file = here("outputs/mods/rpt.r2.A.rds"))

# injury
rpt.R.ni = read_rds(file = here("outputs/mods/rpt.R.ni.rds"))
rpt.R.i = read_rds(file = here("outputs/mods/rpt.R.i.rds"))
rpt.V.ni = read_rds(file = here("outputs/mods/rpt.V.ni.rds"))
rpt.V.i = read_rds(file = here("outputs/mods/rpt.V.i.rds"))
rpt.r2.ni = read_rds(file = here("outputs/mods/rpt.r2.ni.rds"))
rpt.r2.i = read_rds(file = here("outputs/mods/rpt.r2.i.rds"))

## Store values ----
### Sex ----
Vi_f = rpt.V.f$R_boot_link$id
Vi_m = rpt.V.m$R_boot_link$id
Vfe_f = rpt.V.f$R_boot_link$Fixed
Vfe_m = rpt.V.m$R_boot_link$Fixed
VR_f = rpt.V.f$R_boot_link$Residual
VR_m = rpt.V.m$R_boot_link$Residual
R_f = rpt.R.f$R_boot_link$id
R_m = rpt.R.m$R_boot_link$id

# Variance ratio difference figure (deltaV = Vmales - Vfemales)
r2_Vi_f = rpt.r2.f$R_boot_link$id
r2_Vi_m = rpt.r2.m$R_boot_link$id
r2_Vfe_f = rpt.r2.f$R_boot_link$Fixed
r2_Vfe_m = rpt.r2.m$R_boot_link$Fixed
r2_VR_f = rpt.r2.f$R_boot_link$Residual
r2_VR_m = rpt.r2.m$R_boot_link$Residual

df.V.sex = data.frame(Vi_f = Vi_f,
                      Vi_m = Vi_m,
                      Vfe_f = Vfe_f, 
                      Vfe_m = Vfe_m,
                      VR_f = VR_f, 
                      VR_m = VR_m)

df.r2.sex = data.frame(r2_Vi_f = r2_Vi_f,
                      r2_Vi_m = r2_Vi_m,
                      r2_Vfe_f = r2_Vfe_f, 
                      r2_Vfe_m = r2_Vfe_m,
                      r2_VR_f = r2_VR_f, 
                      r2_VR_m = r2_VR_m)


# Store effect sizes
df.delta.V.sex  = data.frame(delta_Vi = Vi_m - Vi_f,
                              delta_Vfe = Vfe_m - Vfe_f,
                              delta_VR = VR_m - VR_f)
df.delta.r2.sex  = data.frame(delta_r2_Vi = r2_Vi_m - r2_Vi_f,
                       delta_r2_Vfe = r2_Vfe_m - r2_Vfe_f,
                       delta_r2_VR = r2_VR_m - r2_VR_f)

### Maturity ----
Vi_j = rpt.V.j$R_boot_link$id
Vi_a = rpt.V.a$R_boot_link$id
Vfe_j = rpt.V.j$R_boot_link$Fixed
Vfe_a = rpt.V.a$R_boot_link$Fixed
VR_j = rpt.V.j$R_boot_link$Residual
VR_a = rpt.V.a$R_boot_link$Residual
R_j = rpt.R.j$R_boot_link$id
R_a = rpt.R.a$R_boot_link$id

# Variance ratio difference figure (deltaV = Vadults - Vjuveniles)
r2_Vi_j = rpt.r2.j$R_boot_link$id
r2_Vi_a = rpt.r2.a$R_boot_link$id
r2_Vfe_j = rpt.r2.j$R_boot_link$Fixed
r2_Vfe_a = rpt.r2.a$R_boot_link$Fixed
r2_VR_j = rpt.r2.j$R_boot_link$Residual
r2_VR_a = rpt.r2.a$R_boot_link$Residual

df.V.mat = data.frame(Vi_j = Vi_j,
                      Vi_a = Vi_a,
                      Vfe_j = Vfe_j, 
                      Vfe_a = Vfe_a,
                      VR_j = VR_j, 
                      VR_a = VR_a)

df.r2.mat = data.frame(r2_Vi_j = r2_Vi_j,
                       r2_Vi_a = r2_Vi_a,
                       r2_Vfe_j = r2_Vfe_j, 
                       r2_Vfe_a = r2_Vfe_a,
                       r2_VR_j = r2_VR_j, 
                       r2_VR_a = r2_VR_a)


# Store effect sizes
df.delta.V.mat  = data.frame(delta_Vi = Vi_a - Vi_j,
                             delta_Vfe = Vfe_a - Vfe_j,
                             delta_VR = VR_a - VR_j)
df.delta.r2.mat  = data.frame(delta_r2_Vi = r2_Vi_a - r2_Vi_j,
                              delta_r2_Vfe = r2_Vfe_a - r2_Vfe_j,
                              delta_r2_VR = r2_VR_a - r2_VR_j)

### Injury ----
Vi_ni = rpt.V.ni$R_boot_link$id
Vi_i = rpt.V.i$R_boot_link$id
Vfe_ni = rpt.V.ni$R_boot_link$Fixed
Vfe_i = rpt.V.i$R_boot_link$Fixed
VR_ni = rpt.V.ni$R_boot_link$Residual
VR_i = rpt.V.i$R_boot_link$Residual
R_ni = rpt.R.ni$R_boot_link$id
R_i = rpt.R.i$R_boot_link$id

# Variance ratio difference figure (deltaV = Vinjured - Vn-injured)
r2_Vi_ni = rpt.r2.ni$R_boot_link$id
r2_Vi_i = rpt.r2.i$R_boot_link$id
r2_Vfe_ni = rpt.r2.ni$R_boot_link$Fixed
r2_Vfe_i = rpt.r2.i$R_boot_link$Fixed
r2_VR_ni = rpt.r2.ni$R_boot_link$Residual
r2_VR_i = rpt.r2.i$R_boot_link$Residual

df.V.inj = data.frame(Vi_ni = Vi_ni,
                      Vi_i = Vi_i,
                      Vfe_ni = Vfe_ni, 
                      Vfe_i = Vfe_i,
                      VR_ni = VR_ni, 
                      VR_i = VR_i)

df.r2.inj = data.frame(r2_Vi_ni = r2_Vi_ni,
                       r2_Vi_i = r2_Vi_i,
                       r2_Vfe_ni = r2_Vfe_ni, 
                       r2_Vfe_i = r2_Vfe_i,
                       r2_VR_ni = r2_VR_ni, 
                       r2_VR_i = r2_VR_i)


# Store effect sizes
df.delta.V.inj  = data.frame(delta_Vi = Vi_i - Vi_ni,
                             delta_Vfe = Vfe_i - Vfe_ni,
                             delta_VR = VR_i - VR_ni)
df.delta.r2.inj  = data.frame(delta_r2_Vi = r2_Vi_i - r2_Vi_ni,
                              delta_r2_Vfe = r2_Vfe_i - r2_Vfe_ni,
                              delta_r2_VR = r2_VR_i - r2_VR_ni)

## Export tables ----
# Note: there's probably a neat way to combine all the information into 1 big table
# using the gt package. I can't see how at the moment so this will have to wait for another project
# The final table were assembled manually and is stored under "tables/tables final/TableS5_variance_comparisons"

### Sex ----
tbl_V_sex = df.V.sex %>% 
describe_posterior() %>% 
  select(c(1, 2, 4,5)) %>% 
  mutate(type = "Variance",
         v.compo = c("Vi", "Vi",
                     "Vfe", "Vfe",
                     "VR", "VR"),
         Sex = rep(c("Females", "Males"), 3)) %>% 
  data.frame()
tbl_r2_sex = df.r2.sex %>% 
  describe_posterior() %>% 
  select(c(1, 2, 4,5)) %>% 
  mutate(type = "R\U00B2",
         v.compo = c("Vi", "Vi",
                     "Vfe", "Vfe",
                     "VR", "VR"),
         Sex = rep(c("Females", "Males"), 3)) %>% 
  data.frame()

tbl_sex = rbind(tbl_V_sex, tbl_r2_sex) %>% 
  select(type, v.compo, Sex, Median, CI_low, CI_high) %>% 
  gt(groupname_col = "type") %>% 
  fmt_number(decimals = 2) %>% 
  cols_label(v.compo = "")
tbl_sex

delta_V_sex_summary = df.delta.V.sex %>% 
  describe_posterior() %>% 
  select(c(1, 2, 4,5, 6)) %>% 
  mutate(Sex = "Sex") %>% 
  data.frame()
delta_r2_sex_summary = df.delta.r2.sex %>% 
  describe_posterior() %>% 
  select(c(1, 2, 4, 5, 6)) %>% 
  mutate(Sex = "Sex") %>% 
  data.frame()


tbl_delta_V_sex = rbind(delta_V_sex_summary, delta_r2_sex_summary) %>% 
  gt() %>%
  fmt_number(decimals = 2) %>% 
  cols_label(Parameter = "∆V") %>% 
  text_case_match(
    "delta_Vi" ~ "∆Vi",
    "delta_Vfe" ~ "∆Vfe",
    "delta_VR" ~ "∆VR",
    "delta_r2_Vi" ~ "∆R2i",
    "delta_r2_Vfe" ~ "∆R2fe",
    "delta_r2_VR" ~ "∆VR2R")

# Export
tbl_sex %>% 
  gtsave(filename = here("outputs/tables/tbl_sex.html"))
tbl_delta_V_sex %>% 
  gtsave(filename = here("outputs/tables/tbl_delta_V_sex.html"))

tbl_sex %>% 
  gtsave(filename = here("outputs/tables/tbl_sex.docx"))
tbl_delta_V_sex %>% 
  gtsave(filename = here("outputs/tables/tbl_delta_V_sex.docx"))



### Maturity ----
tbl_V_mat = df.V.mat %>% 
  describe_posterior() %>% 
  select(c(1, 2, 4,5)) %>% 
  mutate(type = "Variance",
         v.compo = c("Vi", "Vi",
                     "Vfe", "Vfe",
                     "VR", "VR"),
         Maturity = rep(c("Juveniles", "Adults"), 3)) %>% 
  data.frame()
tbl_r2_mat = df.r2.mat %>% 
  describe_posterior() %>% 
  select(c(1, 2, 4,5)) %>% 
  mutate(type = "R\U00B2",
         v.compo = c("Vi", "Vi",
                     "Vfe", "Vfe",
                     "VR", "VR"),
         Maturity = rep(c("Juveniles", "Adults"), 3)) %>% 
  data.frame()

tbl_mat = rbind(tbl_V_mat, tbl_r2_mat) %>% 
  select(type, v.compo, Maturity, Median, CI_low, CI_high) %>% 
  gt(groupname_col = "type") %>% 
  fmt_number(decimals = 2) %>% 
  cols_label(v.compo = "")
tbl_mat


delta_V_mat_summary = df.delta.V.mat %>% 
  describe_posterior() %>% 
  select(c(1, 2, 4,5, 6)) %>% 
  mutate(Maturity = "Maturity") %>% 
  data.frame()
delta_r2_mat_summary = df.delta.r2.mat %>% 
  describe_posterior() %>% 
  select(c(1, 2, 4, 5, 6)) %>% 
  mutate(Maturity = "Maturity") %>% 
  data.frame()


tbl_delta_V_mat = rbind(delta_V_mat_summary, delta_r2_mat_summary) %>% 
  gt() %>%
  fmt_number(decimals = 2) %>% 
  cols_label(Parameter = "∆V") %>% 
  text_case_match(
    "delta_Vi" ~ "∆Vi",
    "delta_Vfe" ~ "∆Vfe",
    "delta_VR" ~ "∆VR",
    "delta_r2_Vi" ~ "∆R2i",
    "delta_r2_Vfe" ~ "∆R2fe",
    "delta_r2_VR" ~ "∆VR2R")


# Export
tbl_mat %>% 
  gtsave(filename = here("outputs/tables/tbl_mat.html"))
tbl_delta_V_mat %>% 
  gtsave(filename = here("outputs/tables/tbl_delta_V_mat.html"))

tbl_mat %>% 
  gtsave(filename = here("outputs/tables/tbl_mat.docx"))
tbl_delta_V_mat %>% 
  gtsave(filename = here("outputs/tables/tbl_delta_V_mat.docx"))


### Injury ----
tbl_V_inj = df.V.inj %>% 
  describe_posterior() %>% 
  select(c(1, 2, 4,5)) %>% 
  mutate(type = "Variance",
         v.compo = c("Vi", "Vi",
                     "Vfe", "Vfe",
                     "VR", "VR"),
         Maturity = rep(c("Juveniles", "Adults"), 3)) %>% 
  data.frame()
tbl_r2_inj = df.r2.inj %>% 
  describe_posterior() %>% 
  select(c(1, 2, 4,5)) %>% 
  mutate(type = "R\U00B2",
         v.compo = c("Vi", "Vi",
                     "Vfe", "Vfe",
                     "VR", "VR"),
         Maturity = rep(c("Juveniles", "Adults"), 3)) %>% 
  data.frame()

tbl_inj = rbind(tbl_V_inj, tbl_r2_inj) %>% 
  select(type, v.compo, Maturity, Median, CI_low, CI_high) %>% 
  gt(groupname_col = "type") %>% 
  fmt_number(decimals = 2) %>% 
  cols_label(v.compo = "")
tbl_inj


delta_V_inj_summary = df.delta.V.inj %>% 
  describe_posterior() %>% 
  select(c(1, 2, 4,5, 6)) %>% 
  mutate(Injury = "Injury") %>% 
  data.frame()
delta_r2_inj_summary = df.delta.r2.inj %>% 
  describe_posterior() %>% 
  select(c(1, 2, 4, 5, 6)) %>% 
  mutate(Injury = "Injury") %>% 
  data.frame()

tbl_delta_V_inj = rbind(delta_V_inj_summary, delta_r2_inj_summary) %>% 
  gt() %>%
  fmt_number(decimals = 2) %>% 
  cols_label(Parameter = "∆V") %>% 
  text_case_match(
    "delta_Vi" ~ "∆Vi",
    "delta_Vfe" ~ "∆Vfe",
    "delta_VR" ~ "∆VR",
    "delta_r2_Vi" ~ "∆R2i",
    "delta_r2_Vfe" ~ "∆R2fe",
    "delta_r2_VR" ~ "∆VR2R")
tbl_delta_V_inj

# Export
tbl_inj %>% 
  gtsave(filename = here("outputs/tables/tbl_inj.html"))
tbl_delta_V_inj %>% 
  gtsave(filename = here("outputs/tables/tbl_delta_V_inj.html"))

tbl_inj %>% 
  gtsave(filename = here("outputs/tables/tbl_inj.docx"))
tbl_delta_V_inj %>% 
  gtsave(filename = here("outputs/tables/tbl_delta_V_inj.docx"))

