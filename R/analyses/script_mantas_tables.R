library(tidyverse); library(here)
library(ggthemes); library(patchwork)
library(tidybayes); library(gt); library(gtsummary)
library(broom.mixed); library(AICcmodavg)
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
## Group leadership ----
# Only need full model here
R2_cond.solo.v.g.full = R2_cond_fun(Vi = rpt.r2.solo.v.g.full$R_boot_link$id,
                          Vsite = rpt.r2.solo.v.g.full$R_boot_link$id,
                          Vfe = rpt.r2.solo.v.g.full$R_boot_link$id,
                          VR = rpt.r2.solo.v.g.full$R_boot_link$Residual)

R2_marg.solo.v.g.full = R2_marg_fun(Vi = rpt.r2.solo.v.g.full$R_boot_link$id,
                          Vsite = rpt.r2.solo.v.g.full$R_boot_link$id,
                          Vfe = rpt.r2.solo.v.g.full$R_boot_link$id,
                          VR = rpt.r2.solo.v.g.full$R_boot_link$Residual)
R2_marg.solo.v.g.abio = R2_marg_fun(Vi = rpt.r2.solo.v.g.abio$R_boot_link$id,
                                    Vsite = rpt.r2.solo.v.g.abio$R_boot_link$id,
                                    Vfe = rpt.r2.solo.v.g.abio$R_boot_link$id,
                                    VR = rpt.r2.solo.v.g.abio$R_boot_link$Residual)
R2_marg.solo.v.g.bio = R2_marg_fun(Vi = rpt.r2.solo.v.g.bio$R_boot_link$id,
                                    Vsite = rpt.r2.solo.v.g.bio$R_boot_link$id,
                                    Vfe = rpt.r2.solo.v.g.bio$R_boot_link$id,
                                    VR = rpt.r2.solo.v.g.bio$R_boot_link$Residual)

## Group size ----
# Only need full model here
R2_cond.gsize.full = R2_cond_fun(Vi = rpt.r2.gsize.full$R_boot_link$id,
                                    Vsite = rpt.r2.gsize.full$R_boot_link$id,
                                    Vfe = rpt.r2.gsize.full$R_boot_link$id,
                                    VR = rpt.r2.gsize.full$R_boot_link$Residual)

R2_marg.gsize.full = R2_marg_fun(Vi = rpt.r2.gsize.full$R_boot_link$id,
                                    Vsite = rpt.r2.gsize.full$R_boot_link$id,
                                    Vfe = rpt.r2.gsize.full$R_boot_link$id,
                                    VR = rpt.r2.gsize.full$R_boot_link$Residual)
R2_marg.gsize.abio = R2_marg_fun(Vi = rpt.r2.gsize.abio$R_boot_link$id,
                                    Vsite = rpt.r2.gsize.abio$R_boot_link$id,
                                    Vfe = rpt.r2.gsize.abio$R_boot_link$id,
                                    VR = rpt.r2.gsize.abio$R_boot_link$Residual)
R2_marg.gsize.bio = R2_marg_fun(Vi = rpt.r2.gsize.bio$R_boot_link$id,
                                   Vsite = rpt.r2.gsize.bio$R_boot_link$id,
                                   Vfe = rpt.r2.gsize.bio$R_boot_link$id,
                                   VR = rpt.r2.gsize.bio$R_boot_link$Residual)

## Group leadership ----
# Only need full model here
R2_cond.lead.full = R2_cond_fun(Vi = rpt.r2.lead.full$R_boot_link$id,
                                    Vsite = rpt.r2.lead.full$R_boot_link$id,
                                    Vfe = rpt.r2.lead.full$R_boot_link$id,
                                    VR = rpt.r2.lead.full$R_boot_link$Residual)

R2_marg.lead.full = R2_marg_fun(Vi = rpt.r2.lead.full$R_boot_link$id,
                                    Vsite = rpt.r2.lead.full$R_boot_link$id,
                                    Vfe = rpt.r2.lead.full$R_boot_link$id,
                                    VR = rpt.r2.lead.full$R_boot_link$Residual)
R2_marg.lead.status = R2_marg_fun(Vi = rpt.r2.lead.status$R_boot_link$id,
                                    Vsite = rpt.r2.lead.status$R_boot_link$id,
                                    Vfe = rpt.r2.lead.status$R_boot_link$id,
                                    VR = rpt.r2.lead.status$R_boot_link$Residual)
R2_marg.lead.inj.state = R2_marg_fun(Vi = rpt.r2.lead.inj.state$R_boot_link$id,
                                   Vsite = rpt.r2.lead.inj.state$R_boot_link$id,
                                   Vfe = rpt.r2.lead.inj.state$R_boot_link$id,
                                   VR = rpt.r2.lead.inj.state$R_boot_link$Residual)

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
         R2_marg.solo.v.g.abio[,2],
         R2_marg.solo.v.g.bio[,2],
         R2_marg.solo.v.g.full[,2]))

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
         R2_marg.gsize.abio[,2],
         R2_marg.gsize.bio[,2],
         R2_marg.gsize.full[,2]))

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
         R2_marg.lead.status[,2],
         R2_marg.lead.inj.state[,2],
         R2_marg.lead.full[,2]))


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


# Reproduce variance and R2 tables ----
### Table group vs. solo ----
# Conditional R2
describe_posterior((r2_Vi+r2_Vfe)/(r2_Vi+r2_Vfe+r2_VR)*100)

Var.tbl = data.frame(Vi = Vi,
                     Vfe = Vfe,
                     VR = VR) %>%
  describe_posterior() %>% 
  mutate(Parameter = fct_recode(Parameter, 
                                Vi = "Vi",
                                Vfe = "Vfe",
                                VR = "VR")) %>% 
  select(c(1, 2,  4, 5)) %>% 
  gt()


# Proportion of variance explained 
R2.tbl = data.frame(r2_Vi = r2_Vi * 100,
                    r2_Vfe = r2_Vfe * 100,
                    r2_VR = r2_VR * 100) %>% 
  describe_posterior() %>% 
  mutate(Parameter = fct_recode(Parameter, 
                                Vi = "r2_Vi",
                                Vfe = "r2_Vfe",
                                VR = "r2_VR")) %>% 
  select(c(1, 2, 4, 5)) %>% 
  gt()

Var.tbl = Var.tbl$`_data`
R2.tbl = R2.tbl$`_data`

Var.R2.tbl = data.frame(Var.tbl, R2.tbl[2:4]) %>% 
  gt(rowname_col = "Parameter") %>% 
  tab_spanner(label = "Variance",
              columns = 2:4) %>% 
  tab_spanner(label = "% Variance explained",
              columns = 5:7) %>% 
  fmt_number(decimals = 2) %>% 
  cols_label(Median.1 = "Median",
             CI_low.1 = "CI_low",
             CI_high.1 = "CI_high")

Var.R2.tbl %>% 
  gtsave(filename = here("outputs/tables/Var.R2.tbl.gsize.html"))

Var.R2.tbl %>% 
  gtsave(filename = here("outputs/tables/Var.R2.tbl.gsize.docx"))


### Table group size ----
# Conditional R2
describe_posterior((r2_Vi+r2_Vfe)/(r2_Vi+r2_Vfe+r2_VR)*100)

Var.tbl = data.frame(Vi = Vi,
                     Vfe = Vfe,
                     VR = VR) %>%
  describe_posterior() %>% 
  mutate(Parameter = fct_recode(Parameter, 
                                Vi = "Vi",
                                Vfe = "Vfe",
                                VR = "VR")) %>% 
  select(c(1, 2,  4, 5)) %>% 
  gt()


# Proportion of variance explained 
R2.tbl = data.frame(r2_Vi = r2_Vi * 100,
                    r2_Vfe = r2_Vfe * 100,
                    r2_VR = r2_VR * 100) %>% 
  describe_posterior() %>% 
  mutate(Parameter = fct_recode(Parameter, 
                                Vi = "r2_Vi",
                                Vfe = "r2_Vfe",
                                VR = "r2_VR")) %>% 
  select(c(1, 2, 4, 5)) %>% 
  gt()

Var.tbl = Var.tbl$`_data`
R2.tbl = R2.tbl$`_data`

Var.R2.tbl = data.frame(Var.tbl, R2.tbl[2:4]) %>% 
  gt(rowname_col = "Parameter") %>% 
  tab_spanner(label = "Variance",
              columns = 2:4) %>% 
  tab_spanner(label = "% Variance explained",
              columns = 5:7) %>% 
  fmt_number(decimals = 2) %>% 
  cols_label(Median.1 = "Median",
             CI_low.1 = "CI_low",
             CI_high.1 = "CI_high")

Var.R2.tbl %>% 
  gtsave(filename = here("outputs/tables/Var.R2.tbl.solo.v.g.html"))

Var.R2.tbl %>% 
  gtsave(filename = here("outputs/tables/Var.R2.tbl.solo.v.g.docx"))


# Reproduce deltaVariance and deltaR2 tables ----
