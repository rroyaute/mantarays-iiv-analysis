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




## What explains variation in foraging strategies (solo vs. group foraging)? ----
### Model fitting ----
glmm.solo.v.g.null = glmer(group ~  1 + (1|id), 
                        family = "binomial",
                        df.tot)

glmm.solo.v.g.abio = glmer(group ~  nyear + site + current + time_ht_sc + (1|id), 
                        family = "binomial",
                        df.tot)

glmm.solo.v.g.bio.ext = glmer(group ~  plankton + no_mantas_sc + (1|id), 
                           family = "binomial",
                           df.tot)

glmm.solo.v.g.full = glmer(group ~  nyear + 
                             site + 
                             current + 
                             time_ht_sc +
                             plankton + 
                             no_mantas_sc + 
                             (1|id), 
                           family = "binomial",
                           df.tot)

# Save models 
saveRDS(glmm.solo.v.g.null, file = here("outputs/mods/glmm.solo.v.g.null.rds"))
saveRDS(glmm.solo.v.g.abio, file = here("outputs/mods/glmm.solo.v.g.abio.rds"))
saveRDS(glmm.solo.v.g.bio.ext, file = here("outputs/mods/glmm.solo.v.g.bio.ext.rds"))
saveRDS(glmm.solo.v.g.full, file = here("outputs/mods/glmm.solo.v.g.full.rds"))


### AIC model comparison ----
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

#### R2 for fixed effects in each model ----
R2.null = r2_nakagawa(glmm.solo.v.g.null)
R2.abio = r2_nakagawa(glmm.solo.v.g.abio)
R2.bio.ext = r2_nakagawa(glmm.solo.v.g.bio.ext)
R2.full = r2_nakagawa(glmm.solo.v.g.full)

saveRDS(R2.full, here("outputs/mods/r2.solo.v.g.rds"))

R2.table = data.frame(
  Model = c("Null", "Abiotic","Biotic (external)", "Abiotic + Biotic (external)"),
  R2 = c(R2.null$R2_marginal,
         R2.abio$R2_marginal,
         R2.bio.ext$R2_marginal,
         R2.full$R2_marginal))

#### Effect plots and coefficient table for full model ----
check_model(glmm.solo.v.g.full)

plot(allEffects(glmm.solo.v.g.full))

glmm.solo.v.g = glmm.solo.v.g.full %>% 
  tbl_regression(
    intercept = T,
    exponentiate = F,
    pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  add_global_p() %>%
  bold_p(t = .05) %>%
  bold_labels() %>%
  italicize_levels() %>% 
  add_n(location = "level")
glmm.solo.v.g

# Plot marginal effects in cleaner way
glmm.pred <- plot(ggeffect(glmm.solo.v.g.full, show_data = T))
glmm.pred

#### Export tables ----
aictab.solo.v.g = full_join(aictab.solo.v.g, data.frame(R2.table)) %>%
  gt() %>%
  fmt_number(decimals = 2) %>% 
  cols_label(Delta_AIC = "∆AIC")

aictab.solo.v.g %>% 
  gtsave(filename = here("outputs/tables/aictab.solo.v.g.html"))

aictab.solo.v.g %>% 
  gtsave(filename = here("outputs/tables/aictab.solo.v.g.docx"))


glmm.solo.v.g %>% 
  as_gt() %>%
  gtsave(filename = here("outputs/tables/glmm.solo.v.g.html"))

glmm.solo.v.g %>% 
  as_gt() %>%
  gtsave(filename = here("outputs/tables/glmm.solo.v.g.docx"))

### Variance explained by individuals ----
# Import rptR model
rpt.V.solo.v.g = read_rds(file = here("outputs/mods/rpt.V.solo.v.g.rds"))
rpt.r2.solo.v.g = read_rds(file = here("outputs/mods/rpt.r2.solo.v.g.rds"))

rpt.V.solo.v.g = rpt(group ~  nyear + site + current + time_ht_sc +
                       plankton + no_mantas_sc + (1|id), 
                  grname = c("id", "Fixed", "Residual"), 
                  datatype = c("Binary"), 
                  # npermut = 1000,
                  parallel = T, 
                  data = df.tot,
                  ratio = F)
saveRDS(rpt.V.solo.v.g, file = here("outputs/mods/rpt.V.solo.v.g.rds"))

rpt.r2.solo.v.g = rpt(group ~  nyear + site + current + time_ht_sc +
                        plankton + no_mantas_sc + (1|id), 
                   grname = c("id", "Fixed", "Residual"), 
                   datatype = c("Binary"), 
                   # npermut = 1000, 
                   parallel = T, 
                   data = df.tot,
                   ratio = T)
saveRDS(rpt.r2.solo.v.g, file = here("outputs/mods/rpt.r2.solo.v.g.rds"))

# Update for permutation test
rpt.V.solo.v.g = rpt(group ~  nyear + site + current + time_ht_sc +
                       plankton + no_mantas_sc + (1|id), 
                     grname = c("id", "Fixed", "Residual"), 
                     datatype = c("Binary"), 
                     npermut = 1000,
                     parallel = T, 
                     data = df.tot,
                     ratio = F, 
                     rptObj = rpt.V.solo.v.g, 
                     update = T)
saveRDS(rpt.V.solo.v.g, file = here("outputs/mods/rpt.V.solo.v.g.rds"))

rpt.r2.solo.v.g = rpt(group ~  nyear + site + current + time_ht_sc +
                        plankton + no_mantas_sc + (1|id), 
                      grname = c("id", "Fixed", "Residual"), 
                      datatype = c("Binary"), 
                      # npermut = 1000, 
                      parallel = T, 
                      data = df.tot,
                      ratio = T, 
                      rptObj = rpt.r2.solo.v.g, 
                      update = T)
saveRDS(rpt.r2.solo.v.g, file = here("outputs/mods/rpt.r2.solo.v.g.rds"))


### Plot R2 variation ----
#### Plot ----
r2_Vi = rpt.r2.solo.v.g$R_boot_link$id
r2_Vfe = rpt.r2.solo.v.g$R_boot_link$Fixed
r2_VR = rpt.r2.solo.v.g$R_boot_link$Residual

Vi = rpt.V.solo.v.g$R_boot_link$id
Vfe = rpt.V.solo.v.g$R_boot_link$Fixed
VR = rpt.V.solo.v.g$R_boot_link$Residual


df.compo.r2 = data.frame(r2_Vi = r2_Vi,
                         r2_Vfe = r2_Vfe,
                         r2_VR = r2_VR) %>%
  pivot_longer(cols = r2_Vi:r2_VR,
               names_to = "v.compo",
               values_to = "var")
df.compo.V = data.frame(Vi = Vi,
                        Vfe = Vfe,
                        VR = VR) %>%
  pivot_longer(cols = Vi:VR,
               names_to = "v.compo",
               values_to = "var")

p1 = df.compo.r2 %>% 
  group_by(v.compo) %>% 
  summarise(var = mean(var)) %>% 
  ggplot(aes(y = var * 100, x = 0, fill = v.compo)) +
  geom_bar(position = "fill", 
           stat = "identity", width = .02) +
  scale_fill_wsj(labels = c(
    bquote(V[fe]), 
    bquote(V[i]),
    bquote(V[R]))) +
  xlab("Variance components") +
  ylab("Variance explained (proportion)") +
  theme_bw(18) + 
  theme(legend.position = c(.9, .5),
        legend.title = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  ggtitle("Group size")

p2 = df.compo.V %>% 
  ggplot(aes(y = v.compo, x = var, fill = v.compo)) +
  stat_halfeye() + 
  scale_fill_wsj() +
  scale_y_discrete(labels = c(
    bquote(V[fe]), 
    bquote(V[i]),
    bquote(V[R]))) +
  xlab("Variance") +
  ylab("Variance component") +
  theme_bw(18) +
  theme(legend.position = "none")

var.compo = p1 + p2
var.compo
saveRDS(var.compo, file = here("outputs/ggplot/var.compo.solo.v.g.rds"))

ggsave(filename = here("outputs/figs/var.compo.solo.v.g.jpeg"), var.compo, 
       width = 12, height = 8)
ggsave(filename = here("outputs/figs/var.compo.solo.v.g.pdf"), var.compo, 
       width = 12, height = 8)

#### Table ----
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

