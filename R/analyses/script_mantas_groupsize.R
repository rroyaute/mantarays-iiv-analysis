library(tidyverse); library(here); library(easystats);
library(kableExtra); library(lme4); library(effects)
library(marginaleffects);  library(ggeffects);  library(rptR)
library(gtsummary); library(ggthemes); library(patchwork)
library(tidybayes); library(gt); library(AICcmodavg)

# TODO:
# verify time to ht column

## Data import ----
df.group = read.csv(here("data/data-clean/group_rp.csv"), 
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


## What explains variation in group leadership? ----
### Model fitting ----
glmm.gsize.null = glmer(group_size ~  1 + (1|id), 
                        family = "poisson",
                        df.group)

glmm.gsize.abio = glmer(group_size ~  site + current.VU.AO + time_ht_sc + (1|id), 
                        family = "poisson",
                        df.group)

glmm.gsize.bio.ext = glmer(group_size ~  plankton + no_mantas_sc + (1|id), 
                           family = "poisson",
                           df.group)
glmm.gsize.full = glmer(group_size ~  site + current.VU.AO + time_ht_sc +
                          plankton + no_mantas_sc + (1|id), 
                        family = "poisson",
                        df.group)

# Save models 
saveRDS(glmm.gsize.null, file = here("outputs/mods/glmm.gsize.null.rds"))
saveRDS(glmm.gsize.abio, file = here("outputs/mods/glmm.gsize.abio.rds"))
saveRDS(glmm.gsize.bio.ext, file = here("outputs/mods/glmm.gsize.bio.ext.rds"))
saveRDS(glmm.gsize.full, file = here("outputs/mods/glmm.gsize.full.rds"))


### AIC model comparison ----
Model.list=list()
Model.list[[1]]=glmm.gsize.null
Model.list[[2]]=glmm.gsize.abio
Model.list[[3]]=glmm.gsize.bio.ext
Model.list[[4]]=glmm.gsize.full


names(Model.list)=c("Null", "Abiotic","Biotic (external)", "Abiotic + Biotic (external)")
# Calculate AIC values and delta_AIC by setting second.ord = F 
#(returns AICc otherwise, which are mostly used with limited samples sizes)
aictab.gsize = data.frame(aictab(Model.list, second.ord = F)) %>% 
  select(1:4) %>% 
  rename("Model" = Modnames)
  
aictab.gsize

#### R2 for fixed effects in each model ----
R2.null = r2_nakagawa(glmm.gsize.null)
R2.abio = r2_nakagawa(glmm.gsize.abio)
R2.bio.ext = r2_nakagawa(glmm.gsize.bio.ext)
R2.full = r2_nakagawa(glmm.gsize.full)

R2.table = data.frame(
  Model = c("Null", "Abiotic","Biotic (external)", "Abiotic + Biotic (external)"),
  R2 = c(R2.null$R2_marginal,
         R2.abio$R2_marginal,
         R2.bio.ext$R2_marginal,
         R2.full$R2_marginal))

#### Effect plots and coefficient table for full model ----
check_model(glmm.gsize.full)

plot(allEffects(glmm.gsize.full))

glmm.gsize = glmm.gsize.full %>% 
  tbl_regression(
  intercept = T,
  exponentiate = F,
  pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  add_global_p() %>%
  bold_p(t = .05) %>%
  bold_labels() %>%
  italicize_levels() %>% 
  add_n(location = "level")
glmm.gsize

#### Export tables ----
aictab.gsize = full_join(aictab.gsize, data.frame(R2.table)) %>%
  gt() %>%
  fmt_number(decimals = 2) %>% 
  cols_label(Delta_AIC = "∆AIC")

aictab.gsize %>% 
  gtsave(filename = here("outputs/tables/aictab.gsize.html"))

aictab.gsize %>% 
  gtsave(filename = here("outputs/tables/aictab.gsize.docx"))


glmm.gsize %>% 
  as_gt() %>%
  gtsave(filename = here("outputs/tables/glmm.gsize.html"))

glmm.gsize %>% 
  as_gt() %>%
  gtsave(filename = here("outputs/tables/glmm.gsize.docx"))

### Variance explained by individuals ----
# Import rptR model
rpt.V.gsize = read_rds(file = here("outputs/mods/rpt.V.gsize.rds"))
rpt.r2.gsize = read_rds(file = here("outputs/mods/rpt.r2.gsize.rds"))

rpt.V.gsize = rpt(group_size ~  site + current.VU.AO + time_ht_sc +
                          plankton + no_mantas_sc + (1|id), 
                 grname = c("id", "Fixed", "Residual"), 
                 datatype = c("Poisson"), 
                 npermut = 1000,
                 parallel = T, 
                 data = df.group,
                 ratio = F)
saveRDS(rpt.V.gsize, file = here("outputs/mods/rpt.V.gsize.rds"))

rpt.r2.gsize = rpt(group_size ~  site + current.VU.AO + time_ht_sc +
                     plankton + no_mantas_sc + (1|id), 
                  grname = c("id", "Fixed", "Residual"), 
                  datatype = c("Poisson"), 
                  npermut = 1000, 
                  parallel = T, 
                  data = df.group,
                  ratio = T)
saveRDS(rpt.r2.gsize, file = here("outputs/mods/rpt.r2.gsize.rds"))


### Plot R2 variation ----
#### Plot ----
r2_Vi = rpt.r2.gsize$R_boot_link$id
r2_Vfe = rpt.r2.gsize$R_boot_link$Fixed
r2_VR = rpt.r2.gsize$R_boot_link$Residual

Vi = rpt.V.gsize$R_boot_link$id
Vfe = rpt.V.gsize$R_boot_link$Fixed
VR = rpt.V.gsize$R_boot_link$Residual


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
saveRDS(var.compo, file = here("outputs/ggplot/var.compo.gsize.rds"))

ggsave(filename = here("outputs/figs/var.compo.gsize.jpeg"), var.compo, 
       width = 12, height = 8)
ggsave(filename = here("outputs/figs/var.compo.gsize.pdf"), var.compo, 
       width = 12, height = 8)

#### Table ----
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

