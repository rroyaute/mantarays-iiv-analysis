library(tidyverse); library(here)
library(ggthemes); library(patchwork)
library(tidybayes); library(rptR)
library(marginaleffects); library(scales)

# Figure 2: Marginal effects plots ----
## Import models ----
glmm.solo.v.g.full = read_rds(file = here("outputs/mods/glmm.solo.v.g.full_site_rd.rds"))
glmm.gsize.full = read_rds(file = here("outputs/mods/glmm.gsize.full.rds"))
glmm.lead.full = read_rds(file = here("outputs/mods/glmm.lead.full.rds"))

## Plots average trends & CI ----
### Solo vs group foraging ----
p1.svg = plot_predictions(glmm.solo.v.g.full, condition = "nyear") + 
  scale_x_discrete(labels=c("year1" = "2014", 
                            "year2" = "2015",
                            "year3" = "2016")) +
  ylim(0, 1) +
  xlab("Year") +
  ylab("Probability of foraging in group") +
  theme_bw() 
p2.svg = plot_predictions(glmm.solo.v.g.full, condition = "current") + 
  ylim(0, 1) +
  xlab("Current strength") +
  ylab("Probability of foraging in group") +
  theme_bw() 
p3.svg = plot_predictions(glmm.solo.v.g.full, condition = "plankton") + 
  ylim(0, 1) +
  xlab("Plankton abundance") +
  ylab("Probability of foraging in group") +
  theme_bw() 
p4.svg = plot_predictions(glmm.solo.v.g.full, condition = "time_ht_sc") + 
  xlab("Time to high tide (sd units)") +
  ylab("Probability of foraging in group") +
  ylim(0, 1) +
  theme_bw() 
p5.svg = plot_predictions(glmm.solo.v.g.full, condition = "no_mantas_sc") + 
  ylim(0, 1) +
  xlab("Number of mantas feeding (sd units)") +
  ylab("Probability of foraging in group") +
  theme_bw() 


### Group size ----
p1.gsize = plot_predictions(glmm.gsize.full, condition = "nyear") + 
  scale_x_discrete(labels=c("year1" = "2014", 
                            "year2" = "2015",
                            "year3" = "2016")) +
  ylim(0,10) +
  xlab("Year") +
  ylab("Group size") +
  theme_bw() 
p2.gsize = plot_predictions(glmm.gsize.full, condition = "current") + 
  ylim(0,10) +
  xlab("Current strength") +
  ylab("Group size") +
  theme_bw() 
p3.gsize = plot_predictions(glmm.gsize.full, condition = "plankton") + 
  ylim(0,10) +
  xlab("Plankton abundance") +
  ylab("Group size") +
  theme_bw() 
p4.gsize = plot_predictions(glmm.gsize.full, condition = "time_ht_sc") + 
  ylim(0,10) +
  ylab("Group size") +
  theme_bw() 
p5.gsize = plot_predictions(glmm.gsize.full, condition = "no_mantas_sc") + 
  ylim(0,10) +
  xlab("Number of mantas feeding (sd units)") +
  ylab("Group size") +
  theme_bw() 



### Group leadership ----
p1.lead = plot_predictions(glmm.lead.full, condition = "sex_f") + 
  ylim(0,1) +
  xlab("Sex") +
  ylab("Probability of leading the group") +
  theme_bw() 
p2.lead = plot_predictions(glmm.lead.full, condition = "maturity") + 
  ylim(0,1) +
  xlab("Maturity") +
  ylab("Probability of leading the group") +
  theme_bw() 
p3.lead = plot_predictions(glmm.lead.full, condition = "shark_bite") + 
  ylim(0,1) +
  xlab("Shark bites") +
  ylab("Probability of leading the group") +
  theme_bw() 
p4.lead = plot_predictions(glmm.lead.full, condition = "anthropogenic") + 
  ylim(0,1) +
  ylab("Anthropogenic injuries") +
  theme_bw() 

### Transform to raw data (for sd unit axes) ----
#### Import data ----
df.tot = read.csv(here("data/data-raw/Manta Data_Annie.csv"),
                  header=TRUE, sep=",", na.strings="NA", dec=".",
                  strip.white=TRUE)

# Transform Number of mantas columns centered mean
df.tot$no_mantas_sc = as.numeric(scale(df.tot$no_mantas))
sd.tot.no_mantas = sd(df.tot$no_mantas)
mu.tot.no_mantas = mean(df.tot$no_mantas)

# Transform Number of mantas columns centered mean
df.tot$time_ht_sc = as.numeric(scale(df.tot$time_ht))
sd.tot.time_ht = sd(df.tot$time_ht)
mu.tot.time_ht = mean(df.tot$time_ht)

df.group = read.csv(here("data/data-raw/group_rp.csv"),
                    header=TRUE, sep=",", na.strings="NA", dec=".",
                    strip.white=TRUE)

# Transform Number of mantas columns scaled to sd units
df.group$no_mantas_sc = as.numeric(scale(df.group$no_mantas))
sd.group.no_mantas = sd(df.group$no_mantas)
mu.group.no_mantas = mean(df.group$no_mantas)

# Transform time to high scaled to sd units
df.group$time_ht_sc = as.numeric(scale(df.group$time_ht))
sd.group.time_ht = sd(df.group$time_ht)
mu.group.time_ht = mean(df.group$time_ht)

# Transform current into numeric variable
df.group$current = as.numeric(df.group$current.VU.AO)


### Combine plots ----
fig.svg = p1.svg + ggtitle("Solo vs. group foraging") + p4.svg + p5.svg
fig.svg

fig.gsize = p1.gsize + ggtitle("Group size") + p2.gsize + p3.gsize + p5.gsize
fig.gsize

fig.lead = p1.lead + ggtitle("Group leadership")

fig.marg.effects = fig.svg / fig.gsize / fig.lead
fig.marg.effects

ggsave(filename = here("outputs/figs/Fig2.marg.effects.jpeg"), fig.marg.effects,
       width = 10, height = 10)
ggsave(filename = here("outputs/figs/Fig2.marg.effects.pdf"), fig.marg.effects,
       width = 10, height = 10)


# ## Add datapoints (later) ----
# ### Import data ----
# df.tot = read.csv(here("data/data-raw/Manta Data_Annie.csv"), 
#                   header=TRUE, sep=",", na.strings="NA", dec=".",
#                   strip.white=TRUE)
# 
# # Transform Number of mantas columns centered mean
# df.tot$no_mantas_sc = as.numeric(scale(df.tot$no_mantas))
# # Transform Number of mantas columns centered mean
# df.tot$time_ht_sc = as.numeric(scale(df.tot$time_ht))
# 
# df.group = read.csv(here("data/data-raw/group_rp.csv"), 
#                     header=TRUE, sep=",", na.strings="NA", dec=".",
#                     strip.white=TRUE)
# 
# # Transform Number of mantas columns scaled to sd units
# df.group$no_mantas_sc = as.numeric(scale(df.group$no_mantas))
# 
# # Transform time to high scaled to sd units
# df.group$time_ht_sc = as.numeric(scale(df.group$time_ht))
# 
# # Transform current into numeric variable
# df.group$current = as.numeric(df.group$current.VU.AO)
# 
# 
# ### Overlay points ----
# p1 + 
#   stat_slab(data = df.tot,
#             aes(x = no_mantas_sc, y = group, 
#                 color = as.factor(group), 
#                 fill = as.factor(group), 
#                 side = ifelse(group == 0, "top", "bottom")),
#             slab_type = "histogram", alpha = .8,
#             scale = 0.4, breaks = 40, size = 1/2) +
#   scale_fill_wsj() +
#   scale_color_wsj() + 
#   xlab("Number of mantas foraging (sd units)") +
#   ylab("Group leadership") +
#   theme_bw(14)
# 
# 
# Figure 3: Variance components and R2 compared among traits -----
## Import rptR objects ----
# Import foraging strategy variance
rpt.V.solo.v.g = read_rds(file = here("outputs/mods/rpt.V.solo.v.g_site.full.rds"))
rpt.r2.solo.v.g = read_rds(file = here("outputs/mods/rpt.r2.solo.v.g_site.full.rds"))

# Import group size variance
rpt.V.gsize = read_rds(file = here("outputs/mods/rpt.V.gsize.full.rds"))
rpt.r2.gsize = read_rds(file = here("outputs/mods/rpt.r2.gsize.full.rds"))

# Import group leadership variance
rpt.r2.lead = read_rds(file = here("outputs/mods/rpt.r2.lead.full.rds"))
rpt.V.lead = read_rds(file = here("outputs/mods/rpt.V.lead.full.rds"))

## Store all variances & R2 values ----
# Foraging
r2_Vi.gfor = rpt.r2.solo.v.g$R_boot_link$id
r2_Vsite.gfor = rpt.r2.solo.v.g$R_boot_link$site
r2_Vfe.gfor = rpt.r2.solo.v.g$R_boot_link$Fixed
r2_VR.gfor = rpt.r2.solo.v.g$R_boot_link$Residual

Vi.gfor = rpt.V.solo.v.g$R_boot_link$id
Vsite.gfor = rpt.V.solo.v.g$R_boot_link$site
Vfe.gfor = rpt.V.solo.v.g$R_boot_link$Fixed
VR.gfor = rpt.V.solo.v.g$R_boot_link$Residual

# Group size
r2_Vi.gisze = rpt.r2.gsize$R_boot_link$id
r2_Vsite.gisze = rpt.r2.gsize$R_boot_link$site
r2_Vfe.gisze = rpt.r2.gsize$R_boot_link$Fixed
r2_VR.gisze = rpt.r2.gsize$R_boot_link$Residual

Vi.gisze = rpt.V.gsize$R_boot_link$id
Vsite.gisze = rpt.V.gsize$R_boot_link$site
Vfe.gisze = rpt.V.gsize$R_boot_link$Fixed
VR.gisze = rpt.V.gsize$R_boot_link$Residual

# Leadership
r2_Vi.lead = rpt.r2.lead$R_boot_link$id
r2_Vfe.lead = rpt.r2.lead$R_boot_link$Fixed
r2_VR.lead = rpt.r2.lead$R_boot_link$Residual

Vi.lead= rpt.V.lead$R_boot_link$id
Vfe.lead = rpt.V.lead$R_boot_link$Fixed
VR.lead = rpt.V.lead$R_boot_link$Residual

## Store values in dataframe ----
df.R2 = data.frame(
  R2 = c(r2_Vi.gfor, r2_Vsite.gfor, r2_Vfe.gfor, r2_VR.gfor,
         r2_Vi.gisze, r2_Vsite.gisze, r2_Vfe.gisze, r2_VR.gisze,
         r2_Vi.lead, r2_Vfe.lead, r2_VR.lead),
  trait = c(rep("Group foraging", 4000),
            rep("Group size", 4000),
            rep("Leadership", 3000)),
  type = c(rep(c(rep("Vi", 1000), 
             rep("Vsite", 1000),
             rep("Vfe", 1000),
             rep("VR", 1000)), 2),
           (c(rep("Vi", 1000),
                   rep("Vfe", 1000),
                   rep("VR", 1000))))) %>% 
  mutate(type = fct_relevel(type,"Vi", "Vsite", "Vfe", "VR"))

df.Var = data.frame(
  var = c(Vi.gfor, Vsite.gfor, Vfe.gfor, VR.gfor,
          Vi.gisze, Vsite.gisze, Vfe.gisze, VR.gisze,
          Vi.lead, Vfe.lead, VR.lead),
  trait = c(rep("Group foraging", 4000),
            rep("Group size", 4000),
            rep("Leadership", 3000)),
  type = c(rep(c(rep("Vi", 1000), 
                 rep("Vsite", 1000),
                 rep("Vfe", 1000),
                 rep("VR", 1000)), 2),
           (c(rep("Vi", 1000),
              rep("Vfe", 1000),
              rep("VR", 1000))))) %>% 
  mutate(type = fct_relevel(type,"Vi", "Vsite", "Vfe", "VR"))
    

## Plot ! ----
p1 = df.R2 %>% 
  group_by(type, trait) %>% 
  summarise(R2 = mean(R2)) %>% 
  ggplot(aes(y = R2, x = trait, fill = type)) +
  geom_bar(position = "fill", 
           stat = "identity", width = .2) +
  scale_fill_wsj(labels = c(
    bquote(R[i]^2), 
    bquote(R[site]^2), 
    bquote(R[fe]^2),
    bquote(R[R]^2))) +
  xlab("") +
  ylab("Variance explained (proportion)") +
  theme_bw(18) + 
  theme(legend.position = c(.9, .5),
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.25, "cm"))

p2 = df.Var %>% 
  filter(trait == "Group foraging") %>% 
  ggplot(aes(y = type, x = var, fill = type)) +
  stat_halfeye(normalize = "groups") + 
  scale_fill_wsj() +
  scale_y_discrete(labels = c(bquote(V[i]), 
                              bquote(V[site]), 
                              bquote(V[fe]),
                              bquote(V[R]))) +
  facet_wrap(~trait, ncol = 1, scales = "free") +
  xlab("Variance") +
  ylab("") +
  theme_bw(18) +
  theme(legend.position = "none")

p3 = df.Var %>% 
  filter(trait == "Group size") %>% 
  ggplot(aes(y = type, x = var, fill = type)) +
  stat_halfeye(normalize = "groups") + 
  scale_fill_wsj() +
  scale_y_discrete(labels = c(bquote(V[i]), 
                              bquote(V[site]), 
                              bquote(V[fe]),
                              bquote(V[R]))) +
  facet_wrap(~trait, ncol = 1, scales = "free") +
  xlab("Variance") +
  ylab("") +
  theme_bw(18) +
  theme(legend.position = "none")

fills = c("#c72e29", "#be9c2e", "#098154")
p4 = df.Var %>% 
  filter(trait == "Leadership") %>% 
  ggplot(aes(y = type, x = var, fill = type)) +
  stat_halfeye(normalize = "groups") + 
  scale_fill_manual(values = fills) +
  scale_y_discrete(labels = c(bquote(V[i]), 
                              bquote(V[fe]),
                              bquote(V[R]))) +
  facet_wrap(~trait, ncol = 1, scales = "free") +
  xlab("Variance") +
  ylab("") +
  theme_bw(18) +
  theme(legend.position = "none")


var.compo = p1 + (p2 / p3 / p4)
var.compo

saveRDS(var.compo, file = here("outputs/ggplot/var.compo.all.rds"))

ggsave(filename = here("outputs/figs/Fig3.var.compo.all.jpeg"), var.compo, width = 12, height = 8)
ggsave(filename = here("outputs/figs/Fig3.var.compo.all.pdf"), var.compo, width = 12, height = 8)


# Figure 4: Male and female variation in group leadership ----
## Import models ----
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

# Import rptR models
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

df.sex = data.frame(r2_Vi = c(r2_Vi_f, r2_Vi_m),
                    r2_Vfe = c(r2_Vfe_f, r2_Vfe_m),
                    r2_VR = c(r2_VR_f, r2_VR_m),
                    Sex = c(rep("F", length(r2_Vi_f)),
                            rep("M", length(r2_Vi_m)))) %>%
  pivot_longer(cols = r2_Vi:r2_VR,
               names_to = "v.compo",
               values_to = "var")

# Store effect sizes
df.sex.2  = data.frame(delta_r2_Vi = r2_Vi_m - r2_Vi_f,
                       delta_r2_Vfe = r2_Vfe_m - r2_Vfe_f,
                       delta_r2_VR = r2_VR_m - r2_VR_f)

### Maturity ---
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

df.mat = data.frame(r2_Vi = c(r2_Vi_j, r2_Vi_a),
                    r2_Vfe = c(r2_Vfe_j, r2_Vfe_a),
                    r2_VR = c(r2_VR_j, r2_VR_a),
                    Maturity = c(rep("Juveniles", length(r2_Vi_j)),
                                 rep("Adults", length(r2_Vi_a)))) %>%
  pivot_longer(cols = r2_Vi:r2_VR,
               names_to = "v.compo",
               values_to = "var")

# Store effect sizes
df.mat.2  = data.frame(delta_r2_Vi = r2_Vi_a - r2_Vi_j,
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

df.inj = data.frame(r2_Vi = c(r2_Vi_ni, r2_Vi_i),
                    r2_Vfe = c(r2_Vfe_ni, r2_Vfe_i),
                    r2_VR = c(r2_VR_ni, r2_VR_i),
                    Injury = c(rep("None", length(r2_Vi_ni)),
                               rep("Injured", length(r2_Vi_i)))) %>%
  pivot_longer(cols = r2_Vi:r2_VR,
               names_to = "v.compo",
               values_to = "var")

# Store effect sizes
df.inj.2  = data.frame(delta_r2_Vi = r2_Vi_i - r2_Vi_ni,
                       delta_r2_Vfe = r2_Vfe_i - r2_Vfe_ni,
                       delta_r2_VR = r2_VR_i - r2_VR_ni)
## Plot! ----
fills = c("#c72e29", "#be9c2e", "#098154")

### Sex ----
p1 = df.sex %>% 
  group_by(v.compo, Sex) %>% 
  summarise(var = mean(var)) %>% 
  ggplot(aes(y = var, x = Sex, fill = v.compo)) +
  geom_bar(position = "fill", 
           stat = "identity", width = .2) +
  scale_fill_manual(values = fills, 
                    labels = c(
                      bquote(R[fe]^2), 
                      bquote(R[i]^2),
                      bquote(R[R]^2))) +
  ylab("Variance explained (proportion)") +
  theme_bw(18) + 
  labs(fill = "") +
  theme(legend.position = c(.9, .5))

delta.vi.r = df.sex.2 %>% 
  ggplot(aes(x = delta_r2_Vi * 100)) +
  stat_halfeye(alpha = .6) + 
  geom_vline(xintercept = 0, 
             linetype = "dashed") +
  xlab(bquote(Delta[R[i]^2])) +
  ylab("Density") +
  theme_bw(18) +
  ggtitle("Difference in variance explained (%)")

delta.vfe.r = df.sex.2 %>% 
  ggplot(aes(x = delta_r2_Vfe * 100)) +
  stat_halfeye(alpha = .6) + 
  geom_vline(xintercept = 0, 
             linetype = "dashed") + 
  xlab(bquote(Delta[R[fe]^2])) +
  ylab("Density") +
  theme_bw(18)

delta.vr.r = df.sex.2 %>% 
  ggplot(aes(x = delta_r2_VR * 100)) +
  stat_halfeye(alpha = .6) + 
  geom_vline(xintercept = 0, 
             linetype = "dashed") + 
  xlab(bquote(Delta[R[R]^2])) +
  ylab("Density") +
  theme_bw(18)

delta.v.r = delta.vi.r / delta.vfe.r / delta.vr.r

delta.v.r.sex = p1 + (delta.vi.r / delta.vfe.r / delta.vr.r)
delta.v.r.sex

ggsave(filename = "outputs/figs/Fig4.delta.v.r.sex.jpeg", delta.v.r.sex, 
       width = 12, height = 8)
ggsave(filename = "outputs/figs/Fig4.delta.v.r.sex.pdf", delta.v.r.sex, 
       width = 12, height = 8)

### Maturity ----
p1 = df.mat %>%
  group_by(v.compo, Maturity) %>%
  summarise(var = mean(var)) %>%
  ggplot(aes(y = var, x = Maturity, fill = v.compo)) +
  geom_bar(position = "fill",
           stat = "identity", width = .2) +
  scale_fill_manual(values = fills, 
                    labels = c(
                      bquote(R[fe]^2), 
                      bquote(R[i]^2),
                      bquote(R[R]^2))) +
  ylab("Variance explained (proportion)") +
  theme_bw(18) +
  labs(fill = "") +
  theme(legend.position = c(.9, .5))

delta.vi.r = df.mat.2 %>%
  ggplot(aes(x = delta_r2_Vi * 100)) +
  stat_halfeye(alpha = .6) +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  xlab(bquote(Delta[R[i]^2])) +
  ylab("Density") +
  theme_bw(18) +
  ggtitle("Difference in variance explained (%)")

delta.vfe.r = df.mat.2 %>%
  ggplot(aes(x = delta_r2_Vfe * 100)) +
  stat_halfeye(alpha = .6) +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  xlab(bquote(Delta[R[fe]^2])) +
  ylab("Density") +
  theme_bw(18)

delta.vr.r = df.mat.2 %>%
  ggplot(aes(x = delta_r2_VR * 100)) +
  stat_halfeye(alpha = .6) +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  xlab(bquote(Delta[R[R]^2])) +
  ylab("Density") +
  theme_bw(18)

delta.v.r = delta.vi.r / delta.vfe.r / delta.vr.r

delta.v.r.mat = p1 + (delta.vi.r / delta.vfe.r / delta.vr.r)
delta.v.r.mat

ggsave(filename = "outputs/figs/delta.v.r.maturity.jpeg", delta.v.r.mat,
       width = 12, height = 8)
ggsave(filename = "outputs/figs/delta.v.r.maturity.pdf", delta.v.r.mat,
       width = 12, height = 8)

### Injury ----
p1 = df.inj %>%
  group_by(v.compo, Injury) %>%
  summarise(var = mean(var)) %>%
  ggplot(aes(y = var, x = Injury, fill = v.compo)) +
  geom_bar(position = "fill",
           stat = "identity", width = .2) +
  scale_fill_manual(values = fills, 
                    labels = c(
                      bquote(R[fe]^2), 
                      bquote(R[i]^2),
                      bquote(R[R]^2))) +
  ylab("Variance explained (proportion)") +
  theme_bw(18) +
  labs(fill = "") +
  theme(legend.position = c(.9, .5))

delta.vi.r = df.inj.2 %>%
  ggplot(aes(x = delta_r2_Vi * 100)) +
  stat_halfeye(alpha = .6) +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  xlab(bquote(Delta[R[i]^2])) +
  ylab("Density") +
  theme_bw(18) +
  ggtitle("Difference in variance explained (%)")

delta.vfe.r = df.inj.2 %>%
  ggplot(aes(x = delta_r2_Vfe * 100)) +
  stat_halfeye(alpha = .6) +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  xlab(bquote(Delta[R[fe]^2])) +
  ylab("Density") +
  theme_bw(18)

delta.vr.r = df.inj.2 %>%
  ggplot(aes(x = delta_r2_VR * 100)) +
  stat_halfeye(alpha = .6) +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  xlab(bquote(Delta[R[R]^2])) +
  ylab("Density") +
  theme_bw(18)

delta.v.r = delta.vi.r / delta.vfe.r / delta.vr.r

delta.v.r.inj = p1 + (delta.vi.r / delta.vfe.r / delta.vr.r)
delta.v.r.inj

ggsave(filename = "outputs/figs/delta.v.r.injury.jpeg", delta.v.r.inj,
       width = 12, height = 8)
ggsave(filename = "outputs/figs/delta.v.r.injury.pdf", delta.v.r.inj,
       width = 12, height = 8)

### Combine into 1 giant-*ss figure! ----

delta.v.r.all = delta.v.r.sex / delta.v.r.mat / delta.v.r.inj
delta.v.r.all

ggsave(filename = "outputs/figs/Fig4.delta.v.r.all.jpeg", delta.v.r.all,
       width = 30, height = 50, units = "cm")
ggsave(filename = "outputs/figs/Fig4.delta.v.r.all.pdf", delta.v.r.all,
       width = 30, height = 50, units = "cm")


