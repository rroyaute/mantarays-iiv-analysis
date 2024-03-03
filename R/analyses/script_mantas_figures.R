library(tidyverse); library(here)
library(ggthemes); library(patchwork)
library(tidybayes); library(rptR)
library(marginaleffects)

# Marginal effects plots ----
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




### Combine plots ----
fig.svg = p1.svg + ggtitle("Solo vs. group foraging") + p4.svg + p5.svg
fig.svg

fig.gsize = p1.gsize + ggtitle("Group size") + p2.gsize + p3.gsize + p5.gsize
fig.gsize

fig.lead = p1.lead + ggtitle("Group leadership")

fig.marg.effects = fig.svg / fig.gsize / fig.lead
fig.marg.effects

ggsave(filename = here("outputs/figs/fig.marg.effects.jpeg"), fig.marg.effects,
       width = 10, height = 10)
ggsave(filename = here("outputs/figs/fig.marg.effects.pdf"), fig.marg.effects,
       width = 10, height = 10)


## Add datapoints ----
### Import data ----
df.tot = read.csv(here("data/data-raw/Manta Data_Annie.csv"), 
                  header=TRUE, sep=",", na.strings="NA", dec=".",
                  strip.white=TRUE)

# Transform Number of mantas columns centered mean
df.tot$no_mantas_sc = as.numeric(scale(df.tot$no_mantas))
# Transform Number of mantas columns centered mean
df.tot$time_ht_sc = as.numeric(scale(df.tot$time_ht))

df.group = read.csv(here("data/data-raw/group_rp.csv"), 
                    header=TRUE, sep=",", na.strings="NA", dec=".",
                    strip.white=TRUE)

# Transform Number of mantas columns scaled to sd units
df.group$no_mantas_sc = as.numeric(scale(df.group$no_mantas))

# Transform time to high scaled to sd units
df.group$time_ht_sc = as.numeric(scale(df.group$time_ht))

# Transform current into numeric variable
df.group$current = as.numeric(df.group$current.VU.AO)


### Overlay points ----
p1 + 
  stat_slab(data = df.tot,
            aes(x = no_mantas_sc, y = group, 
                color = as.factor(group), 
                fill = as.factor(group), 
                side = ifelse(group == 0, "top", "bottom")),
            slab_type = "histogram", alpha = .8,
            scale = 0.4, breaks = 40, size = 1/2) +
  scale_fill_wsj() +
  scale_color_wsj() + 
  xlab("Number of mantas foraging (sd units)") +
  ylab("Group leadership") +
  theme_bw(14)


# Variance components and R2 compared among traits -----
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

ggsave(filename = here("outputs/figs/var.compo.all.jpeg"), var.compo, width = 12, height = 8)
ggsave(filename = here("outputs/figs/var.compo.all.pdf"), var.compo, width = 12, height = 8)
