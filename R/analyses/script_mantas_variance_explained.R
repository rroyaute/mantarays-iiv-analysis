library(tidyverse); library(here)
library(ggthemes); library(patchwork)
library(tidybayes)

# Import rptR objects ----
# Import foraging strategy variance
rpt.V.solo.v.g = read_rds(file = here("outputs/mods/rpt.V.solo.v.g.rds"))
rpt.r2.solo.v.g = read_rds(file = here("outputs/mods/rpt.r2.solo.v.g.rds"))

# Import group size variance
rpt.V.gsize = read_rds(file = here("outputs/mods/rpt.V.gsize.rds"))
rpt.r2.gsize = read_rds(file = here("outputs/mods/rpt.r2.gsize.rds"))

# Import group leadership variance
rpt.r2.lead = read_rds(file = here("outputs/mods/rpt.r2.lead.rds"))
rpt.V.lead = read_rds(file = here("outputs/mods/rpt.V.lead.rds"))

# Store all variances & R2 values ----
# Foraging
r2_Vi.gfor = rpt.r2.solo.v.g$R_boot_link$id
r2_Vfe.gfor = rpt.r2.solo.v.g$R_boot_link$Fixed
r2_VR.gfor = rpt.r2.solo.v.g$R_boot_link$Residual

Vi.gfor = rpt.V.solo.v.g$R_boot_link$id
Vfe.gfor = rpt.V.solo.v.g$R_boot_link$Fixed
VR.gfor = rpt.V.solo.v.g$R_boot_link$Residual

# Group size
r2_Vi.gisze = rpt.r2.gsize$R_boot_link$id
r2_Vfe.gisze = rpt.r2.gsize$R_boot_link$Fixed
r2_VR.gisze = rpt.r2.gsize$R_boot_link$Residual

Vi.gisze = rpt.V.gsize$R_boot_link$id
Vfe.gisze = rpt.V.gsize$R_boot_link$Fixed
VR.gisze = rpt.V.gsize$R_boot_link$Residual

# Leadership
r2_Vi.lead = rpt.r2.lead$R_boot_link$id
r2_Vfe.lead = rpt.r2.lead$R_boot_link$Fixed
r2_VR.lead = rpt.r2.lead$R_boot_link$Residual

Vi.lead= rpt.V.lead$R_boot_link$id
Vfe.lead = rpt.V.lead$R_boot_link$Fixed
VR.lead = rpt.V.lead$R_boot_link$Residual

# Store in dataframe
df.R2 = data.frame(
  R2 = c(r2_Vi.gfor, r2_Vfe.gfor, r2_VR.gfor,
         r2_Vi.lead, r2_Vfe.lead, r2_VR.lead),
  trait = c(rep("Group foraging", 3000),
            rep("Group size", 3000),
            rep("Leadership", 3000)),
  type = c(rep(rep("Vi", 1000),
           rep("Vfe", 1000),
           rep("VR", 1000), 3)))

df.Var = data.frame(
  var = c(Vi.gfor, Vfe.gfor, VR.gfor,
         Vi.lead, Vfe.lead, VR.lead),
  trait = c(rep("Group foraging", 3000),
            rep("Group size", 3000),
            rep("Leadership", 3000)),
  type = c(rep(rep("Vi", 1000),
               rep("Vfe", 1000),
               rep("VR", 1000), 3)))


p1 = df.R2 %>% 
  group_by(v.compo) %>% 
  summarise(R2 = mean(R2)) %>% 
  ggplot(aes(y = R2, x = trait, fill = v.compo)) +
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
        axis.ticks.x = element_blank())

p2 = df.Var %>% 
  ggplot(aes(y = type, x = var, fill = type)) +
  stat_halfeye() + 
  scale_fill_wsj() +
  facet_wrap(~trait, ncol = 1) +
  scale_y_discrete(labels = c(
    bquote(V[fe]), 
    bquote(V[i]),
    bquote(V[R]))) +
  xlab("Variance") +
  ylab("Variance component") +
  theme_bw(18) +
  theme(legend.position = "none") +
  ggtitle()
