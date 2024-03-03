library(tidyverse); library(here); library(easystats)
library(kableExtra); library(lme4); library(effects)
library(marginaleffects);  library(ggeffects);  library(rptR)
library(gtsummary); library(ggthemes); library(patchwork)
library(tidybayes); library(AICcmodavg); library(gt)


# Individual differences in group leadership compared between sexes, age and injury ----
# Import all rptR models ----
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

### Male - female differences ----
#### Fit glmer models ----
glmm.lead.f = glmer(leader ~  maturity +  shark_bite +
                      anthropogenic + (1|id), 
                    family = "binomial",
                    subset(df.group, sex_f == "F"))
glmm.lead.m = glmer(leader ~ maturity +  shark_bite +
                      anthropogenic + (1|id),
                    family = "binomial",
                    subset(df.group, sex_f == "M"))

# Save model file
saveRDS(glmm.lead.f, file = here("outputs/mods/glmm.lead.f.rds"))
saveRDS(glmm.lead.m, file = here("outputs/mods/glmm.lead.m.rds"))

#### Fit rptR models ----

# Adjusted repeatability
rpt.R.f = rpt(leader ~ maturity +  shark_bite +
                anthropogenic + (1|id), 
              grname = "id", 
              datatype = "Binary", 
              parallel = T, 
              data = subset(df.group, sex_f == "F"))
rpt.R.m = rpt(leader ~ maturity +  shark_bite +
                anthropogenic + (1|id), 
              grname = "id", 
              datatype = "Binary", 
              parallel = T, 
              data = subset(df.group, sex_f == "M"))

saveRDS(rpt.R.f, here("outputs/mods/rpt.R.f.rds"))
saveRDS(rpt.R.m, here("outputs/mods/rpt.R.m.rds"))

# All variance components
rpt.V.f = rpt(leader ~ maturity +  shark_bite +
                anthropogenic + (1|id), 
              grname = c("id", "Fixed", "Residual"), 
              datatype = c("Binary"), 
              parallel = T,
              data = subset(df.group, sex_f == "F"),
              ratio = FALSE)
rpt.V.m = rpt(leader ~ maturity +  shark_bite +
                anthropogenic + (1|id), 
              grname = c("id", "Fixed", "Residual"), 
              datatype = "Binary", 
              parallel = T, 
              data = subset(df.group, sex_f == "M"),
              ratio = FALSE)
saveRDS(rpt.V.f, here("outputs/mods/rpt.V.f.rds"))
saveRDS(rpt.V.m, here("outputs/mods/rpt.V.m.rds"))

# All variance ratios
rpt.r2.f = rpt(leader ~ maturity +  shark_bite +
                 anthropogenic + (1|id), 
               grname = c("id", "Fixed", "Residual"), 
               datatype = c("Binary"), 
               parallel = T, 
               data = subset(df.group, sex_f == "F"),
               ratio = T)
rpt.r2.m = rpt(leader ~ maturity +  shark_bite +
                 anthropogenic + (1|id), 
               grname = c("id", "Fixed", "Residual"), 
               datatype = "Binary", 
               parallel = T, 
               data = subset(df.group, sex_f == "M"),
               ratio = T)
saveRDS(rpt.r2.f, here("outputs/mods/rpt.r2.f.rds"))
saveRDS(rpt.r2.m, here("outputs/mods/rpt.r2.m.rds"))

#### Plot the model estimates ----
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
p1 = df.sex %>% 
  group_by(v.compo, Sex) %>% 
  summarise(var = mean(var)) %>% 
  ggplot(aes(y = var, x = Sex, fill = v.compo)) +
  geom_bar(position = "fill", 
           stat = "identity", width = .2) +
  scale_fill_wsj(labels = c(
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

delta.v.r = p1 + (delta.vi.r / delta.vfe.r / delta.vr.r)
delta.v.r

ggsave(filename = "outputs/figs/delta.v.r.sex.jpeg", delta.v.r, 
       width = 12, height = 8)
ggsave(filename = "outputs/figs/delta.v.r.sex.pdf", delta.v.r, 
       width = 12, height = 8)

### Juvenile - adult differences ----
#### Fit glmer models ----
glmm.lead.j = glmer(leader ~ sex_f + shark_bite + anthropogenic + (1|id), 
                    family = "binomial",
                    subset(df.group, maturity == "1"))
glmm.lead.a = glmer(leader ~ sex_f + shark_bite + anthropogenic + (1|id), 
                    family = "binomial",
                    subset(df.group, maturity == "2"))

# Save model file
saveRDS(glmm.lead.j, file = here("outputs/mods/glmm.lead.j.rds"))
saveRDS(glmm.lead.a, file = here("outputs/mods/glmm.lead.a.rds"))

#### Fit rptR models ----

# Adjusted repeatability
rpt.R.j = rpt(leader ~ sex_f + shark_bite + anthropogenic + (1|id),
              grname = "id", 
              datatype = "Binary", 
              parallel = T,
              data = subset(df.group, maturity == "1"))
rpt.R.a = rpt(leader ~ sex_f + shark_bite + anthropogenic + (1|id), 
              grname = "id", 
              datatype = "Binary", 
              parallel = T, 
              data = subset(df.group, maturity == "2"))

saveRDS(rpt.R.j, here("outputs/mods/rpt.R.j.rds"))
saveRDS(rpt.R.a, here("outputs/mods/rpt.R.a.rds"))

# All variance components
rpt.V.j = rpt(leader ~ sex_f + shark_bite + anthropogenic + (1|id), 
              grname = c("id", "Fixed", "Residual"), 
              datatype = c("Binary"), 
              parallel = T, 
              data = subset(df.group, maturity == "1"),
              ratio = FALSE)
rpt.V.a = rpt(leader ~ sex_f + shark_bite + anthropogenic + (1|id),  
              grname = c("id", "Fixed", "Residual"), 
              datatype = "Binary", 
              parallel = T, 
              data = subset(df.group, maturity == "2"),
              ratio = FALSE)
saveRDS(rpt.V.j, here("outputs/mods/rpt.V.j.rds"))
saveRDS(rpt.V.a, here("outputs/mods/rpt.V.a.rds"))

# All variance ratios
rpt.r2.j = rpt(leader ~ sex_f + shark_bite + anthropogenic + (1|id),   
               grname = c("id", "Fixed", "Residual"), 
               datatype = c("Binary"), 
               parallel = T, 
               data = subset(df.group, maturity == "1"),
               ratio = T)
rpt.r2.a = rpt(leader ~ sex_f + shark_bite + anthropogenic + (1|id),  
               grname = c("id", "Fixed", "Residual"), 
               datatype = "Binary", 
               data = subset(df.group, maturity == "2"),
               ratio = T)
saveRDS(rpt.r2.j, here("outputs/mods/rpt.r2.j.rds"))
saveRDS(rpt.r2.a, here("outputs/mods/rpt.r2.a.rds"))

#### Plot the model estimates ----
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
p1 = df.mat %>% 
  group_by(v.compo, Maturity) %>% 
  summarise(var = mean(var)) %>% 
  ggplot(aes(y = var, x = Maturity, fill = v.compo)) +
  geom_bar(position = "fill", 
           stat = "identity", width = .2) +
  scale_fill_wsj(labels = c(
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

delta.v.r = p1 + (delta.vi.r / delta.vfe.r / delta.vr.r)
delta.v.r

ggsave(filename = "outputs/figs/delta.v.r.maturity.jpeg", delta.v.r, 
       width = 12, height = 8)
ggsave(filename = "outputs/figs/delta.v.r.maturity.pdf", delta.v.r, 
       width = 12, height = 8)

### Injured - non-injured differences ----
#### Fit glmer models ----
glmm.lead.ni = glmer(leader ~ sex_f + maturity + (1|id), 
                     family = "binomial",
                     subset(df.group, injury == "0"))
glmm.lead.i = glmer(leader ~ sex_f + maturity + (1|id), 
                    family = "binomial",
                    subset(df.group, injury == "1"))

# Save model file
saveRDS(glmm.lead.ni, file = here("outputs/mods/glmm.lead.ni.rds"))
saveRDS(glmm.lead.i, file = here("outputs/mods/glmm.lead.i.rds"))

#### Fit rptR models ----

# Adjusted repeatability
rpt.R.ni = rpt(leader ~ sex_f + maturity + (1|id), 
               grname = "id", 
               datatype = "Binary", 
               parallel = T,
               data = subset(df.group, injury == "0"))
rpt.R.i = rpt(leader ~ sex_f + maturity + (1|id), 
              grname = "id", 
              datatype = "Binary", 
              parallel = T, 
              data = subset(df.group, injury == "1"))

saveRDS(rpt.R.ni, here("outputs/mods/rpt.R.ni.rds"))
saveRDS(rpt.R.i, here("outputs/mods/rpt.R.i.rds"))

# All variance components
rpt.V.ni = rpt(leader ~ sex_f + maturity + (1|id), 
               grname = c("id", "Fixed", "Residual"), 
               datatype = c("Binary"), 
               parallel = T, 
               data = subset(df.group, injury == "0"),
               ratio = FALSE)
rpt.V.i = rpt(leader ~ sex_f + maturity + (1|id), 
              grname = c("id", "Fixed", "Residual"), 
              datatype = "Binary", 
              parallel = T, 
              data = subset(df.group, injury == "1"),
              ratio = FALSE)
saveRDS(rpt.V.ni, here("outputs/mods/rpt.V.ni.rds"))
saveRDS(rpt.V.i, here("outputs/mods/rpt.V.i.rds"))


# All variance ratios
rpt.r2.ni = rpt(leader ~ sex_f + maturity + (1|id), 
                grname = c("id", "Fixed", "Residual"), 
                datatype = c("Binary"), 
                parallel = T, 
                data = subset(df.group, injury == "0"),
                ratio = T)
rpt.r2.i = rpt(leader ~ sex_f + maturity + (1|id), 
               grname = c("id", "Fixed", "Residual"), 
               datatype = "Binary", 
               data = subset(df.group, injury == "1"),
               ratio = T)
saveRDS(rpt.r2.ni, here("outputs/mods/rpt.r2.ni.rds"))
saveRDS(rpt.r2.i, here("outputs/mods/rpt.r2.i.rds"))

#### Plot the model estimates ----
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
p1 = df.inj %>% 
  group_by(v.compo, Injury) %>% 
  summarise(var = mean(var)) %>% 
  ggplot(aes(y = var, x = Injury, fill = v.compo)) +
  geom_bar(position = "fill", 
           stat = "identity", width = .2) +
  scale_fill_wsj(labels = c(
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

delta.v.r = p1 + (delta.vi.r / delta.vfe.r / delta.vr.r)
delta.v.r

ggsave(filename = "outputs/figs/delta.v.r.injury.jpeg", delta.v.r, 
       width = 12, height = 8)
ggsave(filename = "outputs/figs/delta.v.r.injury.pdf", delta.v.r, 
       width = 12, height = 8)

### Are these differences statistically significant? ----
tbl_delta_r2_sex = describe_posterior(df.sex.2*100) %>% 
  select(c(1:2, 4 :6)) %>% 
  gt() %>%
  fmt_number(decimals = 2) %>% 
  cols_label(Parameter = "∆V") %>% 
  text_case_match(
    "delta_r2_Vi" ~ "∆Vi",
    "delta_r2_Vfe" ~ "∆Vfe",
    "delta_r2_VR" ~ "∆VR")

tbl_delta_r2_maturity = describe_posterior(df.mat.2*100) %>% 
  select(c(1:2, 4 :6)) %>% 
  gt() %>%
  fmt_number(decimals = 2) %>% 
  cols_label(Parameter = "∆V") %>% 
  text_case_match(
    "delta_r2_Vi" ~ "∆Vi",
    "delta_r2_Vfe" ~ "∆Vfe",
    "delta_r2_VR" ~ "∆VR")

tbl_delta_r2_injury = describe_posterior(df.inj.2*100) %>% 
  select(c(1:2, 4 :6)) %>% 
  gt() %>%
  fmt_number(decimals = 2) %>% 
  cols_label(Parameter = "∆V") %>% 
  text_case_match(
    "delta_r2_Vi" ~ "∆Vi",
    "delta_r2_Vfe" ~ "∆Vfe",
    "delta_r2_VR" ~ "∆VR")

tbl_delta_r2_sex; tbl_delta_r2_maturity; tbl_delta_r2_injury

#### Export tables ----
tbl_delta_r2_sex %>% 
  gtsave(filename = here("outputs/tables/tbl_delta_r2_sex.html"))
tbl_delta_r2_sex %>% 
  gtsave(filename = here("outputs/tables/tbl_delta_r2_sex.docx"))

tbl_delta_r2_maturity %>% 
  gtsave(filename = here("outputs/tables/tbl_delta_r2_maturity.html"))
tbl_delta_r2_maturity %>% 
  gtsave(filename = here("outputs/tables/tbl_delta_r2_maturity.docx"))

tbl_delta_r2_injury %>% 
  gtsave(filename = here("outputs/tables/tbl_delta_r2_injury.html"))
tbl_delta_r2_injury %>% 
  gtsave(filename = here("outputs/tables/tbl_delta_r2_injury.docx"))
