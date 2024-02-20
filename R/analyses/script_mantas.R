library(tidyverse); library(here); library(easystats)
library(lme4); library(ordinal); library(effects); library(rptR)
library(brms); library(gtsummary); library(tidybayes)
library(ggthemes); library(patchwork)

# 0. Data Import and cleaning ####
Data<- read.csv(here("data/data-clean/group_rp.csv"), 
                header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
str(Data)
Data$id=as.factor(Data$id)
Data$site=as.factor(Data$site)
Data$sex_f=as.factor(ifelse(Data$sex=="1","F","M"))
Data$size=as.factor(Data$size)
Data$maturity=as.factor(Data$maturity)
Data$plankton=as.factor(Data$plankton)
Data$shark_bite=as.factor(Data$shark_bite)
Data$anthropogenic=as.factor(Data$anthropogenic)
Data$Id = Data$id

# Transform Time columns centered aroun 12:00pm expressed in hours
Data$time_cen=hm(Data$time)
Data$time_cen=as.numeric(Data$time_cen-hours(12))/3600

# subset with only individuals within groups
Data=Data[Data$group==1,]
Data=Data[complete.cases(Data$position),]
str(Data)

# Take only data where all individuals within groups are known - TODO
Data2=Data[Data$group_size==Data$group_size,]
Data$no_mantas_sc = as.numeric(scale(Data$no_mantas))

# 1. Position in group analysis ----
## 1.1 Frequentist models ----
# Position in group
GLMM.pos=glmer(position~sex_f+size+maturity+
                 plankton+shark_bite+anthropogenic+no_mantas_sc+
                 (1|Id) + (1|site), family = "poisson",
               Data)
summary(GLMM.pos)
plot(allEffects(GLMM.pos))

CLMM.pos=clmm(ordered(position)~sex_f+size+maturity+
                plankton+shark_bite+anthropogenic+no_mantas_sc+
                (1|Id) + (1|site),
              Data)
summary(CLMM.pos)

GLMM.pos.bin=glmer(cbind(position, group_size)~sex_f+size+maturity+
                     plankton+shark_bite+anthropogenic+no_mantas_sc+
                     (1|Id) + (1|site), family = "binomial",
                   Data)
summary(GLMM.pos.bin)
plot(allEffects(GLMM.pos.bin))

check_model(GLMM.pos)
check_model(GLMM.pos.bin)
check_model(CLMM.pos)

tbl_regression(GLMM.pos)
tbl_regression(GLMM.pos.bin)
r2_nakagawa(GLMM.pos)
icc(GLMM.pos)
get_variance(GLMM.pos)
variance_decomposition(brms.clmm)

rpt.position=rpt(position~sex_f+size+maturity+
                   plankton+shark_bite+anthropogenic+no_mantas_sc+
                   (1|Id) + (1|site),
                 grname = c("Id", "site","Fixed","Residual"), 
                 datatype = "Poisson",
                 Data, ratio = F, adjusted = F)
rpt.position
plot(rpt.position)

## 1.2 Bayesian models ----
### 1.2.1 Prior predictive checks ----

bf = bf(position~sex_f+size+maturity+
          plankton+shark_bite+anthropogenic+no_mantas_sc+
          (1|Id) + (1|site))

# Individual predictors only
bf = bf(position~sex_f+size+maturity+
          shark_bite+anthropogenic+
          (1|Id))
bf.clmm = bf(position~sex_f+size+maturity+
               shark_bite+anthropogenic+
               (1|Id),
             family = "cumulative")

bf.bin = bf(position | trials(group_size) ~sex_f+size+maturity+
               shark_bite+anthropogenic+
               (1|Id),
             family = "binomial")


get_prior(bf, Data)
get_prior(bf.clmm, Data)
get_prior(bf.bin, Data)

priors.clmm <- 
  # Intercepts priors
  prior(normal(0, 1), class = Intercept) +
  # Regression priors
  prior(normal(0, .05), class = b) +
  # Random effects priors (default to exp(1))
  prior(exponential(1), class = sd)
# Residual prior
# prior(exponential(1), class = sigma) 

priors.clmm %>% 
  parse_dist() %>% 
  filter(class == "b") %>% 
  ggplot(aes(xdist = .dist_obj, y = format(.dist_obj))) +
  stat_dist_halfeye() +
  facet_wrap(~nlpar, scales = "free") +
  ggtitle("Regression coefficients") +
  xlab("Value") + ylab("Density") +
  theme_bw(12) +
  theme(axis.text.y = element_text(angle = 90)) 

brms.clmm.prior = brm(bf.clmm, 
                      data = Data,
                      prior = priors.clmm,
                      sample_prior = "only",
                      warmup = 1000,
                      iter = 2000,
                      seed = 42, 
                      cores = 8,
                      threads = threading(8),
                      control = list(adapt_delta = .99,
                                     max_treedepth = 15),
                      backend = "cmdstanr")
pp_check(brms.clmm.prior, ndraws = 500)

priors.poiss <- 
  # Intercepts priors
  prior(normal(1.6, .5), class = Intercept, lb = 0) +
  # Regression priors
  prior(normal(0, .05), class = b) +
  # Random effects priors (default to exp(1))
  prior(exponential(10), class = sd)

prior(exponential(10)) %>% parse_dist() %>% 
  ggplot(aes(xdist = .dist_obj, y = format(.dist_obj))) +
  stat_dist_halfeye() 

brms.poiss.prior = brm(bf, 
                       data = Data,
                       prior = priors.poiss,
                       family = "poisson",
                       sample_prior = "only",
                       warmup = 1000,
                       iter = 2000,
                       seed = 42, 
                       cores = 8, 
                       threads = threading(8),
                       control = list(adapt_delta = .99,
                                      max_treedepth = 15),
                       backend = "cmdstanr")
pp_check(brms.poiss.prior, ndraws = 500)

priors.bin <- 
  # Intercepts priors
  prior(normal(0, 1.5), class = Intercept) +
  # Regression priors
  prior(normal(0, .05), class = b) +
  # Random effects priors (default to exp(1))
  prior(exponential(10), class = sd)

brms.bin.prior = brm(bf.bin,
                     data = Data,
                     prior = priors.bin,
                     family = "binomial",
                     sample_prior = "only",
                     warmup = 1000,
                     iter = 2000,
                     seed = 42, 
                     cores = 8, 
                     threads = threading(8),
                     control = list(adapt_delta = .99,
                                    max_treedepth = 15),
                     backend = "cmdstanr")
pp_check(brms.bin.prior, ndraws = 500)


### 1.2.2 Fit models to data ----
brms.clmm = brm(bf.clmm, 
                data = Data,
                family = "cumulative",
                prior = priors.clmm,
                warmup = 3000,
                iter = 4000,
                seed = 42, 
                cores = 8,
                threads = threading(8),
                control = list(adapt_delta = .99,
                               max_treedepth = 15),
                backend = "cmdstanr")
pp_check(brms.clmm, ndraws = 500)
# plot(conditional_effects(brms.clmm, categorical = T))

brms.poiss = brm(bf, 
                 data = Data,
                 family = "poisson",
                 prior = priors.poiss,
                 warmup = 3000,
                 iter = 4000,
                 seed = 42, 
                 cores = 8, 
                 threads = threading(8),
                 control = list(adapt_delta = .99,
                                max_treedepth = 15),
                 backend = "cmdstanr")
pp_check(brms.poiss, ndraws = 500)
# plot(conditional_effects(brms.poiss, spaghetti = T, ndraws = 500))

brms.bin = brm(bf.bin,
               data = Data,
               prior = priors.bin,
               family = "binomial",
               warmup = 3000,
               iter = 4000,
               seed = 42, 
               cores = 8, 
               threads = threading(8),
               control = list(adapt_delta = .99,
                              max_treedepth = 15),
               backend = "cmdstanr")
pp_check(brms.bin, ndraws = 500)
# plot(conditional_effects(brms.bin, spaghetti = T, ndraws = 500))

### 1.2.3 Investigate variance explained ----
variance_decomposition(brms.clmm, robust = T,
                       re_formula = ~ (1 | Id))
variance_decomposition(brms.poiss, robust = T, 
                       re_formula = ~ (1 | Id))
variance_decomposition(brms.bin, robust = T, 
                       re_formula = ~ (1 | Id))



### 1.2.4 Summary plots ----
plot.clmm = pp_check(brms.clmm, ndraws = 500) +
  xlim(0, 15) +
  ggtitle("Cumulative Link Mixed Model")

plot.bin = pp_check(brms.bin, ndraws = 500) +
  xlim(0, 15) +
  ggtitle("Binomial GLMM") 
  
plot.poiss = pp_check(brms.poiss, ndraws = 500) +
  xlim(0, 15) +
  ggtitle("Poisson GLMM") 

plot.ppcheck = (plot.clmm / plot.bin / plot.poiss) +
  plot_annotation("Posterior-predictive checks") &
  theme_bw(14)
  
plot.ppcheck

ggsave(filename = "outputs/quarto/img/plot.ppcheck.jpeg", plot.ppcheck)

waic(brms.clmm, brms.bin, brms.poiss)

Vi.clmm = brms.clmm %>% 
  spread_draws(sd_Id__Intercept)

Vi.bin = brms.bin %>% 
  spread_draws(sd_Id__Intercept)

Vi.poiss = brms.poiss %>% 
  spread_draws(sd_Id__Intercept)

df.Vi = data.frame(
  Vi = c(Vi.clmm$sd_Id__Intercept, 
         Vi.bin$sd_Id__Intercept,
         Vi.poiss$sd_Id__Intercept),
  Mod = factor(c(rep("CLMM", length(Vi.clmm$sd_Id__Intercept)),
                    rep("Binomial", length(Vi.bin$sd_Id__Intercept)),
                    rep("Poisson", length(Vi.poiss$sd_Id__Intercept))),
                  levels = c("CLMM", "Binomial", "Poisson")))

plot.Vi = df.Vi %>% 
  ggplot(aes(x = Vi, y = Mod, fill = Mod)) +
  stat_halfeye() + 
  scale_fill_wsj() +
  xlab("Among-individual variance") +
  ylab("Model") +
  theme_bw(14) +
  theme(legend.position = "none")
plot.Vi

ggsave(filename = "outputs/quarto/img/plot.Vi.jpeg", plot.Vi)

## 1.3 Compare variance by groups ----
### 1.3.1 Sexes ----
#### 1.3.1.1 Binomial glmm ----
bf.bin.s = bf(position | trials(group_size) ~ 
                size + maturity + plankton + 
                shark_bite + anthropogenic + no_mantas_sc +
                (1 | Id) + (1 | site) )
brms.bin.f = brm(bf.bin.s,
                 data = subset(Data, sex_f == "F"),
                 prior = priors.bin,
                 family = "binomial",
                 warmup = 3000,
                 iter = 4000,
                 seed = 42, 
                 cores = 8, 
                 threads = threading(8),
                 control = list(adapt_delta = .99,
                                max_treedepth = 15),
                 backend = "cmdstanr")
pp_check(brms.bin.f, ndraws = 500)

brms.bin.m = brm(bf.bin.s,
                 data = subset(Data, sex_f == "M"),
                 prior = priors.bin,
                 family = "binomial",
                 warmup = 3000,
                 iter = 4000,
                 seed = 42, 
                 cores = 8, 
                 threads = threading(8),
                 control = list(adapt_delta = .99,
                                max_treedepth = 15),
                 backend = "cmdstanr")
pp_check(brms.bin.m, ndraws = 500)

get_variance(brms.bin.f, robust = T, re_formula = ~ (1 | Id))$var.intercept
get_variance(brms.bin.m, robust = T, re_formula = ~ (1 | Id))$var.intercept

Vi.f = brms.bin.f %>% 
  spread_draws(sd_Id__Intercept)
Vi.m = brms.bin.m %>% 
  spread_draws(sd_Id__Intercept)
var.dat = data.frame(
  Vi = c(Vi.f$sd_Id__Intercept, Vi.m$sd_Id__Intercept),
  Sex = as.factor(c(rep("F", length(Vi.f$sd_Id__Intercept)),
                    rep("M", length(Vi.m$sd_Id__Intercept)))))
delta = data.frame(delta_Vi = Vi.f$sd_Id__Intercept - Vi.m$sd_Id__Intercept)

var.dat %>% 
  ggplot(aes(x = Vi, y = Sex, fill = Sex)) +
  stat_halfeye() + 
  scale_fill_wsj() +
  theme_bw()
delta %>% 
  ggplot(aes(x = delta_Vi)) +
  stat_halfeye() + 
  scale_fill_wsj() +
  theme_bw()

#### 1.3.1.2 CLMM ----
bf.clmm.s = bf(position ~ 
                 size + maturity + plankton + 
                 shark_bite + anthropogenic + no_mantas_sc +
                 (1 | Id) + (1 | site) )
brms.clmm.f = brm(bf.clmm.s, 
                  data = subset(Data, sex_f == "F"),
                  family = "cumulative",
                  prior = priors.clmm,
                  warmup = 3000,
                  iter = 4000,
                  seed = 42, 
                  cores = 8,
                  threads = threading(8),
                  control = list(adapt_delta = .99,
                                 max_treedepth = 15),
                  backend = "cmdstanr")
pp_check(brms.clmm.f, ndraws = 500)

brms.clmm.m = brm(bf.clmm.s, 
                  data = subset(Data, sex_f == "M"),
                  family = "cumulative",
                  prior = priors.clmm,
                  warmup = 3000,
                  iter = 4000,
                  seed = 42, 
                  cores = 8,
                  threads = threading(8),
                  control = list(adapt_delta = .99,
                                 max_treedepth = 15),
                  backend = "cmdstanr")
pp_check(brms.clmm.m, ndraws = 500)

get_variables(brms.clmm.f)

Vi.f = brms.clmm.f %>% 
  spread_draws(sd_Id__Intercept)
Vi.m = brms.clmm.m %>% 
  spread_draws(sd_Id__Intercept)
var.dat = data.frame(
  Vi = c(Vi.f$sd_Id__Intercept, Vi.m$sd_Id__Intercept),
  Sex = as.factor(c(rep("F", length(Vi.f$sd_Id__Intercept)),
                    rep("M", length(Vi.m$sd_Id__Intercept)))))
delta = data.frame(delta_Vi = Vi.f$sd_Id__Intercept - Vi.m$sd_Id__Intercept)

var.dat %>% 
  ggplot(aes(x = Vi, y = Sex, fill = Sex)) +
  stat_halfeye() + 
  scale_fill_wsj() +
  theme_bw()

delta %>% 
  ggplot(aes(x = delta_Vi)) +
  stat_halfeye() + 
  scale_fill_wsj() +
  theme_bw()
describe_posterior(delta$delta_Vi)




# 2. Leading/following the group ----
GLMM.lead=glmer(leader~sex_f+size+maturity+shark_bite+anthropogenic+
                  (1|Id), 
                family = "binomial",
                   Data)
summary(GLMM.lead)
plot(allEffects(GLMM.lead))

check_model(GLMM.lead)

r2_nakagawa(GLMM.lead, ci = T)
icc(GLMM.lead, ci = T)


## 2.1 Compare variance by groups ----
### 2.1.1 Sexes ----
# GLMM.lead.f=glmer(leader~size+maturity+shark_bite+anthropogenic+
#                     (1|id), 
#                   family = "binomial",
#                   subset(Data, sex_f == "F"))
# GLMM.lead.m=glmer(leader~size+maturity+shark_bite+anthropogenic+
#                     (1|id), 
#                   family = "binomial",
#                   subset(Data, sex_f == "M"))
# summary(GLMM.lead.f); summary(GLMM.lead.m)
# plot(allEffects(GLMM.lead.f)); plot(allEffects(GLMM.lead.m))
 
# Converting size to numeric to avoid convergence issues
Data$size_n = as.numeric(Data$size)

GLMM.lead.f=glmer(leader~size_n+maturity+shark_bite+anthropogenic+
                  (1|id), 
                family = "binomial",
                subset(Data, sex_f == "F"))
GLMM.lead.m=glmer(leader~size_n+maturity+shark_bite+anthropogenic+
                    (1|id), 
                  family = "binomial",
                  subset(Data, sex_f == "M"))

summary(GLMM.lead.f); summary(GLMM.lead.m)
plot(allEffects(GLMM.lead.f)); plot(allEffects(GLMM.lead.m))


rpt.R.f = rpt(formula = leader~size_n+maturity+shark_bite+anthropogenic+
                (1|id),  
              grname = "id", 
              datatype = "Binary", 
              data = subset(Data, sex_f == "F"))
rpt.R.m = rpt(formula = leader~size_n+maturity+shark_bite+anthropogenic+
                (1|id),  
              grname = "id", 
              datatype = "Binary", 
              data = subset(Data, sex_f == "M"))

saveRDS(rpt.R.f, here("outputs/mods/rpt.R.f.bin.rds"))
saveRDS(rpt.R.m, here("outputs/mods/rpt.R.m.bin.rds"))

# All variance components
rpt.V.f <- rpt(formula = leader~size_n+maturity+shark_bite+anthropogenic+
                 (1|id),  
               grname = c("id", "Fixed", "Residual"), 
               datatype = c("Binary"), 
               data = subset(Data, sex_f == "F"),
               ratio = FALSE)
rpt.V.m <- rpt(formula = leader~size_n+maturity+shark_bite+anthropogenic+
                 (1|id),  
               grname = c("id", "Fixed", "Residual"), 
               datatype = "Binary", 
               data = subset(Data, sex_f == "M"),
               ratio = FALSE)

saveRDS(rpt.V.f, here("outputs/mods/rpt.V.f.bin.rds"))
saveRDS(rpt.V.m, here("outputs/mods/rpt.V.m.bin.rds"))

# Store all vectors of bootstrapped values
# Load models
rpt.R.f = read_rds(here("outputs/mods/rpt.R.f.bin.rds"))
rpt.R.m = read_rds(here("outputs/mods/rpt.R.m.bin.rds"))
rpt.V.f = read_rds(here("outputs/mods/rpt.V.f.bin.rds"))
rpt.V.m = read_rds(here("outputs/mods/rpt.V.m.bin.rds"))

plot(rpt.R.f)
plot(rpt.R.m)
plot(rpt.V.f)
plot(rpt.V.m)


Vi_f <- rpt.V.f$R_boot_link$id
Vi_m <- rpt.V.m$R_boot_link$id
Vfe_f <- rpt.V.f$R_boot_link$Fixed
Vfe_m <- rpt.V.m$R_boot_link$Fixed
VR_f <- rpt.V.f$R_boot_link$Residual
VR_m <- rpt.V.m$R_boot_link$Residual
R_f <- rpt.R.f$R_boot_link$id
R_m <- rpt.R.m$R_boot_link$id

df <- data.frame(Vi = c(Vi_f, Vi_m),
                 Vfe = c(Vfe_f, Vfe_m),
                 VR = c(VR_f, VR_m),
                 R = c(R_f, R_m),
                 Sex = c(rep("F", length(Vi_f)),
                         rep("M", length(Vi_m))))

# Store effect sizes
df.2  <- data.frame(delta_Vi = Vi_f - Vi_m,
                    delta_Vfe = Vfe_f - Vfe_m,
                    delta_VR = VR_f - VR_m,
                    delta_R = R_f - R_m)

p1 = df %>% 
  ggplot(aes(x = Vi, fill = Sex)) +
  stat_halfeye(alpha = .6) + 
  scale_fill_wsj() +
  xlab(bquote("Among-individual variance ("*V[i]*")")) +
  ylab("Density") +
  theme_bw(14)
delta.p1 = df.2 %>% 
  ggplot(aes(x = delta_Vi)) +
  stat_halfeye(alpha = .6) + 
  xlab(bquote(Delta[V[i]])) +
  ylab("Density") +
  theme_bw(14)
p1 = p1 + delta.p1

p2 = df %>% 
  ggplot(aes(x = Vfe, fill = Sex)) +
  stat_halfeye(alpha = .6) + 
  scale_fill_wsj() +
  xlab(bquote("Fixed effect variance ("*V[fe]*")")) +
  ylab("Density") +
  theme_bw(14)
delta.p2 = df.2 %>% 
  ggplot(aes(x = delta_Vfe)) +
  stat_halfeye(alpha = .6) + 
  xlab(bquote(Delta[V[fe]])) +
  ylab("Density") +
  theme_bw(14)
p2 = p2 + delta.p2


p3 = df %>% 
  ggplot(aes(x = Vfe, fill = Sex)) +
  stat_halfeye(alpha = .6) + 
  scale_fill_wsj() +
  xlab(bquote("Residual variance ("*V[R]*")")) +
  ylab("Density") +
  theme_bw(14)
delta.p3 = df.2 %>% 
  ggplot(aes(x = delta_VR)) +
  stat_halfeye(alpha = .6) + 
  xlab(bquote(Delta[V[fe]])) +
  ylab("Density") +
  theme_bw(14)
p3 = p3 + delta.p3

p4 = df %>% 
  ggplot(aes(x = R, fill = Sex)) +
  stat_halfeye(alpha = .6) + 
  scale_fill_wsj() +
  xlim(0, 1) +
  xlab(bquote("Repeatability (R)")) +
  ylab("Density") +
  theme_bw(14)
delta.p4 = df.2 %>% 
  ggplot(aes(x = delta_R)) +
  stat_halfeye(alpha = .6) + 
  xlim(0, 1) +
  xlab(bquote(Delta[R])) +
  ylab("Density") +
  theme_bw(14)
p4 = p4 + delta.p4

plot_var_R = p1 / p2 / p3 / p4
plot_var_R

ggsave(filename = "outputs/figs/plot_var_R.jpeg", plot_var_R)

