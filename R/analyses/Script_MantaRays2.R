library(tidyverse); library(here); library(easystats)
library(lme4); library(effects); library(rptR)


#### 0. Data Import and cleaning ####
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

# Transform Time columns centered aroun 12:00pm expressed in hours
Data$time_cen=hm(Data$time)
Data$time_cen=as.numeric(Data$time_cen-hours(12))/3600

# subset with only individuals within groups
Data=Data[Data$group==1,]
Data=Data[complete.cases(Data$position),]
str(Data)

# Take only data where all individuals within groups are known - TODO
Data2=Data[Data$group_size==Data$group_size,]

#### 1. Some preliminary plots for IIV ####
library(cowplot)
ggplot(Data, aes(x=sex_f, y=position, fill=sex_f)) + geom_boxplot() + theme_bw() +
  scale_y_continuous(breaks=seq(0, 12, 2))
ggplot(Data, aes(x=size, y=position, fill=size)) + geom_boxplot() + theme_bw() +
  scale_y_continuous(breaks=seq(0, 12, 2))
ggplot(Data, aes(x=maturity, y=position, fill=maturity)) + geom_boxplot() + theme_bw() +
  scale_y_continuous(breaks=seq(0, 12, 2))
ggplot(Data, aes(x=plankton, y=position, fill=plankton)) + geom_boxplot() + theme_bw() +
  scale_y_continuous(breaks=seq(0, 12, 2))
ggplot(Data, aes(x=shark_bite, y=position, fill=shark_bite)) + geom_boxplot() + theme_bw() +
  scale_y_continuous(breaks=seq(0, 12, 2))
ggplot(Data, aes(x=anthropogenic, y=position, fill=anthropogenic)) + geom_boxplot() + theme_bw() +
  scale_y_continuous(breaks=seq(0, 12, 2))

ggplot(Data, aes(x=sex_f, y=group_size, fill=sex_f)) + geom_boxplot() + theme_bw() +
  scale_y_continuous(breaks=seq(0, 30, 2))
ggplot(Data, aes(x=size, y=group_size, fill=size)) + geom_boxplot() + theme_bw() +
  scale_y_continuous(breaks=seq(0, 30, 2))
ggplot(Data, aes(x=maturity, y=group_size, fill=maturity)) + geom_boxplot() + theme_bw() +
  scale_y_continuous(breaks=seq(0, 30, 2))
ggplot(Data, aes(x=plankton, y=group_size, fill=plankton)) + geom_boxplot() + theme_bw() +
  scale_y_continuous(breaks=seq(0, 30, 2))
ggplot(Data, aes(x=shark_bite, y=group_size, fill=shark_bite)) + geom_boxplot() + theme_bw() +
  scale_y_continuous(breaks=seq(0, 30, 2))
ggplot(Data, aes(x=anthropogenic, y=group_size, fill=anthropogenic)) + geom_boxplot() + theme_bw() +
  scale_y_continuous(breaks=seq(0, 30, 2))

ggplot(Data, aes(x=plankton, y=group_size, fill=plankton)) + geom_boxplot() + theme_bw() +
  scale_y_continuous(breaks=seq(0, 30, 2))

ggplot(Data, aes(x=plankton, y=no_mantas, fill=plankton)) + geom_boxplot() + theme_bw() +
  scale_y_continuous(breaks=seq(0, 150, 10))

ggplot(Data[Data$rep_tot>5,], aes(x=reorder(id, position, FUN=median), 
                 y=position, group=id)) + geom_boxplot() + theme_bw() +
  theme(axis.text.x = element_blank()) + xlab("Individual Manta (nobs > 5)") +
  scale_y_continuous(breaks=seq(0, 12, 2))

# Check for individual variation in group size choice
ggplot(Data[Data$rep_tot>5,], aes(x=reorder(id, group_size, FUN=median), 
                                  y=group_size, group=id)) + geom_boxplot() + theme_bw() +
  theme(axis.text.x = element_blank()) + xlab("Individual Manta (nobs > 5)") +
  scale_y_continuous(breaks=seq(0, 150, 10))

#### 2. "Simple" GLMMs ####

# Do AIC model comparisons here
# Group size
# 2.a Group size - Abiotic variables ( I have changed time to the factor variable hour, e.g. 12:54 = 12)
Mod.Abio=glmer(group_size~site+current.VU.AO+time_cen+
                 (1|id),family=poisson(link=sqrt),Data)
plot(allEffects(Mod.Abio))
AIC(Mod.Abio)

# 2.b Group size - Biotic variables (within-group)
Mod.Bio_wg=glmer(group_size~sex_f+size+maturity+
                   (1|id),family=poisson(link=sqrt),Data)
plot(allEffects(Mod.Bio_wg))
AIC(Mod.Bio_wg)

# 2.c Group size - Biotic variables (external to group)
Mod.Bio_ext=glmer(group_size~plankton+scale(no_mantas)+
                    (1|id),family=poisson(link=sqrt),Data)
plot(allEffects(Mod.Bio_ext))
AIC(Mod.Bio_ext)

# 2.d Group size - Abiotic + Biotic variables (within-group)
Mod.Abio_Bio_wg=glmer(group_size~site+current.VU.AO+time_cen+sex_f+size+maturity+
                            (1|id),family=poisson(link=sqrt),Data)
plot(allEffects(Mod.Abio_Bio_wg))
AIC(Mod.Abio_Bio_wg)

# 2.e Group size - Abiotic + Biotic variables (external to group)
Mod.Abio_Bio_ext=glmer(group_size~site+current.VU.AO+time_cen+plankton+scale(no_mantas)+
                             (1|id),family=poisson(link=sqrt),Data)
plot(allEffects(Mod.Abio_Bio_ext))
AIC(Mod.Abio_Bio_ext)

# 2.f Group size - Biotic variables (within-group) + Biotic (external to group)
Mod.Bio_full=glmer(group_size~sex_f+size+maturity+plankton+scale(no_mantas)+
                     (1|id),family=poisson(link=sqrt),Data)
plot(allEffects(Mod.Bio_full))
AIC(Mod.Bio_full)

# 2.g Group size - Abiotic + Biotic variables (within-group) + Biotic (external to group)
Mod.Abio_Bio_full=glmer(group_size~site+current.VU.AO+time_cen+
                          sex_f+size+maturity+plankton+scale(no_mantas)+
                          (1|id),family=poisson(link=sqrt),Data)
plot(allEffects(Mod.Abio_Bio_full))
AIC(Mod.Abio_Bio_full)

# 2.h Group size - Null model
Mod.null=glmer(group_size~1+(1|id),family=poisson(link=sqrt),Data)
plot(allEffects(Mod.null))
AIC(Mod.null)

# Generate AIC table
library(AICcmodavg)
Model.list=list()
Model.list[[1]]=Mod.Abio
Model.list[[2]]=Mod.Bio_wg
Model.list[[3]]=Mod.Bio_ext
Model.list[[4]]=Mod.Abio_Bio_wg
Model.list[[5]]=Mod.Abio_Bio_ext
Model.list[[6]]=Mod.Bio_full
Model.list[[7]]=Mod.Abio_Bio_full
Model.list[[8]]=Mod.null


names(Model.list)=c("Mod.Abio","Mod.Bio_wg","Mod.Bio_ext","Mod.Abio_Bio_wg",
                    "Mod.Abio_Bio_ext","Mod.Bio_full","Mod.Abio_Bio_full","Mod.null")
# Calculate AIC values and delta_AIC by setting second.ord = F 
#(returns AICc otherwise, which are mostly used with limited samples sizes)
aictab(Model.list,second.ord = F)

# examine the R2 for the 2 best models
Data$obs=as.factor(rownames(Data))
R2_Mod.Abio_Bio_ext=glmer(group_size~site+current.VU.AO+time_cen+scale(no_mantas)+
                            (1|id)+(1|obs),family=poisson(link=sqrt),Data)
# calculate  variance components following Nakagaw & Schielzeth (2010) 
# for Poisson variables (Table 2)
Vid=as.numeric(VarCorr(R2_Mod.Abio_Bio_ext)[2])
Vr=as.numeric(VarCorr(R2_Mod.Abio_Bio_ext)[1])+.25
Vfe=var(as.vector(fixef(R2_Mod.Abio_Bio_ext) %*% t(model.matrix(R2_Mod.Abio_Bio_ext))))
Vp=Vid+Vr+Vfe
# R2
R2_Mod.Abio_Bio_ext=Vfe/Vp
R2_Mod.Abio_Bio_ext

R2_Mod.Abio_Bio_full=glmer(group_size~site+current.VU.AO+time_cen+
                           sex_f+size+maturity+plankton+scale(no_mantas)+
                           (1|id)+(1|obs),family=poisson(link=sqrt),Data)
Vid=as.numeric(VarCorr(R2_Mod.Abio_Bio_full)[2])
Vr=as.numeric(VarCorr(R2_Mod.Abio_Bio_full)[1])+.25
Vfe=var(as.vector(fixef(R2_Mod.Abio_Bio_full) %*% t(model.matrix(R2_Mod.Abio_Bio_full))))
Vp=Vid+Vr+Vfe
# R2
R2_Mod.Abio_Bio_full=Vfe/Vp
R2_Mod.Abio_Bio_full



# Get the fixed effect R2 via rptr
R2_Mod.Abio_Bio_ext=rpt(group_size~site+current.VU.AO+time_cen+plankton+scale(no_mantas)+
                           (1|id),grname = c("id","Fixed"), 
                         Data, ratio = T, adjusted = F,datatype = "Poisson",link = "sqrt")
R2_Mod.Abio_Bio_ext

R2_Mod.Abio_Bio_full=rpt(group_size~site+current.VU.AO+time_cen+
                           sex_f+size+maturity+scale(no_mantas)+
                    (1|id),grname = c("id","Fixed"), 
                  Data, ratio = T, adjusted = F,datatype = "Poisson",link = "sqrt")
R2_Mod.Abio_Bio_full


# Binomial Leader/Follower
summary(glmer(leader~sex_f+size+maturity+plankton+
                shark_bite+anthropogenic+
                scale(group_size)+no_mantas+
                (1|id)+(1|site),family="binomial",Data))
plot(allEffects(glmer(leader~sex_f+size+maturity+plankton+
                        shark_bite+anthropogenic+
                        scale(group_size)+no_mantas+
                        (1|id)+(1|site),family="binomial",Data)))

summary(lmer(position~sex_f+size+maturity+plankton+
               shark_bite+anthropogenic+
               scale(group_size)+no_mantas+
                (1|id)+(1|site),Data))
plot(allEffects(lmer(position~sex_f+size+maturity+plankton+
                       shark_bite+anthropogenic+
                       scale(group_size)+no_mantas+
                       (1|id)+(1|site),Data)))
hist(residuals(lmer(position~sex_f+size+maturity+plankton+
                      shark_bite+anthropogenic+
                      scale(group_size)+no_mantas+
                      (1|id)+(1|site),Data)))


#### 3. Variance partitioning ####
# Quick and dirty method
# Group size
# Add an observation column to capture the residual variance
Data$obs=as.factor(rownames(Data))
GLMM.1=glmer(group_size~sex_f+size+maturity+plankton+
                shark_bite+anthropogenic+scale(no_mantas)+site+
                (1|id)+(1|obs),family=poisson(link=sqrt),Data)
# calculate  variance components following Nakagaw & Schielzeth (2010) 
# for Poisson variables (Table 2)
Vid=as.numeric(VarCorr(GLMM.1)[2])
Vr=as.numeric(VarCorr(GLMM.1)[1])+.25
Vfe=var(as.vector(fixef(GLMM.1) %*% t(model.matrix(GLMM.1))))
Vp=Vid+Vr+Vfe
# ratios
Vid/Vp
Vr/Vp
Vfe/Vp

# Position in group
LMM.1=lmer(position~sex_f+size+maturity+plankton+
       shark_bite+anthropogenic+
       scale(group_size)+no_mantas+(1|id),Data)

Vid=as.numeric(VarCorr(LMM.1)[1])
Vr=sigma(LMM.1)^2
Vfe=var(as.vector(fixef(LMM.1) %*% t(model.matrix(LMM.1))))
Vp=Vid+Vr+Vfe
# ratios
Vid/Vp
Vr/Vp
Vfe/Vp

# rptr version
rpt.groupsize=rpt(group_size~sex_f+size+maturity+plankton+
                    shark_bite+anthropogenic+scale(no_mantas)+site+
                    (1|id),grname = c("id","Fixed","Residual"), 
                  Data, ratio = T, adjusted = F,datatype = "Poisson",link = "sqrt")
rpt.groupsize



rpt.position=rpt(position~sex_f+size+maturity+plankton+
                     shark_bite+anthropogenic+
                     scale(group_size)+scale(no_mantas)+site+
      (1|id)+(1|site),
      grname = c("id","Fixed","Residual"), datatype = "Poisson",
      Data, ratio = F, adjusted = F)
rpt.position

rpt.position.f=rpt(position~ex_f+size+maturity+plankton+
                     shark_bite+anthropogenic+
                     scale(group_size)+scale(no_mantas)+site+
                     (1|id)+(1|site),grname = c("id","site","Fixed","Residual"), 
                     subset(Data,sex_f=="F"), ratio = F, adjusted = F)
rpt.position.f

rpt.position.m=rpt(position~ex_f+size+maturity+plankton+
                     shark_bite+anthropogenic+
                     scale(group_size)+scale(no_mantas)+site+
                       (1|id)+(1|site),grname = c("id","site","Fixed","Residual"), 
                     subset(Data,sex_f=="M"), ratio = F, adjusted = F)
rpt.position.m

#rpt.lead.1=rpt(leader~sex_f+size+maturity+plankton+shark_bite+anthropogenic+
#                     scale(group_size)+
#                     (1|id)+(1|site),grname = c("id","site","Fixed","Residual"), Data, ratio = F, adjusted = F,datatype = "Binary")
rpt.lead.2=rpt(leader~size+maturity+plankton+
                 shark_bite+anthropogenic+
                 scale(group_size)+no_mantas+
      (1|id)+(1|site),grname = c("id","site","Fixed","Residual"), 
      Data, ratio = T, adjusted = F,datatype = "Binary")


rpt.lead.2.f=rpt(leader~size+maturity+plankton+
                 shark_bite+anthropogenic+
                 scale(group_size)+no_mantas+
                 (1|id)+(1|site),grname = c("id","site","Fixed","Residual"), 
                 subset(Data,sex_f=="F"), ratio = T, adjusted = F,datatype = "Binary")
rpt.lead.2.m=rpt(leader~size+maturity+plankton+
                 shark_bite+anthropogenic+
                 scale(group_size)+no_mantas+
                 (1|id)+(1|site),grname = c("id","site","Fixed","Residual"), 
                 subset(Data,sex_f=="F"), ratio = T, adjusted = F,datatype = "Binary")


#### 4. Plot variance explained (R2) by indidividuals, sites, fixed effects and resiudal variance  #### 
# Position only for now
Variance<- read.csv("Manta_variance_summary.csv",header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
Variance<- Variance[Variance$Group=="All" & Variance$Cat1=="R2",]
Variance$Variance
ggplot(data=Variance, aes(x=Trait, y=mean*100, fill=Cat2)) +
  geom_bar(stat="identity",colour="black")


#### 5. Test for differences in group position varuance among sexes, plankton & anthropogeneic injuries
library(MCMCglmm)
prior.univ.IW.1<- list(R = list(V = diag(2), n = 1.002),
                       G = list(G1 = list(V = diag(2), n = 1.002)))
position_MCMC<-MCMCglmm(scale(position)~sex_f+size+maturity+plankton+
                          shark_bite+anthropogenic+
                          scale(group_size)+no_mantas,
                        random=~idh(sex_f):id,rcov=~idh(sex_f):units,
                        data = Data, 
                        nitt=1300000, thin=1000, burnin=300000,
                        verbose=F,prior=prior.univ.IW.1)
summary(position_MCMC)#diag(autocorr(Act_OF_MCMC$Sol)[2, , ])
diag(autocorr(position_MCMC$VCV)[2, , ])
data.frame(posterior.mode(position_MCMC$VCV));HPDinterval(position_MCMC$VCV)

prior.univ.IW.3<- list(R = list(V = diag(3), n = 1.002),
                       G = list(G1 = list(V = diag(3), n = 1.002)))
position_MCMC.2<-MCMCglmm(scale(position)~sex_f+size+maturity+plankton+
                          shark_bite+anthropogenic+
                          scale(group_size)+no_mantas,
                        random=~idh(plankton):id,rcov=~idh(plankton):units,
                        data = Data, 
                        nitt=1300000, thin=1000, burnin=300000,
                        verbose=F,prior=prior.univ.IW.3)
summary(position_MCMC.2)#diag(autocorr(Act_OF_MCMC$Sol)[2, , ])
diag(autocorr(position_MCMC.2$VCV)[2, , ])
data.frame(posterior.mode(position_MCMC.2$VCV));data.frame(HPDinterval(position_MCMC.2$VCV))
plot(position_MCMC.2$VCV)

position_MCMC.3<-MCMCglmm(scale(position)~sex_f+size+maturity+plankton+
                            shark_bite+anthropogenic+
                            scale(group_size)+no_mantas,
                          random=~idh(anthropogenic):id,rcov=~idh(anthropogenic):units,
                          data = Data, 
                          nitt=1300000, thin=1000, burnin=300000,
                          verbose=F,prior=prior.univ.IW.1)
summary(position_MCMC.3)#diag(autocorr(Act_OF_MCMC$Sol)[2, , ])
diag(autocorr(position_MCMC.3$VCV)[2, , ])
data.frame(posterior.mode(position_MCMC.3$VCV));HPDinterval(position_MCMC.3$VCV)
plot(position_MCMC.3$VCV)

# Compare variances for probability of leading group, doesn't work well yet
prior.univ.IW.2<- list(R = list(V = diag(2), n = 1.002, fix = FALSE),
                       G = list(G1 = list(V = diag(2), n = 1.002)))
leader_MCMC<-MCMCglmm(leader~sex_f+size+maturity+plankton+shark_bite+anthropogenic+
                        scale(group_size), family = "categorical",
                      random=~idh(sex_f):id,rcov=~idh(sex_f):units,
                      data = Data, 
                      nitt=1300000, thin=1000, burnin=300000,
                      verbose=F,prior=prior.univ.IW.1)
summary(leader_MCMC)#diag(autocorr(Act_OF_MCMC$Sol)[2, , ])
diag(autocorr(leader_MCMC$VCV)[2, , ])
data.frame(posterior.mode(leader_MCMC$VCV));HPDinterval(position_MCMC$VCV)
plot(leader_MCMC$VCV)
# Plotting variance differences
# Data frames
Var_sex=data.frame(cbind(data.frame(posterior.mode(position_MCMC$VCV)),data.frame(HPDinterval(position_MCMC$VCV))))
names(Var_sex)=c("mode","lower_ci","upper_ci")
Var_sex$Sex=factor(rep(c("Females","Males"),2))
Var_sex$Variance=factor(c("Vi","Vi","Vw","Vw"))

Var_plankton=data.frame(cbind(data.frame(posterior.mode(position_MCMC.2$VCV)),data.frame(HPDinterval(position_MCMC.2$VCV))))
names(Var_plankton)=c("mode","lower_ci","upper_ci")
Var_plankton$Plankton=factor(rep(c("Low","Medium","High"),2))
Var_plankton$Plankton=ordered(Var_plankton$Plankton, levels = c("Low","Medium","High"))
Var_plankton$Variance=factor(c("Vi","Vi","Vi","Vw","Vw","Vw"))

Var_anthro=data.frame(cbind(data.frame(posterior.mode(position_MCMC.3$VCV)),data.frame(HPDinterval(position_MCMC.3$VCV))))
names(Var_anthro)=c("mode","lower_ci","upper_ci")
Var_anthro$Anthro=factor(rep(c("No","Yes"),2))
Var_anthro$Variance=factor(c("Vi","Vi","Vw","Vw"))


# Plots
pd <- position_dodge(0.5)

ggplot(Var_sex, aes(x=Sex, y=mode, shape=Variance)) + geom_point(position=pd, size = 3.5) + 
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci), width=.1, position=pd) + theme_bw() + ylim(0,1) +
  scale_shape_manual(values=c(15,16)) + ylab("Variance") + xlab("Sex") + facet_wrap(~Variance) +
  guides(fill=FALSE)


ggplot(Var_plankton, aes(x=Plankton, y=mode, shape=Variance)) + geom_point(position=pd, size = 3.5) + 
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci), width=.1, position=pd) + theme_bw() + ylim(0,1) +
  scale_shape_manual(values=c(15,16)) + ylab("Variance") + xlab("Plankton abundance") + facet_wrap(~Variance) +
  guides(fill=FALSE)

ggplot(Var_anthro, aes(x=Anthro, y=mode, shape=Variance)) + geom_point(position=pd, size = 3.5) + 
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci), width=.1, position=pd) + theme_bw() + ylim(0,1) +
  scale_shape_manual(values=c(15,16)) + ylab("Variance") + xlab("Anthropogenic injury") + facet_wrap(~Variance) +
  guides(fill=FALSE)
