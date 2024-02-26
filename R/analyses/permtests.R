library(tidyverse); library(here); library(rptR)

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


## Perm Tests ----
rpt.V.solo.v.g = read_rds(file = here("outputs/mods/rpt.V.solo.v.g.rds"))
rpt.r2.solo.v.g = read_rds(file = here("outputs/mods/rpt.r2.solo.v.g.rds"))


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