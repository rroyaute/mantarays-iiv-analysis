library(tidyverse); library(here); library(easystats);
library(kableExtra); library(gtsummary); library(ggthemes); library(patchwork)
library(tidybayes)

var.compo.lead = load(here("outputs/ggplot/var.compo.lead.rdata"))
var.compo.gsize = load(here("outputs/ggplot/var.compo.gsize.rdata"))