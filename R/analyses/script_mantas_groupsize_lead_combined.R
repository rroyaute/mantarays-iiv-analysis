library(tidyverse); library(here)
library(ggthemes); library(patchwork)
library(tidybayes)

var.compo.lead = read_rds(here("outputs/ggplot/var.compo.lead.rds"))
var.compo.gsize = read_rds(here("outputs/ggplot/var.compo.gsize.rds"))

# var.compo.lead = var.compo.lead + 
#   ggtitle("Probability of leading the group")
#   
#   
#   plot_annotation("Probability of leading the group", 
#                   theme = theme(plot.title = element_text(size = 28)),
#                   tag_levels = list(""))
# var.compo.gsize = var.compo.gsize + 
#   plot_annotation("Group size", 
#                   theme = theme(plot.title = element_text(size = 28)),
#                   tag_levels = list(c("")))

var.compo.alltraits = var.compo.lead / var.compo.gsize

var.compo.alltraits = saveRDS(var.compo.alltraits, here("outputs/ggplot/var.compo.alltraits.rds"))

ggsave(filename = here("outputs/figs/var.compo.alltraits.jpeg"), var.compo.alltraits, 
       width = 12, height = 16)
ggsave(filename = here("outputs/figs/var.compo.alltraits.pdf"), var.compo.alltraits, 
       width = 12, height = 16)
