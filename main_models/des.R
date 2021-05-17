setwd("~/Documents/Projects/guatemala")
library(stargazer)
library(dplyr)
library(tidyr)
library(ggplot2)
options(stringsAsFactors=FALSE)

data = read.csv("data.csv")

# High-violence departments (by population)
high_vio = c("Alta Verapaz", "Baja Verapaz",
  "Peten", "Huehuetenango", "Quiche", "Chimaltenango")



cor(data$ldist_panam, data$roads_dirt_sh)

# Table of government killings by department
vio_dpto = data %>% group_by(dpto) %>%
  summarize( govt_vi_p = round( (sum(govt_vi) / sum(pop73) * 1000), 3) )
pdf("../../../dissertation/empirics/guatemala/govt_vi_by_dpto.pdf",
  height = 4, width = 5.5)
  ggplot(vio_dpto, aes(x = reorder(dpto, govt_vi_p), y = govt_vi_p)) +
    geom_bar(stat = "identity") +
    theme_classic() +
    theme(panel.background = element_blank(),
          legend.position = c(0, 1.03), legend.justification = c(0,1),
          legend.title = element_blank(),
          legend.background = element_blank(),
          panel.grid = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          # panel.grid.major.y = element_line(size = 0.01, color = "gray"),
          strip.text = element_text(size = 12),
          plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
          strip.background = element_blank()) +
    scale_y_continuous(breaks = seq(0, 60, 10)) +
    labs(y = "Killings / 1000 hab.", x = "") +
    coord_flip()
dev.off()
vio_dpto = apply(vio_dpto, 1, function(x) paste0(x[1], " & ", x[2], " \\\\"))
fileconnection = file("../../../dissertation/empirics/guatemala/govt_vi_by_dpto_table_rows.tex")
writeLines(paste(vio_dpto, collapse="\n"), fileconnection)
close(fileconnection)
