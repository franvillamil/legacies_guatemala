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

# Long-form dataset
datal = data %>%
  gather(key, value, matches("(URNGcia|FRG|UNE|fulldcha|part)(\\d+)")) %>%
  extract(key, c("party", "elec"), "(URNGcia|FRG|UNE|fulldcha|part)(\\d+)") %>%
  spread(party, value)

## -----------------------------------------------------------------
## FUNCTIONS

add_rest_vars = function(newdata){
  nd = newdata
  newvars = data.frame(
    lpop73 = rep(mean(data$lpop73, na.rm = T), nrow(nd)),
    ind73 = rep(mean(data$ind73, na.rm = T), nrow(nd)),
    elev_sd = rep(mean(data$elev_sd, na.rm = T) , nrow(nd)),
    roads_dirt_sh = rep(mean(data$roads_dirt_sh, na.rm = T), nrow(nd)),
    forest = rep(mean(data$forest, na.rm = T), nrow(nd)),
    ldist_guate = rep(mean(data$ldist_guate, na.rm = T), nrow(nd)),
    larea = rep(mean(data$larea, na.rm = T), nrow(nd)),
    rebels_vi_pre78_l = rep(mean(data$rebels_vi_pre78_l, na.rm = T), nrow(nd)),
    dpto = rep("Quiche", nrow(nd)),
    elec = rep("1999", nrow(nd)))
  including = !names(newvars) %in% names(nd)
  nd = cbind(nd, newvars[, including])
  return(nd)
}

predprob_df = function(newdata, model, label = NULL){
  nd = newdata
  nd$y = predict(model, newdata = nd, type = "response")
  nd$se = predict(model, newdata = nd, type = "response", se.fit = T)$se.fit
  nd$upr = nd$y + 1.96 * nd$se
  nd$lwr = nd$y - 1.96 * nd$se

  if(!is.null(label)){nd = cbind(nd, label = rep(label, nrow(nd)))}
  return(nd)
}

## ----------------------------------------------------------------
## BASE MODELS

m_izq = lm(URNGcia ~ govt_vi_l +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = datal)
m_izq_hv = lm(URNGcia ~ govt_vi_l +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = subset(datal, dpto %in% high_vio))

m_dcha = lm(FRG ~ govt_vi_l +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = datal)
m_dcha_hv = lm(FRG ~ govt_vi_l +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = subset(datal, dpto %in% high_vio))

# see: https://stackoverflow.com/questions/32404903/order-variables-with-interaction-in-stargazer-regression-output
hv_dpts_note = "\\scriptsize Most affected departments"

# Table: URNG and FRG, base
fileconnection = file("../../../dissertation/empirics/guatemala/table_base_pooled.tex")
notes_table = "\\parbox[t]{0.75\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. All models pool all observations across all elections, including election FEs. Department FEs included, not shown. Most affected departments include Huehuetenango, Chimaltenango, Quiché, Alta Verapaz, Baja Verapaz, and Petén.}"
writeLines(
  stargazer(m_izq, m_dcha, m_izq_hv, m_dcha_hv,
    title = "Base models on wartime violence and postwar voting",
    label = "tab-gt:base_models",
    omit = "dpto",
    omit.stat = c("f", "ser"), intercept.bottom = F,
    covariate.labels = c("(Intercept)", "State-led killings",
    "Log. Population 1973", "\\% Indigenous 1973",
    "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
    "Rebel violence pre-78", "2003 election", "2007 election", "2011 election",
    "2015 election"),
    multicolumn = FALSE,
    column.sep.width = "-20pt",
    dep.var.caption = "",
    dep.var.labels = c("URNG", "FRG", "URNG", "FRG"),
    column.labels = c("", hv_dpts_note), column.separate = c(2, 2),
    font.size = "small",
    digits = 3, digits.extra = 0,
    star.char = c("+", "*", "**", "***"),
    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
    notes.align = "c", align = T, no.space = TRUE,
    notes.label = "", notes.append = F,
    add.lines=list(c("Department FE", rep("\\multicolumn{1}{c}{Yes}", 4))),
    notes = notes_table),
fileconnection)
close(fileconnection)


## ----------------------------------------------------------------
## MODELS w INTERACTION

# % Roads non-paved

m_izq_roads = lm(URNGcia ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = datal)
m_izq_hv_roads = lm(URNGcia ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = subset(datal, dpto %in% high_vio))
m_dcha_roads = lm(FRG ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = datal)
m_dcha_hv_roads = lm(FRG ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = subset(datal, dpto %in% high_vio))

# Table: URNG/FRG and paved roads
fileconnection = file("../../../dissertation/empirics/guatemala/table_roads_nonpaved.tex")
notes_table = "\\parbox[t]{0.75\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. All models pool all observations across all elections, including election FEs. Department FEs included, not shown. Most affected departments include Huehuetenango, Chimaltenango, Quiché, Alta Verapaz, Baja Verapaz, and Petén.}"
writeLines(
  stargazer(m_izq_roads, m_dcha_roads, m_izq_hv_roads, m_dcha_hv_roads,
    title = "Wartime violence, prewar mobilization, and voting",
    label = "tab-gt:int_roads",
    omit = "dpto",
    omit.stat = c("f", "ser"), intercept.bottom = F,
    order = c("Constant", "govt_vi_l$", "dirt_sh$"),
    covariate.labels = c("(Intercept)", "State-led killings",
      "\\% Non-paved roads", "Violence $\\times$ Non-paved",
      "Log. Population 1973", "\\% Indigenous 1973",
      "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
      "Rebel violence pre-78", "2003 election", "2007 election", "2011 election",
      "2015 election"),
    multicolumn = FALSE,
    column.sep.width = "-20pt",
    dep.var.caption = "",
    dep.var.labels = c("URNG", "FRG", "URNG", "FRG"),
    column.labels = c("", hv_dpts_note), column.separate = c(2, 2),
    font.size = "small",
    digits = 3, digits.extra = 0,
    star.char = c("+", "*", "**", "***"),
    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
    notes.align = "c", align = T, no.space = TRUE,
    notes.label = "", notes.append = F,
    add.lines=list(c("Department FE", rep("\\multicolumn{1}{c}{Yes}", 4))),
    notes = notes_table),
fileconnection)
close(fileconnection)

# Log Distance to Pan-American Highway

m_izq_panam = lm(URNGcia ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = datal)
m_izq_hv_panam = lm(URNGcia ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = subset(datal, dpto %in% high_vio))
m_dcha_panam = lm(FRG ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = datal)
m_dcha_hv_panam = lm(FRG ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = subset(datal, dpto %in% high_vio))

# Table: URNG/FRG and paved roads
fileconnection = file("../../../dissertation/empirics/guatemala/table_dist_panam.tex")
notes_table = "\\parbox[t]{0.75\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. All models pool all observations across all elections, including election FEs. Department FEs included, not shown. Most affected departments include Huehuetenango, Chimaltenango, Quiché, Alta Verapaz, Baja Verapaz, and Petén.}"
writeLines(
  stargazer(m_izq_panam, m_dcha_panam, m_izq_hv_panam, m_dcha_hv_panam,
    title = "Wartime violence, prewar mobilization, and voting",
    label = "tab-gt:int_panam",
    omit = "dpto",
    omit.stat = c("f", "ser"), intercept.bottom = F,
    order = c("Constant", "govt_vi_l$", "panam$"),
    covariate.labels = c("(Intercept)", "State-led killings",
      "Log. Dist to Pan-Am Hwy", "Violence $\\times$ Dist to Pan-Am",
      "Log. Population 1973", "\\% Indigenous 1973",
      "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
      "Rebel violence pre-78", "2003 election", "2007 election", "2011 election",
      "2015 election"),
    multicolumn = FALSE,
    column.sep.width = "-20pt",
    dep.var.caption = "",
    dep.var.labels = c("URNG", "FRG", "URNG", "FRG"),
    column.labels = c("", hv_dpts_note), column.separate = c(2, 2),
    font.size = "small",
    digits = 3, digits.extra = 0,
    star.char = c("+", "*", "**", "***"),
    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
    notes.align = "c", align = T, no.space = TRUE,
    notes.label = "", notes.append = F,
    add.lines=list(c("Department FE", rep("\\multicolumn{1}{c}{Yes}", 4))),
    notes = notes_table),
fileconnection)
close(fileconnection)

## ----------------------------------------------------------------
## PLOTS

newdata = expand.grid(govt_vi_l = seq(0, 5, 1), roads_dirt_sh = c(0, 0.5, 1))
newdata$ldist_panam = rep(c(0, 3, 6), each = 6)
newdata = add_rest_vars(newdata)

# Predicted probabilities
URNG_roads = predprob_df(newdata, m_izq_roads, label = "URNG_roads")
URNG_panam = predprob_df(newdata, m_izq_panam, label = "URNG_panam")
FRG_roads = predprob_df(newdata, m_dcha_roads, label = "FRG_roads")
FRG_panam = predprob_df(newdata, m_dcha_panam, label = "FRG_panam")

URNG_roads$roads_dirt_sh = factor(URNG_roads$roads_dirt_sh)
levels(URNG_roads$roads_dirt_sh) = c("All roads paved", "50% non-paved roads", "All non-paved")
FRG_roads$roads_dirt_sh = factor(FRG_roads$roads_dirt_sh)
levels(FRG_roads$roads_dirt_sh) = c("All roads paved", "50% non-paved roads", "All non-paved")
URNG_panam$ldist_panam = factor(URNG_panam$ldist_panam)
levels(URNG_panam$ldist_panam) = c("Next to PanAm Highway", "ca. 20km away", "ca. 400km away")
FRG_panam$ldist_panam = factor(FRG_panam$ldist_panam)
levels(FRG_panam$ldist_panam) = c("Next to PanAm Highway", "ca. 20km away", "ca. 400km away")

# Plot: URNG & roads
pdf("../../../dissertation/empirics/guatemala/pp_urng_roads.pdf",
  height = 3, width = 7)
ggplot(URNG_roads, aes(y = y, x = govt_vi_l)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  facet_wrap(~roads_dirt_sh) +
  theme_classic() +
  theme(panel.background = element_blank(),
        legend.position = c(0, 1.03), legend.justification = c(0,1),
        legend.title = element_blank(),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size = 12),
        plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
        strip.background = element_blank()) +
  labs(y = "Predicted share to URNG", x = "State violence (log killings / 1000 hab)")
dev.off()

# Plots: URNG & dist to PanAm Hwy
pdf("../../../dissertation/empirics/guatemala/pp_urng_panam.pdf",
  height = 3, width = 7)
ggplot(URNG_panam, aes(y = y, x = govt_vi_l)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  facet_wrap(~ldist_panam) +
  theme_classic() +
  theme(panel.background = element_blank(),
        legend.position = c(0, 1.03), legend.justification = c(0,1),
        legend.title = element_blank(),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size = 12),
        plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
        strip.background = element_blank()) +
  labs(y = "Predicted share to URNG", x = "State violence (log killings / 1000 hab)")
dev.off()

# Plot: FRG & roads
pdf("../../../dissertation/empirics/guatemala/pp_frg_roads.pdf",
  height = 3, width = 7)
ggplot(FRG_roads, aes(y = y, x = govt_vi_l)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  facet_wrap(~roads_dirt_sh) +
  theme_classic() +
  theme(panel.background = element_blank(),
        legend.position = c(0, 1.03), legend.justification = c(0,1),
        legend.title = element_blank(),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size = 12),
        plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
        strip.background = element_blank()) +
  labs(y = "Predicted share to FRG", x = "State violence (log killings / 1000 hab)")
dev.off()

# Plots: FRG & dist to PanAm Hwy
pdf("../../../dissertation/empirics/guatemala/pp_frg_panam.pdf",
  height = 3, width = 7)
ggplot(FRG_panam, aes(y = y, x = govt_vi_l)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  facet_wrap(~ldist_panam) +
  theme_classic() +
  theme(panel.background = element_blank(),
        legend.position = c(0, 1.03), legend.justification = c(0,1),
        legend.title = element_blank(),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size = 12),
        plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
        strip.background = element_blank()) +
  labs(y = "Predicted share to FRG", x = "State violence (log killings / 1000 hab)")
dev.off()

## ----------------------------------------------------------------
## ALTERNATIVE EXPLANATIONS & ROBUSTNESS OF RESULTS

# Correlates of violence
m_vio1 = lm(govt_vi_l ~ roads_dirt_sh + ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate +
  larea + rebels_vi_pre78_l,
  data = data)
m_vio2 = lm(govt_vi_l ~ roads_dirt_sh + ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate +
  larea + rebels_vi_pre78_l + factor(dpto),
  data = data)
m_vio3 = lm(govt_vi_l ~ roads_dirt_sh + ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate +
  larea + rebels_vi_pre78_l,
  data = subset(data, dpto %in% high_vio))
m_vio4 = lm(govt_vi_l ~ roads_dirt_sh + ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate +
  larea + rebels_vi_pre78_l + factor(dpto),
  data = subset(data, dpto %in% high_vio))

m_vio5 = lm(rebels_vi_l ~ roads_dirt_sh + ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate +
  larea + rebels_vi_pre78_l,
  data = data)
m_vio6 = lm(rebels_vi_l ~ roads_dirt_sh + ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate +
  larea + rebels_vi_pre78_l + factor(dpto),
  data = data)
m_vio7 = lm(rebels_vi_l ~ roads_dirt_sh + ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate +
  larea + rebels_vi_pre78_l,
  data = subset(data, dpto %in% high_vio))
m_vio8 = lm(rebels_vi_l ~ roads_dirt_sh + ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate +
  larea + rebels_vi_pre78_l + factor(dpto),
  data = subset(data, dpto %in% high_vio))

# Table: correlates of violence
fileconnection = file("../../../dissertation/empirics/guatemala/table_correlates_state_vio.tex")
notes_table = "\\parbox[t]{0.75\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Most affected departments include Huehuetenango, Chimaltenango, Quiché, Alta Verapaz, Baja Verapaz, and Petén.}"
writeLines(
  stargazer(m_vio1, m_vio2, m_vio3, m_vio4,
    title = "Determinants of wartime violence by the state",
    label = "tab-gt:corr_vio_state",
    omit = "dpto",
    omit.stat = c("f", "ser"), intercept.bottom = F,
    covariate.labels = c("(Intercept)", "\\% Roads non-paved",
    "Log. Distance to Pan-Am Hwy", "Log. Population 1973", "\\% Indigenous 1973",
    "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
    "Rebel violence pre-78"),
    multicolumn = FALSE,
    column.sep.width = "-20pt",
    dep.var.caption = "",
    dep.var.labels.include = FALSE,
    column.labels = c("", paste0("\\multicolumn{1}{c}{", hv_dpts_note, "}")),
    column.separate = c(2, 2),
    font.size = "small",
    digits = 3, digits.extra = 0,
    star.char = c("+", "*", "**", "***"),
    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
    notes.align = "c", align = T, no.space = TRUE,
    notes.label = "", notes.append = F,
    add.lines=list(c("Department FE",
      rep(c("\\multicolumn{1}{c}{No}", "\\multicolumn{1}{c}{Yes}"), 2))),
    notes = notes_table),
fileconnection)
close(fileconnection)

# Table: correlates of violence
fileconnection = file("../../../dissertation/empirics/guatemala/table_correlates_rebel_vio.tex")
notes_table = "\\parbox[t]{0.75\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Most affected departments include Huehuetenango, Chimaltenango, Quiché, Alta Verapaz, Baja Verapaz, and Petén.}"
writeLines(
  stargazer(m_vio5, m_vio6, m_vio7, m_vio8,
    title = "Determinants of wartime violence by the rebels",
    label = "tab-gt:corr_vio_rebel",
    omit = "dpto",
    omit.stat = c("f", "ser"), intercept.bottom = F,
    covariate.labels = c("(Intercept)", "\\% Roads non-paved",
    "Log. Distance to Pan-Am Hwy", "Log. Population 1973", "\\% Indigenous 1973",
    "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
    "Rebel violence pre-78"),
    multicolumn = FALSE,
    column.sep.width = "-20pt",
    dep.var.caption = "",
    dep.var.labels.include = FALSE,
    column.labels = c("", hv_dpts_note),
    column.separate = c(2, 2),
    font.size = "small",
    digits = 3, digits.extra = 0,
    star.char = c("+", "*", "**", "***"),
    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
    notes.align = "c", align = T, no.space = TRUE,
    notes.label = "", notes.append = F,
    add.lines=list(c("Department FE",
      rep(c("\\multicolumn{1}{c}{No}", "\\multicolumn{1}{c}{Yes}"), 2))),
    notes = notes_table),
fileconnection)
close(fileconnection)

# % Roads non-paved and distance to Pan-American Highway

pdf("../../../dissertation/empirics/guatemala/roads_panam_corr.pdf",
  height = 3.5, width = 3.5)
ggplot(data, aes(x = ldist_panam, y = roads_dirt_sh)) +
  geom_point(size = 0.75) +
  geom_smooth(method = "lm", formula = y ~ x,
    colour = "black", size = 0.5) +
  theme_classic() +
  theme(panel.background = element_blank(),
        legend.position = c(0, 1.03), legend.justification = c(0,1),
        legend.title = element_blank(),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size = 12),
        plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
        strip.background = element_blank()) +
  labs(y = "% Non-paved roads", x = "Distance to Pan-Am Highway (Log. km)")
dev.off()

pdf("../../../dissertation/empirics/guatemala/roads_panam_corr_hv.pdf",
  height = 3.5, width = 3.5)
ggplot(subset(data, dpto %in% high_vio), aes(x = ldist_panam, y = roads_dirt_sh)) +
  geom_point(size = 0.75) +
  geom_smooth(method = "lm", formula = y ~ x,
    colour = "black", size = 0.5) +
  theme_classic() +
  theme(panel.background = element_blank(),
        legend.position = c(0, 1.03), legend.justification = c(0,1),
        legend.title = element_blank(),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size = 12),
        plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
        strip.background = element_blank()) +
  labs(y = "% Non-paved roads", x = "Distance to Pan-Am Highway (Log. km)")
dev.off()


## ----------------------------------------------------------------
## FURTHER MODELS FOR APPENDIX

# By year, base models ----------------

m_izq99 = lm(URNGcia1999 ~ govt_vi_l +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_izq03 = lm(URNGcia2003 ~ govt_vi_l +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_izq07 = lm(URNGcia2007 ~ govt_vi_l +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_izq11 = lm(URNGcia2011 ~ govt_vi_l +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_izq15 = lm(URNGcia2015 ~ govt_vi_l +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_dcha99 = lm(FRG1999 ~ govt_vi_l +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_dcha03 = lm(FRG2003 ~ govt_vi_l +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_dcha07 = lm(FRG2007 ~ govt_vi_l +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_dcha15 = lm(FRG2015 ~ govt_vi_l +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)

# Table (appendix): URNG by year
fileconnection = file("../../../dissertation/empirics/guatemala/table_app_base_URNG_yrs.tex")
notes_table = "\\parbox[t]{0.75\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Each model includes cross-sectional data on a specific election.}"
writeLines(
  stargazer(m_izq99, m_izq03, m_izq07, m_izq11, m_izq15,
    title = "Wartime violence and URNG share, by year (base)",
    label = "tab-gt-app:base_URNGyrs",
    omit = "dpto",
    omit.stat = c("f", "ser"), intercept.bottom = F,
    covariate.labels = c("(Intercept)", "State-led killings",
    "Log. Population 1973", "\\% Indigenous 1973",
    "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
    "Rebel violence pre-78"),
    multicolumn = FALSE,
    column.sep.width = "-20pt",
    dep.var.caption = "",
    dep.var.labels = c("1999", "2003", "2007", "2011", "2015"),
    font.size = "small",
    digits = 3, digits.extra = 0,
    star.char = c("+", "*", "**", "***"),
    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
    notes.align = "c", align = T, no.space = TRUE,
    notes.label = "", notes.append = F,
    add.lines=list(c("Department FE", rep("\\multicolumn{1}{c}{Yes}", 4))),
    notes = notes_table),
fileconnection)
close(fileconnection)

# Table (appendix): FRG by year
fileconnection = file("../../../dissertation/empirics/guatemala/table_app_base_FRG_yrs.tex")
notes_table = "\\parbox[t]{0.75\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Each model includes cross-sectional data on a specific election.}"
writeLines(
  stargazer(m_dcha99, m_dcha03, m_dcha07, m_dcha15,
    title = "Wartime violence and FRG share, by year (base)",
    label = "tab-gt-app:base_FRGyrs",
    omit = "dpto",
    omit.stat = c("f", "ser"), intercept.bottom = F,
    covariate.labels = c("(Intercept)", "State-led killings",
    "Log. Population 1973", "\\% Indigenous 1973",
    "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
    "Rebel violence pre-78"),
    multicolumn = FALSE,
    column.sep.width = "-20pt",
    dep.var.caption = "",
    dep.var.labels = c("1999", "2003", "2007", "2011", "2015"),
    font.size = "small",
    digits = 3, digits.extra = 0,
    star.char = c("+", "*", "**", "***"),
    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
    notes.align = "c", align = T, no.space = TRUE,
    notes.label = "", notes.append = F,
    add.lines=list(c("Department FE", rep("\\multicolumn{1}{c}{Yes}", 4))),
    notes = notes_table),
fileconnection)
close(fileconnection)

# Interaction across years (URNG) ----------------

m_izq_roads99 = lm(URNGcia1999 ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_izq_roads03 = lm(URNGcia2003 ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_izq_roads07 = lm(URNGcia2007 ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_izq_roads11 = lm(URNGcia2011 ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_izq_roads15 = lm(URNGcia2015 ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)

m_izq_panam99 = lm(URNGcia1999 ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_izq_panam03 = lm(URNGcia2003 ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_izq_panam07 = lm(URNGcia2007 ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_izq_panam11 = lm(URNGcia2011 ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_izq_panam15 = lm(URNGcia2015 ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)

# Table (appendix): URNG and non-paved roads, by year
fileconnection = file("../../../dissertation/empirics/guatemala/table_app_URNG_roads_yrs.tex")
notes_table = "\\parbox[t]{0.75\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Each model includes cross-sectional data on a specific election.}"
writeLines(
  stargazer(m_izq_roads99, m_izq_roads03, m_izq_roads07, m_izq_roads11, m_izq_roads15,
    title = "Wartime violence and URNG share, by year (interaction)",
    label = "tab-gt-app:roads_URNGyrs",
    omit = "dpto",
    omit.stat = c("f", "ser"), intercept.bottom = F,
    order = c("Constant", "govt_vi_l$", "dirt_sh$"),
    covariate.labels = c("(Intercept)", "State-led killings",
      "\\% Non-paved roads", "Violence $\\times$ Non-paved",
      "Log. Population 1973", "\\% Indigenous 1973",
      "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
      "Rebel violence pre-78"),
    multicolumn = FALSE,
    column.sep.width = "-20pt",
    dep.var.caption = "",
    dep.var.labels = c("1999", "2003", "2007", "2011", "2015"),
    font.size = "small",
    digits = 3, digits.extra = 0,
    star.char = c("+", "*", "**", "***"),
    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
    notes.align = "c", align = T, no.space = TRUE,
    notes.label = "", notes.append = F,
    add.lines=list(c("Department FE", rep("\\multicolumn{1}{c}{Yes}", 5))),
    notes = notes_table),
fileconnection)
close(fileconnection)

# Table (appendix): URNG and dist to PanAm Hwy, by year
fileconnection = file("../../../dissertation/empirics/guatemala/table_app_URNG_panam_yrs.tex")
notes_table = "\\parbox[t]{0.75\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Each model includes cross-sectional data on a specific election.}"
writeLines(
  stargazer(m_izq_panam99, m_izq_panam03, m_izq_panam07, m_izq_panam11, m_izq_panam15,
    title = "Wartime violence and URNG share, by year (interaction)",
    label = "tab-gt-app:panam_URNGyrs",
    omit = "dpto",
    omit.stat = c("f", "ser"), intercept.bottom = F,
    order = c("Constant", "govt_vi_l$", "panam$"),
    covariate.labels = c("(Intercept)", "State-led killings",
      "Log. Dist to Pan-Am Hwy", "Violence $\\times$ Dist to Pan-Am",
      "Log. Population 1973", "\\% Indigenous 1973",
      "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
      "Rebel violence pre-78"),
    multicolumn = FALSE,
    column.sep.width = "-20pt",
    dep.var.caption = "",
    dep.var.labels = c("1999", "2003", "2007", "2011", "2015"),
    font.size = "small",
    digits = 3, digits.extra = 0,
    star.char = c("+", "*", "**", "***"),
    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
    notes.align = "c", align = T, no.space = TRUE,
    notes.label = "", notes.append = F,
    add.lines=list(c("Department FE", rep("\\multicolumn{1}{c}{Yes}", 5))),
    notes = notes_table),
fileconnection)
close(fileconnection)


m_dcha_roads99 = lm(FRG1999 ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_dcha_roads03 = lm(FRG2003 ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_dcha_roads07 = lm(FRG2007 ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_dcha_roads15 = lm(FRG2015 ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)

m_dcha_panam99 = lm(FRG1999 ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_dcha_panam03 = lm(FRG2003 ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_dcha_panam07 = lm(FRG2007 ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_dcha_panam15 = lm(FRG2015 ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)

# Table (appendix): URNG and non-paved roads, by year
fileconnection = file("../../../dissertation/empirics/guatemala/table_app_FRG_roads_yrs.tex")
notes_table = "\\parbox[t]{0.75\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Each model includes cross-sectional data on a specific election.}"
writeLines(
  stargazer(m_dcha_roads99, m_dcha_roads03, m_dcha_roads07, m_dcha_roads15,
    title = "Wartime violence and FRG share, by year (interaction)",
    label = "tab-gt-app:roads_FRGyrs",
    omit = "dpto",
    omit.stat = c("f", "ser"), intercept.bottom = F,
    order = c("Constant", "govt_vi_l$", "dirt_sh$"),
    covariate.labels = c("(Intercept)", "State-led killings",
      "\\% Non-paved roads", "Violence $\\times$ Non-paved",
      "Log. Population 1973", "\\% Indigenous 1973",
      "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
      "Rebel violence pre-78"),
    multicolumn = FALSE,
    column.sep.width = "-20pt",
    dep.var.caption = "",
    dep.var.labels = c("1999", "2003", "2007", "2015"),
    font.size = "small",
    digits = 3, digits.extra = 0,
    star.char = c("+", "*", "**", "***"),
    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
    notes.align = "c", align = T, no.space = TRUE,
    notes.label = "", notes.append = F,
    add.lines=list(c("Department FE", rep("\\multicolumn{1}{c}{Yes}", 4))),
    notes = notes_table),
fileconnection)
close(fileconnection)

# Table (appendix): URNG and dist to PanAm Hwy, by year
fileconnection = file("../../../dissertation/empirics/guatemala/table_app_FRG_panam_yrs.tex")
notes_table = "\\parbox[t]{0.75\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Each model includes cross-sectional data on a specific election.}"
writeLines(
  stargazer(m_dcha_panam99, m_dcha_panam03, m_dcha_panam07, m_dcha_panam15,
    title = "Wartime violence and FRG share, by year (interaction)",
    label = "tab-gt-app:panam_FRGyrs",
    omit = "dpto",
    omit.stat = c("f", "ser"), intercept.bottom = F,
    order = c("Constant", "govt_vi_l$", "panam$"),
    covariate.labels = c("(Intercept)", "State-led killings",
      "Log. Dist to Pan-Am Hwy", "Violence $\\times$ Dist to Pan-Am",
      "Log. Population 1973", "\\% Indigenous 1973",
      "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
      "Rebel violence pre-78"),
    multicolumn = FALSE,
    column.sep.width = "-20pt",
    dep.var.caption = "",
    dep.var.labels = c("1999", "2003", "2007", "2015"),
    font.size = "small",
    digits = 3, digits.extra = 0,
    star.char = c("+", "*", "**", "***"),
    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
    notes.align = "c", align = T, no.space = TRUE,
    notes.label = "", notes.append = F,
    add.lines=list(c("Department FE", rep("\\multicolumn{1}{c}{Yes}", 4))),
    notes = notes_table),
fileconnection)
close(fileconnection)

# PLOTS: interaction across years

# Pred probs: URNG & roads/PanAm by year
URNG_roads_yrs = rbind(
  predprob_df(newdata, m_izq_roads99, label = "1999"),
  predprob_df(newdata, m_izq_roads03, label = "2003"),
  predprob_df(newdata, m_izq_roads07, label = "2007"),
  predprob_df(newdata, m_izq_roads11, label = "2011"),
  predprob_df(newdata, m_izq_roads15, label = "2015"))
URNG_panam_yrs = rbind(
  predprob_df(newdata, m_izq_panam99, label = "1999"),
  predprob_df(newdata, m_izq_panam03, label = "2003"),
  predprob_df(newdata, m_izq_panam07, label = "2007"),
  predprob_df(newdata, m_izq_panam11, label = "2011"),
  predprob_df(newdata, m_izq_panam15, label = "2015"))
URNG_roads_yrs$roads_dirt_sh = factor(URNG_roads_yrs$roads_dirt_sh)
levels(URNG_roads_yrs$roads_dirt_sh) = c("All roads paved", "50% non-paved roads", "All non-paved")
URNG_panam_yrs$ldist_panam = factor(URNG_panam_yrs$ldist_panam)
levels(URNG_panam_yrs$ldist_panam) = c("Next to PanAm Highway", "ca. 20km away", "ca. 400km away")

# Plot: URNG & roads/PanAm by year
pdf("../../../dissertation/empirics/guatemala/pp_urng_roads_yrs.pdf",
  height = 8, width = 7)
ggplot(URNG_roads_yrs, aes(y = y, x = govt_vi_l)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  facet_grid(label~roads_dirt_sh) +
  theme_bw() +
  theme(panel.background = element_blank(),
        legend.position = c(0, 1.03), legend.justification = c(0,1),
        legend.title = element_blank(),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        # panel.border = element_blank(),
        strip.text = element_text(size = 12),
        plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
        strip.background = element_blank()) +
  labs(y = "Predicted share to URNG", x = "State violence (log killings / 1000 hab)")
dev.off()

pdf("../../../dissertation/empirics/guatemala/pp_urng_panam_yrs.pdf",
  height = 8, width = 7)
ggplot(URNG_panam_yrs, aes(y = y, x = govt_vi_l)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  facet_grid(label~ldist_panam) +
  theme_bw() +
  theme(panel.background = element_blank(),
        legend.position = c(0, 1.03), legend.justification = c(0,1),
        legend.title = element_blank(),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        # panel.border = element_blank(),
        strip.text = element_text(size = 12),
        plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
        strip.background = element_blank()) +
  labs(y = "Predicted share to URNG", x = "State violence (log killings / 1000 hab)")
dev.off()

# Pred probs: FRG & roads/PanAm by year
FRG_roads_yrs = rbind(
  predprob_df(newdata, m_dcha_roads99, label = "1999"),
  predprob_df(newdata, m_dcha_roads03, label = "2003"),
  predprob_df(newdata, m_dcha_roads07, label = "2007"),
  predprob_df(newdata, m_dcha_roads15, label = "2015"))
FRG_panam_yrs = rbind(
  predprob_df(newdata, m_dcha_panam99, label = "1999"),
  predprob_df(newdata, m_dcha_panam03, label = "2003"),
  predprob_df(newdata, m_dcha_panam07, label = "2007"),
  predprob_df(newdata, m_dcha_panam15, label = "2015"))
FRG_roads_yrs$roads_dirt_sh = factor(FRG_roads_yrs$roads_dirt_sh)
levels(FRG_roads_yrs$roads_dirt_sh) = c("All roads paved", "50% non-paved roads", "All non-paved")
FRG_panam_yrs$ldist_panam = factor(FRG_panam_yrs$ldist_panam)
levels(FRG_panam_yrs$ldist_panam) = c("Next to PanAm Highway", "ca. 20km away", "ca. 400km away")

# Plot: FRG & roads/PanAm by year
pdf("../../../dissertation/empirics/guatemala/pp_frg_roads_yrs.pdf",
  height = 6, width = 7)
ggplot(FRG_roads_yrs, aes(y = y, x = govt_vi_l)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  facet_grid(label~roads_dirt_sh) +
  theme_bw() +
  theme(panel.background = element_blank(),
        legend.position = c(0, 1.03), legend.justification = c(0,1),
        legend.title = element_blank(),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        # panel.border = element_blank(),
        strip.text = element_text(size = 12),
        plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
        strip.background = element_blank()) +
  labs(y = "Predicted share to FRG", x = "State violence (log killings / 1000 hab)")
dev.off()

pdf("../../../dissertation/empirics/guatemala/pp_frg_panam_yrs.pdf",
  height = 6, width = 7)
ggplot(FRG_panam_yrs, aes(y = y, x = govt_vi_l)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  facet_grid(label~ldist_panam) +
  theme_bw() +
  theme(panel.background = element_blank(),
        legend.position = c(0, 1.03), legend.justification = c(0,1),
        legend.title = element_blank(),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        # panel.border = element_blank(),
        strip.text = element_text(size = 12),
        plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
        strip.background = element_blank()) +
  labs(y = "Predicted share to FRG", x = "State violence (log killings / 1000 hab)")
dev.off()

# Using FRG + PP as dep var -----------------------------

m_fulldcha_roads = lm(fulldcha ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = datal)
m_fulldcha_hv_roads = lm(fulldcha ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = subset(datal, dpto %in% high_vio))
m_fulldcha_panam = lm(fulldcha ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = datal)
m_fulldcha_hv_panam = lm(fulldcha ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = subset(datal, dpto %in% high_vio))


# Table: FRG & PP (both mediators)
fileconnection = file("../../../dissertation/empirics/guatemala/table_full_dcha.tex")
notes_table = "\\parbox[t]{0.75\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. All models pool all observations across all elections, including election FEs. Department FEs included, not shown. Most affected departments include Huehuetenango, Chimaltenango, Quiché, Alta Verapaz, Baja Verapaz, and Petén.}"
writeLines(
  stargazer(m_fulldcha_roads, m_fulldcha_hv_roads, m_fulldcha_panam, m_fulldcha_hv_panam,
    title = "Wartime violence, prewar mobilization, and voting for FRG and Partido Patriota",
    label = "tab-gt:full_dcha",
    omit = "dpto",
    omit.stat = c("f", "ser"), intercept.bottom = F,
    order = c("Constant", "govt_vi_l$", "dirt_sh$", "panam$"),
    covariate.labels = c("(Intercept)", "State-led killings",
      "\\% Non-paved roads", "Violence $\\times$ Non-paved",
      "Log. Dist to Pan-Am Hwy", "Violence $\\times$ Dist to Pan-Am",
      "Log. Population 1973", "\\% Indigenous 1973",
      "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
      "Rebel violence pre-78", "2003 election", "2007 election", "2011 election",
      "2015 election"),
    multicolumn = FALSE,
    column.sep.width = "-20pt",
    dep.var.caption = "FRG + PP share",
    dep.var.labels.include = FALSE,
    column.labels = c("", hv_dpts_note), column.separate = c(2, 2),
    font.size = "small",
    digits = 3, digits.extra = 0,
    star.char = c("+", "*", "**", "***"),
    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
    notes.align = "c", align = T, no.space = TRUE,
    notes.label = "", notes.append = F,
    add.lines=list(c("Department FE", rep("\\multicolumn{1}{c}{Yes}", 4))),
    notes = notes_table),
fileconnection)
close(fileconnection)


fulldcha_roads = predprob_df(newdata, m_fulldcha_roads, label = "fulldcha_roads")
fulldcha_panam = predprob_df(newdata, m_fulldcha_panam, label = "fulldcha_panam")

fulldcha_roads$roads_dirt_sh = factor(fulldcha_roads$roads_dirt_sh)
levels(fulldcha_roads$roads_dirt_sh) = c("All roads paved", "50% non-paved roads", "All non-paved")
fulldcha_panam$ldist_panam = factor(fulldcha_panam$ldist_panam)
levels(fulldcha_panam$ldist_panam) = c("Next to PanAm Highway", "ca. 20km away", "ca. 400km away")

# Plot: fulldcha & roads
pdf("../../../dissertation/empirics/guatemala/pp_fulldcha_roads.pdf",
  height = 3, width = 7)
ggplot(fulldcha_roads, aes(y = y, x = govt_vi_l)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  facet_wrap(~roads_dirt_sh) +
  theme_classic() +
  theme(panel.background = element_blank(),
        legend.position = c(0, 1.03), legend.justification = c(0,1),
        legend.title = element_blank(),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size = 12),
        plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
        strip.background = element_blank()) +
  labs(y = "Predicted share to FRG + Partido Patriota",
    x = "State violence (log killings / 1000 hab)")
dev.off()

# Plots: fulldcha & dist to PanAm Hwy
pdf("../../../dissertation/empirics/guatemala/pp_fulldcha_panam.pdf",
  height = 3, width = 7)
ggplot(fulldcha_panam, aes(y = y, x = govt_vi_l)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  facet_wrap(~ldist_panam) +
  theme_classic() +
  theme(panel.background = element_blank(),
        legend.position = c(0, 1.03), legend.justification = c(0,1),
        legend.title = element_blank(),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size = 12),
        plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
        strip.background = element_blank()) +
  labs(y = "Predicted share to FRG + Partido Patriota", x = "State violence (log killings / 1000 hab)")
dev.off()

# Using only CEH as violence variable ----------------

m_ceh_izq_roads = lm(URNGcia ~ ceh_govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = datal)
m_ceh_izq_hv_roads = lm(URNGcia ~ ceh_govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = subset(datal, dpto %in% high_vio))
m_ceh_dcha_roads = lm(FRG ~ ceh_govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = datal)
m_ceh_dcha_hv_roads = lm(FRG ~ ceh_govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = subset(datal, dpto %in% high_vio))

m_ceh_izq_panam = lm(URNGcia ~ ceh_govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = datal)
m_ceh_izq_hv_panam = lm(URNGcia ~ ceh_govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = subset(datal, dpto %in% high_vio))
m_ceh_dcha_panam = lm(FRG ~ ceh_govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = datal)
m_ceh_dcha_hv_panam = lm(FRG ~ ceh_govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = subset(datal, dpto %in% high_vio))

# Table: (CEH data) URNG/FRG and paved roads
fileconnection = file("../../../dissertation/empirics/guatemala/table_roads_nonpaved_ceh.tex")
notes_table = "\\parbox[t]{0.75\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. All models pool all observations across all elections, including election FEs. Department FEs included, not shown. Most affected departments include Huehuetenango, Chimaltenango, Quiché, Alta Verapaz, Baja Verapaz, and Petén.}"
writeLines(
  stargazer(m_ceh_izq_roads, m_ceh_dcha_roads, m_ceh_izq_hv_roads, m_ceh_dcha_hv_roads,
    title = "Wartime violence (using only CEH), prewar mobilization, and voting",
    label = "tab-gt:int_roads_ceh",
    omit = "dpto",
    omit.stat = c("f", "ser"), intercept.bottom = F,
    order = c("Constant", "ceh_govt_vi_l$", "dirt_sh$"),
    covariate.labels = c("(Intercept)", "State-led killings",
      "\\% Non-paved roads", "Violence $\\times$ Non-paved",
      "Log. Population 1973", "\\% Indigenous 1973",
      "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
      "Rebel violence pre-78", "2003 election", "2007 election", "2011 election",
      "2015 election"),
    multicolumn = FALSE,
    column.sep.width = "-20pt",
    dep.var.caption = "",
    dep.var.labels = c("URNG", "FRG", "URNG", "FRG"),
    column.labels = c("", hv_dpts_note), column.separate = c(2, 2),
    font.size = "small",
    digits = 3, digits.extra = 0,
    star.char = c("+", "*", "**", "***"),
    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
    notes.align = "c", align = T, no.space = TRUE,
    notes.label = "", notes.append = F,
    add.lines=list(c("Department FE", rep("\\multicolumn{1}{c}{Yes}", 4))),
    notes = notes_table),
fileconnection)
close(fileconnection)

# Table: (CEH data) URNG/FRG and paved roads
fileconnection = file("../../../dissertation/empirics/guatemala/table_dist_panam_ceh.tex")
notes_table = "\\parbox[t]{0.75\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. All models pool all observations across all elections, including election FEs. Department FEs included, not shown. Most affected departments include Huehuetenango, Chimaltenango, Quiché, Alta Verapaz, Baja Verapaz, and Petén.}"
writeLines(
  stargazer(m_ceh_izq_panam, m_ceh_dcha_panam, m_ceh_izq_hv_panam, m_ceh_dcha_hv_panam,
    title = "Wartime violence (using only CEH), prewar mobilization, and voting",
    label = "tab-gt:int_panam_ceh",
    omit = "dpto",
    omit.stat = c("f", "ser"), intercept.bottom = F,
    order = c("Constant", "ceh_govt_vi_l$", "panam$"),
    covariate.labels = c("(Intercept)", "State-led killings",
      "Log. Dist to Pan-Am Hwy", "Violence $\\times$ Dist to Pan-Am",
      "Log. Population 1973", "\\% Indigenous 1973",
      "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
      "Rebel violence pre-78", "2003 election", "2007 election", "2011 election",
      "2015 election"),
    multicolumn = FALSE,
    column.sep.width = "-20pt",
    dep.var.caption = "",
    dep.var.labels = c("URNG", "FRG", "URNG", "FRG"),
    column.labels = c("", hv_dpts_note), column.separate = c(2, 2),
    font.size = "small",
    digits = 3, digits.extra = 0,
    star.char = c("+", "*", "**", "***"),
    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
    notes.align = "c", align = T, no.space = TRUE,
    notes.label = "", notes.append = F,
    add.lines=list(c("Department FE", rep("\\multicolumn{1}{c}{Yes}", 4))),
    notes = notes_table),
fileconnection)
close(fileconnection)
