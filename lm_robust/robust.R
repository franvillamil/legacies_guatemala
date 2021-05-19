# setwd("~/Documents/Projects/legacies_guatemala")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("stargazer", "dplyr", "tidyr", "ggplot2")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))}
# Load
lapply(pkg, library, character.only = TRUE)

# -----------------------------------------------------------------
# PREPARATION

# Functions
source("func/predprob_df.R")
source("func/my_stargazer.R")

# Load data
data = read.csv("dataset/output/data.csv")

# High-violence departments and note for stargazer
high_vio = c("Alta Verapaz", "Baja Verapaz",
  "Peten", "Huehuetenango", "Quiche", "Chimaltenango")

# Long-form dataset
dl = data %>%
  gather(key, value, matches("(URNGcia|FRG|fulldcha|fullizq)(\\d+)")) %>%
  extract(key, c("party", "elec"), "(URNGcia|FRG|fulldcha|fullizq)(\\d+)") %>%
  spread(party, value)

# -----------------------------------------------------------------
# Using only CEH data for violence variable

m_ceh_URNG_roads = lm(URNGcia ~ ceh_govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = dl)
m_ceh_URNG_hv_roads = lm(URNGcia ~ ceh_govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = subset(dl, dpto %in% high_vio))
m_ceh_FRG_roads = lm(FRG ~ ceh_govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = dl)
m_ceh_FRG_hv_roads = lm(FRG ~ ceh_govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = subset(dl, dpto %in% high_vio))

m_ceh_URNG_panam = lm(URNGcia ~ ceh_govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = dl)
m_ceh_URNG_hv_panam = lm(URNGcia ~ ceh_govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = subset(dl, dpto %in% high_vio))
m_ceh_FRG_panam = lm(FRG ~ ceh_govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = dl)
m_ceh_FRG_hv_panam = lm(FRG ~ ceh_govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = subset(dl, dpto %in% high_vio))

my_stargazer(dest_file = "lm_robust/output/tab_lm_roads_ceh.tex",
  model_list = list(m_ceh_URNG_roads, m_ceh_FRG_roads, m_ceh_URNG_hv_roads, m_ceh_FRG_hv_roads),
  title = "Wartime violence (using only CEH), local road network, and voting",
  label = "tab:lm_roads_ceh",
  order = c("Constant", "ceh_govt_vi_l$", "dirt_sh$"),
  covariate.labels = c("(Intercept)", "State-led killings",
    "\\% Non-paved roads", "Violence $\\times$ Non-paved",
    "Log. Population 1973", "\\% Indigenous 1973", "\\% Literate 1973",
    "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
    "Rebel violence pre-78"))

my_stargazer(dest_file = "lm_robust/output/tab_lm_panam_ceh.tex",
  model_list = list(m_ceh_URNG_panam, m_ceh_FRG_panam, m_ceh_URNG_hv_panam, m_ceh_FRG_hv_panam),
  title = "Wartime violence (using only CEH), distance to PanAm Hwy, and voting",
  label = "tab:lm_panam_ceh",
  order = c("Constant", "ceh_govt_vi_l$", "panam$"),
  covariate.labels = c("(Intercept)", "State-led killings",
    "Log. Dist to Pan-Am Hwy", "Violence $\\times$ Dist to Pan-Am",
    "Log. Population 1973", "\\% Indigenous 1973", "\\% Literate 1973",
    "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
    "Rebel violence pre-78"))

# -----------------------------------------------------------------
# Include also PP (and FCN in 2015) as rightist parties

m_fulldcha_roads = lm(fulldcha ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = dl)
m_fulldcha_hv_roads = lm(fulldcha ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = subset(dl, dpto %in% high_vio))
m_fulldcha_panam = lm(fulldcha ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = dl)
m_fulldcha_hv_panam = lm(fulldcha ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = subset(dl, dpto %in% high_vio))

my_stargazer(dest_file = "lm_robust/output/tab_fulldcha.tex",
  model_list = list(m_fulldcha_roads, m_fulldcha_hv_roads,
    m_fulldcha_panam, m_fulldcha_hv_panam),
  title = "Wartime violence, prewar mobilization, and voting for FRG, Partido Patriota, and FCN",
  label = "tab:lm_fulldcha",
  order = c("Constant", "govt_vi_l$", "dirt_sh$", "panam$"),
  covariate.labels = c("(Intercept)", "State-led killings",
    "\\% Non-paved roads", "Violence $\\times$ Non-paved",
    "Log. Dist to Pan-Am Hwy", "Violence $\\times$ Dist to Pan-Am",
    "Log. Population 1973", "\\% Indigenous 1973", "\\% Literate 1973",
    "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
    "Rebel violence pre-78"))

# Predicted probability plot
# New dataframe
newdata = expand.grid(govt_vi_l = seq(0, 5, 1), roads_dirt_sh = c(0, 0.5, 1))
newdata$ldist_panam = rep(c(0, 3, 6), each = 6)
# And rest of variables
newdata$lpop73 = mean(data$lpop73, na.rm = T)
newdata$ind73 = mean(data$ind73, na.rm = T)
newdata$lit73 = mean(data$ind73, na.rm = T)
newdata$elev_sd = mean(data$elev_sd, na.rm = T)
newdata$forest = mean(data$forest, na.rm = T)
newdata$ldist_guate = mean(data$ldist_guate, na.rm = T)
newdata$larea = mean(data$larea, na.rm = T)
newdata$rebels_vi_pre78_l = mean(data$rebels_vi_pre78_l, na.rm = T)
newdata$dpto = "Quiche"
newdata$elec = "1999"

fulldcha_roads = predprob_df(newdata, m_fulldcha_roads, label = "fulldcha_roads")
fulldcha_panam = predprob_df(newdata, m_fulldcha_panam, label = "fulldcha_panam")

fulldcha_roads$roads_dirt_sh = factor(fulldcha_roads$roads_dirt_sh)
levels(fulldcha_roads$roads_dirt_sh) = c("All roads paved", "50% non-paved roads", "All non-paved")
fulldcha_panam$ldist_panam = factor(fulldcha_panam$ldist_panam)
levels(fulldcha_panam$ldist_panam) = c("Next to PanAm Highway", "ca. 20km away", "ca. 400km away")

pdf("lm_robust/output/pp_fulldcha_roads.pdf", height = 3, width = 7)
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
  labs(y = "Predicted share to FRG + PP + FCN",
    x = "State violence (log killings / 1000 hab)")
dev.off()

pdf("lm_robust/output/pp_fulldcha_panam.pdf",
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
  labs(y = "Predicted share to FRG + PP + FCN",
    x = "State violence (log killings / 1000 hab)")
dev.off()

# -----------------------------------------------------------------
# Include also UNE as leftist

m_fullizq_roads = lm(fullizq ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = dl)
m_fullizq_hv_roads = lm(fullizq ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = subset(dl, dpto %in% high_vio))
m_fullizq_panam = lm(fullizq ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = dl)
m_fullizq_hv_panam = lm(fullizq ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = subset(dl, dpto %in% high_vio))

my_stargazer(dest_file = "lm_robust/output/tab_fullizq.tex",
  model_list = list(m_fullizq_roads, m_fullizq_hv_roads,
    m_fullizq_panam, m_fullizq_hv_panam),
  title = "Wartime violence, prewar mobilization, and voting for URNG and UNE",
  label = "tab:lm_fullizq",
  order = c("Constant", "govt_vi_l$", "dirt_sh$", "panam$"),
  covariate.labels = c("(Intercept)", "State-led killings",
    "\\% Non-paved roads", "Violence $\\times$ Non-paved",
    "Log. Dist to Pan-Am Hwy", "Violence $\\times$ Dist to Pan-Am",
    "Log. Population 1973", "\\% Indigenous 1973", "\\% Literate 1973",
    "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
    "Rebel violence pre-78"))

# Predicted probability plot
# New dataframe
newdata = expand.grid(govt_vi_l = seq(0, 5, 1), roads_dirt_sh = c(0, 0.5, 1))
newdata$ldist_panam = rep(c(0, 3, 6), each = 6)
# And rest of variables
newdata$lpop73 = mean(data$lpop73, na.rm = T)
newdata$ind73 = mean(data$ind73, na.rm = T)
newdata$lit73 = mean(data$ind73, na.rm = T)
newdata$elev_sd = mean(data$elev_sd, na.rm = T)
newdata$forest = mean(data$forest, na.rm = T)
newdata$ldist_guate = mean(data$ldist_guate, na.rm = T)
newdata$larea = mean(data$larea, na.rm = T)
newdata$rebels_vi_pre78_l = mean(data$rebels_vi_pre78_l, na.rm = T)
newdata$dpto = "Quiche"
newdata$elec = "1999"

fullizq_roads = predprob_df(newdata, m_fullizq_roads, label = "fullizq_roads")
fullizq_panam = predprob_df(newdata, m_fullizq_panam, label = "fullizq_panam")

fullizq_roads$roads_dirt_sh = factor(fullizq_roads$roads_dirt_sh)
levels(fullizq_roads$roads_dirt_sh) = c("All roads paved", "50% non-paved roads", "All non-paved")
fullizq_panam$ldist_panam = factor(fullizq_panam$ldist_panam)
levels(fullizq_panam$ldist_panam) = c("Next to PanAm Highway", "ca. 20km away", "ca. 400km away")

pdf("lm_robust/output/pp_fullizq_roads.pdf", height = 3, width = 7)
ggplot(fullizq_roads, aes(y = y, x = govt_vi_l)) +
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
  labs(y = "Predicted share to URNG + UNE",
    x = "State violence (log killings / 1000 hab)")
dev.off()

pdf("lm_robust/output/pp_fullizq_panam.pdf",
  height = 3, width = 7)
ggplot(fullizq_panam, aes(y = y, x = govt_vi_l)) +
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
  labs(y = "Predicted share to URNG + UNE",
    x = "State violence (log killings / 1000 hab)")
dev.off()

# -----------------------------------------------------------------
# Main-text models by year

# Base
m_URNG99 = lm(URNGcia1999 ~ govt_vi_l +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_URNG03 = lm(URNGcia2003 ~ govt_vi_l +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_URNG07 = lm(URNGcia2007 ~ govt_vi_l +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_URNG11 = lm(URNGcia2011 ~ govt_vi_l +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_URNG15 = lm(URNGcia2015 ~ govt_vi_l +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_FRG99 = lm(FRG1999 ~ govt_vi_l +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_FRG03 = lm(FRG2003 ~ govt_vi_l +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_FRG07 = lm(FRG2007 ~ govt_vi_l +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_FRG15 = lm(FRG2015 ~ govt_vi_l +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)

# Interaction with roads
m_URNG_roads99 = lm(URNGcia1999 ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_URNG_roads03 = lm(URNGcia2003 ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_URNG_roads07 = lm(URNGcia2007 ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_URNG_roads11 = lm(URNGcia2011 ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_URNG_roads15 = lm(URNGcia2015 ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_FRG_roads99 = lm(FRG1999 ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_FRG_roads03 = lm(FRG2003 ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_FRG_roads07 = lm(FRG2007 ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_FRG_roads15 = lm(FRG2015 ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)

# Interaction with distance to Pan-American Highway
m_URNG_panam99 = lm(URNGcia1999 ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_URNG_panam03 = lm(URNGcia2003 ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_URNG_panam07 = lm(URNGcia2007 ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_URNG_panam11 = lm(URNGcia2011 ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_URNG_panam15 = lm(URNGcia2015 ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_FRG_panam99 = lm(FRG1999 ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_FRG_panam03 = lm(FRG2003 ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_FRG_panam07 = lm(FRG2007 ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)
m_FRG_panam15 = lm(FRG2015 ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto), data = data)

mlist_URNG = list(m_URNG99, m_URNG03, m_URNG07, m_URNG11, m_URNG15)
mlist_URNG_roads = list(m_URNG_roads99, m_URNG_roads03,
  m_URNG_roads07, m_URNG_roads11, m_URNG_roads15)
mlist_URNG_panam = list(m_URNG_panam99, m_URNG_panam03,
  m_URNG_panam07, m_URNG_panam11, m_URNG_panam15)
mlist_FRG = list(m_FRG99, m_FRG03, m_FRG07, m_FRG15)
mlist_FRG_roads = list(m_FRG_roads99, m_FRG_roads03, m_FRG_roads07, m_FRG_roads15)
mlist_FRG_panam = list(m_FRG_panam99, m_FRG_panam03, m_FRG_panam07, m_FRG_panam15)

my_stargazer(dest_file = "lm_robust/output/tab_URNG_base_year.tex",
  model_list = mlist_URNG,
  title = "Wartime violence and URNG share, by year (base models)",
  label = "tab:lm_URNG_base_year",
  order = c("Constant", "govt_vi_l"),
  covariate.labels = c("(Intercept)", "State-led killings",
  "Log. Population 1973", "\\% Indigenous 1973", "\\% Literate 1973",
  "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
  "Rebel violence pre-78"),
  dep.var.labels = c("1999", "2003", "2007", "2011", "2015"),
  add.lines = list(c("Department FE", rep("\\multicolumn{1}{c}{Yes}", length(mlist_URNG)))),
  column.labels = NULL, column.separate = NULL,
  notes_table = "\\parbox[t]{0.65\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Each model includes cross-sectional data on a specific election. Deparment FE not shown.}")

my_stargazer(dest_file = "lm_robust/output/tab_FRG_base_year.tex",
  model_list = mlist_FRG,
  title = "Wartime violence and FRG share, by year (base models)",
  label = "tab:lm_FRG_base_year",
  order = c("Constant", "govt_vi_l"),
  covariate.labels = c("(Intercept)", "State-led killings",
  "Log. Population 1973", "\\% Indigenous 1973", "\\% Literate 1973",
  "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
  "Rebel violence pre-78"),
  dep.var.labels = c("1999", "2003", "2007", "2015"),
  add.lines = list(c("Department FE", rep("\\multicolumn{1}{c}{Yes}", length(mlist_FRG)))),
  column.labels = NULL, column.separate = NULL,
  notes_table = "\\parbox[t]{0.65\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Each model includes cross-sectional data on a specific election. Deparment FE not shown.}")

my_stargazer(dest_file = "lm_robust/output/tab_URNG_roads_year.tex",
  model_list = mlist_URNG_roads,
  title = "Wartime violence and URNG share, by year (interaction, roads)",
  label = "tab:lm_URNG_roads_year",
  order = c("Constant", "govt_vi_l$", "dirt_sh$"),
  covariate.labels = c("(Intercept)", "State-led killings",
    "\\% Non-paved roads", "Violence $\\times$ Non-paved",
    "Log. Population 1973", "\\% Indigenous 1973", "\\% Literate 1973",
    "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
    "Rebel violence pre-78"),
  dep.var.labels = c("1999", "2003", "2007", "2011", "2015"),
  add.lines = list(c("Department FE", rep("\\multicolumn{1}{c}{Yes}", length(mlist_URNG_roads)))),
  column.labels = NULL, column.separate = NULL,
  notes_table = "\\parbox[t]{0.65\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Each model includes cross-sectional data on a specific election. Deparment FE not shown.}")

my_stargazer(dest_file = "lm_robust/output/tab_FRG_roads_year.tex",
  model_list = mlist_FRG_roads,
  title = "Wartime violence and FRG share, by year (interaction, roads)",
  label = "tab:lm_FRG_roads_year",
  order = c("Constant", "govt_vi_l$", "dirt_sh$"),
  covariate.labels = c("(Intercept)", "State-led killings",
    "\\% Non-paved roads", "Violence $\\times$ Non-paved",
    "Log. Population 1973", "\\% Indigenous 1973", "\\% Literate 1973",
    "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
    "Rebel violence pre-78"),
  dep.var.labels = c("1999", "2003", "2007", "2015"),
  add.lines = list(c("Department FE", rep("\\multicolumn{1}{c}{Yes}", length(mlist_FRG_roads)))),
  column.labels = NULL, column.separate = NULL,
  notes_table = "\\parbox[t]{0.65\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Each model includes cross-sectional data on a specific election. Deparment FE not shown.}")

my_stargazer(dest_file = "lm_robust/output/tab_URNG_panam_year.tex",
  model_list = mlist_URNG_panam,
  title = "Wartime violence and URNG share, by year (interaction, PanAm)",
  label = "tab:lm_URNG_panam_year",
  order = c("Constant", "govt_vi_l$", "panam$"),
  covariate.labels = c("(Intercept)", "State-led killings",
    "Log. Dist to Pan-Am Hwy", "Violence $\\times$ Dist to Pan-Am",
    "Log. Population 1973", "\\% Indigenous 1973", "\\% Literate 1973",
    "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
    "Rebel violence pre-78"),
  dep.var.labels = c("1999", "2003", "2007", "2011", "2015"),
  add.lines = list(c("Department FE", rep("\\multicolumn{1}{c}{Yes}", length(mlist_URNG_panam)))),
  column.labels = NULL, column.separate = NULL,
  notes_table = "\\parbox[t]{0.65\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Each model includes cross-sectional data on a specific election. Deparment FE not shown.}")

my_stargazer(dest_file = "lm_robust/output/tab_FRG_panam_year.tex",
  model_list = mlist_FRG_panam,
  title = "Wartime violence and FRG share, by year (interaction, PanAm)",
  label = "tab:lm_FRG_panam_year",
  order = c("Constant", "govt_vi_l$", "panam$"),
  covariate.labels = c("(Intercept)", "State-led killings",
    "Log. Dist to Pan-Am Hwy", "Violence $\\times$ Dist to Pan-Am",
    "Log. Population 1973", "\\% Indigenous 1973", "\\% Literate 1973",
    "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
    "Rebel violence pre-78"),
  dep.var.labels = c("1999", "2003", "2007", "2015"),
  add.lines = list(c("Department FE", rep("\\multicolumn{1}{c}{Yes}", length(mlist_FRG_panam)))),
  column.labels = NULL, column.separate = NULL,
  notes_table = "\\parbox[t]{0.65\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Each model includes cross-sectional data on a specific election. Deparment FE not shown.}")


# Predicted probability plots, by year
# URNG & roads/PanAm
URNG_roads_year = rbind(
  predprob_df(newdata, m_URNG_roads99, label = "1999"),
  predprob_df(newdata, m_URNG_roads03, label = "2003"),
  predprob_df(newdata, m_URNG_roads07, label = "2007"),
  predprob_df(newdata, m_URNG_roads11, label = "2011"),
  predprob_df(newdata, m_URNG_roads15, label = "2015"))
URNG_panam_year = rbind(
  predprob_df(newdata, m_URNG_panam99, label = "1999"),
  predprob_df(newdata, m_URNG_panam03, label = "2003"),
  predprob_df(newdata, m_URNG_panam07, label = "2007"),
  predprob_df(newdata, m_URNG_panam11, label = "2011"),
  predprob_df(newdata, m_URNG_panam15, label = "2015"))
URNG_roads_year$roads_dirt_sh = factor(URNG_roads_year$roads_dirt_sh)
levels(URNG_roads_year$roads_dirt_sh) = c("All roads paved", "50% non-paved roads", "All non-paved")
URNG_panam_year$ldist_panam = factor(URNG_panam_year$ldist_panam)
levels(URNG_panam_year$ldist_panam) = c("Next to PanAm Highway", "ca. 20km away", "ca. 400km away")

# Plot: URNG & roads/PanAm by year
pdf("lm_robust/output/pp_URNG_roads_year.pdf", height = 8, width = 7)
ggplot(URNG_roads_year, aes(y = y, x = govt_vi_l)) +
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

pdf("lm_robust/output/pp_URNG_panam_year.pdf", height = 8, width = 7)
ggplot(URNG_panam_year, aes(y = y, x = govt_vi_l)) +
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

# FRG & roads/PanAm
FRG_roads_year = rbind(
  predprob_df(newdata, m_FRG_roads99, label = "1999"),
  predprob_df(newdata, m_FRG_roads03, label = "2003"),
  predprob_df(newdata, m_FRG_roads07, label = "2007"),
  predprob_df(newdata, m_FRG_roads15, label = "2015"))
FRG_panam_year = rbind(
  predprob_df(newdata, m_FRG_panam99, label = "1999"),
  predprob_df(newdata, m_FRG_panam03, label = "2003"),
  predprob_df(newdata, m_FRG_panam07, label = "2007"),
  predprob_df(newdata, m_FRG_panam15, label = "2015"))
FRG_roads_year$roads_dirt_sh = factor(FRG_roads_year$roads_dirt_sh)
levels(FRG_roads_year$roads_dirt_sh) = c("All roads paved", "50% non-paved roads", "All non-paved")
FRG_panam_year$ldist_panam = factor(FRG_panam_year$ldist_panam)
levels(FRG_panam_year$ldist_panam) = c("Next to PanAm Highway", "ca. 20km away", "ca. 400km away")

# Plot: FRG & roads/PanAm by year
pdf("lm_robust/output/pp_FRG_roads_year.pdf", height = 6, width = 7)
ggplot(FRG_roads_year, aes(y = y, x = govt_vi_l)) +
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

pdf("lm_robust/output/pp_FRG_panam_year.pdf", height = 6, width = 7)
ggplot(FRG_panam_year, aes(y = y, x = govt_vi_l)) +
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
