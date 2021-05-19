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
  gather(key, value, matches("(URNGcia|FRG)(\\d+)")) %>%
  extract(key, c("party", "elec"), "(URNGcia|FRG)(\\d+)") %>%
  spread(party, value)

# -----------------------------------------------------------------
# MAIN MODELS

# Base models
m_URNG = lm(URNGcia ~ govt_vi_l +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = dl)
m_URNG_hv = lm(URNGcia ~ govt_vi_l +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = subset(dl, dpto %in% high_vio))
m_FRG = lm(FRG ~ govt_vi_l +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = dl)
m_FRG_hv = lm(FRG ~ govt_vi_l +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = subset(dl, dpto %in% high_vio))

my_stargazer(dest_file = "lm/output/tab_lm_base.tex",
  model_list = list(m_URNG, m_FRG, m_URNG_hv, m_FRG_hv),
  title = "Base models on wartime violence and postwar voting",
  label = "tab:lm_base",
  order = c("Constant", "govt_vi_l", "lpop", "lpop73", "ind73", "lit73",
    "elev_sd", "forest", "ldist_guate", "larea", "rebels_vi"),
  covariate.labels = c("(Intercept)", "State-led killings",
    "Log. Population 1973", "\\% Indigenous 1973", "\\% Literate 1973",
    "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
    "Rebel violence pre-78"))

# Interaction with roads
m_URNG_roads = lm(URNGcia ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = dl)
m_URNG_hv_roads = lm(URNGcia ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = subset(dl, dpto %in% high_vio))
m_FRG_roads = lm(FRG ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = dl)
m_FRG_hv_roads = lm(FRG ~ govt_vi_l * roads_dirt_sh +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = subset(dl, dpto %in% high_vio))

my_stargazer(dest_file = "lm/output/tab_lm_roads.tex",
  model_list = list(m_URNG_roads, m_FRG_roads, m_URNG_hv_roads, m_FRG_hv_roads),
  title = "Wartime violence, local road network, and voting",
  label = "tab:lm_roads",
  order = c("Constant", "govt_vi_l$", "dirt_sh$"),
  covariate.labels = c("(Intercept)", "State-led killings",
    "\\% Non-paved roads", "Violence $\\times$ Non-paved",
    "Log. Population 1973", "\\% Indigenous 1973", "\\% Literate 1973",
    "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
    "Rebel violence pre-78"))


# Interaction with dist to PanAm
m_URNG_panam = lm(URNGcia ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = dl)
m_URNG_hv_panam = lm(URNGcia ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = subset(dl, dpto %in% high_vio))
m_FRG_panam = lm(FRG ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = dl)
m_FRG_hv_panam = lm(FRG ~ govt_vi_l * ldist_panam +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate + larea + rebels_vi_pre78_l +
  factor(dpto) + factor(elec), data = subset(dl, dpto %in% high_vio))

my_stargazer(dest_file = "lm/output/tab_lm_panam.tex",
  model_list = list(m_URNG_panam, m_FRG_panam, m_URNG_hv_panam, m_FRG_hv_panam),
  title = "Wartime violence, distance to PanAm Highway, and voting",
  label = "tab:lm_panam",
  order = c("Constant", "govt_vi_l$", "panam$"),
  covariate.labels = c("(Intercept)", "State-led killings",
    "Log. Dist to Pan-Am Hwy", "Violence $\\times$ Dist to Pan-Am",
    "Log. Population 1973", "\\% Indigenous 1973", "\\% Literate 1973",
    "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
    "Rebel violence pre-78"))


# -----------------------------------------------------------------
# PREDICTED PROBABILITY PLOTS

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

# Predicted probabilities
URNG_roads = predprob_df(newdata, m_URNG_roads, label = "URNG_roads")
URNG_panam = predprob_df(newdata, m_URNG_panam, label = "URNG_panam")
FRG_roads = predprob_df(newdata, m_FRG_roads, label = "FRG_roads")
FRG_panam = predprob_df(newdata, m_FRG_panam, label = "FRG_panam")

# Change labels
URNG_roads$roads_dirt_sh = factor(URNG_roads$roads_dirt_sh)
levels(URNG_roads$roads_dirt_sh) = c("All roads paved", "50% non-paved roads", "All non-paved")
FRG_roads$roads_dirt_sh = factor(FRG_roads$roads_dirt_sh)
levels(FRG_roads$roads_dirt_sh) = c("All roads paved", "50% non-paved roads", "All non-paved")
URNG_panam$ldist_panam = factor(URNG_panam$ldist_panam)
levels(URNG_panam$ldist_panam) = c("Next to PanAm Highway", "ca. 20km away", "ca. 400km away")
FRG_panam$ldist_panam = factor(FRG_panam$ldist_panam)
levels(FRG_panam$ldist_panam) = c("Next to PanAm Highway", "ca. 20km away", "ca. 400km away")

# Plot: URNG & roads
pdf("lm/output/pp_URNG_roads.pdf", height = 3, width = 7)
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
pdf("lm/output/pp_URNG_panam.pdf", height = 3, width = 7)
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
pdf("lm/output/pp_FRG_roads.pdf", height = 3, width = 7)
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
pdf("lm/output/pp_FRG_panam.pdf", height = 3, width = 7)
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
