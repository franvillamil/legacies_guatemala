setwd("~/Documents/Projects/legacies_guatemala")
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

# -----------------------------------------------------------------
# USING VIOLENCE AS DEPENDENT VARIABLE

# Violence by the state
m_govt_vi_noFE_basic = lm(govt_vi_l ~ roads_dirt_sh + ldist_panam,
  data = data)
m_govt_vi_basic = lm(govt_vi_l ~ roads_dirt_sh + ldist_panam + factor(dpto),
  data = data)
m_govt_vi_noFE = lm(govt_vi_l ~ roads_dirt_sh + ldist_panam +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate +
  larea + rebels_vi_pre78_l,
  data = data)
m_govt_vi = lm(govt_vi_l ~ roads_dirt_sh + ldist_panam +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate +
  larea + rebels_vi_pre78_l + factor(dpto),
  data = data)

m_govt_vi_noFE_basic_hv = lm(govt_vi_l ~ roads_dirt_sh + ldist_panam,
  data = subset(data, dpto %in% high_vio))
m_govt_vi_basic_hv = lm(govt_vi_l ~ roads_dirt_sh + ldist_panam + factor(dpto),
  data = subset(data, dpto %in% high_vio))
m_govt_vi_noFE_hv = lm(govt_vi_l ~ roads_dirt_sh + ldist_panam +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate +
  larea + rebels_vi_pre78_l,
  data = subset(data, dpto %in% high_vio))
m_govt_vi_hv = lm(govt_vi_l ~ roads_dirt_sh + ldist_panam +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate +
  larea + rebels_vi_pre78_l + factor(dpto),
  data = subset(data, dpto %in% high_vio))

my_stargazer(dest_file = "alt_exp/output/tab_govt_vi.tex",
  model_list = list(m_govt_vi_noFE_basic, m_govt_vi_basic, m_govt_vi_noFE, m_govt_vi),
  title = "Determinants of wartime violence by the state",
  label = "tab:lm_govt_vi",
  order = c("Constant", "dirt_sh$", "panam$"),
  covariate.labels = c("(Intercept)",
    "\\% Roads non-paved", "Log. Distance to Pan-Am Hwy",
    "Log. Population 1973", "\\% Indigenous 1973", "\\% Literate 1973",
    "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
    "Rebel violence pre-78"),
  dep.var.labels.include = FALSE,
  dep.var.labels = NULL,
  add.lines = list(c("Department FE",
    "\\multicolumn{1}{c}{No}", "\\multicolumn{1}{c}{Yes}",
    "\\multicolumn{1}{c}{No}", "\\multicolumn{1}{c}{Yes}")),
  column.labels = NULL, column.separate = NULL,
  notes_table = "\\parbox[t]{0.65\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Department FEs not shown.}")

my_stargazer(dest_file = "alt_exp/output/tab_govt_vi_hv.tex",
  model_list = list(m_govt_vi_noFE_basic_hv, m_govt_vi_basic_hv, m_govt_vi_noFE_hv, m_govt_vi_hv),
  title = "Determinants of wartime violence by the state (most affected departments)",
  label = "tab:lm_govt_vi_hv",
  order = c("Constant", "dirt_sh$", "panam$"),
  covariate.labels = c("(Intercept)",
    "\\% Roads non-paved", "Log. Distance to Pan-Am Hwy",
    "Log. Population 1973", "\\% Indigenous 1973", "\\% Literate 1973",
    "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
    "Rebel violence pre-78"),
  dep.var.labels.include = FALSE,
  dep.var.labels = NULL,
  add.lines = list(c("Department FE",
    "\\multicolumn{1}{c}{No}", "\\multicolumn{1}{c}{Yes}",
    "\\multicolumn{1}{c}{No}", "\\multicolumn{1}{c}{Yes}")),
  column.labels = NULL, column.separate = NULL,
  notes_table = "\\parbox[t]{0.65\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Department FEs not shown. Most affected departments include Huehuetenango, Chimaltenango, Quiché, Alta Verapaz, Baja Verapaz, and Petén.}")

# Violence by the rebels
m_rebels_vi_noFE_basic = lm(rebels_vi_l ~ roads_dirt_sh + ldist_panam,
  data = data)
m_rebels_vi_basic = lm(rebels_vi_l ~ roads_dirt_sh + ldist_panam + factor(dpto),
  data = data)
m_rebels_vi_noFE = lm(rebels_vi_l ~ roads_dirt_sh + ldist_panam +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate +
  larea + rebels_vi_pre78_l,
  data = data)
m_rebels_vi = lm(rebels_vi_l ~ roads_dirt_sh + ldist_panam +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate +
  larea + rebels_vi_pre78_l + factor(dpto),
  data = data)

m_rebels_vi_noFE_basic_hv = lm(rebels_vi_l ~ roads_dirt_sh + ldist_panam,
  data = subset(data, dpto %in% high_vio))
m_rebels_vi_basic_hv = lm(rebels_vi_l ~ roads_dirt_sh + ldist_panam + factor(dpto),
  data = subset(data, dpto %in% high_vio))
m_rebels_vi_noFE_hv = lm(rebels_vi_l ~ roads_dirt_sh + ldist_panam +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate +
  larea + rebels_vi_pre78_l,
  data = subset(data, dpto %in% high_vio))
m_rebels_vi_hv = lm(rebels_vi_l ~ roads_dirt_sh + ldist_panam +
  lpop73 + ind73 + lit73 + elev_sd + forest + ldist_guate +
  larea + rebels_vi_pre78_l + factor(dpto),
  data = subset(data, dpto %in% high_vio))

my_stargazer(dest_file = "alt_exp/output/tab_rebels_vi_hv.tex",
  model_list = list(m_rebels_vi_noFE_basic, m_rebels_vi_basic,
    m_rebels_vi_noFE, m_rebels_vi),
  title = "Determinants of wartime violence by the rebels",
  label = "tab:lm_rebels_vi",
  order = c("Constant", "dirt_sh$", "panam$"),
  covariate.labels = c("(Intercept)",
    "\\% Roads non-paved", "Log. Distance to Pan-Am Hwy",
    "Log. Population 1973", "\\% Indigenous 1973", "\\% Literate 1973",
    "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
    "Rebel violence pre-78"),
  dep.var.labels.include = FALSE,
  dep.var.labels = NULL,
  add.lines = list(c("Department FE",
    "\\multicolumn{1}{c}{No}", "\\multicolumn{1}{c}{Yes}",
    "\\multicolumn{1}{c}{No}", "\\multicolumn{1}{c}{Yes}")),
  column.labels = NULL, column.separate = NULL,
  notes_table = "\\parbox[t]{0.65\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Department FEs not shown.}")

my_stargazer(dest_file = "alt_exp/output/tab_rebels_vi.tex",
  model_list = list(m_rebels_vi_noFE_basic_hv,
    m_rebels_vi_basic_hv, m_rebels_vi_noFE_hv, m_rebels_vi_hv),
  title = "Determinants of wartime violence by the rebels (most affected departments)",
  label = "tab:lm_rebels_vi_hv",
  order = c("Constant", "dirt_sh$", "panam$"),
  covariate.labels = c("(Intercept)",
    "\\% Roads non-paved", "Log. Distance to Pan-Am Hwy",
    "Log. Population 1973", "\\% Indigenous 1973", "\\% Literate 1973",
    "Elevation SD", "Forest cover", "Log. Dist to capital", "Log. Area",
    "Rebel violence pre-78"),
  dep.var.labels.include = FALSE,
  dep.var.labels = NULL,
  add.lines = list(c("Department FE",
    "\\multicolumn{1}{c}{No}", "\\multicolumn{1}{c}{Yes}",
    "\\multicolumn{1}{c}{No}", "\\multicolumn{1}{c}{Yes}")),
  column.labels = NULL, column.separate = NULL,
  notes_table = "\\parbox[t]{0.65\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Department FEs not shown. Most affected departments include Huehuetenango, Chimaltenango, Quiché, Alta Verapaz, Baja Verapaz, and Petén.}")
