# setwd("~/Documents/Projects/legacies_guatemala")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("dplyr", "tidyr")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))}
# Load
lapply(pkg, library, character.only = TRUE)

# -----------------------------------------------------------------
# PREPARATION

# Load list of municipalities
munilist = read.csv("input/munilist.csv")
# Merge in case of changes since the 1970s
munilist$id[!is.na(munilist$previo)] = munilist$previo[!is.na(munilist$previo)]

# Load input files
ciidh = read.csv("input/ciidh.csv")
ceh = read.csv("input/ceh_massacres_80_85.csv")
terrain = read.csv("input/terrain_vars.csv")
census = read.csv("input/census_73_81.csv")
elec = read.csv("input/elections_1999-2015.csv")

# -----------------------------------------------------------------
# ELECTIONS

# Assign ID
elec$id = munilist$id[match(paste(elec$dpto, elec$muni),
  tolower(paste(munilist$dpto, munilist$muni)))]
# Aggregate
data = elec %>%
  group_by(id) %>%
  summarize(
    # URNG
    URNGcia1999 = sum(DIA.URNG[elec == 1999], na.rm = T),
    URNGcia2003 = sum(URNG[elec == 2003], na.rm = T),
    URNGcia2007 = sum(URNG.MAIZ[elec == 2007], na.rm = T),
    URNGcia2011 = sum(WINAQ.URNG.MAIZ.ANN[elec == 2011], na.rm = T),
    URNGcia2015 = sum(WINAQ.URNG.MAIZ[elec == 2015], na.rm = T),
    # FRG
    FRG1999 = sum(FRG[elec == 1999], na.rm = T),
    FRG2003 = sum(FRG[elec == 2003], na.rm = T),
    FRG2007 = sum(FRG[elec == 2007], na.rm = T),
    FRG2015 = sum(PRI[elec == 2015], na.rm = T),
    # Partido Patriota
    PP2003 = sum(PP.MR.PSN[elec == 2003], na.rm = TRUE),
    PP2007 = sum(PP[elec == 2007], na.rm = TRUE),
    PP2011 = sum(PP[elec == 2011], na.rm = TRUE),
    PP2015 = sum(PP[elec == 2015], na.rm = TRUE),
    # UNE
    UNE2003 = sum(UNE[elec == 2003], na.rm = TRUE),
    UNE2007 = sum(UNE[elec == 2007], na.rm = TRUE),
    UNE2015 = sum(UNE[elec == 2015], na.rm = TRUE),
    # FCN
    FCN2015 = sum(FCN.NACION[elec == 2015], na.rm = TRUE),
    # Totals
    total1999 = sum(total_validos[elec == 1999], na.rm = TRUE),
    total2003 = sum(total_validos[elec == 2003], na.rm = TRUE),
    total2007 = sum(total_validos[elec == 2007], na.rm = TRUE),
    total2011 = sum(total_validos[elec == 2011], na.rm = TRUE),
    total2015 = sum(total_validos[elec == 2015], na.rm = TRUE)) %>%
  mutate(
    # Main parties
    URNGcia1999 = URNGcia1999 / total1999,
    URNGcia2003 = URNGcia2003 / total2003,
    URNGcia2007 = URNGcia2007 / total2007,
    URNGcia2011 = URNGcia2011 / total2011,
    URNGcia2015 = URNGcia2015 / total2015,
    FRG1999 = FRG1999 / total1999,
    FRG2003 = FRG2003 / total2003,
    FRG2007 = FRG2007 / total2007,
    FRG2015 = FRG2015 / total2015,
    UNE2003 = UNE2003 / total2003,
    UNE2007 = UNE2007 / total2007,
    UNE2015 = UNE2015 / total2015,
    PP2003 = PP2003 / total2003,
    PP2007 = PP2007 / total2007,
    PP2011 = PP2011 / total2011,
    PP2015 = PP2015 / total2015,
    FCN2015 = FCN2015 / total2015) %>%
  mutate(
    # Aggregates
    fulldcha1999 = FRG1999,
    fulldcha2003 = FRG2003 + PP2003,
    fulldcha2007 = FRG2007 + PP2007,
    fulldcha2011 = PP2011,
    fulldcha2015 = FRG2015 + PP2015 + FCN2015,
    fullizq1999 = URNGcia1999,
    fullizq2003 = URNGcia2003 + UNE2003,
    fullizq2007 = URNGcia2007 + UNE2007,
    fullizq2011 = URNGcia2011,
    fullizq2015 = URNGcia2015 + UNE2015) %>%
  select(-matches("^(total|PP|UNE|FCN)"))

# -----------------------------------------------------------------
# MUNI & DPTO NAMES

data = cbind(
  dpto = munilist$dpto[match(data$id, munilist$id)],
  muni =  munilist$muni[match(data$id, munilist$id)],
  data)

# -----------------------------------------------------------------
# CENSUS

# Restrct and prepare
census = census %>%
  select(MUNICIPIO, DEPARTMENT, TOTPOP73, PCIND73, PCLIT73) %>%
  rename(
    muni = MUNICIPIO,
    dpto = DEPARTMENT,
    pop73 = TOTPOP73,
    ind73 = PCIND73,
    lit73 = PCLIT73)

# Match with IDs, check, merge
census$id = munilist$id[match(paste(census$dpto, census$muni),
  paste(munilist$dpto, munilist$muni))]
if(!(all(data$id %in% census$id) & all(census$id %in% data$id))){stop("!")}
data = merge(data, census[, !names(census) %in% c("muni", "dpto")])

# -----------------------------------------------------------------
# VIOLENCE

# CIIDH: Subset, transform, aggregate
ciidh_agg = ciidh %>%
  filter(n_year %in% 1978:1985) %>%
  filter(n_type %in% c("killed", "disapp_dead")) %>%
  group_by(id) %>%
  summarize(
    ciidh_govt_vi = sum(c_tot[p_arm == 1 | p_pol == 1 | p_par == 1 | p_pac == 1]),
    rebels_vi = sum(c_tot[p_urn == 1]))

# CEH : Subset, transform, aggregate
ceh_agg = ceh %>%
  group_by(id) %>%
  summarize(ceh_govt_vi = sum(victims, na.rm = TRUE))

# Put together
vio = merge(ciidh_agg, ceh_agg, all = TRUE) %>%
  group_by(id) %>%
  summarize(
    govt_vi = sum(ciidh_govt_vi, ceh_govt_vi, na.rm = TRUE),
    ceh_govt_vi = sum(ceh_govt_vi, na.rm = TRUE),
    rebels_vi = sum(rebels_vi, na.rm = TRUE))

# Join with data and change NA to 0
if(!all(vio$id %in% data$id)){warning("!")}
data = merge(data, vio, all.x = TRUE) %>%
  mutate(
    govt_vi = ifelse(is.na(govt_vi), 0, govt_vi),
    ceh_govt_vi = ifelse(is.na(ceh_govt_vi), 0, ceh_govt_vi),
    rebels_vi = ifelse(is.na(rebels_vi), 0, rebels_vi))

# Pre-1978 violence
ciidh_pre78 = ciidh %>%
  filter(n_year < 1978) %>%
  group_by(id) %>%
  summarize(rebels_vi_pre78 = sum(c_tot[p_urn == 1]))

# Check and merge
if(!all(ciidh_pre78$id %in% data$id)){warning("!")}
data = merge(data, ciidh_pre78, all.x = TRUE) %>%
  mutate(rebels_vi_pre78 = ifelse(is.na(rebels_vi_pre78), 0, rebels_vi_pre78))

# -----------------------------------------------------------------
# GIS VARIABLES

# Checkc and merge
if(!(all(data$id %in% terrain$id) & all(terrain$id %in% data$id))){stop("!")}
data = merge(data, terrain[, !names(terrain) %in% c("muni", "dpto")])

# -----------------------------------------------------------------
# TRANSFORMING VARIABLES

data = data %>%
  mutate(
    govt_vi_l = log(govt_vi / pop73 * 1000 + 1),
    ceh_govt_vi_l = log(ceh_govt_vi / pop73 * 1000 + 1),
    rebels_vi_l = log(rebels_vi / pop73 * 1000 + 1),
    rebels_vi_pre78_l = log(rebels_vi_pre78 / pop73 * 1000 + 1),
    lpop73 = log(pop73),
    larea = log(area),
    ldist_guate = log(dist_guate + 1),
    ldist_panam = log(dist_panam + 1),
    roads_dirt_sh = 1 - roads_paved_sh
  )

# -----------------------------------------------------------------
# SAVE

write.csv(data, "dataset/output/data.csv", row.names = FALSE)
