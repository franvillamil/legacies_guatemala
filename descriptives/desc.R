setwd("~/Documents/Projects/legacies_guatemala")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("sp", "rgdal", "rgeos", "classInt", "RColorBrewer", "scales",
  "ggplot2", "plyr", "dplyr", "tidyr", "xtable", "corrplot", "muniSpain")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))}
# Load
lapply(pkg, library, character.only = TRUE)

# -----------------------------------------------------------------
# PREPARATION

# Function for maps
create_col_var = function(data, var_name, breaks){
  # breaks = c(0, quantile(data[, var_name], breaks, na.rm = TRUE))
  tmp_var = ifelse(data[, var_name] > breaks[1], reds[1], "white")
  tmp_var[data[, var_name] >= breaks[2]] = reds[2]
  tmp_var[data[, var_name] >= breaks[3]] = reds[3]
  tmp_var[data[, var_name] >= breaks[4]] = reds[4]
  tmp_var[is.na(data[, var_name])] = "grey"
  return(tmp_var)
}

# Load dataset
data = read.csv("dataset/output/data.csv")

# Load electoral data
elec = read.csv("input/elections_1999-2015.csv")

# Load shapefile and create dpto & full country shps
adm = readOGR("input/GTM_adm2_updated.shp", layer = "GTM_adm2_updated")
adm_dpt = gUnaryUnion(adm, id = adm@data$NAME_1)
guate = gUnaryUnion(adm)

# Load roads shp, create subset for paved roads, limit to GT territory
roads = readOGR("input/caminos_gtm.shp", layer = "caminos_gtm")
roads = spTransform(roads, CRS(proj4string(adm)))
roads_asf = gIntersection(roads[roads$cobertura == "Asfaltado",], guate)
roads = gIntersection(roads, guate)

# Load Panamerican Highway
panam = readOGR("input/panamericana.shp", layer = "panamericana")

# -----------------------------------------------------------------
# SUMMARY STATS

covs = data[, c("govt_vi_l", "roads_dirt_sh", "ldist_panam",
  "lpop73", "ind73", "lit73", "elev_sd", "forest",
  "ldist_guate", "larea", "rebels_vi_pre78_l")] %>%
  rename(`Log. State killings / 1000hab` = `govt_vi_l`,
    `\\% Non-paved roads` = `roads_dirt_sh`,
    `Log. Distance to PanAm Hwy` = `ldist_panam`,
    `Log. Population 1973` = `lpop73`,
    `\\% Indigenous 1973` = `ind73`,
    `\\% Literate 1973` = `lit73`,
    `Elevation SD` = `elev_sd`,
    `\\% Forest cover` = `forest`,
    `Log. distance to capital` = `ldist_guate`,
    `Log. Area` = `larea`,
    `Rebel violence pre-1978` = `rebels_vi_pre78_l`)

# Create table df
descs = vector()
for(i in 1:ncol(covs)){
  descs = rbind(descs, round(summary(covs[, i]), 2))}
descs = as.data.frame(descs)
descs = cbind(names(covs), descs)
names(descs) = c("Variable", "Min", "Q1", "Median", "Mean", "Q3", "Max")

# Write tex table
fileconnection = file("descriptives/output/tab_summary_stats.tex")
writeLines(
  paste0(
    "\\begin{table}[!htbp] \\centering", "\n",
    "\\caption{Summary statistics for the covariates}", "\n",
    "\\label{tab:sumstats}", "\n",
    "\\small", "\n",
    paste0("\\begin{tabular}{l", strrep("c", ncol(descs)-1), "}"), "\n",
    "\\\\[-1.8ex]\\hline", "\n",
    "\\hline \\\\[-1.8ex]", "\n",
    "\\\\[-1.8ex]", "\n",
    paste(names(descs), collapse = " & "), " \\\\", "\n",
    "\\hline \\\\[-1.8ex]", "\n",
    paste(descs[1,], collapse = " & "), " \\\\", "\n",
    paste(descs[2,], collapse = " & "), " \\\\", "\n",
    paste(descs[3,], collapse = " & "), " \\\\", "\n",
    paste(descs[4,], collapse = " & "), " \\\\", "\n",
    paste(descs[5,], collapse = " & "), " \\\\", "\n",
    paste(descs[6,], collapse = " & "), " \\\\", "\n",
    paste(descs[7,], collapse = " & "), " \\\\", "\n",
    paste(descs[8,], collapse = " & "), " \\\\", "\n",
    paste(descs[9,], collapse = " & "), " \\\\", "\n",
    paste(descs[10,], collapse = " & "), " \\\\", "\n",
    "\\hline", "\n",
    "\\hline \\\\[-1.8ex]", "\n",
    "\\end{tabular}", "\n",
    "\\end{table}", "\n"
  ), fileconnection)
close(fileconnection)

# Prepate correlation matrix and names
cm = cor(covs, use = "pairwise.complete.obs")
rownames(cm)[rownames(cm) == "Log. State killings / 1000hab"] = "State violence"
rownames(cm)[rownames(cm) == "\\% Non-paved roads"] = "% Non-paved"
rownames(cm)[rownames(cm) == "Log. Distance to PanAm Hwy"] = "Log. Dist PanAm"
rownames(cm)[rownames(cm) == "Log. Population 1973"] = "Log. Pop 1973"
rownames(cm)[rownames(cm) == "\\% Indigenous 1973"] = "% Indig 1973"
rownames(cm)[rownames(cm) == "\\% Literate 1973"] = "% Lit 1973"
rownames(cm)[rownames(cm) == "Elevation SD"] = "Elev SD"
rownames(cm)[rownames(cm) == "\\% Forest cover"] = "% Forest"
rownames(cm)[rownames(cm) == "Log. distance to capital"] = "Log. Dist Capital"
rownames(cm)[rownames(cm) == "Log. Area"] = "Log. Area"
rownames(cm)[rownames(cm) == "Rebel violence pre-1978"] = "Rebel pre-1978"
colnames(cm) = rownames(cm)
# Colors
col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
# Correlation plot
pdf("descriptives/output/corrplot.pdf", height = 7, width = 6)
corrplot(cm, method = "color", col = col(200), type = "lower",
  diag = FALSE, addCoef.col = "black", tl.col = "black")
dev.off()

# ------------------------------------------------------------
# ELECTORAL RESULTS


URNG_sh = c(
  with(subset(elec, elec==1999), sum(DIA.URNG, na.rm=T)/sum(total_validos, na.rm=T)),
  with(subset(elec, elec==2003), sum(URNG, na.rm=T)/sum(total_validos, na.rm=T)),
  with(subset(elec, elec==2007), sum(URNG.MAIZ, na.rm=T)/sum(total_validos, na.rm=T)),
  with(subset(elec, elec==2011), sum(WINAQ.URNG.MAIZ.ANN, na.rm=T)/sum(total_validos, na.rm=T)),
  with(subset(elec, elec==2015), sum(WINAQ.URNG.MAIZ, na.rm=T)/sum(total_validos, na.rm=T)))
URNG_sh = round(URNG_sh * 100, 2)

FRG_sh = c(
  with(subset(elec, elec==1999), sum(FRG, na.rm=T)/sum(total_validos, na.rm=T)),
  with(subset(elec, elec==2003), sum(FRG, na.rm=T)/sum(total_validos, na.rm=T)),
  with(subset(elec, elec==2007), sum(FRG, na.rm=T)/sum(total_validos, na.rm=T)),
  with(subset(elec, elec==2015), sum(PRI, na.rm=T)/sum(total_validos, na.rm=T)))
FRG_sh = c(round(FRG_sh[1:3] * 100, 2), "--", round(FRG_sh[4] * 100, 2))

rows = paste0("URNG & ", paste(URNG_sh, collapse = "\\% & "), "\\% \\\\", "\n",
  "FRG & ", paste(FRG_sh, collapse = "\\% & "), "\\% \\\\")
rows = gsub("--\\\\%", "--", rows)

fc = file("descriptives/output/results_nat.tex")
writeLines(rows, fc)
close(fc)

# ------------------------------------------------------------
# MAPS

reds = brewer.pal(4, "Reds")

# State violence
data$govt_vi_p = with(data, govt_vi / pop73 * 1000)
pdf("descriptives/output/map_govt_vi.pdf", width = 5, height = 5)
  par(mar = c(0,0,0,0))
  plot(adm, col = create_col_var(data, "govt_vi_p", c(0, 1, 10, 50)),
    border = "gray", lwd = 0.0075)
  plot(adm_dpt, border = "black", lwd = 0.01, add = TRUE)
  legend(-89, 14.5, cex = 0.65, title = " Killings / 1000 hab.",
    legend = c("0", "0-1", "1-10", "10-50", "+50"),
    fill = c("white", reds[1:4]))
dev.off()

# URNG 1999
pdf("descriptives/output/map_URNG1999.pdf", width = 5, height = 5)
  par(mar = c(0,0,0,0))
  plot(adm, col = create_col_var(data, "URNGcia1999", c(0.1, 0.1, 0.25, 0.5)),
    border = "gray", lwd = 0.0075)
  plot(adm_dpt, border = "black", lwd = 0.01, add = TRUE)
  legend(-89, 14.5, cex = 0.65,
    legend = c("0-10%", "10-25%", "25-50%", "+50%"),
    fill = c("white", reds[2:4]))
dev.off()

# FRG 1999
pdf("descriptives/output/map_FRG1999.pdf", width = 5, height = 5)
  par(mar = c(0,0,0,0))
  plot(adm, col = create_col_var(data, "FRG1999", c(0.25, 0.25, 0.5, 0.75)),
    border = "gray", lwd = 0.0075)
  plot(adm_dpt, border = "black", lwd = 0.01, add = TRUE)
  legend(-89, 14.5, cex = 0.65,
    legend = c("0-25%", "25-50%", "50-75%", "+75%"),
    fill = c("white", reds[2:4]))
dev.off()

# Share of dirt roads
pdf("descriptives/output/map_roads_nonpaved_sh.pdf", width = 5, height = 5)
  par(mar = c(0,0,0,0))
  plot(adm, col = create_col_var(data, "roads_dirt_sh", c(0.25, 0.5, 0.75, 1)),
    border = "gray", lwd = 0.0075)
  plot(adm_dpt, border = "black", lwd = 0.01, add = TRUE)
  legend(-89, 14.5, cex = 0.65,
    legend = c("0-25%", "25-50%", "50-75%", "75-99%", "100%"),
    fill = c("white", reds[1:4]))
dev.off()

# Map of roads
pdf("descriptives/output/map_roads.pdf", width = 5, height = 5)
  par(mar = c(0,0,0,0))
  plot(guate, lwd = 0.5)
  plot(roads, col = gray(0.75), lwd = 0.25, add = TRUE)
  plot(roads_asf, col = "black", lwd = 0.5, add = TRUE)
dev.off()

# Map of PanAm Hwy and distance
pdf("descriptives/output/map_panam.pdf", width = 5, height = 5)
  par(mar = c(0,0,0,0))
  plot(adm, col = create_col_var(data, "dist_panam", c(0, 25, 50, 200)),
    border = "gray", lwd = 0.0075)
  plot(adm_dpt, border = "black", lwd = 0.01, add = TRUE)
  plot(panam, col = "blue", add = TRUE)
  legend(-89, 14.5, cex = 0.65,
    legend = c("Pan-Am Hwy", "0km", "0-25km", "25-50km", "50-200km", "+200km"),
    fill = c(NA, "white", reds[1:4]),
    border = c(NA, rep("black", 5)),
    lty = c(1, rep(NA, 5)),
    col = c("blue", rep(NA, 5)),
    merge = TRUE)
dev.off()
