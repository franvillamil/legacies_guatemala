rm(list=ls())
setwd("~/Google Drive/Academic/PhD/Projects/Guatemala/data")
library(sp)
library(rgdal)
library(rgeos)
library(classInt)
library(RColorBrewer)
library(scales)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(xtable)
library(corrplot)
options(stringsAsFactors = FALSE)
load("function_adapt.RData")

data = read.csv("data.csv")
adm = readOGR("GIS/adm/GTM_adm2_updated.shp", layer = "GTM_adm2_updated")
adm_dpt = gUnaryUnion(adm, id = adm@data$NAME_1)
guate = gUnaryUnion(adm)
roads = readOGR("GIS/caminos/caminos_gtm.shp", layer = "caminos_gtm")
roads = spTransform(roads, CRS(proj4string(adm)))
roads_asf = gIntersection(roads[roads$cobertura == "Asfaltado",], guate)
roads = gIntersection(roads, guate)
panam = readOGR("GIS/caminos/panamericana.shp", layer = "panamericana")
roads2006 = readOGR("GIS/RCA2016_250K/Rca16.shp", layer = "Rca16")
roads2006 = spTransform(roads2006, CRS(proj4string(adm)))
roads2006_asf = gIntersection(roads2006[roads2006$REVESTIMIE == "Pavimentado",], guate)
roads2006 = gIntersection(roads2006, guate)

## ------------------------------------------------------------
## SUMMARY STATISTICS

variables = data[, c("govt_vi_l", "roads_dirt_sh",
  "ldist_panam", "lpop73", "ind73", "elev_sd", "forest",
  "ldist_guate", "larea", "rebels_vi_pre78_l")]

names(variables) = c(
  "Log. State killings / 1000hab",
  "\\% Non-paved roads",
  "Log. Distance to PanAm Hwy",
  "Log. Population 1973",
  "\\% Indigenous 1973",
  "Elevation SD",
  "\\% Forest cover",
  "Log. distance to capital",
  "Log. Area",
  "Rebel violence pre-1978")

# Summary statistics

descs = vector()
for(i in 1:ncol(variables)){
  descs = rbind(descs, round(summary(variables[, i]), 2))}
descs = as.data.frame(descs)
descs = cbind(names(variables), descs)
names(descs) = c("Variable", "Min", "Q1", "Median", "Mean", "Q3", "Max")

fileconnection = file("../../../dissertation/empirics/guatemala/table_app_descriptives.tex")
writeLines(
  paste0(
    "\\begin{table}[!htbp] \\centering", "\n",
    "\\caption{Summary statistics for the Guatemalan data}", "\n",
    "\\label{tab-gt-app:descriptives}", "\n",
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

# Correlation plot

names(variables) = c("State violence", "% Non-paved", "Log. Dist PanAm",
  "Log. Pop 1973", "% Indig 1973", "Elev SD", "% Forest",
  "Log. Dist Capital", "Log. Area", "Rebel pre-1978")
corr_mat = cor(variables, use = "pairwise.complete.obs")
col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

pdf("../../../dissertation/empirics/guatemala/correlation.pdf",
  height = 7, width = 6)
corrplot(corr_mat, method = "color", col = col(200), type = "lower",
  diag = FALSE, addCoef.col = "black", tl.col = "black")
dev.off()

system2("pdfcrop", args = c(
  "/Users/franvillamil/Google\\ Drive/Academic/PhD/dissertation/empirics/guatemala/correlation.pdf",
  "/Users/franvillamil/Google\\ Drive/Academic/PhD/dissertation/empirics/guatemala/correlation.pdf"))

## ------------------------------------------------------------
## RAW DATA

ciidh = subset(read.csv("violence/ciidh.csv"),
  #n_year %in% 1978:1985 &
  n_type %in% c("killed", "disapp_dead"))
ciidh$p_gov = with(ciidh, ifelse((p_arm == 1 | p_pol == 1 | p_par == 1), 1, 0))
names(ciidh)[names(ciidh) == "n_year"] = "year"
ciidh_agg = ddply(ciidh, .(year), summarize,
  govt_ev = length(n_type[p_gov == 1 | p_pac == 1]),
  govt_vi = sum(c_tot[p_gov == 1 | p_pac == 1]),
  nopac_vi = sum(c_tot[p_gov == 1 & p_pac == 0]),
  inclpac_vi = sum(c_tot[p_pac == 1]),
  onlypac_vi = sum(c_tot[p_gov == 0 & p_pac == 1]),
  rebels_vi = sum(c_tot[p_urn == 1]))

ceh = read.csv("violence/ceh_massacres_80_85.csv")
ceh_agg = ddply(ceh, .(year), summarize,
  govt_ev = length(victims),
  govt_vi = sum(victims, na.rm = T),
  inclpac_vi = sum(victims[military=="Mil and Pac"], na.rm = T),
  nopac_vi = sum(victims[military=="Y"], na.rm = T))

vio_agg = rbind.fill(ciidh_agg, ceh_agg)
vio_agg = ddply(vio_agg[!is.na(vio_agg$year) & vio_agg$year>1960,],
    .(year), summarize,
  govt_ev = sum(govt_ev, na.rm = T),
  govt_vi = sum(govt_vi, na.rm = T),
  inclpac_vi = sum(inclpac_vi, na.rm = T),
  nopac_vi = sum(nopac_vi, na.rm = T),
  onlypac_vi = sum(onlypac_vi, na.rm = T),
  rebels_vi = sum(rebels_vi, na.rm = T))

# Violence by govt/rebel by year

pdf("../../../dissertation/empirics/guatemala/govt_vi_years.pdf",
  height = 2.5, width = 8)
ggplot(vio_agg, aes(x = year, y = govt_vi)) +
  geom_bar(stat = "identity") + #scale_y_log10() +
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
  scale_x_continuous(breaks = seq(1960, 1996, 5)) +
  # geom_vline(xintercept = 1977.5, linetype = "dotted") +
  # geom_vline(xintercept = 1985.5, linetype = "dotted") +
  # annotate("text", x = 1988, y = 20000, size = 3,
  #   label = "1978-1985, period included\nin the analyses") +
  labs(y = "Number of victims", x = "")
dev.off()

pdf("../../../dissertation/empirics/guatemala/govt_vi_years_log.pdf",
  height = 2.5, width = 8)
ggplot(vio_agg, aes(x = year, y = govt_vi)) +
  geom_bar(stat = "identity") + scale_y_log10() +
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
  scale_x_continuous(breaks = seq(1960, 1996, 5)) +
  # geom_vline(xintercept = 1977.5, linetype = "dotted") +
  # geom_vline(xintercept = 1985.5, linetype = "dotted") +
  labs(y = "Number of victims", x = "")
dev.off()

pdf("../../../dissertation/empirics/guatemala/rebel_vi_years.pdf",
  height = 2.5, width = 8)
ggplot(vio_agg, aes(x = year, y = rebels_vi)) +
  geom_bar(stat = "identity") +
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
  scale_x_continuous(breaks = seq(1960, 1996, 5)) +
  # geom_vline(xintercept = 1978, linetype = "dotted") +
  # geom_vline(xintercept = 1985, linetype = "dotted") +
  labs(y = "Number of victims", x = "")
dev.off()

# Participation of PAC, army etc

vio_agg$Aonlypac = vio_agg$onlypac_vi / vio_agg$govt_vi
vio_agg$Bboth = vio_agg$inclpac_vi / vio_agg$govt_vi
vio_agg$Conlygovt = vio_agg$nopac_vi / vio_agg$govt_vi

vio_agg_long = vio_agg %>%
  gather(actor, percentage, Aonlypac:Conlygovt)
vio_agg_long$actor = factor(vio_agg_long$actor)
levels(vio_agg_long$actor) = c("Only PAC", "Both", "Only state")

pdf("../../../dissertation/empirics/guatemala/govt_pac_vi_sh.pdf",
  height = 3, width = 9)
ggplot(vio_agg_long, aes(y = percentage, x = year, fill = actor)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(panel.background = element_blank(),
        # legend.position = c(0, 1.03), legend.justification = c(0,1),
        legend.position = "right",
        legend.title = element_blank(),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size = 12),
        plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
        strip.background = element_blank()) +
  scale_fill_manual("Perpetrator", values = c("#0e80d8", "#666666", "#ee6b6f")) +
  labs(x = "", y = "Share of victims by perpetrator")
dev.off()

## ------------------------------------------------------------
## MAPS

reds = brewer.pal(4, "Reds")

create_col_var = function(data, var_name, breaks){
  # breaks = c(0, quantile(data[, var_name], breaks, na.rm = TRUE))
  tmp_var = ifelse(data[, var_name] > breaks[1], reds[1], "white")
  tmp_var[data[, var_name] >= breaks[2]] = reds[2]
  tmp_var[data[, var_name] >= breaks[3]] = reds[3]
  tmp_var[data[, var_name] >= breaks[4]] = reds[4]
  tmp_var[is.na(data[, var_name])] = "grey"
  return(tmp_var)
}

# Basic sample ---------------------------------------------------

labels = as.data.frame(coordinates(gCentroid(adm_dpt, byid = TRUE)))
labels$dpto = names(adm_dpt)
labels$y[labels$dpto == "Retalhuleu"] = 14.4 # prev: 14.41869
labels$y[labels$dpto == "Suchitepequez"] = 14.45 # prev: 14.40344
labels$x[labels$dpto == "Suchitepequez"] = -91.3 # prev: -91.40866
labels$y[labels$dpto == "Solola"] = 14.8 # prev: 14.71926
labels = labels[order(round(labels$x, 1), rev(round(labels$y, 1))),]
labels$num = 1:nrow(labels)

pdf("../../../dissertation/empirics/guatemala/map_sample.pdf",
  height = 5, width = 5)
par(mar = c(0,0,0,6.5), xpd = TRUE)
plot(adm, border = "gray", lwd = 0.0075)
plot(adm_dpt, border = "black", lwd = 0.01, add = TRUE)
text(labels$x, labels$y, label = labels$num, cex = 0.6, font = 2)
legend(-88.1, 17.5, cex = 0.65, title = "", bty='n', legend = labels$num)
legend(-88, 17.5, cex = 0.65, title = "Departments",
  legend = paste0(strrep(" ", 3), labels$dpto))
dev.off()

# Violence by government and rebels ------------------------------

data$govt_vi_p = with(data, govt_vi / pop73 * 1000)
data$rebels_vi_p = with(data, rebels_vi / pop73 * 1000)

pdf("../../../dissertation/empirics/guatemala/map_govt_vi.pdf",
  height = 5, width = 5)
par(mar = c(0,0,0,0))
plot(adm, col = create_col_var(data, "govt_vi_p", c(0, 1, 10, 50)),
  border = "gray", lwd = 0.0075)
plot(adm_dpt, border = "black", lwd = 0.01, add = TRUE)
legend(-89, 14.5, cex = 0.65, title = " Killings / 1000 hab.",
  legend = c("0", "0-1", "1-10", "10-50", "+50"),
  fill = c("white", reds[1:4]))
dev.off()

pdf("../../../dissertation/empirics/guatemala/map_rebel_vi.pdf",
  height = 5, width = 5)
par(mar = c(0,0,0,0))
plot(adm, col = create_col_var(data, "rebels_vi_p", c(0, 1, 5, 50)),
  border = "gray", lwd = 0.0075)
plot(adm_dpt, border = "black", lwd = 0.01, add = TRUE)
legend(-89, 14.5, cex = 0.65, title = " Killings / 1000 hab.",
  legend = c("0", "0-1", "1-5", "+5"),
  fill = c("white", reds[1:3]))
dev.off()

# URNG & FRG results by election --------------------------

pdf("../../../dissertation/empirics/guatemala/map_URNG1999.pdf",
  height = 5, width = 5)
par(mar = c(0,0,0,0))
plot(adm, col = create_col_var(data, "URNGcia1999", c(0.1, 0.1, 0.25, 0.5)),
  border = "gray", lwd = 0.0075)
plot(adm_dpt, border = "black", lwd = 0.01, add = TRUE)
legend(-89, 14.5, cex = 0.65,
  legend = c("0-10%", "10-25%", "25-50%", "+50%"),
  fill = c("white", reds[2:4]))
dev.off()

pdf("../../../dissertation/empirics/guatemala/map_FRG1999.pdf",
  height = 5, width = 5)
par(mar = c(0,0,0,0))
plot(adm, col = create_col_var(data, "FRG1999", c(0.25, 0.25, 0.5, 0.75)),
  border = "gray", lwd = 0.0075)
plot(adm_dpt, border = "black", lwd = 0.01, add = TRUE)
legend(-89, 14.5, cex = 0.65,
  legend = c("0-25%", "25-50%", "50-75%", "+75%"),
  fill = c("white", reds[2:4]))
dev.off()

# Road infrastructure (paved & PanAm) ------------------------

pdf("../../../dissertation/empirics/guatemala/map_roads_nonpaved_sh.pdf",
  height = 5, width = 5)
par(mar = c(0,0,0,0))
plot(adm, col = create_col_var(data, "roads_dirt_sh", c(0.25, 0.5, 0.75, 1)),
  border = "gray", lwd = 0.0075)
plot(adm_dpt, border = "black", lwd = 0.01, add = TRUE)
legend(-89, 14.5, cex = 0.65,
  legend = c("0-25%", "25-50%", "50-75%", "75-99%", "100%"),
  fill = c("white", reds[1:4]))
dev.off()

pdf("../../../dissertation/empirics/guatemala/map_roads.pdf",
  height = 5, width = 5)
par(mar = c(0,0,0,0))
plot(guate, lwd = 0.5)
plot(roads, col = gray(0.75), lwd = 0.25, add = TRUE)
plot(roads_asf, col = "black", lwd = 0.5, add = TRUE)
dev.off()

pdf("../../../dissertation/empirics/guatemala/map_roads2006.pdf",
  height = 5, width = 5)
par(mar = c(0,0,0,0))
plot(guate, lwd = 0.5)
plot(roads2006, col = gray(0.75), lwd = 0.25, add = TRUE)
plot(roads2006_asf, col = "black", lwd = 0.5, add = TRUE)
dev.off()

pdf("../../../dissertation/empirics/guatemala/map_panam.pdf",
  height = 5, width = 5)
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

# (Appendix, other types of violence: PAC etc) ---------------

# (Appendix, all other years for elections) ------------------

pdf("../../../dissertation/empirics/guatemala/map_URNG2003.pdf",
  height = 5, width = 5)
par(mar = c(0,0,0,0))
plot(adm, col = create_col_var(data, "URNGcia2003", c(0.05, 0.1, 0.25, 0.5)),
  border = "gray", lwd = 0.0075)
plot(adm_dpt, border = "black", lwd = 0.01, add = TRUE)
legend(-89, 14.5, cex = 0.65,
  legend = c("0-5%", "5-10%", "10-25%", "25-50%", "+50%"),
  fill = c("white", reds[1:4]))
dev.off()

pdf("../../../dissertation/empirics/guatemala/map_URNG2007.pdf",
  height = 5, width = 5)
par(mar = c(0,0,0,0))
plot(adm, col = create_col_var(data, "URNGcia2007", c(0.05, 0.1, 0.25, 0.5)),
  border = "gray", lwd = 0.0075)
plot(adm_dpt, border = "black", lwd = 0.01, add = TRUE)
legend(-89, 14.5, cex = 0.65,
  legend = c("0-5%", "5-10%", "10-25%", "25-50%", "+50%"),
  fill = c("white", reds[1:4]))
dev.off()

pdf("../../../dissertation/empirics/guatemala/map_URNG2011.pdf",
  height = 5, width = 5)
par(mar = c(0,0,0,0))
plot(adm, col = create_col_var(data, "URNGcia2011", c(0.05, 0.1, 0.25, 0.5)),
  border = "gray", lwd = 0.0075)
plot(adm_dpt, border = "black", lwd = 0.01, add = TRUE)
legend(-89, 14.5, cex = 0.65,
  legend = c("0-5%", "5-10%", "10-25%", "25-50%", "+50%"),
  fill = c("white", reds[1:4]))
dev.off()

pdf("../../../dissertation/empirics/guatemala/map_URNG2015.pdf",
  height = 5, width = 5)
par(mar = c(0,0,0,0))
plot(adm, col = create_col_var(data, "URNGcia2015", c(0.05, 0.1, 0.25, 0.5)),
  border = "gray", lwd = 0.0075)
plot(adm_dpt, border = "black", lwd = 0.01, add = TRUE)
legend(-89, 14.5, cex = 0.65,
  legend = c("0-5%", "5-10%", "10-25%", "25-50%", "+50%"),
  fill = c("white", reds[1:4]))
dev.off()


pdf("../../../dissertation/empirics/guatemala/map_FRG2003.pdf",
  height = 5, width = 5)
par(mar = c(0,0,0,0))
plot(adm, col = create_col_var(data, "FRG2003", c(0.05, 0.1, 0.25, 0.5)),
  border = "gray", lwd = 0.0075)
plot(adm_dpt, border = "black", lwd = 0.01, add = TRUE)
legend(-89, 14.5, cex = 0.65,
  legend = c("0-5%", "5-10%", "10-25%", "25-50%", "+50%"),
  fill = c("white", reds[1:4]))
dev.off()

pdf("../../../dissertation/empirics/guatemala/map_FRG2007.pdf",
  height = 5, width = 5)
par(mar = c(0,0,0,0))
plot(adm, col = create_col_var(data, "FRG2007", c(0.05, 0.1, 0.25, 0.5)),
  border = "gray", lwd = 0.0075)
plot(adm_dpt, border = "black", lwd = 0.01, add = TRUE)
legend(-89, 14.5, cex = 0.65,
  legend = c("0-5%", "5-10%", "10-25%", "25-50%", "+50%"),
  fill = c("white", reds[1:4]))
dev.off()

pdf("../../../dissertation/empirics/guatemala/map_FRG2015.pdf",
  height = 5, width = 5)
par(mar = c(0,0,0,0))
plot(adm, col = create_col_var(data, "FRG2015", c(0.05, 0.1, 0.25, 0.5)),
  border = "gray", lwd = 0.0075)
plot(adm_dpt, border = "black", lwd = 0.01, add = TRUE)
legend(-89, 14.5, cex = 0.65,
  legend = c("0-5%", "5-10%", "10-25%", "25-50%", "+50%"),
  fill = c("white", reds[1:4]))
dev.off()
