# My Stargazer
my_stargazer = function(dest_file, model_list,
  title, label,
  dep.var.labels.include = TRUE,
  dep.var.labels = gsub("cia", "", sapply(model_list, function(x) as.character(formula(x))[2])),
  order, covariate.labels,
  column.labels = c("", "\\scriptsize Most affected departments"),
  column.separate = c(2, 2),
  omit = c("dpto", "elec"),
  add.lines = list(
    c("Department FE", rep("\\multicolumn{1}{c}{Yes}", length(model_list))),
    c("Election FE", rep("\\multicolumn{1}{c}{Yes}", length(model_list)))),
  notes_table = "\\parbox[t]{0.65\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. All models pool all observations across all elections. Election and department FEs not shown. Most affected departments include Huehuetenango, Chimaltenango, Quiché, Alta Verapaz, Baja Verapaz, and Petén.}"){

  filecon = file(dest_file)
  writeLines(
    stargazer(model_list, omit = omit, title = title, label = label,
      order = order, covariate.labels = covariate.labels, notes = notes_table,
      omit.stat = c("f", "ser"),
      intercept.bottom = FALSE,
      column.sep.width = "-20pt",
      multicolumn = FALSE,
      dep.var.caption = "",
      dep.var.labels.include = dep.var.labels.include,
      dep.var.labels = dep.var.labels,
      column.labels = column.labels,
      column.separate = column.separate,
      font.size = "small",
      digits = 3,
      digits.extra = 0,
      star.char = c("+", "*", "**", "***"),
      star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
      notes.align = "c",
      align = TRUE,
      no.space = TRUE,
      add.lines = add.lines,
      notes.label = "", notes.append = FALSE),
    filecon)
    close(filecon)

}
