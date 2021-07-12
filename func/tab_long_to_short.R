tab_long_to_short = function(file, rm_str, rm_end){

  # Read and trim
  tablong = readLines(file)
  rm_str_i = which(grepl(rm_str, tablong))
  rm_end_i = which(grepl(rm_end, tablong))+1
  tabshort = tablong[-(rm_str_i:rm_end_i)]
  # Add new line (Controls)
  dup_i = which(grepl("Department FE", tabshort))
  tabshort = tabshort[c(1:(dup_i-1), rep(dup_i, 2), (dup_i+1):length(tabshort))]
  tabshort[dup_i] = gsub("Department FE", "Controls", tabshort[dup_i])
  # Save again
  newfile = gsub("\\.tex", "_short.tex", file)
  fc = file(newfile)
  writeLines(tabshort, con = fc)
  close(fc)

}
