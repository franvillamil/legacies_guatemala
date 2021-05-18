predprob_df = function(newdata, model, label = NULL){
  nd = newdata
  nd$y = predict(model, newdata = nd, type = "response")
  nd$se = predict(model, newdata = nd, type = "response", se.fit = T)$se.fit
  nd$upr = nd$y + 1.96 * nd$se
  nd$lwr = nd$y - 1.96 * nd$se

  if(!is.null(label)){nd = cbind(nd, label = rep(label, nrow(nd)))}
  return(nd)
}
