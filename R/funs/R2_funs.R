R2_cond_fun = function(Vi, Vsite, Vfe, VR){
  Vrand = Vi + Vsite
  Vtot = Vi + Vsite + VR
  R2_cond = describe_posterior(
    (Vrand + Vfe),
    centrality = "mean") #/ Vtot)
  
  return(R2_cond)
}

R2_marg_fun = function(Vi, Vsite, Vfe, VR){
  Vtot = Vi + Vsite + VR
  R2_marg = describe_posterior(
    Vfe,
    centrality = "mean")# / Vtot)
  
  return(R2_marg)
}
