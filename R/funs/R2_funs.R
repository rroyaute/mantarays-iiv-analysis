# Point estimates returned from rptr

R2_cond_fun.point.est = function(Vi, Vsite, Vfe, VR){
  Vrand = sum(Vi , Vsite, na.rm = T)
  Vtot = sum(Vi , Vsite , Vfe , VR, na.rm = T)
  R2_cond = (Vrand + Vfe)/Vtot
  
  return(R2_cond)
}

R2_marg_fun.point.est = function(Vi, Vsite, Vfe, VR){
  Vtot = sum(Vi , Vsite , Vfe , VR, na.rm = T)
  R2_marg = Vfe/Vtot
  
  return(R2_marg)
}

# All variance components R2 (point estimates)
R2_fun.point.est.all = function(Vi, Vsite, Vfe, VR){
  Vtot = sum(Vi , Vsite , Vfe , VR, na.rm = T)
  R2_i = Vi/Vtot
  R2_site = Vsite/Vtot
  R2_fe = Vfe/Vtot
  R2_R = VR/Vtot
  
  R2.df = data.frame(
    R2_type = c("R2i", "R2site", "R2fe", "R2R"),
    R2 = c(R2_i, R2_site, R2_fe, R2_R)
  )
  
  return(R2.df)
}

# Medians & CI of bootstrap distribution from rptr
R2.dist = function(Vi, Vsite, Vfe, VR){
  Vtot = ifelse(is.na(Vsite) == T,
                Vi + Vfe + VR,
                Vi + Vsite + Vfe + VR)
    Vi + Vsite + Vfe + VR
  R2_i = Vi/Vtot
  R2_site = Vsite/Vtot
  R2_fe = Vfe/Vtot
  R2_R = VR/Vtot
  
  R2.df = data.frame(
    R2_i = R2_i,
    R2_site = R2_site,
    R2_fe = R2_fe,
    R2_R = R2_R)
  
  R2.df = describe_posterior(R2.df) #/ Vtot)
  
  return(R2.df)
}

V.dist = function(Vi, Vsite, Vfe, VR){
  
  V.df = data.frame(
    Vi = Vi,
    Vsite = Vsite,
    Vfe = Vfe,
    VR = VR)
  
  V.df = describe_posterior(V.df) #/ Vtot)
  
  return(V.df)
}

# Marginal and conditional repatability summaries from rptR distribution
R2_m_c.dist = function(Vi, Vsite, Vfe, VR){
  Vtot = ifelse(is.na(Vsite) == T,
                Vi + Vfe + VR,
                Vi + Vsite + Vfe + VR)
  Vrand = ifelse(is.na(Vsite) == T,
                 Vi,
                 Vi + Vsite)
  
  R2_m_c = data.frame(
    R2_marg = Vfe/Vtot * 100,
    R2_cond = (Vfe + Vrand)/Vtot * 100)
  
  
  R2.df = describe_posterior(R2_m_c, )
  
  return(R2.df)
}

