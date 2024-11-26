delta_bic <- function(net1,net2,data){
  bic1 = BIC(net1,data = data1)
  bic2 = BIC(net2,data = data1)
  
  return(abs(bic1-bic2))
}

F1_score <- function(learned,real){
  
  kaltsit = unlist(compare(learned,real))
  tp = kaltsit[1]
  fp = kaltsit[2]
  fn = kaltsit[3]
  tn = 0
  
  r = tp/(tp + fn)
  p = tp/(tp + fp)
  F1 = 2*r*p/(r+p)
  F1 = as.numeric(F1)
  
  return(F1)
}

rp <- function(learned, real){
  
  kaltsit = unlist(compare(learned,real))
  tp = kaltsit[1]
  fp = kaltsit[2]
  fn = kaltsit[3]
  tn = 0
  
  r = tp/(tp + fn)
  p = tp/(tp + fp)
  result = c(as.numeric(r),as.numeric(p))
  
  return(result)
}

BSF_score <- function(learned,real,nodes){
  
  kaltsit = unlist(compare(learned,real))
  tp = as.numeric(kaltsit[1]) 
  fp = as.numeric(kaltsit[2])
  fn = as.numeric(kaltsit[3])
  tn = 0
  size = length(nodes)
  
  a = nrow(real$arcs)
  i = size*(size-1)/2 - a
  
  BSF = 0.5 * (tp/a + tn/i - fp/i - fn/a)
  
  return(BSF)
}

SHD_score <- function(learned,real){
  # browser()
  
  kaltsit = 0
  arcs1 = learned$arcs
  arcs2 = real$arcs
  unequal = 0
  reverses = 0
  arcs1_cache = arcs1[,c(2,1)]
  for(i in 1:nrow(arcs1)){
    if(compare_arcs(arcs1[i,],arcs2))
      unequal = unequal + 1
  }
  
  for(i in 1:nrow(arcs1)){
    if(compare_arcs(arcs1_cache[i,],arcs2))
      next
    else
      reverses = reverses + 1
  }
  
  for(i in 1:nrow(arcs2)){
    if(compare_arcs(arcs2[i,],arcs1))
      unequal = unequal + 1
  }
  
  kaltsit = unequal - reverses
  return(kaltsit)
}

shd_sc <- function(learned,real){
  shd = 0
  n = ncol(learned)
  learned[abs(learned) > 0.3] = 1
  learned[abs(learned) < 0.3] = 0
  judge1 = length(which(xor(learned,real) == 1))
  learnedmat = learned + t(learned)
  realmat = real + t(real)
  learnedmat[upper.tri(learnedmat)] = 0
  realmat[upper.tri(realmat)] = 0
  judge2 = length(which(xor(realmat,learnedmat) == 1))
  shd = (judge1 + judge2)/2
  
  return(shd)
}