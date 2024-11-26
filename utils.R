get_constraints <- function(net, con_rate, tight){
  mat_0 <- amat(net)
  n <- ncol(mat_0)
  if (tight == 'path'){
    mat_all <- 0
    mat_n <- mat_0 %*% mat_0 
    time <- 2
    # while(sum(mat_n) != 0){
    #   mat_all <- mat_all + mat_n * (1/time)
    #   time <- time + 1
    #   mat_n <- mat_n %*% mat_0
    #   break
    # }
    mat_all <- mat_n
  }
  else if (tight == 'edge')
    mat_all <- mat_0
  else
    stop('Error: Incorrect tight mode!')
  # mat_all1 <- as.numeric(as.logical(mat_all))
  cons <- which(mat_all > 0)
  num_con <- length(cons) *  (1 - con_rate)
  set_one <- sample(cons, num_con, replace = FALSE)
  # mat_all1[set_one] <- 0
  mat_all[set_one] <- 0
  return(mat_all)
}

compute_TPR_FPR <- function(W_std, W_bin){
  W_std <- sign(abs(W_std))
  TPR <- sum(as.numeric(W_std & W_bin)) / sum(W_std)
  FPR <- sum(as.numeric((1 - W_std) & W_bin)) / sum(1 - W_std)
  return(list(TPR = TPR, FPR = FPR))
}

preprocess <- function(cc, n){
  net <- empty.graph(as.character(c(1:n)), num = 1)
  newnet <- empty.graph(as.character(c(1:n)), num = 1)
  colnames(cc) <- as.character(c(1:n))
  rownames(cc) <- as.character(c(1:n))
  amat(net) <- cc
  arcset <- arcs(net)
  deleterow <- c()
  for (i in 1:nrow(arcset)) {
    node1 <- arcset[i, 1]
    node2 <- arcset[i, 2]
    if(length(intersect(descendants(net, node1), ancestors(net, node2))) != 0){
      deleterow <- c(deleterow, i)
    }
  }
  if(length(deleterow) == 0)
    arcs(newnet) <- arcset
  else
    arcs(newnet) <- arcset[-deleterow, ]
  dd <- amat(newnet)
  colnames(arcset) <- NULL
  rownames(arcset) <- NULL
  # graphviz.plot(net)
  # graphviz.plot(newnet)
  return(dd)
}


ASD <- function(cachelist){
  
  
  caches <- cachelist$caches
  cachescores <- cachelist$caches_score
  caches_new <- list()
  for(i in 1:length(caches)){
    cache <- caches[[i]]
    cachescore <- cachescores[[i]]
    
    # for(j in 1:length(cache)){
    #   a <- unlist(strsplit(cache[j], split = ','))
    #   if(a == "NULL")
    #     break
    # }
    # cache <- cache[1:j]
    # cachesccore <- cachescore[1:j]
    
    del <- c()
    for(j in 1:length(cache)){
      a <- unlist(strsplit(cache[j], split = ','))
      if(length(a) > 3){
        del <- c(del, j)
        next
      }
    }
    if(!is.null(del)){
      cache <- cache[-del]
      cachescore <- cachescore[-del]
    }
    
    # for(j in 1:(length(cache) -1)){
    #   a <- unlist(strsplit(cache[j], split = ','))
    #   if(a == "NULL")
    #     break
    #   if(length(a) > 3){
    #     del <- c(del, j)
    #     next
    #   }
    #   for(k in (j + 1):length(cache)){
    #     b <- unlist(strsplit(cache[k], split = ','))
    #     if(length(b) > 3){
    #       del <- c(del, k)
    #       next
    #     }
    #     if(all(a %in% b))
    #       del <- c(del, j)
    #   }
    # }
    # if(!is.null(del))
    #   cache <- cache[-unique(del)]
    # 
    # del <- c()
    # for(k in 1:length(cache)){
    #   a <- unlist(strsplit(cache[k], split = ','))
    #   if(a != "NULL" && length(a) < 3){
    #     for(j in 1:(k-1)){
    #       b <- unlist(strsplit(cache[j], split = ','))
    #       if(all(a %in% b))
    #         del <- c(del, k)
    #     }
    #   }
    # }
    # 
    # if(!is.null(del))
    #   cache <- cache[-unique(del)]
    caches[[i]] <- cache
    cachescores[[i]] <- cachescore
  }
  cachelist$caches <- caches
  cachelist$caches_score <- cachescores
  
  
  return(cachelist)
}