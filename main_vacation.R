library(bnlearn)
setwd('D:/code/CON_NOTEARS')
options(CUDA_VISIBLE_DEVICES = -1)
options (warn = -1)

NumOfNodes <- c(40)
NumOfDatasets <- c(20)
NumOfConstraints <- c(1.0)

noise_type = 2 ## 1: gaussian + exp; 2: gaussian; 3: exp.
constraint_type = 3 ## 1: weight + unweight; 2 : weight; 3: unweight.
tight_type = 2 ## 1: path + edge; 2: path; 3: edge
mode1_type = 2 ## 1: exp + inv; 2: exp; 3: inv.
mode2_type = 3 ## 1: TMat + NMat; 2: TMat; 3: AMat.
penalize_type = 3 ## 1: Yes + No 2: Yes 3: No


###Parameters#####
if(noise_type == 1){
  Pam_noise_type <- c('gaussian', 'exp')
}else if (noise_type == 2){
  Pam_noise_type <- 'gaussian'
}else if (noise_type == 3){
  Pam_noise_type <- 'exp'
}else
  stop('Error: Invaild noise_type input!')

if(tight_type == 1){
  Pam_tight_type <- c('path', 'edge')
}else if (tight_type == 2){
  Pam_tight_type <- 'path'
}else if (tight_type == 3){
  Pam_tight_type <- 'edge'
}else
  stop('Error: Invaild constraint_type input!')

if(constraint_type == 1){
  Pam_constraint_type <- c('weight', 'unweight')
}else if (constraint_type == 2){
  Pam_constraint_type <- 'weight'
}else if (constraint_type == 3){
  Pam_constraint_type <- 'unweight'
}else
  stop('Error: Invaild constraint_type input!')

if(mode1_type == 1){
  Pam_mode1_type <- c('exp', 'inv')
}else if (mode1_type == 2){
  Pam_mode1_type <- 'exp'
}else if (mode1_type == 3){
  Pam_mode1_type <- 'inv'
}else
  stop('Error: Invaild mode1_type input!')

if(mode2_type == 1){
  Pam_mode2_type <- c('TMat',  'NMat')
}else if (mode2_type == 2){
  Pam_mode2_type <- 'TMat'
}else if (mode2_type == 3){
  Pam_mode2_type <- 'AMat'
}else
  stop('Error: Invaild mode2_type input!')

if(penalize_type == 1){
  Pam_penalize_type <- c('Yes',  'No')
}else if (penalize_type == 2){
  Pam_penalize_type <- 'Yes'
}else if (penalize_type == 3){
  Pam_penalize_type <- 'No'
}else
  stop('Error: Invaild penalize_type input!')

#####running#####
result <-NULL
for(noiset in Pam_noise_type){
  for(n in NumOfNodes){
    noise_type <- noiset
    nodes <- as.character(c(1: n))
    net <- random.graph(nodes, num = 1) #methods: ordered, ic-dag, melancon, empty.
    nn <- paste('n = ', n, ' noise_type = ', noise_type, ' W_bin', sep = '')
    write.csv(amat(net), paste('vacation/W_bin/', nn, '.csv', sep = ''))
    for (m in NumOfDatasets) {
      # n <- 20
      # m <- 20
      w <- 1
      
      lr <- 1e-3
      W <- NULL
      rho <- 1.0
      rho_max <- 1e+20
      lambda <- 0.0
      beta <- 0.1
      num_iter <- 10000
      
      max_k <- 1000
      gamma <- 0.25
      eta <- 10
      epsilon <- 1e-8
      threshold <- 0.4
      h_pre <- 10000000000
      
      returnlist <- GenerateDataset(n, m, w, noise_type, net)
      X <- returnlist$dataset
      W_std <- returnlist$W_std
      nn <- paste('m = ', m, ' n = ', n, ' noise_type = ', noise_type, ' W_std', sep = '')
      write.csv(W_std, paste('vacation/W_std/', nn, '.csv', sep = ''))
      nn <- paste('m = ', m, ' n = ', n, ' noise_type = ', noise_type, ' X ', sep = '')
      write.csv(X, paste('vacation/X/', nn, '.csv', sep = ''))

      
      #####without ancestral constraints##########
      Con_Mat <- matrix(rep(0,n *n), n)
      t1 <- proc.time()
      for(k in 1:max_k){
        while (rho < rho_max) {
          returnlist <- ModelTraing(X, m, n, lr, W, rho, lambda, beta, num_iter, Con_Mat, method = 'notears', mode = 'exp', mode2 = 'TMat', penalize = 'No')
          h <- returnlist$tf_hw
          W_est <- returnlist$tf_W
          cat('lambda: ', lambda, 'lambda_muliple: ', lambda * h, 'rho: ', rho, 'rho_mutiple: ', (rho/2) * h * h, '\n')
          if(h > 0.25 * h_pre)
            rho <- rho * eta
          else
            break
        }
        W <- W_est
        h_pre <- h
        lambda <- lambda + rho * h
        if((h < epsilon) || (rho >= rho_max))
          break
      }
      
      nn <- paste('m = ', m, ' n = ', n, ' noise_type = ', noise_type, ' notears ', sep = '')
      write.csv(W_est, paste('vacation/W_est/', nn, '.csv', sep = ''))
      
      t2 <- proc.time() - t1
      W_bin <- (sign((abs(W_est) - threshold)) + 1)/2
      ept_graph <- empty.graph(nodes)
      amat(ept_graph) <- W_bin
      # PlotHot(t(W_std), W_est, m, n)
      returnlist <- compute_TPR_FPR(t(W_std), W_bin)
      TPR <- returnlist$TPR
      FPR <- returnlist$FPR
      SHD <- shd(ept_graph, net)
      std <- data.frame(Nodes = n, Dataset = m, RCon = 0, Hway = 'Empty', Mway = 'Empty', Noise = noise_type, Conmat = 'Empty', Tight = 'Empty', Penalize = 'No',
                        TPR = TPR, FPR = FPR, SHD = SHD, Time = t2[1])
      result <- rbind(result, std)
      write.csv(result, 'vacation/result.csv')
      cat('Noise_type: ', noise_type, ' m: ', m, ' n: ', n, ' con_rate: 0, mode1: Empty, mode2: Empty, Conmat: Empty, Tight: Empty, Penalize: No ',
          'TPR: ', TPR, ' FPR: ', FPR, ' SHD: ', SHD, ' time: ', t2[1], '\n\n\n')
      
      
      for(con_rate in NumOfConstraints){
        #####with ancestral constraints##########
        for (tight in Pam_tight_type){
          cc <- get_constraints(net, con_rate, tight)
          for(conmat in Pam_constraint_type){
            if(conmat == 'unweight'){
              Con_Mat <- as.numeric(as.logical(cc))
              Con_Mat <- matrix(Con_Mat, ncol = n)
            }
            else if (conmat == 'weight')
              Con_Mat <- cc
            else
              stop('Error: Incorrect Constraints.')
            for(penalize in Pam_penalize_type){
              for(mode1 in Pam_mode1_type){
                for(mode2 in Pam_mode2_type){
                  h_pre <- 10000000000
                  W <- NULL
                  rho <- 1.0
                  lambda <- 0
                  t3 <- proc.time()
                  for(k in 1:max_k){
                    while (rho < rho_max) {
                      returnlist <- ModelTraing(X, m, n, lr, W, rho, lambda, beta, num_iter, Con_Mat, method = 'mine', mode = mode1, mode2 = mode2, penalize = penalize)
                      h <- returnlist$tf_hw
                      W_est_con <- returnlist$tf_W
                      cat('lambda: ', lambda, 'lambda_muliple: ', lambda * h, 'rho: ', rho, 'rho_mutiple: ', (rho/2) * h * h, '\n')
                      if(h > 0.25 * h_pre)
                        rho <- rho * eta
                      else
                        break
                    }
                    W <- W_est_con
                    h_pre <- h
                    lambda <- lambda + rho * h
                    if((h < epsilon) || (rho >= rho_max))
                      break
                  }
                  
                  nn <- paste('m = ', m, ' n = ', n, ' noise_type = ', noise_type, ' con_rate = ', con_rate, ' mode1 = ', mode1, ' mode2 = ', mode2, ' conmat = ', conmat, ' tight = ', tight,  ' ConMethod ', sep = '')
                  write.csv(W_est_con, paste('vacation/W_est/', nn, '.csv', sep = ''))
                  
                  t4 <- proc.time() - t3
                  W_bin_con <- (sign((abs(W_est_con) - threshold)) + 1)/2
                  ept_graph_con <- empty.graph(nodes)
                  amat(ept_graph_con) <- W_bin_con
                  # PlotHot(t(W_std), W_est_con, m, n)
                  returnlist <- compute_TPR_FPR(t(W_std), W_bin_con)
                  TPR2 <- returnlist$TPR
                  FPR2 <- returnlist$FPR
                  SHD2 <- shd(ept_graph_con, net)
                  std <- data.frame(Nodes = n, Dataset = m, RCon = con_rate, Hway = mode1, Mway = mode2, Noise = noise_type, Conmat = conmat, Tight = tight, Penalize = penalize,
                                    TPR = TPR2, FPR = FPR2, SHD = SHD2, Time = t4[1])
                  result <- rbind(result, std)
                  write.csv(result, 'vacation/result.csv')
                  cat('Noise_type: ', noise_type, ' m: ', m, ' n: ', n, ' con_rate: ', con_rate, ' mode1: ', mode1, ' mode2: ', mode2, ' conmat = ', conmat, ' tight = ', tight, ' penalize = ', penalize,
                      'TPR: ', TPR2, ' FPR: ', FPR2, ' SHD: ', SHD2, ' time: ', t4[1], '\n\n\n')
                }
              }
            }
          }
        }
      }
    }
  }
}

