library(bnlearn)
setwd('D:/code/CON_NOTEARS')
options(CUDA_VISIBLE_DEVICES = -1)
options (warn = -1)
library(philentropy)

stl <- c('sachs')
NumOfNodes <- c(200)
NumOfDatasets <- c(30)
NumOfConstraints <- c(0.2)
p1s <- c(0.01)

noise_type = 2 ## 1: gaussian + exp; 2: gaussian; 3: exp.
constraint_type = 3 ## 1: weight + unweight; 2 : weight; 3: unweight.
tight_type = 3## 1: path + edge + ordering; 2: path; 3: edge + path



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
  Pam_tight_type <- c('edge', 'path', 'ordering')
}else if (tight_type == 2){
  Pam_tight_type <- 'path'
}else if (tight_type == 3){
  Pam_tight_type <- c('edge')
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


#####running#####
##Syn
# result <-NULL
# for(noiset in Pam_noise_type){
#   #合成
#   if(noiset == 'exp')
#     setwd('D:/code/CON_NOTEARS/another')
# 
#   for(n in NumOfNodes){
#     noise_type <- noiset
#     nodes <- as.character(c(1: n))
#     net <- random.graph(nodes, num = 1) #methods: ordered, ic-dag, melancon, empty.
#     nn <- paste('Syn',n, '_DAGtrue', sep = '')
#     arclist <- arcs(net)
#     colnames(arclist) <- c('Var1', 'Var2')
#     write.csv(arclist, paste('middle/TrueDAG/', nn, '.csv', sep = ''), row.names = FALSE)
#   #天然
#   # for(p in stl){
#   #   noise_type <- noiset
#   #   load(paste('networks/', p, '.rda', sep = ''))
#   #   net <- bn.net(bn)
#   #   n <- length(nodes(net))
#   #   nodes <- as.character(c(1: n))
#   #   nodes(net) <- nodes
#   #   arclist <- arcs(net)
#   #   colnames(arclist) <- c('Var1', 'Var2')
#   #   nn <- paste(p, '_DAGtrue', sep = '')
#   #   write.csv(arclist, paste('middle/TrueDAG/', nn, sep = ''))
#     for (m in NumOfDatasets) {
#       # n <- 20
#       # m <- 20
#       w <- 1
# 
#       lr <- 1e-3
#       W <- NULL
#       rho <- 1.0
#       rho_max <- 1e+20
#       lambda1 <- 0.0
#       lambda2 <- 0.0
#       beta <- 0.1
#       num_iter <- 10000
# 
#       max_k <- 10000
#       gamma <- 0.25
#       eta <- 10
#       epsilon <- 1e-8
#       threshold <- 0.4
#       h_pre <- 10000000000
# 
#       returnlist <- GenerateDataset(n, m, w, noise_type, net)
#       X <- returnlist$dataset
#       W_std <- returnlist$W_std
#       nn <- paste('Syn',n, sep = '')
#       write.csv(W_std, paste('middle/WeightTrueDAG/', nn, '.csv', sep = ''), col.names = FALSE, row.names = FALSE)
#       nn <- paste('Syn',n, '_',m, sep = '')
#       write.csv(X, paste('middle/data/', nn, '.csv', sep = ''), col.names = FALSE, row.names = FALSE)
# 
# 
#       #####without ancestral constraints##########
#       Con_Mat <- matrix(rep(0,n *n), n)
#       t1 <- proc.time()
#       for(k in 1:max_k){
#         while (rho < rho_max) {
#           returnlist <- ModelTraing_Inequ(X, m, n, lr, W, rho, lambda1, lambda2,
#                                           Con_Mat, beta, num_iter, method = 'notears', p1 = 0)
#           h <- returnlist$tf_hw
#           W_est <- returnlist$tf_W
#           cat('lambda: ', lambda1, 'rho: ', rho, '\n')
#           if(h > 0.25 * h_pre)
#             rho <- rho * eta
#           else
#             break
#         }
#         W <- W_est
#         h_pre <- h
#         lambda1 <- lambda1 + rho * h
#         if((h < epsilon) || (rho >= rho_max))
#           break
#       }
# 
#       nn <- paste('Syn',n, '_0_',m, sep = '')
#       write.csv(W_est_con, paste('middle/MyMethod/', nn, '.csv', sep = ''),col.names = FALSE, row.names = FALSE)
# 
#       t2 <- proc.time() - t1
#       W_bin <- (sign((abs(W_est) - threshold)) + 1)/2
#       ept_graph <- empty.graph(nodes)
#       amat(ept_graph) <- W_bin
#       # PlotHot(W_std, W_est, m, n)
#       returnlist <- compute_TPR_FPR(W_std, W_bin)
#       TPR <- returnlist$TPR
#       FPR <- returnlist$FPR
#       SHD <- shd(ept_graph, net)
#       # SHD <- shd_sc(amat(ept_graph), amat(net))
#       std <- data.frame(Nodes = n, Dataset = m, RCon = 0,  Noise = noise_type, Conmat = 'Empty', Tight = 'Empty', p1 = 0,
#                         TPR = TPR, FPR = FPR, SHD = SHD, Time = t2[1])
#       result <- rbind(result, std)
#       write.csv(result, 'middle/result.csv')
#       cat('Noise_type: ', noise_type, ' m: ', m, ' n: ', n, ' con_rate: 0, Conmat: Empty, Tight: Empty, ',
#           'TPR: ', TPR, ' FPR: ', FPR, ' SHD: ', SHD, ' time: ', t2[1], '\n\n\n')
# 
# 
#       for (tight in Pam_tight_type){
#         if (tight == 'ordering'){
#           con_pre <- 10000000000
#           h_pre <- 10000000000
#           W <- NULL
#           rho <- 1.0
#           lambda1 <- 0
#           lambda2 <- 0
#           stop_sign_hw <- 1
#           stop_sing_conw <- 1
#           beta <- 0.1
#           t3 <- proc.time()
#           for(k in 1:max_k){
#             while (rho < rho_max) {
#                 returnlist <- ModelTraing_Inequ(X, m, n, lr, W, rho, lambda1, lambda2, Con_Mat,
#                                                 beta, num_iter, method = 'notears_relu_ordering', p1 = 0)
#               h <- returnlist$tf_hw
#               W_est_con <- returnlist$tf_W
#               cat('lambda: ', lambda1, 'rho: ', rho, '\n')
#               if(h > 0.25 * h_pre)
#                 rho <- rho * eta
#               else
#                 break
#             }
#             W <- W_est_con
#             h_pre <- h
#             lambda1 <- lambda1 + rho * h
#             if((h < epsilon) || (rho >= rho_max))
#               break
#           }
# 
#           nn <- paste('Syn',n, '_ordering_',m, sep = '')
#           write.csv(W_est_con, paste('middle/MyMethod/', nn, '.csv', sep = ''), col.names = FALSE, row.names = FALSE)
# 
#           t4 <- proc.time() - t3
#           W_bin_con <- (sign((abs(W_est_con) - threshold)) + 1)/2
#           # W_bin_con <- preprocess(W_bin_con, n)
#           ept_graph_con <- empty.graph(nodes)
#           amat(ept_graph_con) <- W_bin_con
#           # PlotHot(W_std, W_est_con, m, n)
#           returnlist <- compute_TPR_FPR(W_std, W_bin_con)
#           TPR2 <- returnlist$TPR
#           FPR2 <- returnlist$FPR
#           SHD2 <- shd(ept_graph_con, net)
#           # SHD2 <- shd_sc(amat(ept_graph_con), amat(net))
#           std <- data.frame(Nodes = n, Dataset = m, RCon = 0, Noise = noise_type, Conmat = 'Empty', Tight = tight, p1 = 0,
#                             TPR = TPR2, FPR = FPR2, SHD = SHD2, Time = t4[1])
#           result <- rbind(result, std)
#           write.csv(result, 'middle/result.csv')
#           cat('Noise_type: ', noise_type, ' m: ', m, ' n: ', n, ' tight = ', tight,
#               'TPR: ', TPR2, ' FPR: ', FPR2, ' SHD: ', SHD2, ' time: ', t4[1], '\n\n\n')
#         }
#         else{
#           for(iop in c(1:1)){
#             for(p1 in p1s){
#               for(con_rate in NumOfConstraints){
#                 sha <- 0
#                 while (sha == 0) {
#                   if(n %in% c(20))
#                     sha <- 1
#                   if(con_rate == 1.0)
#                     sha <- 1
#                   cc <- get_constraints(net, con_rate, tight)
#                   # cc <- cc %*% cc + cc
#                   # cc <- aa %*% cc
#                   
#                   
#                   for(conmat in Pam_constraint_type){
#                     if(conmat == 'unweight'){
#                       Con_Mat <- as.numeric(as.logical(cc))
#                       Con_Mat <- matrix(Con_Mat, ncol = n)
#                       # Con_Mat <- sign(cc)
#                     }
#                     else if (conmat == 'weight')
#                       Con_Mat <- cc
#                     else
#                       stop('Error: Incorrect Constraints.')
#                     
#                     # if(tight == 'path'){
#                     # Con_Mat <-preprocess(Con_Mat, n)
#                     # }
#                     con_pre <- 10000000000
#                     h_pre <- 10000000000
#                     W <- NULL
#                     rho <- 1.0
#                     lambda1 <- 0
#                     lambda2 <- 0
#                     stop_sign_hw <- 1
#                     stop_sing_conw <- 1
#                     beta <- 0.1
#                     t3 <- proc.time()
#                     # # one equal + soft
#                     # if(tight == 'edge'){
#                     for(k in 1:max_k){
#                       while (rho < rho_max) {
#                         if(tight == 'path')
#                           returnlist <- ModelTraing_Inequ(X, m, n, lr, W, rho, lambda1, lambda2, Con_Mat,
#                                                           beta, num_iter, method = 'notears_relu_path', p1 = p1)
#                         if(tight == 'edge')
#                           returnlist <- ModelTraing_Inequ(X, m, n, lr, W, rho, lambda1, lambda2, Con_Mat,
#                                                           beta, num_iter, method = 'notears_relu', p1 = 0)
#                         h <- returnlist$tf_hw
#                         W_est_con <- returnlist$tf_W
#                         cat('lambda: ', lambda1, 'rho: ', rho, '\n')
#                         if(h > 0.25 * h_pre)
#                           rho <- rho * eta
#                         else
#                           break
#                       }
#                       W <- W_est_con
#                       h_pre <- h
#                       lambda1 <- lambda1 + rho * h
#                       if((h < epsilon) || (rho >= rho_max))
#                         break
#                     }
#                     
#                     t4 <- proc.time() - t3
#                     W_bin_con <- (sign((abs(W_est_con) - threshold)) + 1)/2
#                     # W_bin_con <- preprocess(W_bin_con, n)
#                     ept_graph_con <- empty.graph(nodes)
#                     amat(ept_graph_con) <- W_bin_con
#                     # PlotHot(W_std, W_est_con, m, n)
#                     returnlist <- compute_TPR_FPR(W_std, W_bin_con)
#                     TPR2 <- returnlist$TPR
#                     FPR2 <- returnlist$FPR
#                     SHD2 <- shd(ept_graph_con, net)
#                     # SHD2 <- shd_sc(amat(ept_graph_con), amat(net))
#                     # if(SHD2 <= SHD){
#                     nn <- paste('Syn',n, '_', tight, '_', con_rate,'_',m, '_', p1,'_', iop, sep = '')
#                     write.csv(W_est_con, paste('middle/MyMethod/', nn, '.csv', sep = ''), col.names = FALSE, row.names = FALSE)
#                     sha <- 1
#                     # }
#                     std <- data.frame(Nodes = n, Dataset = m, RCon = con_rate, Noise = noise_type, Conmat = conmat, Tight = tight, p1 = p1,
#                                       TPR = TPR2, FPR = FPR2, SHD = SHD2, Time = t4[1])
#                     result <- rbind(result, std)
#                     write.csv(result, 'middle/result.csv')
#                     cat('Noise_type: ', noise_type, ' m: ', m, ' n: ', n, ' con_rate: ', con_rate,  ' conmat = ', conmat, ' tight = ', tight,
#                         'TPR: ', TPR2, ' FPR: ', FPR2, ' SHD: ', SHD2, ' time: ', t4[1], '\n\n\n')
#                   }
#                 }
#                 #####with ancestral constraints##########
#                 
#               }
#             }
#           }
#         }
#       }
#     }
#   }
# }

##True
result <-NULL
for(noiset in Pam_noise_type){
  #合成
  if(noiset == 'exp')
    setwd('D:/code/CON_NOTEARS/another')

    for(p in stl){
      noise_type <- noiset
      load(paste('networks/', p, '.rda', sep = ''))
      net <- bn.net(bn)
      n <- length(nodes(net))
      nodes <- as.character(c(1: n))
      nodes(net) <- nodes
      arclist <- arcs(net)
      colnames(arclist) <- c('Var1', 'Var2')
      nn <- paste(p, '_DAGtrue', sep = '')
      write.csv(arclist, paste('middle/TrueDAG/', nn, '.csv', sep = ''))
    for (m in NumOfDatasets) {
      # n <- 20
      # m <- 20
      w <- 1

      lr <- 1e-3
      W <- NULL
      rho <- 1.0
      rho_max <- 1e+20
      lambda1 <- 0.0
      lambda2 <- 0.0
      beta <- 0.1
      num_iter <- 10000

      max_k <- 10000
      gamma <- 0.25
      eta <- 10
      epsilon <- 1e-8
      threshold <- 0.4
      h_pre <- 10000000000

      # returnlist <- GenerateDataset(n, m, w, noise_type, net)
      # X <- returnlist$dataset
      # ts1 <- Independence_selection(X, 2)
      # writescores(ts1$caches, ts1$caches_score, nodes, paste('middle/GOBNILP/score/', p, '_30.txt', sep = ''), 10000000)
      # W_std <- returnlist$W_std
      X <- read.csv('RealData/sachs1.csv', header = FALSE)
      X[1,1] = 17
      X[,1] <- as.numeric(X[,1])
      colnames(X) <- as.integer(nodes)
      W_std <- NULL
      # nn <- paste(p, sep = '')
      # write.csv(W_std, paste('middle/WeightTrueDAG/', nn, '.csv', sep = ''), col.names = FALSE, row.names = FALSE)
      # nn <- paste(p, '_',m, sep = '')
      # write.csv(X, paste('middle/data/', nn, '.csv', sep = ''), col.names = FALSE, row.names = FALSE)
      # score<- BIC(net, X)
      # std <- data.frame(Networks = p, Dataset = m, RCon = 0, Tight = 'Standard',Score = score, Time = 0)
      # result <- rbind(result, std)


      #####without ancestral constraints##########
      Con_Mat <- matrix(rep(0,n *n), n)
      t1 <- proc.time()
      # for(k in 1:max_k){
      #   while (rho < rho_max) {
      #     returnlist <- ModelTraing_Inequ(X, m, n, lr, W, rho, lambda1, lambda2,
      #                                     Con_Mat, beta, num_iter, method = 'notears')
      #     h <- returnlist$tf_hw
      #     W_est <- returnlist$tf_W
      #     cat('lambda: ', lambda1, 'rho: ', rho, '\n')
      #     if(h > 0.25 * h_pre)
      #       rho <- rho * eta
      #     else
      #       break
      #   }
      #   W <- W_est
      #   h_pre <- h
      #   lambda1 <- lambda1 + rho * h
      #   if((h < epsilon) || (rho >= rho_max))
      #     break
      # }
      # 
      # nn <- paste(p, '_0', sep = '')
      # write.csv(W_est_con, paste('middle/MyMethod/', nn, '.csv', sep = ''),col.names = FALSE, row.names = FALSE)
      # 
      # t2 <- proc.time() - t1
      # W_bin <- (sign((abs(W_est) - threshold)) + 1)/2
      # ept_graph <- empty.graph(nodes)
      # amat(ept_graph) <- W_bin
      # # score<- BIC(ept_graph, X)
      # SHD <- shd(ept_graph, net)
      # std <- data.frame(Networks = p, Dataset = m, RCon = 0, Tight = 'Empty',SHD = SHD, Time = t2[1])
      # result <- rbind(result, std)
      # write.csv(result, 'middle/result.csv')


      for (tight in Pam_tight_type){
        if (tight == 'ordering'){
          con_pre <- 10000000000
          h_pre <- 10000000000
          W <- NULL
          rho <- 1.0
          lambda1 <- 0
          lambda2 <- 0
          stop_sign_hw <- 1
          stop_sing_conw <- 1
          beta <- 0.1
          t3 <- proc.time()
          for(k in 1:max_k){
            while (rho < rho_max) {
              returnlist <- ModelTraing_Inequ(X, m, n, lr, W, rho, lambda1, lambda2, Con_Mat,
                                              beta, num_iter, method = 'notears_relu_ordering', p1 = 0)
              h <- returnlist$tf_hw
              W_est_con <- returnlist$tf_W
              cat('lambda: ', lambda1, 'rho: ', rho, '\n')
              if(h > 0.25 * h_pre)
                rho <- rho * eta
              else
                break
            }
            W <- W_est_con
            h_pre <- h
            lambda1 <- lambda1 + rho * h
            if((h < epsilon) || (rho >= rho_max))
              break
          }

          nn <- paste(p, '_ordering', sep = '')
          write.csv(W_est_con, paste('middle/MyMethod/', nn, '.csv', sep = ''), col.names = FALSE, row.names = FALSE)

          t4 <- proc.time() - t3
          W_bin_con <- (sign((abs(W_est_con) - threshold)) + 1)/2
          ept_graph_con <- empty.graph(nodes)
          amat(ept_graph_con) <- W_bin_con
          score2 <- BIC(ept_graph_con, X)
          std <- data.frame(Networks = p, Dataset = m, RCon = 0, Tight = 'Ordering',Score = score2, Time = t4[1])
          result <- rbind(result, std)
          write.csv(result, 'middle/result.csv')
        }
        else{
          for(con_rate in NumOfConstraints){
            sha <- 0
            while (sha == 0) {
              cc <- get_constraints(net, con_rate, tight)
              # cc <- cc %*% cc + cc
              # cc <- aa %*% cc


              for(conmat in Pam_constraint_type){
                if(conmat == 'unweight'){
                  Con_Mat <- as.numeric(as.logical(cc))
                  Con_Mat <- matrix(Con_Mat, ncol = n)
                  # Con_Mat <- sign(cc)
                }
                else if (conmat == 'weight')
                  Con_Mat <- cc
                else
                  stop('Error: Incorrect Constraints.')

                # if(tight == 'path'){
                # Con_Mat <-preprocess(Con_Mat, n)
                # }
                con_pre <- 10000000000
                h_pre <- 10000000000
                W <- NULL
                rho <- 1.0
                lambda1 <- 0
                lambda2 <- 0
                stop_sign_hw <- 1
                stop_sing_conw <- 1
                beta <- 0.1
                t3 <- proc.time()
                # # one equal + soft
                # if(tight == 'edge'){
                for(k in 1:max_k){
                  while (rho < rho_max) {
                    if(tight == 'path')
                      returnlist <- ModelTraing_Inequ(X, m, n, lr, W, rho, lambda1, lambda2, Con_Mat,
                                                      beta, num_iter, method = 'notears_relu_path', p1 = 0.01)
                    if(tight == 'edge')
                      returnlist <- ModelTraing_Inequ(X, m, n, lr, W, rho, lambda1, lambda2, Con_Mat,
                                                      beta, num_iter, method = 'notears_relu', p1 = 0)
                    h <- returnlist$tf_hw
                    W_est_con <- returnlist$tf_W
                    cat('lambda: ', lambda1, 'rho: ', rho, '\n')
                    if(h > 0.25 * h_pre)
                      rho <- rho * eta
                    else
                      break
                  }
                  W <- W_est_con
                  h_pre <- h
                  lambda1 <- lambda1 + rho * h
                  if((h < epsilon) || (rho >= rho_max))
                    break
                }

                t4 <- proc.time() - t3
                W_bin_con <- (sign((abs(W_est_con) - threshold)) + 1)/2
                # W_bin_con <- preprocess(W_bin_con, n)
                ept_graph_con <- empty.graph(nodes)
                amat(ept_graph_con) <- W_bin_con
                # score2 <- BIC(ept_graph_con, X)
                SHD2 <- shd(ept_graph_con, net)
                # if(score2 >= score){
                  nn <- paste(p, '_', tight, '_', con_rate, sep = '')
                  write.csv(W_est_con, paste('middle/MyMethod/', nn, '.csv', sep = ''), col.names = FALSE, row.names = FALSE)
                  sha <- 1
                # }
                std <- data.frame(Networks = p, Dataset = m, RCon = con_rate, Tight = tight,SHD = SHD2, Time = t4[1])
                result <- rbind(result, std)
                write.csv(result, 'middle/result.csv')
              }
            }
            #####with ancestral constraints##########

          }
        }
      }
    }
  }
}