setwd('D:/code/CON_NOTEARS')



###compute SHD TPR FPR
stl <- c( 'sachs')

threshold <- 0.4
result <- NULL

for(p in stl){
  # asd <- read.csv(file = paste('middle/NoTears/sachs_853_notears.csv' , sep = ''), header = FALSE)
  #asd <- read.csv(file = paste('middle/MyMethod/sachs_path_0.2.csv' , sep = ''), header = FALSE)
  asd <- read.table(file = paste('middle/DAG-GNN/sachs853predG', sep = ''))
  nodes  <- as.character(c(1:ncol(asd)))
  
  W_bin_con <- (sign((abs(asd) - threshold)) + 1)/2
  W_bin_con <- as.matrix(W_bin_con)
  colnames(W_bin_con) <- nodes
  rownames(W_bin_con) <- nodes
  ept_graph_con <- empty.graph(nodes)
  amat(ept_graph_con) <- W_bin_con
  
  load(paste('networks/', p, '.rda', sep = ''))
  net <- bn.net(bn)
  nodes(net) <- nodes
  
  rt <- compute_TPR_FPR(amat(net), W_bin_con)
  cat(rt$TPR, rt$FPR)
  #SHD2 <- shd_sc(amat(ept_graph_con), amat(net))
  #std <- data.frame(Nodes = length(nodes), Methods = 'NoTears', data = p,
  #                  SHD = SHD2)
  #result <- rbind(result, std)
  # write.csv(result, 'middle/NoTears/result.csv')
}

# for(p in stl){
#   asd <- read.csv(file = paste('data_nonlinear_exp/NoTears/Synnonlinexp80_', p, '_notears.csv' , sep = ''), header = FALSE)
#   # asd <- read.table(file = paste('data_linear_exp/DAG-GNN/', 'Synlinexp80', p,'predG', sep = ''))
#   W_std <- read.csv(paste('data_nonlinear_exp/WeightTrueDAG/Syn80.csv', sep = ''))
#   nodes  <- as.character(c(1:ncol(W_std)))
# 
#   W_bin_con <- (sign((abs(asd) - threshold)) + 1)/2
#   W_bin_con <- as.matrix(W_bin_con)
#   colnames(W_bin_con) <- nodes
#   rownames(W_bin_con) <- nodes
# 
#   W_std <- sign(abs(W_std))
#   W_std <- as.matrix(W_std)
#   colnames(W_std) <- nodes
#   rownames(W_std) <- nodes
# 
# 
#   ept_graph_con <- empty.graph(nodes)
#   amat(ept_graph_con) <- W_bin_con
#   net <- empty.graph(nodes)
#   amat(net) <- W_std
# 
#   # PlotHot(W_std, W_est_con, m, n)
#   returnlist <- compute_TPR_FPR(W_std, W_bin_con)
#   TPR2 <- returnlist$TPR
#   FPR2 <- returnlist$FPR
# 
#   SHD2 <- shd(ept_graph_con, net)
#   std <- data.frame(Nodes = length(nodes), Methods = 'NoTears', data = p,
#                     TPR = TPR2, FPR = FPR2, SHD = SHD2)
#   result <- rbind(result, std)
#   write.csv(result, 'data_nonlinear_exp/NoTears/result.csv')
# }

###compute score
# stl <- c('alarm','child',
#          'insurance',  'win95pts', 'water','barley','mildew',
#          'hailfinder', 'hepar2', 'pathfinder')

# threshold <- 0.5
# result <- NULL
# for(p in stl){
#   asd <- read.csv(file = paste('another/middle_mid/NoTears/', p, '_notears.csv' , sep = ''), header = FALSE)
#   # asd <- read.table(file = paste('another/middle_mid/DAG-GNN/', p, '30predG', sep = ''))
#   X <- read.csv(file = paste('another/middle_mid/data/', p, '_30.csv', sep = ''))
#   nodes  <- as.character(c(1:ncol(asd)))
#   
#   load(paste('networks/', p, '.rda', sep = ''))
#   net <- bn.net(bn)
#   nodes(net) <- nodes
#   colnames(X) <- nodes
#   
#   W_bin_con <- (sign((abs(asd) - threshold)) + 1)/2
#   W_bin_con <- as.matrix(W_bin_con)
#   colnames(W_bin_con) <- nodes
#   rownames(W_bin_con) <- nodes
#   
#   ept_graph_con <- empty.graph(nodes)
#   amat(ept_graph_con) <- W_bin_con
#   
#   score <- BIC(ept_graph_con, X)
#   cat(p, ' ', score, '\n')
# }





####support code for GOBNILP
# stl <- c('alarm', 'barley', 'child', 'insurance', 'mildew', 'water')
# for (p in stl){
#   X <- read.csv(paste('another/middle_mid/data/', p, '_30.csv', sep = ''))
#   nodes <- as.character(c(1:ncol(X)))
#   colnames(X) <- nodes
#   load(paste('networks/', p, '.rda', sep = ''))
#   net <- bn.net(bn)
#   nodes(net) <- nodes
#   score <- BIC(net, X)
#   cat(p, ' ', score, '\n')
#   # ts1 <- Independence_selection(X, 20)
#   # writescores(ts1$caches, ts1$caches_score, nodes, paste('another/middle/GOBNILP/score/', p, '_30.txt', sep = ''), 10000000)
# }



