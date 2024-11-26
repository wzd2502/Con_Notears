setwd('D:/code/CON_NOTEARS')
library(bnlearn)

m <- 20
n <- 20
threshold <- 0.3
nodes <- as.character(c(1:n))
W_std <- read.csv('result/m = 10 n = 20 W_std.csv', header = TRUE)
W_est <- read.csv('result/m = 10 n = 20 W_est.csv', header = TRUE)
W_est_con <- read.csv('result/m = 10 n = 20 W_est_con.csv', header = TRUE)
W_std[,1] <- NULL
W_est[,1] <- NULL
W_est_con[,1] <- NULL

W_sstd <- (sign((abs(W_std) - threshold)) + 1)/2
colnames(W_sstd) <- nodes
net <- empty.graph(nodes)
amat(net) <- as.matrix(W_sstd)


W_bin <- (sign((abs(W_est) - threshold)) + 1)/2
colnames(W_bin) <- nodes
ept_graph <- empty.graph(nodes)
amat(ept_graph) <- as.matrix(W_bin)
# PlotHot(t(W_std), W_est, m, n)
returnlist <- compute_TPR_FPR(t(W_std), W_bin)
TPR <- returnlist$TPR
FPR <- returnlist$FPR
SHD <- shd(ept_graph, net)
cat('m: ', m, ' n: ', n, ' Method: W_est, TPR: ', TPR, ' FPR: ', FPR, ' SHD: ', SHD, '\n')

W_bin_con <- (sign((abs(W_est_con) - threshold)) + 1)/2
colnames(W_bin_con) <- nodes
ept_graph_con <- empty.graph(nodes)
amat(ept_graph_con) <- as.matrix(W_bin_con)
# PlotHot(t(W_std), W_est_con, m, n)
returnlist <- compute_TPR_FPR(t(W_std), W_bin_con)
TPR2 <- returnlist$TPR
FPR2 <- returnlist$FPR
SHD2 <- shd(ept_graph_con, net)
cat('m: ', m, ' n: ', n, ' Method: W_est_con, TPR: ', TPR2, ' FPR: ', FPR2, ' SHD: ', SHD2, '\n')