library(bnlearn)
setwd('D:/code/CON_NOTEARS')
options(CUDA_VISIBLE_DEVICES = -1)
options (warn = -1)

subdata <- read.csv('trainingdata.csv', header = TRUE)
subdata <- lapply(subdata, as.factor)
subdata <- as.data.frame(subdata)

X <- read.csv('trainingdata.csv', header = TRUE)
nodesname <- colnames(X)
cnodes <- as.character(c(1:length(nodesname)))
names(cnodes) <- nodesname

colnames(X) <- cnodes
W_std <- NULL



subnetwork <- empty.graph(nodesname)
fromlist <- c('threaten', 'threaten', 'state', 'arrive_time', 'arrive_time', 'arrive_time', 'arrive_time', 'arrive_time', 'type', 'type', 'type', 'type', 'type', 'type')
tolist <- c('type', 'state', 'heading', 'latitude', 'altitude', 'longitude', 'speed', 'distance', 'altitude', 'sensor', 'sources', 'duratioin', 'detected', 'interval')
arclist <- matrix(c(fromlist, tolist), ncol = 2)
arcs(subnetwork) <- arclist
nodes(subnetwork) <- cnodes[nodes(subnetwork)]

result <-NULL
con_rate <- 0.1

n <- length(cnodes)
m <- nrow(X)

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

cc <- get_constraints(subnetwork, con_rate, 'edge')
Con_Mat <- as.numeric(as.logical(cc))
Con_Mat <- matrix(Con_Mat, ncol = n)

con_pre <- 10000000000
h_pre <- 10000000000
W <- NULL
rho <- 1.0
lambda1 <- 0
lambda2 <- 0
stop_sign_hw <- 1
stop_sing_conw <- 1
beta <- 0.1

for(k in 1:max_k){
  while (rho < rho_max) {
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
W_bin_con <- (sign((abs(W_est_con) - threshold)) + 1)/2
# W_bin_con <- preprocess(W_bin_con, n)
ept_graph_con <- empty.graph(cnodes)
amat(ept_graph_con) <- W_bin_con
# score2 <- BIC(ept_graph_con, X)
SHD2 <- shd(ept_graph_con, subnetwork)
# if(score2 >= score){
nodes(ept_graph_con) <- names(nodes(ept_graph_con))
write.bif("section5.bif", bn.fit(ept_graph_con, subdata))
