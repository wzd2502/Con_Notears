library(bnlearn)

GenerateDataset <- function(n, m, w, noise_type, network){
  ########### Random Graph ############
  nodes <- nodes(network)
  
  ########### Random Weight Matrix ############
  net <- amat(network)
  weights_num <- runif( n ^ 2, min = 0.5 * w, max = 2 * w)
  weights_sgn <- sample(c(-1, 1), size = n ^ 2, replace = TRUE)
  weights <- weights_num * weights_sgn
  weights <- matrix(data = weights, ncol = n, nrow = n)
  weights_ma <- net * weights
  
  ########### Random Data ############
  dataset <- as.data.frame(matrix(rep(0, m * n), ncol = n))
  colnames(dataset) <- nodes
  to_ordering  <- node.ordering(network)
  for (node in to_ordering){
    if(noise_type == 'gaussian')
      noise <- rnorm(m, mean = 0, sd = 1)
    else if (noise_type == 'exp')
      noise <- rexp(m, rate = 1)
    else
      stop('Error:Incorrect noise type.')
    parent_node <- parents(network, node)
    if(length(parent_node) == 0)
      dataset[[node]] <- noise
    else
      dataset[node] <- as.matrix(dataset[parent_node]) %*% weights_ma[as.numeric(parent_node), as.numeric(node)] + noise
  }
  return(list(dataset = dataset, W_std = weights_ma))
}

GenerateDateSetWithMatrix <- function(n, m, noise_type, dag, weights_ma)
{
  # colnames(dag) <- c(1:n)
  # colnames(weights_ma) <- c(1:n)
  rownames(weights_ma) <- colnames(weights_ma)
  dataset <- as.data.frame(matrix(rep(0, m * n), ncol = n))
  network <- empty.graph(colnames(dag))
  dag1 <- as.matrix(dag)
  amat(network) <- dag1
  colnames(dataset) <- colnames(dag)
  to_ordering  <- node.ordering(network)
  for (node in to_ordering){
    if(noise_type == 'gaussian')
      noise <- rnorm(m, mean = 0, sd = 1)
    else if (noise_type == 'exp')
      noise <- rexp(m, rate = 1)
    else
      stop('Error:Incorrect noise type.')
    parent_node <- parents(network, node)
    if(length(parent_node) == 0)
      dataset[[node]] <- noise
    else
      dataset[node] <- as.matrix(dataset[parent_node]) %*% cos(weights_ma[parent_node, node]) + noise
  }
  return(list(dataset = dataset))
  write.csv()
}

main <- function()
{
  
}