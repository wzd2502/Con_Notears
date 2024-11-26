library(tensorflow)
library(hash)

ModelTraing_Inequ <- function(X, m, n, lr, W, rho, lambda_1, lambda_2, Con_Mat, beta, num_iter, method, p1){
  ######## Build tensorflow graph ##########
  tf$compat$v1$reset_default_graph()
  tf$compat$v1$disable_eager_execution()
  
  # tf_learning_rate <- tf$compat$v1$placeholder(tf$float32)
  # tf_X <- tf$compat$v1$placeholder(tf$float32, shape = as.integer(c(m, n)))
  tf_X <- tf$convert_to_tensor(as.matrix(X), tf$float32)
  up <- Con_Mat
  Con_Mat <- tf$convert_to_tensor(Con_Mat, tf$float32)
  Con_TMat <- tf$transpose(Con_Mat)
  Con_AMat <- tf$add(Con_Mat, Con_TMat)
  Con_Unmat <- tf$ones_like(Con_Mat) - Con_Mat
  Con_Order <- tf$linalg$band_part(tf$ones(as.integer(c(n, n))), as.integer(-1), as.integer(0)) 
  tf_learning_rate <- lr
  if(is.null(W))
    tf_W <- tf$Variable(tf$zeros(as.integer(c(n, n))), tf$float32)
  else
    tf_W <- tf$Variable(tf$convert_to_tensor(W, tf$float32))
  # tf$linalg$set_diag(tf_W, tf$zeros(as.integer(n), dtype=tf$float32))
  tf_least_squares <- tf$square(tf$norm(tf_X - tf$matmul(tf_X, tf_W))) * 1/(2*m) + beta * tf$norm(tf_W, ord = 1)
  tf_hW <- tf$linalg$trace(tf$linalg$expm(tf_W * tf_W )) - n
  

  if(method == 'notears_con'){
    ## inequ method:||*||<0.4
    # tf_conw <- tf$norm( tf$multiply(tf_W, Con_Mat), ord = 1 ) -  1* sum(up)
    # tf_conw <- tf$reduce_min( tf$multiply(tf$abs(tf_W), Con_Mat) + tf$ones_like(Con_Mat) - Con_Mat ) - 0.4
    tf_conw <- tf$reduce_sum( tf$math$log(2.5 * (tf$abs(tf_W * Con_Mat) - 0.4 * Con_Mat) + 1))
    judge <- tf$greater_equal((tf_conw - lambda_2/rho), 0)
    judge <- tf$cast(judge, tf$float32)
    tf_loss1 <- tf_least_squares + lambda_1 * tf_hW - lambda_2 * tf_conw +  (rho/2) *((tf$square(tf$norm(tf_hW))) + (tf$square(tf$norm(tf_conw))) )
    tf_loss2 <- tf_least_squares + lambda_1 * tf_hW  + (rho/2) *tf$square(tf$norm(tf_hW)) - (lambda_2 ^ 2)/(2 * rho)
    tf_loss <- judge * tf_loss2 + (1 - judge) * tf_loss1
  }
  else if (method == 'notears'){
    tf_conw <- tf$norm( tf$nn$relu( -1 *(tf$multiply(tf$abs(tf_W), Con_Mat) - 0.4 * Con_Mat)), ord = 1)
    tf_conw2 <- tf$norm( tf$nn$relu((tf$abs(tf_W) - 2 * tf$ones_like(Con_Mat))), ord = 1)
    # tf_hW <- tf_hW + tf_conw + tf_conw2
    tf_loss <- tf_least_squares + lambda_1 * tf_hW + (rho/2) * tf$square(tf$norm(tf_hW))
    # tf_conw3 <- tf$norm(  (tf$multiply(tf$abs(tf_W), Con_Mat)), ord = 1)
    # tf_conw4 <- tf$norm(  (tf$multiply(tf$abs(tf_W), Con_TMat)), ord = 1)
  }else if (method == 'notears_relu'){
    tf_conw <- tf$norm( tf$nn$relu( -1 *(tf$multiply(tf$abs(tf_W), Con_Mat) - 0.4 * Con_Mat)), ord = 1)
    tf_conw2 <- tf$norm( tf$nn$relu((tf$abs(tf_W) - 2 * tf$ones_like(Con_Mat))), ord = 1)
    tf_hW <- tf_hW + tf_conw + tf_conw2
    tf_loss <- tf_least_squares + lambda_1 * tf_hW + (rho/2) * tf$square(tf$norm(tf_hW))
    # tf_loss <- tf_loss + tf$norm(tf$multiply(tf_W, tf$ones_like(Con_Mat) - Con_Mat) , ord = 1) -  tf$norm(tf$multiply(tf_W,  Con_Mat) , ord = 1)
    # tf_conw3 <- tf$norm(  (tf$multiply(tf$abs(tf_W), Con_Mat)), ord = 1)
    # tf_conw4 <- tf$norm(  (tf$multiply(tf$abs(tf_W), Con_TMat)), ord = 1)
  }
  else if (method == 'notears_relu_ordering'){
    tf_conw <-  tf$norm(  (tf$multiply(tf$abs(tf_W), Con_Order)), ord = 1)
    tf_conw2 <- tf$norm( tf$nn$relu((tf$abs(tf_W) - 2 * tf$ones_like(Con_Mat))), ord = 1)
    tf_hW <- tf_hW + tf_conw2
    tf_loss <- tf_least_squares + lambda_1 * tf_hW + (rho/2) * tf$square(tf$norm(tf_hW)) + tf_conw
    # tf_loss <- tf_loss + tf$norm(tf$multiply(tf_W, tf$ones_like(Con_Mat) - Con_Mat) , ord = 1) -  tf$norm(tf$multiply(tf_W,  Con_Mat) , ord = 1)
    # tf_conw3 <- tf$norm(  (tf$multiply(tf$abs(tf_W), Con_Mat)), ord = 1)
    # tf_conw4 <- tf$norm(  (tf$multiply(tf$abs(tf_W), Con_TMat)), ord = 1)
  }
  else if (method == 'notears_relu_path'){
    # tf_W_copy <- get_binaryTensor(tf$abs(tf_W), 0.4, mode = 'EW')
    # tf_Path <- tf$compat$v1$matrix_inverse(tf$compat$v1$matrix_diag(rep(1.0, n)) -  tf_W_copy)- tf$compat$v1$matrix_diag(rep(1.0, n))
    # tf_Path <- tf$linalg$expm(tf_W_copy)- tf$compat$v1$matrix_diag(rep(1.0, n))
    # tf_Path <- get_binaryTensor(tf_Path, 0, mode = 'EW')
    # tf_con <- get_maxprod(tf$abs(tf_W), tf_Path, n)
    # tf_con <- tf_Path
    # tf_con <- tf$matmul(tf_Path, tf$abs(tf_W))
    # tf_con1 <-  tf$compat$v1$matrix_inverse(tf$compat$v1$matrix_diag(rep(1.0, n)) -  tf$abs(tf_W)) - tf$abs(tf_W) - tf$compat$v1$matrix_diag(rep(1.0, n))
    # tf_con1 <- get_maxprod(tf$abs(tf_W), tf$abs(tf_W), n)
    tf_con1 <- tf$matmul(tf$abs(tf_W), tf$abs(tf_W))
    # tf_con2 <- tf$matmul(tf_con1, tf$abs(tf_W))
    # tf_con3 <- tf$matmul(tf_con2, tf$abs(tf_W))
    # tf_con4 <- tf$matmul(tf_con3, tf$abs(tf_W))
    # tf_con2 <- get_maxprod(tf$abs(tf_W), tf_con1, n)
    # tf_con3 <- get_maxprod(tf$abs(tf_W), tf_con2, n)
    tf_con <- tf_con1
    # tf_conw <- tf$norm( tf$nn$relu( -1 *(tf$multiply(tf_con, Con_Mat) - 0.4 * Con_Mat)), ord = 1)
    # tf_conw <- -0.5 * tf$norm( tf$nn$relu( -1 *(tf$multiply(tf$abs(tf_W), Con_Mat) - 0.4 * Con_Mat)), ord = 1) +
    #   0.5 * tf$norm( tf$nn$relu( -1 *(tf$multiply(tf$matmul(tf$abs(tf_W), tf_W_copy), Con_Mat) - 0.4 * Con_Mat)), ord = 1) +
    #   0.5 *  tf$norm( tf$nn$relu( -1 *(tf$multiply(tf$matmul(tf$abs(tf_W), tf$matmul(tf_W_copy, tf_W_copy)), Con_Mat) - 0.4 * Con_Mat)), ord = 1)
    tf_conw <-  tf$norm( tf$nn$relu( -1 *(tf$multiply(tf_con, Con_Mat) - n * p1 * Con_Mat)), ord = 1)
    # tf_conw <-  tf$norm(tf$multiply(tf_con, Con_Mat), ord = 1) 
    # tf_conw <-  tf$norm(  1 *(tf$multiply(tf_con, Con_Mat)), ord = 1)
    # tf_conw3 <- tf$norm(  (tf$multiply(tf$abs(tf_W), Con_Mat)), ord = 1)
    # tf_conw4 <- tf$norm(  (tf$multiply(tf$abs(tf_W), Con_TMat)), ord = 1)
    # tf_conw <-tf$norm(-1 *(tf$multiply(tf_con, Con_Mat)), ord = 1)
    tf_conw2 <- 1 * tf$norm( tf$nn$relu((tf_con - n * 0.02 * tf$ones_like(Con_Mat))), ord = 1)

    tf_hW <- tf_hW 
    tf_loss <- tf_least_squares + lambda_1 * tf_hW + (rho/2) * tf$square(tf$norm(tf_hW))+ 1 *( tf_conw )  + tf_conw2 + beta * 0 * tf$norm(tf_W, ord = 1)
    # tf_loss <- tf_loss + tf$norm(tf$multiply(tf_W, tf$ones_like(Con_Mat) - Con_Mat) , ord = 1) -  tf$norm(tf$multiply(tf_W,  Con_Mat) , ord = 1)
  }
  
  #soft constraint
  # tf_loss <- tf_loss + tf$norm(tf$multiply(tf_W, tf$ones_like(Con_Mat) - Con_Mat) , ord = 1) -  tf$norm(tf$multiply(tf_W,  Con_Mat) , ord = 1) - beta * tf$norm(tf_W, ord = 1)
  # 
  tf_optimizer <- tf$compat$v1$train$AdamOptimizer(tf_learning_rate)
  tf_train <- tf_optimizer$minimize(tf_loss)
  
  ######### Training ############
  sess <- tf$compat$v1$Session(config = tf$compat$v1$ConfigProto(gpu_options = tf$compat$v1$GPUOptions(allow_growth = TRUE)))
  sess$run(tf$compat$v1$global_variables_initializer())
  for (iter in 1:num_iter){
    returnlist <- sess$run(c(tf_train, tf_loss, tf_W, tf_hW, tf_conw, tf_least_squares, tf_conw2))
    loss <- returnlist[[2]]
    W <- returnlist[[3]]
    hw <- returnlist[[4]]
    conw <- returnlist[[5]]
    least <- returnlist[[6]] 
    conw2 <- returnlist[[7]]
    if (iter %% (num_iter/10) == 0){
      cat('iteration: ', iter, " loss: ", loss, 'hw: ', hw, ' conw: ', conw, ' conw2:', conw2, 'least_square:', least, '\n')
    }
  }
  return(returnlist = list(tf_loss = loss, tf_hw = hw, tf_W = W, tf_conw = conw))
}

get_binaryTensor <- function(tf_W, threshold, mode){
  if (mode == 'W'){
    zero <- tf$zeros_like(tf_W)
    return(tf$where(tf_W > threshold, tf_W, zero))
  }
  if (mode == 'EW'){
    ones <- tf$ones_like(tf_W)
    zeros <- tf$zeros_like(tf_W)
    return(tf$where(tf_W > threshold, ones, zeros))
  }
}

get_maxprod <- function(tf_W, tf_Path, n){
  tf_W <- tf$tile(tf_W, as.integer(c(1, n)))
  tf_W <- tf$reshape(tf_W, as.integer( c(n * n, n)))
  tf_Path <- tf$tile(tf_Path, as.integer(c(n, 1)) )
  tf_Re <- tf$multiply(tf_W, tf_Path)
  tf_Re <- tf$reduce_max(tf_Re, axis = as.integer(c(1)))
  tf_Re <- tf$reshape(tf_Re, as.integer(c(n, n)))
  return(tf_Re)
}