library(ggplot2)
library(reshape2)
library(lattice)
library(ggpubr)

PlotHot <- function(W, W_est, m, n){
  W <- melt(W)
  colnames(W) <- c('parent', 'son', 'value')
  
  W_est <- melt(W_est)
  colnames(W_est) <- c('parent', 'son', 'value')
  
  p1<-ggplot(W, aes(x = parent, y = son, fill = value)) #
  p2<-p1+geom_raster()+ scale_fill_gradient2(low="red", high="blue", mid="white") + labs(title = paste('m = ', m, ' n = ', n, ' W_std', sep = ''))
  p3<-ggplot(W_est, aes(x = parent, y = son, fill = value)) #
  p4<-p3+geom_raster()+ scale_fill_gradient2(low="red", high="blue", mid="white") + labs(title = paste('m = ', m, ' n = ', n, ' W_est', sep = ''))
  multiplot( p2,  p4, cols = 2)
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

PlotHot_Single <- function(W){
  # W <- melt(W)
  colnames(W) <- c('parent', 'son', 'value')
  
  p1<-ggplot(W, aes(x = parent, y = son, fill = value)) #
  p2<-p1+geom_raster()+ scale_fill_gradient2(low="red", high="blue", mid="white") #
  p2
}

Plot_Mine <- function(){
  a <- read.csv('vacation/result.csv')
  a <- a[which((a$RCon == 0) | (a$RCon == 0.5)),]
  a <- a[which((a$Noise =='gaussian')),]
  a$RCon = NULL
  a <- unite(a, 'Method', Hway, Mway, Conmat)
  
  
  a$Nodes <- as.factor(a$Nodes)
  a$Dataset <- as.factor(a$Dataset)
  # a$RCon <- as.factor(a$RCon)
  a$Method <- as.factor(a$Method)
  
  colors <- rainbow(5)
  symbols <- c(17,18,19,20,21)
  linetype <- c(1,1,1,1,1)
  key.species <- list(x = 0.75, y = 0.75, corner = c(0,0), title ='RCon',
                      text = list(c('Empty', 'AMat_unweight', 'AMat_weight', 'TMat_unweight', 'TMat_weight')),
                      points = list(pch = symbols, col = colors))
  xyplot(FPR~Dataset|Nodes, data = a, groups = Method, type = 'o',cex = 0.5, pch = symbols, col = colors, lty = linetype, lwd = 2, ylab = 'SHD', xlab = 'NumOfData', key = key.species, scales = 'free')
}

Plot_ConRate <- function(){

  Nodes <- c()
  Methods <- c()
  Rate <- c()
  TPR <- c()
  FPR <- c()
  SHD <- c()
  a <- read.csv('middle_linear_gaussian/NoTears/result.csv')
  for (i in 1:nrow(a)){
    Nodes <-c(Nodes, a[i,'Nodes'])
    Methods <- c(Methods, a[i, 'Methods'])
    Rate <- c(Rate, 0)
    TPR <-c(TPR, a[i, 'TPR'])
    FPR <-c(FPR, a[i, 'FPR'])
    SHD <- c(SHD, a[i, 'SHD'])
  }

  a <- read.csv('middle_linear_gaussian/DAG-GNN/result.csv')
  for (i in 1:nrow(a)){
    Nodes <-c(Nodes, a[i,'Nodes'])
    Methods <- c(Methods, a[i, 'Methods'])
    Rate <- c(Rate, 0)
    TPR <-c(TPR, a[i, 'TPR'])
    FPR <-c(FPR, a[i, 'FPR'])
    SHD <- c(SHD, a[i, 'SHD'])
  }

  a <- read.csv('middle_linear_gaussian/result.csv')
  for (i in 1:nrow(a)){
    Nodes <-c(Nodes, a[i,'Nodes'])
    Methods <- c(Methods, paste('CCF_', a[i, 'Tight'], sep = ''))
    Rate <- c(Rate, a[i, 'RCon'])
    TPR <-c(TPR, a[i, 'TPR'])
    FPR <-c(FPR, a[i, 'FPR'])
    SHD <- c(SHD, a[i, 'SHD'])
  }  
  result <- data.frame(Nodes = Nodes, Methods = Methods, Rate = Rate, TPR = TPR, FPR = FPR, SHD = SHD)
  
  need <- 'In'
  if(need == 'In'){
    result1 <- result[which(result$Methods %in% c('CCF_Empty', 'CCF_path')),]
    result2 <- result[which(result$Methods %in% c('CCF_Empty', 'CCF_edge')),]
    result1$Rate <- factor(result1$Rate,
                              levels = c(0, 0.2, 0.5, 1.0),
                              labels = c("CCF_Empty","CCF_Path (p = 0.2)","CCF_Path (p = 0.5)", "CCF_Path (p = 1.0)"))
    result1$Methods <- NULL
    colnames(result1)[2] <- 'Methods'
    
    result2$Rate <- factor(result2$Rate,
                           levels = c(0, 0.2, 0.5, 1.0),
                           labels = c("CCF_Empty","CCF_Edge (p = 0.2)","CCF_Edge (p = 0.5)", "CCF_Edge (p = 1.0)"))
    result2$Methods <- NULL
    colnames(result2)[2] <- 'Methods'
  }
  if(need == "Out"){
    result3 <- result[which(result$Rate %in% c(0, 0.5)),]
  }
  
  
  p1 <- ggplot(data = result1, aes(x = Nodes, y = SHD,  color = Methods, shape = Methods)) + 
    geom_point(size = 3) + 
    geom_line(size = 1) + 
    labs(x = "Nodes", y = "SHD")  + theme(legend.position = 'none') +
    scale_x_continuous(breaks=seq(0, 120, 20)) 
  p2 <- ggplot(data = result1, aes(x = Nodes, y = TPR,  color = Methods, shape = Methods)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = "Nodes", y = "TPR")  + theme(legend.position = 'none')+
    scale_x_continuous(breaks=seq(0, 120, 20)) 
  p3 <- ggplot(data = result1, aes(x = Nodes, y = FPR,  color = Methods, shape = Methods)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = "Nodes", y = "FPR")  + theme(legend.position = 'none')+
    scale_x_continuous(breaks=seq(0, 120, 20)) 

  p4 <- ggplot(data = result2, aes(x = Nodes, y = SHD,  color = Methods, shape = Methods)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = "Nodes", y = "SHD") + theme(legend.position = 'none')+
    scale_x_continuous(breaks=seq(0, 120, 20)) 
  p5 <- ggplot(data = result2, aes(x = Nodes, y = TPR,  color = Methods, shape = Methods)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = "Nodes", y = "TPR")  + theme(legend.position = 'none')+
    scale_x_continuous(breaks=seq(0, 120, 20)) 
  p6 <- ggplot(data = result2, aes(x = Nodes, y = FPR,  color = Methods, shape = Methods)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = "Nodes", y = "FPR")  + theme(legend.position = 'none')+
    scale_x_continuous(breaks=seq(0, 120, 20)) 


  p7 <- ggarrange(p1, p2, p3,  ncol=3, nrow=1, common.legend = TRUE, legend="bottom")
  p8 <- ggarrange(p4, p5, p6,  ncol=3, nrow=1, common.legend = TRUE, legend="bottom")
  ggarrange(p7, p8,  ncol=1, nrow=2, common.legend = FALSE)
}

Plot_Linear_gaussian_exp <- function(){
  
  Nodes <- c()
  Methods <- c()
  Rate <- c()
  TPR <- c()
  FPR <- c()
  SHD <- c()
  a <- read.csv('middle_linear_gaussian/NoTears/result.csv')
  for (i in 1:nrow(a)){
    Nodes <-c(Nodes, a[i,'Nodes'])
    Methods <- c(Methods, a[i, 'Methods'])
    Rate <- c(Rate, 0)
    TPR <-c(TPR, a[i, 'TPR'])
    FPR <-c(FPR, a[i, 'FPR'])
    SHD <- c(SHD, a[i, 'SHD'])
  }
  
  a <- read.csv('middle_linear_gaussian/DAG-GNN/result.csv')
  for (i in 1:nrow(a)){
    Nodes <-c(Nodes, a[i,'Nodes'])
    Methods <- c(Methods, a[i, 'Methods'])
    Rate <- c(Rate, 0)
    TPR <-c(TPR, a[i, 'TPR'])
    FPR <-c(FPR, a[i, 'FPR'])
    SHD <- c(SHD, a[i, 'SHD'])
  }
  
  a <- read.csv('middle_linear_gaussian/result.csv')
  for (i in 1:nrow(a)){
    Nodes <-c(Nodes, a[i,'Nodes'])
    Methods <- c(Methods, paste('CCF_', a[i, 'Tight'], sep = ''))
    Rate <- c(Rate, a[i, 'RCon'])
    TPR <-c(TPR, a[i, 'TPR'])
    FPR <-c(FPR, a[i, 'FPR'])
    SHD <- c(SHD, a[i, 'SHD'])
  }  
  
  result <- data.frame(Nodes = Nodes, Methods = Methods, Rate = Rate, TPR = TPR, FPR = FPR, SHD = SHD)
  result <- result[which(result$Rate %in% c(0, 0.5)),]
  
  
  p1 <- ggplot(data = result, aes(x = Nodes, y = SHD,  color = Methods, shape = Methods)) + 
    geom_point(size = 3) + 
    geom_line(size = 1) + 
    labs(x = "Nodes", y = "SHD", title = "Gaussian Noise")  + theme(legend.position = 'none') +
c
  p2 <- ggplot(data = result, aes(x = Nodes, y = TPR,  color = Methods, shape = Methods)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = "Nodes", y = "TPR", title = "")  + theme(legend.position = 'none')+
    scale_x_continuous(breaks=seq(0, 120, 20)) 
  p3 <- ggplot(data = result, aes(x = Nodes, y = FPR,  color = Methods, shape = Methods)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = "Nodes", y = "FPR", title = "")  + theme(legend.position = 'none')+
    scale_x_continuous(breaks=seq(0, 120, 20)) 
  
  
  Nodes <- c()
  Methods <- c()
  Rate <- c()
  TPR <- c()
  FPR <- c()
  SHD <- c()
  a <- read.csv('middle_linear_exp/NoTears/result.csv')
  for (i in 1:nrow(a)){
    Nodes <-c(Nodes, a[i,'Nodes'])
    Methods <- c(Methods, a[i, 'Methods'])
    Rate <- c(Rate, 0)
    TPR <-c(TPR, a[i, 'TPR'])
    FPR <-c(FPR, a[i, 'FPR'])
    SHD <- c(SHD, a[i, 'SHD'])
  }
  
  a <- read.csv('middle_linear_exp/DAG-GNN/result.csv')
  for (i in 1:nrow(a)){
    Nodes <-c(Nodes, a[i,'Nodes'])
    Methods <- c(Methods, a[i, 'Methods'])
    Rate <- c(Rate, 0)
    TPR <-c(TPR, a[i, 'TPR'])
    FPR <-c(FPR, a[i, 'FPR'])
    SHD <- c(SHD, a[i, 'SHD'])
  }
  
  a <- read.csv('middle_linear_exp/result.csv')
  for (i in 1:nrow(a)){
    Nodes <-c(Nodes, a[i,'Nodes'])
    Methods <- c(Methods, paste('CCF_', a[i, 'Tight'], sep = ''))
    Rate <- c(Rate, a[i, 'RCon'])
    TPR <-c(TPR, a[i, 'TPR'])
    FPR <-c(FPR, a[i, 'FPR'])
    SHD <- c(SHD, a[i, 'SHD'])
  }  
  
  result <- data.frame(Nodes = Nodes, Methods = Methods, Rate = Rate, TPR = TPR, FPR = FPR, SHD = SHD)
  result <- result[which(result$Rate %in% c(0, 0.5)),]
  
  p4 <- ggplot(data = result, aes(x = Nodes, y = SHD,  color = Methods, shape = Methods)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = "Nodes", y = "SHD", title = "Exponential Noise") + theme(legend.position = 'none')+
    scale_x_continuous(breaks=seq(0, 120, 20)) 
  p5 <- ggplot(data = result, aes(x = Nodes, y = TPR,  color = Methods, shape = Methods)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = "Nodes", y = "TPR" , title = "")  + theme(legend.position = 'none')+
    scale_x_continuous(breaks=seq(0, 120, 20))
  p6 <- ggplot(data = result, aes(x = Nodes, y = FPR,  color = Methods, shape = Methods)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = "Nodes", y = "FPR", title = "")  + theme(legend.position = 'none')+
    scale_x_continuous(breaks=seq(0, 120, 20)) 
  
  ggarrange(p1,p2,p3,p4,p5,p6,  ncol=3, nrow=2, common.legend = TRUE,legend="right")
}

Plot_nonLinear_gaussian_exp <- function(){
  
  Nodes <- c()
  Methods <- c()
  Rate <- c()
  TPR <- c()
  FPR <- c()
  SHD <- c()
  a <- read.csv('middle_nonlinear_gaussian/NoTears/result.csv')
  for (i in 1:nrow(a)){
    Nodes <-c(Nodes, a[i,'Nodes'])
    Methods <- c(Methods, 'NoTears')
    Rate <- c(Rate, 0)
    TPR <-c(TPR, a[i, 'TPR'])
    FPR <-c(FPR, a[i, 'FPR'])
    SHD <- c(SHD, a[i, 'SHD'])
  }
  
  a <- read.csv('middle_nonlinear_gaussian/DAG-GNN/result.csv')
  for (i in 1:nrow(a)){
    Nodes <-c(Nodes, a[i,'Nodes'])
    Methods <- c(Methods, a[i, 'Methods'])
    Rate <- c(Rate, 0)
    TPR <-c(TPR, a[i, 'TPR'])
    FPR <-c(FPR, a[i, 'FPR'])
    SHD <- c(SHD, a[i, 'SHD'])
  }
  
  a <- read.csv('middle_nonlinear_gaussian/result.csv')
  for (i in 1:nrow(a)){
    Nodes <-c(Nodes, a[i,'Nodes'])
    Methods <- c(Methods, paste('CCF_', a[i, 'Tight'], sep = ''))
    Rate <- c(Rate, a[i, 'RCon'])
    TPR <-c(TPR, a[i, 'TPR'])
    FPR <-c(FPR, a[i, 'FPR'])
    SHD <- c(SHD, a[i, 'SHD'])
  }  
  
  result <- data.frame(Nodes = Nodes, Methods = Methods, Rate = Rate, TPR = TPR, FPR = FPR, SHD = SHD)
  result <- result[which(result$Rate %in% c(0, 0.5)),]
  
  
  p1 <- ggplot(data = result, aes(x = Nodes, y = SHD,  color = Methods, shape = Methods)) + 
    geom_point(size = 3) + 
    geom_line(size = 1) + 
    labs(x = "Nodes", y = "SHD", title = "Gaussian Noise")  + theme(legend.position = 'none') +
    scale_x_continuous(breaks=seq(0, 120, 20)) 
  p2 <- ggplot(data = result, aes(x = Nodes, y = TPR,  color = Methods, shape = Methods)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = "Nodes", y = "TPR", title = "")  + theme(legend.position = 'none')+
    scale_x_continuous(breaks=seq(0, 120, 20)) 
  p3 <- ggplot(data = result, aes(x = Nodes, y = FPR,  color = Methods, shape = Methods)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = "Nodes", y = "FPR", title = "")  + theme(legend.position = 'none')+
    scale_x_continuous(breaks=seq(0, 120, 20)) 
  
  
  Nodes <- c()
  Methods <- c()
  Rate <- c()
  TPR <- c()
  FPR <- c()
  SHD <- c()
  a <- read.csv('middle_nonlinear_exp/NoTears/result.csv')
  for (i in 1:nrow(a)){
    Nodes <-c(Nodes, a[i,'Nodes'])
    Methods <- c(Methods, a[i, 'Methods'])
    Rate <- c(Rate, 0)
    TPR <-c(TPR, a[i, 'TPR'])
    FPR <-c(FPR, a[i, 'FPR'])
    SHD <- c(SHD, a[i, 'SHD'])
  }

  a <- read.csv('middle_nonlinear_exp/DAG-GNN/result.csv')
  for (i in 1:nrow(a)){
    Nodes <-c(Nodes, a[i,'Nodes'])
    Methods <- c(Methods, a[i, 'Methods'])
    Rate <- c(Rate, 0)
    TPR <-c(TPR, a[i, 'TPR'])
    FPR <-c(FPR, a[i, 'FPR'])
    SHD <- c(SHD, a[i, 'SHD'])
  }

  a <- read.csv('middle_nonlinear_exp/result.csv')
  for (i in 1:nrow(a)){
    Nodes <-c(Nodes, a[i,'Nodes'])
    Methods <- c(Methods, paste('CCF_', a[i, 'Tight'], sep = ''))
    Rate <- c(Rate, a[i, 'RCon'])
    TPR <-c(TPR, a[i, 'TPR'])
    FPR <-c(FPR, a[i, 'FPR'])
    SHD <- c(SHD, a[i, 'SHD'])
  }

  result <- data.frame(Nodes = Nodes, Methods = Methods, Rate = Rate, TPR = TPR, FPR = FPR, SHD = SHD)
  result <- result[which(result$Rate %in% c(0, 0.5)),]

  p4 <- ggplot(data = result, aes(x = Nodes, y = SHD,  color = Methods, shape = Methods)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = "Nodes", y = "SHD", title = "Exponential Noise") + theme(legend.position = 'none')+
    scale_x_continuous(breaks=seq(0, 120, 20))
  p5 <- ggplot(data = result, aes(x = Nodes, y = TPR,  color = Methods, shape = Methods)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = "Nodes", y = "TPR" , title = "")  + theme(legend.position = 'none')+
    scale_x_continuous(breaks=seq(0, 120, 20))
  p6 <- ggplot(data = result, aes(x = Nodes, y = FPR,  color = Methods, shape = Methods)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = "Nodes", y = "FPR", title = "")  + theme(legend.position = 'none')+
    scale_x_continuous(breaks=seq(0, 120, 20))
  
  ggarrange(p1,p2,p3,p4,p5,p6,  ncol=3, nrow=2, common.legend = TRUE,legend="right")
}

Plot_Weight<- function(){
  
  a1 <- sample(1:40,40)
  W <- read.csv(file = 'middle_linear_gaussian/WeightTrueDAG/Syn40.csv')
  W <- as.matrix(W)
  colnames(W) <- as.character(a1)
  rownames(W) <- as.character(a1)
  W <- melt(W)
  W_DAGGNN <- read.table(file = 'middle_linear_gaussian/DAG-GNN/Syn4030predG')
  W_DAGGNN <- as.matrix(W_DAGGNN)
  colnames(W_DAGGNN) <- as.character(a1)
  rownames(W_DAGGNN) <- as.character(a1)
  W_DAGGNN <- melt(W_DAGGNN)
  W_CCF <- read.csv(file = 'middle_linear_gaussian/MyMethod/Syn40_path_0.5.csv')
  W_CCF <- as.matrix(W_CCF)
  colnames(W_CCF) <- as.character(a1)
  rownames(W_CCF) <- as.character(a1)
  W_CCF <- melt(W_CCF)
  
  p1 <-ggplot(W,aes(x=Var2,y=Var1,fill=value))+xlab('W_true')+ylab(NULL) +geom_raster() +
    scale_fill_gradient2(low='blue',high='red', mid = 'white') +
    theme(panel.background = element_rect(fill = 'white', color = 'black'),panel.grid.major =element_blank(), 
          panel.grid.minor = element_blank()) + scale_x_continuous(limits = c(0,41),expand = c(0, 0)) + scale_y_continuous(limits = c(0,41),expand = c(0, 0)) +
    labs(title = 'Linear Gaussian Model')
  p2 <- ggplot(W_DAGGNN,aes(x=Var2,y=Var1,fill=value))+xlab('W_DAG-GNN')+ylab(NULL) +geom_raster() +
    scale_fill_gradient2(low='blue',high='red', mid = 'white')+
    theme(panel.background = element_rect(fill = 'white', color = 'black'),panel.grid.major =element_blank(), 
          panel.grid.minor = element_blank())+ scale_x_continuous(limits = c(0,41),expand = c(0, 0)) + scale_y_continuous(limits = c(0,41),expand = c(0, 0)) +
    labs(title = '')
  p3 <- ggplot(W_CCF,aes(x=Var2,y=Var1,fill=value))+xlab('W_NOTEARS')+ylab(NULL) +geom_raster() +
    scale_fill_gradient2(low='blue',high='red', mid = 'white')+
    theme(panel.background = element_rect(fill = 'white', color = 'black'),panel.grid.major =element_blank(), 
          panel.grid.minor = element_blank())+ scale_x_continuous(limits = c(0,41),expand = c(0, 0)) + scale_y_continuous(limits = c(0,41),expand = c(0, 0)) +
    labs(title = '')
  
  a1 <- sample(1:40,40)
  W <- read.csv(file = 'middle_nonlinear_gaussian/WeightTrueDAG/Syn40.csv')
  W <- as.matrix(W)
  colnames(W) <- as.character(a1)
  rownames(W) <- as.character(a1)
  W <- melt(W)
  W_DAGGNN <- read.table(file = 'middle_nonlinear_gaussian/DAG-GNN/Syn4030predG')
  W_DAGGNN <- as.matrix(W_DAGGNN)
  colnames(W_DAGGNN) <- as.character(a1)
  rownames(W_DAGGNN) <- as.character(a1)
  W_DAGGNN <- melt(W_DAGGNN)
  W_CCF <- read.csv(file = 'middle_nonlinear_gaussian/MyMethod/Syn40_path_0.5.csv')
  W_CCF <- as.matrix(W_CCF)
  colnames(W_CCF) <- as.character(a1)
  rownames(W_CCF) <- as.character(a1)
  W_CCF <- melt(W_CCF)
  
  p4 <-ggplot(W,aes(x=Var2,y=Var1,fill=value))+xlab('W_true')+ylab(NULL) +geom_raster() +
    scale_fill_gradient2(low='blue',high='red', mid = 'white') +
    theme(panel.background = element_rect(fill = 'white', color = 'black'),panel.grid.major =element_blank(), 
          panel.grid.minor = element_blank()) + scale_x_continuous(limits = c(0,41),expand = c(0, 0)) + scale_y_continuous(limits = c(0,41),expand = c(0, 0)) +
    labs(title = 'Non-linear Gaussian Model')
  p5 <- ggplot(W_DAGGNN,aes(x=Var2,y=Var1,fill=value))+xlab('W_DAGGNN')+ylab(NULL) +geom_raster() +
    scale_fill_gradient2(low='blue',high='red', mid = 'white')+
    theme(panel.background = element_rect(fill = 'white', color = 'black'),panel.grid.major =element_blank(), 
          panel.grid.minor = element_blank())+ scale_x_continuous(limits = c(0,41),expand = c(0, 0)) + scale_y_continuous(limits = c(0,41),expand = c(0, 0)) +
    labs(title = '')
  p6 <- ggplot(W_CCF,aes(x=Var2,y=Var1,fill=value))+xlab('W_CCFpath')+ylab(NULL) +geom_raster() +
    scale_fill_gradient2(low='blue',high='red', mid = 'white')+
    theme(panel.background = element_rect(fill = 'white', color = 'black'),panel.grid.major =element_blank(), 
          panel.grid.minor = element_blank())+ scale_x_continuous(limits = c(0,41),expand = c(0, 0)) + scale_y_continuous(limits = c(0,41),expand = c(0, 0)) +
    labs(title = '')
  
  # ggarrange(p1,p2,p3,p4,p5,p6,  ncol=3, nrow=2, common.legend = TRUE,legend="right")
  ggarrange(p1,p2,  ncol=2, nrow=1, common.legend = TRUE,legend="right")
}

Plot_threshold <- function(){
  data <- read.csv(file = 'threshold/result.csv')
  data <- data[-1,]
  data$p1 <- as.factor(data$p1)
  g1 <- ggplot(data, aes(x = p1, y = SHD, fill = p1)) +
    geom_boxplot(alpha=0.7) +
    scale_y_continuous(name = "SHD")+
    scale_x_discrete(name = "thresholding") +
    theme_bw() + guides(fill=F) + theme(panel.grid.major=element_blank()) +
    theme(plot.title = element_text(size = 14, face =  "bold"),
          text = element_text(size = 12),
          axis.title = element_text(face="bold"),
          axis.text.x=element_text(size = 11)) 
  g2 <- ggplot(data, aes(x = p1, y = TPR, fill = p1)) +
    geom_boxplot(alpha=0.7) +
    scale_y_continuous(name = "TPR")+
    scale_x_discrete(name = "thresholding") +
    theme_bw() + guides(fill=F) + theme(panel.grid.major=element_blank()) +
    theme(plot.title = element_text(size = 14, face =  "bold"),
          text = element_text(size = 12),
          axis.title = element_text(face="bold"),
          axis.text.x=element_text(size = 11)) 
  g3 <- ggplot(data, aes(x = p1, y = FPR, fill = p1)) +
    geom_boxplot(alpha=0.7) +
    scale_y_continuous(name = "FPR")+
    scale_x_discrete(name = "thresholding") +
    theme_bw() + guides(fill=F) + theme(panel.grid.major=element_blank()) +
    theme(plot.title = element_text(size = 14, face =  "bold"),
          text = element_text(size = 12),
          axis.title = element_text(face="bold"),
          axis.text.x=element_text(size = 11)) 
  ggarrange(g1,g2,g3,  ncol=3, nrow=1, common.legend = FALSE,legend="right")
}

Plot_data_Linear_gaussian_exp <- function(){
  
  NumData <- c()
  Methods <- c()
  Rate <- c()
  TPR <- c()
  FPR <- c()
  SHD <- c()
  a <- read.csv('data_linear_gaussian/NoTears/result.csv')
  for (i in 1:nrow(a)){
    NumData <-c(NumData, a[i,'data'])
    Methods <- c(Methods, a[i, 'Methods'])
    Rate <- c(Rate, 0)
    TPR <-c(TPR, a[i, 'TPR'])
    FPR <-c(FPR, a[i, 'FPR'])
    SHD <- c(SHD, a[i, 'SHD'])
  }
  
  a <- read.csv('data_linear_gaussian/DAG-GNN/result.csv')
  for (i in 1:nrow(a)){
    NumData <-c(NumData, a[i,'data'])
    Methods <- c(Methods, a[i, 'Methods'])
    Rate <- c(Rate, 0)
    TPR <-c(TPR, a[i, 'TPR'])
    FPR <-c(FPR, a[i, 'FPR'])
    SHD <- c(SHD, a[i, 'SHD'])
  }
  
  a <- read.csv('data_linear_gaussian/result.csv')
  for (i in 1:nrow(a)){
    NumData <-c(NumData, a[i,'Dataset'])
    if(a[i, 'Tight'] == 'edge')
      Methods <- c(Methods, 'CCF_Edge')
    else if(a[i, 'Tight'] == 'path')
      Methods <- c(Methods, 'CCF_Path')
    else if(a[i, 'Tight'] == 'ordering')
      Methods <- c(Methods, 'CCF_Ordering')
    else
      Methods <- c(Methods, paste('CCF_', a[i, 'Tight'], sep = ''))  
    Rate <- c(Rate, a[i, 'RCon'])
    TPR <-c(TPR, a[i, 'TPR'])
    FPR <-c(FPR, a[i, 'FPR'])
    SHD <- c(SHD, a[i, 'SHD'])
  }  
  
  result <- data.frame(NumData = NumData, Methods = Methods, Rate = Rate, TPR = TPR, FPR = FPR, SHD = SHD)
  result <- result[which(result$Rate %in% c(0, 0.5)),]
  result <- result[which(result$Methods != "CCF_Empty"),]
  
  
  p1 <- ggplot(data = result, aes(x = NumData, y = SHD,  color = Methods, shape = Methods)) + 
    geom_point(size = 3) + 
    geom_line(size = 1) + 
    labs(x = "NumOfData", y = "SHD", title = "Gaussian Noise")  + theme(legend.position = 'none') + 
    scale_x_continuous(breaks=c(10,30,60,100)) + theme_bw()
  p2 <- ggplot(data = result, aes(x = NumData, y = TPR,  color = Methods, shape = Methods)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = "NumOfData", y = "TPR", title = "")  + theme(legend.position = 'none') +
    scale_x_continuous(breaks=c(10,30,60,100)) + theme_bw()
  p3 <- ggplot(data = result, aes(x = NumData, y = FPR,  color = Methods, shape = Methods)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = "NumOfData", y = "FPR", title = "")  + theme(legend.position = 'none') +
    scale_x_continuous(breaks=c(10,30,60,100)) + theme_bw()
  
  
  NumData <- c()
  Methods <- c()
  Rate <- c()
  TPR <- c()
  FPR <- c()
  SHD <- c()
  a <- read.csv('data_linear_exp/NoTears/result.csv')
  for (i in 1:nrow(a)){
    NumData <-c(NumData, a[i,'data'])
    Methods <- c(Methods, a[i, 'Methods'])
    Rate <- c(Rate, 0)
    TPR <-c(TPR, a[i, 'TPR'])
    FPR <-c(FPR, a[i, 'FPR'])
    SHD <- c(SHD, a[i, 'SHD'])
  }
  
  a <- read.csv('data_linear_exp/DAG-GNN/result.csv')
  for (i in 1:nrow(a)){
    NumData <-c(NumData, a[i,'data'])
    Methods <- c(Methods, a[i, 'Methods'])
    Rate <- c(Rate, 0)
    TPR <-c(TPR, a[i, 'TPR'])
    FPR <-c(FPR, a[i, 'FPR'])
    SHD <- c(SHD, a[i, 'SHD'])
  }
  
  a <- read.csv('data_linear_exp/result.csv')
  for (i in 1:nrow(a)){
    NumData <-c(NumData, a[i,'Dataset'])
    if(a[i, 'Tight'] == 'edge')
      Methods <- c(Methods, 'CCF_Edge')
    else if(a[i, 'Tight'] == 'path')
      Methods <- c(Methods, 'CCF_Path')
    else if(a[i, 'Tight'] == 'ordering')
      Methods <- c(Methods, 'CCF_Ordering')
    else
      Methods <- c(Methods, paste('CCF_', a[i, 'Tight'], sep = ''))  
    Rate <- c(Rate, a[i, 'RCon'])
    TPR <-c(TPR, a[i, 'TPR'])
    FPR <-c(FPR, a[i, 'FPR'])
    SHD <- c(SHD, a[i, 'SHD'])
  }  
  
  result <- data.frame(NumData = NumData, Methods = Methods, Rate = Rate, TPR = TPR, FPR = FPR, SHD = SHD)
  result <- result[which(result$Rate %in% c(0, 0.5)),]
  result <- result[which(result$Methods != "CCF_Empty"),]
  
  p4 <- ggplot(data = result, aes(x = NumData, y = SHD,  color = Methods, shape = Methods)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = "NumOfData", y = "SHD", title = "Exponential Noise") + theme(legend.position = 'none')+
    scale_x_continuous(breaks=c(10,30,60,100)) + theme_bw()
  p5 <- ggplot(data = result, aes(x = NumData, y = TPR,  color = Methods, shape = Methods)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = "NumOfData", y = "TPR" , title = "")  + theme(legend.position = 'none')+
    scale_x_continuous(breaks=c(10,30,60,100)) + theme_bw()
  p6 <- ggplot(data = result, aes(x = NumData, y = FPR,  color = Methods, shape = Methods)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = "NumOfData", y = "FPR", title = "")  + theme(legend.position = 'none')+
    scale_x_continuous(breaks=c(10,30,60,100)) + theme_bw()
  
  ggarrange(p1,p2,p3,p4,p5,p6,  ncol=3, nrow=2, common.legend = TRUE,legend="right")
}

Plot_data_nonlinear_gaussian_exp <- function(){
  
  NumData <- c()
  Methods <- c()
  Rate <- c()
  TPR <- c()
  FPR <- c()
  SHD <- c()
  a <- read.csv('data_nonlinear_gaussian/NoTears/result.csv')
  for (i in 1:nrow(a)){
    NumData <-c(NumData, a[i,'data'])
    Methods <- c(Methods, a[i, 'Methods'])
    Rate <- c(Rate, 0)
    TPR <-c(TPR, a[i, 'TPR'])
    FPR <-c(FPR, a[i, 'FPR'])
    SHD <- c(SHD, a[i, 'SHD'])
  }
  
  a <- read.csv('data_nonlinear_gaussian/DAG-GNN/result.csv')
  for (i in 1:nrow(a)){
    NumData <-c(NumData, a[i,'data'])
    Methods <- c(Methods, a[i, 'Methods'])
    Rate <- c(Rate, 0)
    TPR <-c(TPR, a[i, 'TPR'])
    FPR <-c(FPR, a[i, 'FPR'])
    SHD <- c(SHD, a[i, 'SHD'])
  }
  
  a <- read.csv('data_nonlinear_gaussian/result.csv')
  for (i in 1:nrow(a)){
    NumData <-c(NumData, a[i,'Dataset'])
    Methods <- c(Methods, paste('CCF_', a[i, 'Tight'], sep = ''))
    Rate <- c(Rate, a[i, 'RCon'])
    TPR <-c(TPR, a[i, 'TPR'])
    FPR <-c(FPR, a[i, 'FPR'])
    SHD <- c(SHD, a[i, 'SHD'])
  }  
  
  result <- data.frame(NumData = NumData, Methods = Methods, Rate = Rate, TPR = TPR, FPR = FPR, SHD = SHD)
  result <- result[which(result$Rate %in% c(0, 0.5)),]
  
  
  p1 <- ggplot(data = result, aes(x = NumData, y = SHD,  color = Methods, shape = Methods)) + 
    geom_point(size = 3) + 
    geom_line(size = 1) + 
    labs(x = "NumOfData", y = "SHD", title = "Gaussian Noise")  + theme(legend.position = 'none') + 
    scale_x_continuous(breaks=c(10,30,60,100)) + theme_bw()
  p2 <- ggplot(data = result, aes(x = NumData, y = TPR,  color = Methods, shape = Methods)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = "NumOfData", y = "TPR", title = "")  + theme(legend.position = 'none') +
    scale_x_continuous(breaks=c(10,30,60,100)) + theme_bw()
  p3 <- ggplot(data = result, aes(x = NumData, y = FPR,  color = Methods, shape = Methods)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = "NumOfData", y = "FPR", title = "")  + theme(legend.position = 'none') +
    scale_x_continuous(breaks=c(10,30,60,100)) + theme_bw()
  
  
  NumData <- c()
  Methods <- c()
  Rate <- c()
  TPR <- c()
  FPR <- c()
  SHD <- c()
  a <- read.csv('data_nonlinear_exp/NoTears/result.csv')
  for (i in 1:nrow(a)){
    NumData <-c(NumData, a[i,'data'])
    Methods <- c(Methods, a[i, 'Methods'])
    Rate <- c(Rate, 0)
    TPR <-c(TPR, a[i, 'TPR'])
    FPR <-c(FPR, a[i, 'FPR'])
    SHD <- c(SHD, a[i, 'SHD'])
  }
  
  a <- read.csv('data_nonlinear_exp/DAG-GNN/result.csv')
  for (i in 1:nrow(a)){
    NumData <-c(NumData, a[i,'data'])
    Methods <- c(Methods, a[i, 'Methods'])
    Rate <- c(Rate, 0)
    TPR <-c(TPR, a[i, 'TPR'])
    FPR <-c(FPR, a[i, 'FPR'])
    SHD <- c(SHD, a[i, 'SHD'])
  }
  
  a <- read.csv('data_nonlinear_exp/result.csv')
  for (i in 1:nrow(a)){
    NumData <-c(NumData, a[i,'Dataset'])
    Methods <- c(Methods, paste('CCF_', a[i, 'Tight'], sep = ''))
    Rate <- c(Rate, a[i, 'RCon'])
    TPR <-c(TPR, a[i, 'TPR'])
    FPR <-c(FPR, a[i, 'FPR'])
    SHD <- c(SHD, a[i, 'SHD'])
  }  
  
  result <- data.frame(NumData = NumData, Methods = Methods, Rate = Rate, TPR = TPR, FPR = FPR, SHD = SHD)
  result <- result[which(result$Rate %in% c(0, 0.5)),]
  
  p4 <- ggplot(data = result, aes(x = NumData, y = SHD,  color = Methods, shape = Methods)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = "NumOfData", y = "SHD", title = "Exponential Noise") + theme(legend.position = 'none')+
    scale_x_continuous(breaks=c(10,30,60,100)) + theme_bw()
  p5 <- ggplot(data = result, aes(x = NumData, y = TPR,  color = Methods, shape = Methods)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = "NumOfData", y = "TPR" , title = "")  + theme(legend.position = 'none')+
    scale_x_continuous(breaks=c(10,30,60,100)) + theme_bw()
  p6 <- ggplot(data = result, aes(x = NumData, y = FPR,  color = Methods, shape = Methods)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = "NumOfData", y = "FPR", title = "")  + theme(legend.position = 'none')+
    scale_x_continuous(breaks=c(10,30,60,100)) + theme_bw()
  
  ggarrange(p1,p2,p3,p4,p5,p6,  ncol=3, nrow=2, common.legend = TRUE,legend="right")
}