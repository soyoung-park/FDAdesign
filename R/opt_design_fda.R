#' Optimal Sampling Design for Functional Data Analysis 
#'
#' Selects optimal sampling points for functional data under Functional Principal Component Analysis (FPCA)
#' and Functional Linear Model (FLM) frameworks. Unified objective function is used to determine optimal points.
#' Joint optimal design points can also be obtained with appropriately defined design criterion matrix, B.
#'
#' @param p   number of optimal sampling points to be selected
#' @param Phi    d by L matrix of eigenfunctions evaluated at d candidate points; L is number of PCs.
#' @param lambda  eigenvalues; a vector of length L
#' @param B  design criterion matrix (e.g. for recovering curves, B = diag(L); a square matrix with dim = L
#' @param sigma2  measurement error variance associated with functional object.
#' 
#' @return index_opt   index of d candidate points that corresponds to the selected optimal points.
#' @return obj_opt   prediction error with the p selected optimal points; i.e. objective function evaluated at the p selected optimal points.
#' @return obj_opt_limit   prediction error with d candidate points (smallest prediction error).
#' @return error.level   obj_opt/obj_opt_limit; relative measure of how large prediction error with the p selected optimal points is to that with d candidate points.
#' @return index_all_comb   all possible combinations of p points from the candidate set; p by (d choose p) matrix
#' @return obj_eval_all   objective function evaluated at \code{index_all_comb}.
#' @return INPUT   input of \code{opt_design_fda} provided as input.
#' 
#' @examples
#' \dontrun{rm(list=ls())
#' library(face)
#' # define true eigen-components and npc
#' K = 5
#' efn_sin <- function(k,t){return(sqrt(2)*sin((k+1)*pi*t))}
#' efn_cos <- function(k,t){return(sqrt(2)*cos((k)*pi*t))}
#' evl <- function(k){return(2^-k*10)} 
#' evalue0 <- sapply(1:K, function(k) evl(k = k))
#' # set sample size and number of repeated measures per subject (7 to 10)
#' n = 400
#' mi.min = 7; mi.max = 10
#' # set true signal to noise ration and compute corresponding 
#' SNR = 5
#' sigma2.true <- sum(evalue0)/SNR
#' # set variance for scalar response Y
#' sigma2.y <- 2^2
#' # set true basis coefficients
#' beta <- matrix(c(4, 2.5, 1.5, 1, 0.5))
#' 
#' #===============================================================
#' # Function Data Generation (irregular / sparse)
#' #===============================================================
#' 
#' set.seed(2016)
#' mi = round(runif(n = n, min = mi.min, max = mi.max))
#' scr.true <- matrix(NA, nrow=n, ncol=K)
#' i.vec <- tt.vec <- c(); Y1.vec <- c()
#' for(subj in 1:n){
#'   # each subject #
#'   m = mi[subj]
#'   ti = sort(runif(n = m, min = 0, max=1))
#'   eigfn <- matrix(NA, nrow=length(ti), ncol = length(evalue0))
#'   for(k in 1:K){
#'     if(!is.integer(k/2)){
#'       eigfn[,k] <- efn_sin(k=k, t=ti)
#'     }else{
#'       eigfn[,k] <- efn_cos(k=k, t=ti)
#'     }
#'   }
#'   scr <- do.call(cbind, lapply(evalue0, function(l) rnorm(1, mean = 0, sd=sqrt(l))))
#'   scr.true[subj,] <- scr
#'   
#'   random <- as.vector(eigfn %*% t(scr))
#'   err <- rnorm(length(random), mean = 0, sd = sqrt(sigma2.true))
#'   
#'   Y1 <- random + err
#'   
#'   i.vec <- c(i.vec,rep(subj, length(ti)))
#'   tt.vec <- c(tt.vec, ti)
#'   Y1.vec <- c(Y1.vec, Y1)
#' }
#' error.y <- rnorm(n = n, mean = 0, sd = sqrt(sigma2.y))
#' scalar.y <- scr.true%*%beta + error.y
#' 
#' myFuncDat <- data.frame(argvals=tt.vec, subj=i.vec, y=Y1.vec)
#' 
#' #===============================================================
#' # Functional Principal Component Analysis (FPCA) Case 
#' #===============================================================
#' 
#' T0 <- 21
#' t.eq <- seq(0,1,length.out=T0)
#' fit <- face.sparse(data = myFuncDat, knots = 10, 
#'                    argvals.new=t.eq, newdata = myFuncDat,
#'                    calculate.scores=TRUE, pve = 0.95)
#' Phi.hat <- fit$eigenfunctions
#' lambda.hat <- fit$eigenvalues
#' sigma2.hat <- mean(as.vector(as.matrix(fit$var.error.new)))
#' 
#' p = 3
#' optT.hat <- opt_design_fda(p=p, Phi=Phi.hat, lambda=lambda.hat, sigma2=sigma2.hat)
#' 
#' names(optT.hat) 
#' # [1] "index_opt"      "obj_opt"        "obj_opt_limit"  "error.level"    "index_all_comb" "obj_eval_all"   "INPUT"         
#' 
#' # selected optimal sampling points
#' optT.hat$index_opt  # [1]  5 14 18
#' t.eq[optT.hat$index_opt]  #[1]  0.20 0.65 0.85
#' # objective function evaluated with T0 = 21 grid of points (the best we can do)
#' optT.hat$obj_opt_limit  #[1]  0.4435131
#' # prediction error with three optimal points
#' optT.hat$obj_opt   # [1] 2.142494
#' # error level with p = 3 
#' optT.hat$obj_opt/optT.hat$obj_opt_limit; optT.hat$error.level # [1] 4.830735
#' 
#' # example of selection_p() function
#' optT.hat.all <- selection_p(p_vec = c(1,3,4), threshold = 5, Phi=Phi.hat, lambda=lambda.hat, sigma2=sigma2.hat)
#' optT.hat.all$p.sel #  [1] 3
#' optT.hat.all$opt.sel[[1]]$index_opt  # [1]  5 14 18 (same as optT.hat$index_opt)
#' 
#' # example of interactive_plot() function
#' optT.hat.first.three <- selection_p(p_vec = 1:3, threshold = 5, Phi=Phi.hat, lambda=lambda.hat, sigma2=sigma2.hat)
#' interactive_plot(optT.hat.first.three)
#' 
#' #===============================================================
#' # Functional Linear Model (FLM) for fixed p = 3
#' #===============================================================
#' scr.hat <- fit$scores$scores
#' Xhat <- scr.hat %*% t(Phi.hat)
#' fit1 <- pfr(scalar.y ~ lf(Xhat, k = 10))
#' coef <- coef(fit1)
#' beta.hat <- t(coef$value)%*% Phi.hat / T0
#' beta.hat <- matrix(beta.hat, nrow=length(beta.hat))
#' 
#' optT.hat <- opt_design_fda(p=p, B = beta.hat%*%t(beta.hat),
#'                            Phi=Phi.hat, lambda=lambda.hat, sigma2=sigma2.hat)
#' 
#' # selected optimal sampling points
#' optT.hat$index_opt  # [1]  4  5 15
#' t.eq[optT.hat$index_opt]  # [1] 0.15 0.20 0.70
#' # objective function evaluated with T0 = 21 grid of points (the best we can do)
#' optT.hat$obj_opt_limit  # [1] 2.696117
#' # prediction error with three optimal points
#' optT.hat$obj_opt   # [1] 7.350046
#' # error level with p = 3 
#' optT.hat$obj_opt/optT.hat$obj_opt_limit; optT.hat$error.level # [1] 2.72616
#' # example of selection_p() function
#' optT.hat.all <- selection_p(p_vec = c(1,3,4), threshold = 5, B = beta.hat%*%t(beta.hat),
#'                          Phi=Phi.hat, lambda=lambda.hat, sigma2=sigma2.hat)
#' optT.hat.all$p.sel #  [1] 3
#' optT.hat.all$opt.sel[[1]]$index_opt  # [1]  4  5 15 (same as optT.hat$index_opt)
#' 
#' # example of interactive_plot() function
#' optT.hat.first.three <- selection_p(p_vec = 1:3, threshold = 5, B = beta.hat%*%t(beta.hat),
#'                                  Phi=Phi.hat, lambda=lambda.hat, sigma2=sigma2.hat)
#' interactive_plot(optT.hat.first.three)
#' 
#' #===============================================================
#' # Joint Optimal Design for fixed p = 3
#' #===============================================================
#' B <- diag(length(lambda.hat))/sum(lambda.hat)
#' B <- B + beta.hat%*% t(beta.hat)/sum(lambda.hat*beta.hat^2)
#' 
#' optT.hat <- opt_fda_search(p=p, B = B,
#'                            Phi=Phi.hat, lambda=lambda.hat, sigma2=sigma2.hat)
#' 
#' # selected optimal sampling points
#' optT.hat$index_opt  # [1]  4 13 17
#' t.eq[optT.hat$index_opt]  # [1] 0.15 0.60 0.80
#' # objective function evaluated with T0 = 21 grid of points (the best we can do)
#' optT.hat$obj_opt_limit  # [1] 0.08552796
#' # prediction error with three optimal points
#' optT.hat$obj_opt   # [1] 0.3832871
#' # error level with p = 3 
#' optT.hat$obj_opt/optT.hat$obj_opt_limit; optT.hat$error.level # [1] 4.481425
#' 
#' # example of selection_p() function
#' optT.hat.all <- selection_p(p_vec = c(1,3,4), threshold = 5, B = B,
#'                          Phi=Phi.hat, lambda=lambda.hat, sigma2=sigma2.hat)
#' optT.hat.all$p.sel #  [1] 3
#' optT.hat.all$opt.sel[[1]]$index_opt  # [1]  4 13 17 (same as optT.hat$index_opt)
#' 
#' # example of interactive_plot() function
#' optT.hat.first.three <- selection_p(p_vec = 1:3, threshold = 5, B = B,
#'                                  Phi=Phi.hat, lambda=lambda.hat, sigma2=sigma2.hat)
#' interactive_plot(optT.hat.first.three)
#' }
#' @author So Young Park \email{spark13@@ncsu.edu},
#' Luo Xiao \email{lxiao5@@ncsu.edu},
#' Ana-Maria Staicu \email{astaicu@@ncsu.edu}
#' @seealso \code{\link{opt_design_fda}} /
#'          \code{\link{selection_p}} /
#'          \code{\link{interactive_plot}}
#' @import shiny
#' @import ggplot2
#' @export
opt_design_fda <- function(p = 2, Phi, lambda, B = NULL, sigma2 = 10^-14){
  # p=p; Phi=Phi.hat; lambda=lambda.hat; sigma2=sigma2.hat; B = NULL
  
  if(min(sigma2) < 0) stop("A positive value for sigma2 is needed!")
  if(min(lambda) <0) stop("Non-negative vlaues for lambda are needed!")
  L <- length(lambda)
  if(L!=ncol(Phi)) stop("No. of columns of Phi should be the same as length of lambda!")
  if(is.null(B)) B <- diag(L)
  if(L!=nrow(B)) stop("Dimension of square matrix Phi should be the same as length of lambda!")
  if(L!=ncol(B)) stop("Dimension of square matrix Phi should be the same as length of lambda!")
  
  TT <- nrow(Phi)
  Btilde <- if(L == 1) lambda*B else diag(sqrt(lambda))%*%B%*%diag(sqrt(lambda))
  if(length(sigma2)==1) sigma2 = rep(sigma2, TT)
  
  opt_fda_obj <- function(index){
    A <- if(L == 1) Phi[index,]*sqrt(lambda) else Phi[index,]%*%diag(sqrt(lambda))
    Sigma2 <- matrix(0,nrow=length(index),ncol=length(index))
    diag(Sigma2) <- sigma2[index]
    S <- t(A)%*%solve(A%*%t(A) + Sigma2)%*%A
    return( sum(diag(Btilde - Btilde%*%S)) )
  }

  index_all_comb <- combn(TT,p)
  obj <- apply(index_all_comb, 2, opt_fda_obj)
  obj_opt <- min(obj)
  index_opt <- index_all_comb[,which.min(obj)]
  
  obj_opt_limit <- opt_fda_obj(1:nrow(Phi))
  error.level = obj_opt/obj_opt_limit
  
  INPUT <- list(p = p, Phi=Phi,lambda=lambda, sigma2 = sigma2 , B = B)
  
  return(list(index_opt=index_opt, 
              obj_opt=obj_opt, 
              obj_opt_limit = obj_opt_limit,
              error.level=error.level,
              index_all_comb = index_all_comb,
              obj_eval_all = obj,
              INPUT = INPUT))
}