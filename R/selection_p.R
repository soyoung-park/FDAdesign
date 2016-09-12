#' Optimal Sampling Design for Functional Data Analysis - selection of number of optimal points
#'
#' Selects number of optimal points, p, from a vector of p's (\code{p_vec}) given as input. 
#' This function also returns results of the \link{opt_design_fda} function for given p's (\code{p_vec}).
#'
#' @param p_vec   a vector of p's 
#' @param threshold   user-specified threshold h for selecting p (relative error levels)
#' @param Phi   d by L matrix of eigenfunctions evaluated at d candidate points; L is number of PCs.
#' @param lambda   eigenvalues; a vector of length L
#' @param B   design criterion matrix (e.g. for recovering curves, B = diag(L); a square matrix with dim = L
#' @param sigma2   measurement error variance associated with functional object.
#' 
#' @return plot  plot of error levels corresponding to \code{p_vec}.
#' @return p.sel  number of optimal points, p, determined with given threshold.
#' @return opt.sel  result of \link{opt_design_fda} corresponding to the selected p, \code{p.sel}.
#' @return opt_result results of \link{opt_design_fda} corresponding to \code{p_vec}.
#' @return INPUT input of \code{selection_p} provided as input.
#' 
#' @examples 
#' # see example for opt_design_fda function
#' @author So Young Park \email{spark13@@ncsu.edu},
#' Luo Xiao \email{lxiao5@@ncsu.edu},
#' Ana-Maria Staicu \email{astaicu@@ncsu.edu}
#' @seealso \code{\link{opt_design_fda}} / \code{\link{selection_p}} / \code{\link{interactive_plot}}
#' @import shiny
#' @import ggplot2
#' @export
selection_p <- function(p_vec = 1:5, threshold = 5, Phi, lambda, B = NULL, sigma2 = 10^-14){
  # p_vec=1:5; threshold = 5; Phi=Phi.hat; lambda=lambda.hat; sigma2=sigma2.hat; B = NULL

  opt_result <- list()
  for(j in 1:length(p_vec)){
    p <- p_vec[j]
    opt_result[[j]]<- opt_design_fda(p = p, Phi = Phi, lambda = lambda, B = B, sigma2 = sigma2)
  }
  plab <- c()
  for(j in 1:(length(p_vec)-1)){
    plab <- paste0(plab, paste0(p_vec[j], ", "))
  }
  plab <- paste0(plab, p_vec[length(p_vec)])
  error_level_vec <- unlist(lapply(opt_result, function(a) a$error.level))
  p.sel <- p_vec[which(error_level_vec < threshold)[1]]
  
  plot1 <- plot(p_vec, error_level_vec, type="b", lwd=2, ylim = range(c(error_level_vec, threshold)),
                cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5,
                main = paste0("Error Level for p = (", plab,")"),
                xlab = "Number of Optimal Points (p)", ylab = "Error Level")
  plot2 <- abline(h = threshold, col = "red", lty = 3)
  
  
  INPUT <- list(p_vec = p_vec, Phi=Phi,lambda=lambda, sigma2 = sigma2 , B = B)
  
  return(list(plot1, plot2,
              p.sel = p.sel,
              opt.sel = opt_result[which(error_level_vec < threshold)[1]],
              opt_result = opt_result,
              INPUT = INPUT))
}
# test <- select_p(p_vec=c(1,3,4), threshold = 5,
#          Phi=Phi.hat, lambda=lambda.hat, sigma2=sigma2.hat, B = NULL)

