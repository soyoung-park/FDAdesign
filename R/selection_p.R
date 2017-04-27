#' Optimal Sampling Design for Functional Data Analysis - selection of number of optimal points
#'
#' Selects number of optimal points, p. 
#' This function also returns results of the \link{opt_design_fda} function for {p = 1, ..., p.sel+1}.
#'
#' @param delta   selection parameter 
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
selection_p <- function(delta = 0.1, pmax = 10, Phi, lambda, B = NULL, sigma2 = 10^-14){
  
  maxF <- sum(diag(B%*%diag(lambda)))
  
  opt_result <- list()
  lev <- c()
  for(p in 1:pmax){
    opt_result[[p]]<- opt_design_fda(p = p, Phi = Phi, lambda = lambda, B = B, sigma2 = sigma2)
    lev[p] <- opt_result[[p]]$obj_opt/maxF  +delta*p
    if(p > 1){
      if((lev[p]-lev[p-1])>0 ) break
    } 
    
  }
  p.sel <- p-1
  
  ylim <- c(range(lev)[1] - diff(range(lev))*0.2, range(lev)[2] + diff(range(lev))*0.2)
  plot1 <- plot(1:p, lev, type="b", lwd=2,
                cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5,
                main = paste0(""),ylim = ylim,
                xlab = "Number of Optimal Points (p)", ylab = "Selection Criterion")
  plot2 <- abline(h = lev[p.sel], col = "red", lty = 3)
  plot3 <- points(p.sel, lev[p.sel], col = "red", pch = 16, cex = 1.5)
  
  INPUT <- list(delta = delta, Phi=Phi,lambda=lambda, sigma2 = sigma2 , B = B)
  
  return(list(plot1, plot2, plot3,
              p.sel = p.sel,
              opt.sel = opt_result[[p.sel]],
              opt_result = opt_result,
              INPUT = INPUT))
}
 