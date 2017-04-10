#' Function to spatially sample points
#'
#' Spatially thin eBird records based on a user-defined grid.
#'
#' @param xxx Vector of longitude or E-W coordinates.
#' @param yyy Vector of latitide or N-S coordinates.
#' @param xlim Together xlim and ylim define a bounding box within which lookup occurs.
#' I.e. all (xxx, yyy) pairs outside of this box are ignored.
#' @param ylim Together xlim and ylim define a bounding box within which lookup occurs.
#' I.e. all (xxx, yyy) pairs outside of this box are ignored.
#' @param nx Number of grid cells in x direction.
#' @param ny Number of grid cells in y direction.
#' @param jitter Defaults to FALSE. Set to TRUE to randomize grid location.
#' @param size Maximum number of points per cell to sample
#' @param replace Defaults to FALSE. Set to TRUE to sample with replacement.
#'
#' @details This function generates a stratified sample over a regular grid of strata, 
#' i.e. grid cells. It is relatively efficient computationally speaking. The sample()
#' function cannot take a sample larger than the population (in a cell) when
#' 'replace = FALSE' If 'replace = FALSE' and the size parameter is larger than the 
#' cell populatuion size this function passes back a vector of length size, 
#' but it will contain only as many unique points in the cell
#' and the rest of the entries will be NA's.   
#'
#' @return Index vector of selected locations.
#'
#' @export
#'
#' @importFrom plyr rbind.fill.matrix
#'
#' @references Team eBird. Daniel Fink?
#'
#' @examples
#' #Generate Random Points over 2D field 
#' nnn <- 1000
#' xxx <- runif(nnn, 0, 10)
#' yyy <- runif(nnn, 0, 10)
#' par(cex = 0.5)
#' plot(xxx, yyy, 
#'  xlim = c(-1,11), 
#'  ylim = c(-1,11), 
#'  pch=20, 
#'  col="red", 
#'  cex=0.5)
#' sgc <- sample.grid.cell(
#'  xxx, 
#'  yyy, 
#'  xlim = c(-1,3), 
#'  ylim = c(3,6),
#'  nx = 5, 
#'  ny = 2, 
#'  jitter = T, 
#'  size = 1, 
#'  replace = F ) 
#' length(sgc$sample.index)
#' sum(!is.na(sgc$sample.index))
#' points(
#'  xxx[!is.na(sgc$cell.number)], 
#'  yyy[!is.na(sgc$cell.number)], 
#'  col = "blue", 
#'  pch = 20, 
#'  cex = 0.5)  
#' points(xxx[sgc$sample.index], yyy[sgc$sample.index], pch=20, cex=1, col="green")
#'
#' # Reconstruct the Grid that was used
#' for (iii in 1:(sgc$nx+1))
#'  lines( rep(sgc$bb[1,1] + (iii-1)*sgc$xwidth, 2), range(sgc$bb[,"yyy"]), col="grey")   
#' for (iii in 1:(sgc$ny+1))
#'  lines( range(sgc$bb[,"xxx"]), rep(sgc$bb[1,2] + (iii-1)*sgc$ywidth, 2), , col="grey")   
#'
#' table(sgc$cell.number)
#' length(table(sgc$cell.number))
#' sort(unique(sgc$sample.index))
#' sort(unique(c(1:length(xxx))[!is.na(sgc$cell.number)]))

sample.grid.cell <- function(
  xxx, 
  yyy, 
  xlim = c(NA,NA), 
  ylim = c(NA,NA),
  nx, 
  ny, 
  jitter = F,
  size, 
  replace = F ){	

  # Stratified sample over Grid Cell Number
  sample_fun <- function(x, size, replace){
    # Cells without samples are excluded in the tapply call - if (length(x)==0) return(NA) 
    # Cells with a single sample cause problems, see help(sample)
    # So, I am going to handle this situation "by hand"
    result <- rep(NA, size)
    if (length(x)==1 & replace==F) {
      #cat("sf: length(x)==1 & replace==F",x,"\n")
      result <- rep(NA, size)
      result[1] <- x 
    }
    if (length(x)==1 & replace==T) {
      #cat("sf: length(x)==1 & replace==T",x,"\n")
      result <- rep(x, size)
    }
    if (length(x)>1 & replace == F & size > length(x) ){
      result <- rep(NA, size)
      result[1:length(x)] <- x 
    }
    if (length(x)>1 & replace == F & size <= length(x) ){
      result <- sample(x=x, size=size, replace=replace)	
    }		
    if (length(x)>1 & replace == T ){
      result <- sample(x=x, size=size, replace=replace)	
    }
    return(result) 
  }
  lgc <- lookup.grid.cell(
    xxx, yyy, xlim, ylim, nx, ny, jitter)  
  n.index <- tapply( 
    c(1:length(xxx))[!is.na(lgc$cell.number)], 
    as.factor(lgc$cell.number[!is.na(lgc$cell.number)]), 
    sample_fun, size, replace) 
  n.index <- rbind.fill.matrix(n.index)	
  return(list(
    cell.number = lgc$cell.number, 
    sample.index = n.index, 
    bb = lgc$bb, 
    nx = lgc$nx, 
    ny = lgc$ny, 
    xwidth = lgc$xwidth, 
    ywidth = lgc$ywidth  ))
}
