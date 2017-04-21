#note that we currently do not export this function. should we? it is called
#internally by sample.grid.cell. does one ever call it directly?

# -----------------------------------------------------------------------------
# FUNCTION - LOOKUP GRID CELL NUMBER
# INPUT: 
#   xxx = vector of longitude or E-W coordinates
#   yyy = vector of latitide or N-S coordinates
#   xlim & ylim = Together these define a bounding box
#     within which lookup occurs. I.e. all (xxx, yyy). 
#     pairs outside of this box are ignored. 
#   nx & ny = number of grid cells in each direction
#   jitter = T/F randomize grid location 
#
# OUTPUT: 
#   The output data structure is a list with the following components. 
# 
# vector of grid cell numbers (keys) in row-major order
# 
# CORNER CASES: 
#   partial or no points - pass pack NA if data point is out of the bounding box
#
# -----------------------------------------------------------------------------
# TEST: lookup.grid.cell
# -----------------------------------------------------------------------------
#   nnn <- 2500
#   xxx <- runif(nnn, 0, 10)
#   yyy <- runif(nnn, 0, 10)
#   par(cex = 0.5)
#   plot(xxx, yyy, 
#     xlim = c(-1,11), 
#     ylim = c(-1,11), 
#     pch=20, 
#     col="red", 
#     cex=0.5)
#   lgc <- lookup.grid.cell(
#     xxx, 
#     yyy, 
#     xlim = c(4,6), 
#     ylim = c(4,6),
#     nx = 2, 
#     ny = 2, 
#     jitter = TRUE )
#   points(
#     xxx[!is.na(lgc$cell.number)], 
#     yyy[!is.na(lgc$cell.number)], 
#     col = "blue") 
# Reconstruct the Grid that was used
#   for (iii in 1:(lgc$nx+1))
#     lines( rep(lgc$bb[1,1] + (iii-1)*lgc$xwidth, 2), range(lgc$bb[,"yyy"]), col="grey")   
#   for (iii in 1:(lgc$ny+1))
#     lines( range(lgc$bb[,"xxx"]), rep(lgc$bb[1,2] + (iii-1)*lgc$ywidth, 2), , col="grey")   
# Timing Tests
#   nnn <- 1000000
#   xxx <- runif(nnn, 0, 10)
#   yyy <- runif(nnn, 0, 10)
#   system.time(
#     grid.cell.number <- lookup.grid.cell(
#       xxx, 
#       yyy, 
#       xlim = c(0,10), 
#       ylim = c(0,10),
#       nx = 100, 
#       ny = 100, 
#       jitter = TRUE )  )
#
# -----------------------------------------------------------------------------

lookup.grid.cell <- function(
  xxx, 
  yyy, 
  xlim = c(NA,NA), 
  ylim = c(NA,NA),
  nx = 64, 
  ny = 64, 
  jitter = FALSE ){
  # -----------------------------------
  cell.number <- rep(NA, length(xxx))
  if (any(is.na(xlim))) xlim <- range(xxx, na.rm=TRUE)
  if (any(is.na(ylim))) ylim <- range(yyy, na.rm=TRUE)
  xxx.width <- abs(xlim[2]-xlim[1])/nx
  yyy.width <- abs(ylim[2]-ylim[1])/ny
  # Lower Left Corner
  x.ll <- min(xlim)
  y.ll <- min(ylim)
  # If Jittered, domain is bigger & number of grid cells increases	
  if (jitter){
    x.ll <- x.ll - runif(1)*xxx.width
    y.ll <- y.ll - runif(1)*yyy.width
    nx <- 1 + (max(xlim) - min( c(x.ll, xlim) )) %/% xxx.width
    ny <- 1 + (max(ylim) - min( c(y.ll, ylim) )) %/% yyy.width
  }
  # ID data within grid
  ingrid.index <- 
    xxx >= x.ll & xxx <= x.ll + nx*xxx.width & 
    yyy >= y.ll & yyy <= y.ll + ny*yyy.width
  if (sum(ingrid.index)>0){
    # Assign Row, Column, Grid Cell Number
    col.number <- 1 + ( xxx[ingrid.index] - x.ll ) %/% xxx.width
    row.number <- 1 + ( yyy[ingrid.index] - y.ll ) %/% yyy.width
    cell.number[ingrid.index] <- col.number + (row.number-1)*nx 
    # for (iii in 1:length(grid.cell.number)){
    # 	text(
    # 		x = xxx[iii] + xxx.width/2, 
    # 		y = yyy[iii] + yyy.width/2,
    # 		labels = paste( grid.cell.number[iii]) )
    # 	text(
    # 		x = xxx[iii]  + xxx.width/2, 
    # 		y = yyy[iii]  + 0,
    # 		labels = paste( iii ), 
    # 		col = "red" )			
    # 	}
  } 	
  return( list(
    cell.number = cell.number,
    # jittered bounding box 
    bb = matrix( 
      c(x.ll, y.ll, x.ll + nx*xxx.width, y.ll + ny*yyy.width), 2, 2, 
      byrow=TRUE, dimnames=list(c("ll", "ur"), c("xxx", "yyy"))), 
    nx = nx, 
    ny = ny, 
    xwidth = xxx.width, 
    ywidth = yyy.width  ))
}
