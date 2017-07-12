#' Spatially thin a data frame of eBird records
#'
#' Single data frame spatial thinning within an R session.
#'
#' @param ebird.data Dataframe of eBird records.
#' @param xxx Either E-W coordinates, or the word "extent". If extent is desired,
#' make sure to specify for both xxx & yyy. Function will not handle mixture gracefully.
#' @param yyy Either N-S coordinates, or the word "extent". If extent is desired,
#' make sure to specify for both xxx & yyy. Function will not handle mixture gracefully.
#' @param latitude The name of the latitude column in the files.
#' @param longitude The name of the longitude column in the files.
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
#' @details This applies sample.grid.cell() to single eBird data frame. 
#'
#' @return A spatially thinned data frame.
#'
#' @export
#'
#' @importFrom plyr rbind.fill.matrix
#'
#' @references Team eBird.
#'
#' @examples
#' data(ex)
#' plot(ex$LONGITUDE~ex$LATITUDE, col="blue", pch=20, cex=2)
#' thinned <- spThinner(ebird.data=ex, xxx="extent", yyy="extent",
#'   latitude="LATITUDE", longitude="LONGITUDE", nx=2, ny=2, size=3)
#' points(y=thinned$LONGITUDE, x=thinned$LATITUDE, col="red", pch=20, cex=1)
#' #the blue points are the original data, the red points are the accepted points
#' #after spatial thinning.

spThinner <- function(ebird.data, xxx, yyy, latitude, longitude,
  xlim = c(NA,NA), ylim = c(NA,NA), nx, ny, jitter = FALSE, size, replace = FALSE)
{
    #if xxx & yyy are specified as extent, pull the vector of lat and long
    if(xxx=="extent" & yyy=="extent")
    {
    	xxx <- ebird.data[,longitude]
    	yyy <- ebird.data[,latitude]
    }

    #thin it according to the sample.grid.cell parameters
    sgc <- sample.grid.cell(xxx=xxx, yyy=yyy, xlim=xlim, ylim=ylim,
      nx=nx, ny=ny, jitter=jitter, size=size, replace=replace)

    #subset to the spatially thinned records
    accept <- sgc$sample.index[,1]
    accept <- accept[!is.na(accept)]
    results <- ebird.data[accept,]
    results
}
