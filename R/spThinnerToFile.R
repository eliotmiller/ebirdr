#' Spatially thin a set of eBird records
#'
#' Wrapper to lapply sample.grid.cell over multiple files.
#'
#' @param xxx Either E-W coordinates, or the word "extent". If extent is desired,
#' make sure to specify for both xxx & yyy. Function will not handle mixture gracefully.
#' @param yyy Either N-S coordinates, or the word "extent". If extent is desired,
#' make sure to specify for both xxx & yyy. Function will not handle mixture gracefully.
#' @param lat.col The name of the latitude column in the files.
#' @param long.col The name of the longitude column in the files.
#' @param xlim Together xlim and ylim define a bounding box within which lookup occurs.
#' I.e. all (xxx, yyy) pairs outside of this box are ignored.
#' @param ylim Together xlim and ylim define a bounding box within which lookup occurs.
#' I.e. all (xxx, yyy) pairs outside of this box are ignored.
#' @param nx Number of grid cells in x direction.
#' @param ny Number of grid cells in y direction.
#' @param jitter Defaults to FALSE. Set to TRUE to randomize grid location.
#' @param size Maximum number of points per cell to sample
#' @param replace Defaults to FALSE. Set to TRUE to sample with replacement.
#' @param read.wd Path to the read directory, where the files to be thinned are found.
#' @param write.wd Path to the write directory.
#' @param cores How many cores to use for parallel processing.
#'
#' @details This lapplies sample.grid.cell() to a series of eBird files. The read
#' directory should contain only files to be cleaned, will not gracefully handle
#' other types of files. Important note--depending on how the arguments are passed
#' in, sample.grid.cell creates a grid unique
#' to the minimum/maximum lat/long of the input data. So, grids with the same
#' parameters will be applied to each file in the read working directory. Unless
#' the input files have identical extents, or unless fixed coordinates are
#' provided, this means that the actual size and
#' spacing of the grid will change between files. Some preliminary tests are in
#' order to ensure what works for your data.
#'
#' @return Spatially thinned csv files written to the write directory.
#'
#' @export
#'
#' @importFrom plyr rbind.fill.matrix
#'
#' @references Team eBird.

spThinnerToFile <- function(xxx, yyy, lat.col, long.col, xlim = c(NA,NA),
	ylim = c(NA,NA), nx, ny, jitter = FALSE, size, replace = FALSE, read.wd,
	write.wd, cores)
{
  registerDoParallel(cores)
  
  if(missing(read.wd))
  {
    stop("read directory must be supplied")
  }

  if(missing(write.wd))
  {
    stop("write directory must be supplied")
  }

  #create a character vector listing all the files in the read.wd
  allFiles <- list.files(path=read.wd)

  #append cleaned to the end of the file names here
  outFiles <- sub(".csv", "_thinned.csv", allFiles)
 
  #go into a foreach loop where it spatially thins and saves out
  #the row dimensions of the files before and after thinning
  output <- foreach(i = 1:length(allFiles), .combine=rbind) %dopar%
  {
    #load in a single file in the list of files
    temp <- as.data.frame(fread(paste(read.wd, allFiles[i], sep="/")))

    #if xxx & yyy are specified as extent, pull the vector of lat and long
    if(xxx=="extent" & yyy=="extent")
    {
    	xxx <- temp[,long.col]
    	yyy <- temp[,lat.col]
    }

    #thin it according to the sample.grid.cell parameters
    sgc <- sample.grid.cell(xxx=xxx, yyy=yyy, xlim=xlim, ylim=ylim,
      nx=nx, ny=ny, jitter=jitter, size=size, replace=replace)

    #subset to the spatially thinned records
    accept <- sgc$sample.index[,1]
    accept <- accept[!is.na(accept)]
    toWrite <- temp[accept,]

    #save out to the write.wd.
    fwrite(toWrite, file=paste(write.wd, outFiles[i], sep="/"), row.names=FALSE)

    #create a little object that shows before and after dimensions
    matrix(c(dim(temp)[1], dim(toWrite)[1]), nrow=1, ncol=2, byrow=TRUE)
  }

  rownames(output) <- sub(".csv", "", allFiles)
  colnames(output) <- c("before","after")
  output
}
