#' Identify hypervolumes over multiple files
#'
#' Given a set of eBird files and specified traits, identify hypervolumes and save to disk.
#'
#' @param bandwidth.arg Options are either "silverman", "cross-validation", or a
#' fixed number, per standard hypervolume calculations. See Blonder et al. 2014 for more
#' details, including his FAQ page.
#' @param r.points The repsperpoint argument from hypervolume. If missing, will be set to
#' NULL and will default to hypervolume's default, 500 * the number of dimensions.
#' @param species.col The name of the column identifying the species to which the traits
#' belong. Not needed per se, but the function currently takes whatever column the user
#' provides here and binds the traits.col to it, ignoring this first column for the
#' hypervolume calculations. In other words, provide the name of some column here. 
#' @param trait.cols A character vector detailing which columns from the eBird files you
#' would like to calculate a hypervolume for.
#' @param read.wd The path to the directory where the eBird records are housed.
#' @param write.wd The path to the directory where the hypervolumes will be saved.
#' @param cores The number of cores to use for parallel processing. This package currently
#' requires your computer to be able to handle at, the bare minimum, installation of the
#' various parallel processing packages. A forthcoming feature will allow you to specify
#' 'seq' here, and would mean ebirdr could be run even if you can't install the parallel
#' processing software.
#'
#' @details Interesting and potentially useful method from Blonder et al. 2014, although
#' not entirely clear how best to set the bandwidth.arg parameters. Has worked well for
#' me in practice. 
#'
#' @return Prints progress to the workspace, but saves nothing there. Results written
#' as RDS files to the write.wd.
#'
#' @export
#'
#' @references Blonder, B., C. Lamanna, C. Violle, and B. J. Enquist. 2014.
#' The n-dimensional hypervolume. Global Ecology and Biogeography 23:595-609.
#'
#' @importFrom hypervolume hypervolume estimate_bandwidth

hypervolumes <- function(bandwidth.arg, r.points, species.col, trait.cols, read.wd,
	write.wd, cores)
{
	registerDoParallel(cores)

	if(missing(r.points))
	{
		r.points <- NULL
	}

	#create a character vector listing all the files in the read.wd
	allFiles <- list.files(path=read.wd)

	#prep the outfile names
	outFiles <- strsplit(allFiles, "_")
	genera <- lapply(outFiles, "[", 1)
	species <- lapply(outFiles, "[", 2)
	outFiles <- paste(unlist(genera), unlist(species), sep="_")

	#create a character vector that is species.col pasted to traits.col, to use so that
	#file is in the right format for hypervolume calcs below
	columns <- c(species.col, trait.cols)

	#go into a for loop where for each file in read.wd, you read in and calculate
	#hypervolume and save out
	foreach(i = 1:length(allFiles)) %dopar%
	{
		#read in that species' points and prep into a simplified data frame in the
		#appropriate format
		temp <- as.data.frame(data.table::fread(paste(read.wd, allFiles[i], sep="/")))

		temp <- temp[,columns]
		
		#calculate the hypervolume for that species. Blonder does not seem super
		#optimistic about silverman method, but it seems to work well for me.
		if(bandwidth.arg=="cross-validation")
		{
			results <- hypervolume::hypervolume(data=temp[,2:dim(temp)[2]],
				repsperpoint=r.points,
				bandwidth=estimate_bandwidth(temp[,2:dim(temp)[2]],
				method="cross-validation"))
		}
		else if(bandwidth.arg=="silverman")
		{
			results <- hypervolume::hypervolume(data=temp[,2:dim(temp)[2]],
				repsperpoint=r.points,
				bandwidth=hypervolume::estimate_bandwidth(temp[,2:dim(temp)[2]],
				method="silverman"))
		}
		else
		{
			results <- hypervolume::hypervolume(data=temp[,2:dim(temp)[2]],
				repsperpoint=r.points,
				bandwidth=bandwidth.arg)
		}

		saveRDS(results, file=paste(write.wd, paste(outFiles[i], ".RDS", sep=""),
			sep="/"))
	}
}
