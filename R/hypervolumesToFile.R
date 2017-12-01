#' Identify hypervolumes over multiple files
#'
#' Given a set of eBird files and specified traits, identify hypervolumes and save to disk.
#'
#' @param files Character vector specifying which files from the read.wd to process.
#' If missing, will assume all files in read.wd should be included.
#' Full file names (without path) are required.
#' @param hv.type Options are "box", "gaussian", and "svm". 
#' @param bandwidth.arg Options are either "silverman", "cross-validation", or a
#' fixed number, per standard hypervolume calculations. See Blonder et al. 2014 for more
#' details, including his FAQ page.
#' @param r.points The repsperpoint argument from hypervolume. If missing, will be set to
#' NULL and will default to hypervolume's default,
#' ceiling((10^(3 + sqrt(ncol(data))))/nrow(data)).
#' @param species.col The name of the column identifying the species to which the traits
#' belong. Not needed per se, but the function currently takes whatever column the user
#' provides here and binds the traits.col to it, ignoring this first column for the
#' hypervolume calculations. In other words, it is expected that you provide the name of
#' some column here. 
#' @param trait.cols A character vector detailing which columns from the eBird files you
#' would like to calculate a hypervolume for.
#' @param ... Can be used to pass things to the hypervolume function, e.g., svm.nu for
#' the svm method, quantile.requested for gaussian, etc.
#' @param read.wd The path to the directory where the eBird records are housed.
#' @param write.wd The path to the directory where the hypervolumes will be saved.
#' @param cores The number of cores to use for parallel processing. 
#'
#' @details Interesting and seemingly very useful method from Blonder et al. 2014, although
#' not entirely clear how best to set the bandwidth.arg parameters.
#'
#' @return Prints progress to the workspace, but saves nothing there. Results written
#' as RDS files to the write.wd.
#'
#' @export
#'
#' @references Blonder, B., C. Lamanna, C. Violle, and B. J. Enquist. 2014.
#' The n-dimensional hypervolume. Global Ecology and Biogeography 23:595-609.

hypervolumesToFile <- function(files, hv.type, bandwidth.arg, r.points, species.col,
	trait.cols, read.wd, write.wd, cores, ...)
{
 	#because hypervolume is just a suggests, use this to ensure people have it installed
 	if(!requireNamespace("hypervolume", quietly = TRUE))
 	{
 		stop("The hypervolume package is needed for this. Install it.", call. = FALSE)
 	}

 	#if r.points is not provided, set it to null for below
	if(missing(r.points))
	{
		r.points <- NULL
	}

	registerDoParallel(cores)

	#if missing files, create a character vector listing all the files in the read.wd
	if(missing(files))
	{
		allFiles <- list.files(path=read.wd)
	}

	else
	{
		allFiles <- files
	}

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

		#if r.points was missing and therefore set to NULL, set it to default sensu
		#hypervolume
		if(is.null(r.points))
		{
			r.points <- ceiling((10^(3 + sqrt(ncol(temp[,2:dim(temp)[2]]))))/nrow(temp))
		}

		#if hv.type is either gaussian or box, you'll need a bandwidth
		if(hv.type == "box" | hv.type == "gaussian")
		{
			if(bandwidth.arg == "cross-validation" | bandwidth.arg == "silverman")
			{
				band <- hypervolume::estimate_bandwidth(temp[,2:dim(temp)[2]],
					method=bandwidth.arg)
			}

			#this is if it's a fixed number. if it's neither, there's going to be
			#an ungraceful error here
			else
			{
				band <- bandwidth.arg
			}
		}
		
		#calculate the hypervolume for the species. the 2*band is based solely on
		#the default for hypervolume box, no idea if it's good
		if(hv.type=="box")
		{
			results <- hypervolume::hypervolume(data=temp[,2:dim(temp)[2]],
				samples.per.point=r.points, kde.bandwidth=2*band, method="box", ...)
		}
		else if(hv.type=="gaussian")
		{
			results <- hypervolume::hypervolume(data=temp[,2:dim(temp)[2]],
				samples.per.point=r.points, kde.bandwidth=band, method="gaussian", ...)
		}
		else if(hv.type=="svm")
		{
			results <- hypervolume::hypervolume(data=temp[,2:dim(temp)[2]],
				samples.per.point=r.points, method="svm", ...)
		}

		saveRDS(results, file=paste(write.wd, paste(outFiles[i], ".RDS", sep=""),
			sep="/"))
	}
}
