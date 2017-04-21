#' Load a set of hypervolumes
#'
#' Load a set of hypervolumes into a single hypervolume list for plotting, etc.
#'
#' @param read.wd Working directory where the hypervolumes are stored.
#' @param to.load If left blank, will load all files in the read.wd. Otherwise, can
#' be used to only load a subset of species in the folder. Provide as a character
#' vector of the format 'Genus_species'.
#'
#' @details The hypervolume package comes with a number of functions that can be used
#' over hypervolume list objects. The default plotting option, for instance, can be
#' rather useful. 
#'
#' @return A hypervolume list with elements named according to the file names
#' in read.wd.
#'
#' @export
#'

loadHypervolumes <- function(read.wd, to.load)
{
	#list all the files in read.wd
	allFiles <- list.files(path=read.wd)

	#identify all the species to load
	allSpp <- strsplit(allFiles, "_")
	allSpp <- paste(lapply(allSpp, "[", 1), lapply(allSpp, "[", 2), sep="_")
	indices <- 1:length(allSpp)
	names(indices) <- allSpp

	#if to.load isn't missing, cut allFiles & allSpp to just those in to.load
	if(!missing(to.load))
	{
		allSpp <- allSpp[allSpp %in% to.load]
		allFiles <- allFiles[indices[names(indices) %in% allSpp]]
	}

	#load all the files you are supposed to
	HVresults <- lapply(allFiles, function(x)
		readRDS(paste(read.wd, x, sep="/")))

	#reclass this as a HypervolumeList and give it species names
	hvs <- new("HypervolumeList", HVList = HVresults)

	for(i in 1:length(hvs@HVList))
	{
		hvs@HVList[[i]]@Name <- allSpp[i]
	}

	#return the hypervolumes
	hvs
}
