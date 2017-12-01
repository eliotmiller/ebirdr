#' Run a PCA over a set of eBird records
#'
#' Combine a set of large eBird files and run a PCA over a specified set of columns.
#'
#' @param comparisons List of character vectors. Each character vector lists the
#' species that will be combined before the PCA is run. The name of the character
#' vector (i.e. the name of the elements of the list), are used to save out summaries
#' of the PCA, e.g. percent variance explained by each axis. Probably more complicated
#' than it needs to be, see examples.
#' @param read.wd Path to directory where the eBird records with environmental data are
#' stored.
#' @param write.wd Path to directory where PCA results will be saved.
#' @param aux.wd Path to directory where the summarized PCA results will be saved.
#' @param columns Character vector of column names in the existing eBird records that
#' you will run the PCA over.
#' @param scale.center Default is FALSE, i.e. a covariance matrix PCA will be run. To
#' run a correlation matrix PCA, set to TRUE. Scaling and centering are accomplished
#' manually before the PCA is run, which I think might be slow to run for really large
#' files. That part is untested, and it may be quicker to run it over a big.matrix--
#' currently this just runs over a regular matrix before converting afterwards to
#' a big matrix.
#' @param SVD Default is FALSE. The big.PCA function supposedly runs much faster if
#' SVD is set to TRUE, but the results are not exactly the same as regular prcomp if
#' it is set to TRUE. Specifically, the standard deviation of each axis is the same
#' with SVD set to TRUE, which means(?) that each axis is equally important, which
#' potentially seems odd to me for later hypervolume calculations.
#' @param axes How many axes from the PCA to retain and return.
#'
#' @details Depending on the size of the files, it can be nearly impossible to run a
#' standard base PCA over the
#' environmental variables in a set of eBird files. This function uses functions from
#' the data.table, bigmemory and bigpca packages to run a PCA. For small files it is
#' slower than a base R PCA, but it theoretically can handle very large inputs that
#' would normally crash R. I have a version of this file that runs in parallel, but
#' not sure we need it, and may swamp the RAM. Assess whether worth including parallel
#' option at some
#' point. There is a major assumption built into the 'comparisons' list above, which
#' is that the files in read.wd begin with the species' names, with underscores
#' between the genus and species. There currently is no version of this function that
#' doesn't write results directly to file.
#'
#' @return Nothing to the workspace. For each species in 'comparisons', this function
#' saves a csv with the scores of all observations for that species into write.wd,
#' and a summary table with the proportion of variance explained, etc., into aux.wd.
#'
#' @export
#'
#' @importFrom dplyr bind_rows
#' @importFrom data.table data.table
#'
#' @examples
#' #define the comparisons you'll run it over
#' #temp <- strsplit(list.files(), "_")
#' #woodpeckers <- paste(lapply(temp, "[", 1), lapply(temp, "[", 2), sep="_")
#' #woodpeckers <- list(woodpeckers)
#' #names(woodpeckers) <- "woodpeckers"

bigPCAToFile <- function(comparisons, read.wd, write.wd, aux.wd, columns,
	scale.center=FALSE, SVD=FALSE, axes)
{
 	#because bigpca is just a suggests, use this to ensure people have it installed
 	if(!requireNamespace("bigpca", quietly = TRUE))
 	{
 		stop("The bigpca package is needed for this. Install it.", call. = FALSE)
 	}

	#list all the files in read.wd
	allFiles <- list.files(path=read.wd)

	#define the species and create an index so you know which files to load later
	allSpp <- strsplit(allFiles, "_")
	allSpp <- paste(lapply(allSpp, "[", 1), lapply(allSpp, "[", 2), sep="_")
	indices <- 1:length(allSpp)
	names(indices) <- allSpp

	#for each comparison i
	for(i in 1:length(comparisons))
	{
		#define all the files to load in comparison i
		toLoad <- allFiles[indices[names(indices) %in% comparisons[[i]]]]

		#define all the species you are going to load
		sppToLoad <- allSpp[indices[names(indices) %in% comparisons[[i]]]]

		#set up a blank list to load into
		loaded <- list()
		
		#load all species j in comparison i into loaded
		for(j in 1:length(toLoad))
		{
			#load the file
			loaded[[j]] <- data.table::fread(paste(read.wd, toLoad[j], sep="/"))

			#cut it down to just the columns you want to ordinate
			loaded[[j]] <- loaded[[j]][,columns, with=FALSE]

			#bind in a column of that species' name, to facilitate identifying species'
			#points later. this step could probably be improved to reduce memory use
			loaded[[j]] <- data.table::data.table(species=sppToLoad[j], loaded[[j]])
		}
		
		#rbind all the species' processed files together
		bound <- dplyr::bind_rows(loaded)

		#pull the relevant rows out
		tempMatrix <- as.matrix(bound[,2:dim(bound)[2], with=FALSE])
		
		#if scale.center is TRUE, scale and center the matrix before ordinating
		if(scale.center)
		{
			#first center the matrix
			tempMatrix <- tempMatrix - rep(1, nrow(tempMatrix)) %*% t(colMeans(tempMatrix))
		
			#now divide each column by its standard deviation. checked that these things work
			#and do the same thing as just running scale()
			tempMatrix <- apply(tempMatrix, 2, function(x) x/sd(x))
		}

		#convert this to a big.matrix object
		bigMatrix <- bigmemory::as.big.matrix(tempMatrix)

		#run the big pca function over it. this takes over ten times as long in some
		#situations, but it should not be memory limited
		tempPCA <- bigpca::big.PCA(bigMatrix, pcs.to.keep=axes, center=FALSE,
			return.loadings=TRUE, SVD=SVD)
		
		#combine with the species names
		pca <- data.table::data.table(species=bound$species, tempPCA$PCs)

		#split the data.table on species' names and save out each separately
		splitUp <- split(pca, factor(pca$species))
		
		#save each to file. could parallelize this (splitUp is a list so could
		#mclapply or whatever too)
		for(j in 1:length(toLoad))
		{
			outName <- paste(sppToLoad[j], "_pca.csv", sep="")
			data.table::fwrite(splitUp[[j]], file=paste(write.wd, outName, sep="/"),
				row.names=FALSE)
		}

		#summarize the PCA results and save those out. first calculate SD per axis,
		#then square that for variance. build up into a dataframe and save out
		tempSD <- (apply(tempPCA$PCs, 2, sd))^2
		PCAresults <- rbind(SD = sqrt(tempSD),
			Proportion = tempSD/sum(tempSD),
			Cumulative = cumsum(tempSD)/sum(tempSD))
		outName <- paste(names(comparisons)[i], "_pca_summary.csv", sep="")
		write.csv(PCAresults, file=paste(aux.wd, outName, sep="/"))
	}
}
