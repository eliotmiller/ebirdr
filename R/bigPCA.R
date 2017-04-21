#' Run a PCA over a set of eBird records
#'
#' Combine a set of large eBird files and run a PCA over a specified set of columns.
#'
#' @param comparisons List where each element is a named character vector. The name of
#' the element specifies what the PCA results will be called, and the character vector
#' lists the species that will be combined before the PCA is run.
#' @param read.wd Working directory where the eBird records with environmental data are
#' stored.
#' @param write.wd Working directory where PCA results will be saved.
#' @param columns Character vector of column names in the existing eBird records that
#' you will run the PCA over.
#' @param scale.center Default is FALSE, i.e. a covariance matrix PCA will be run. To
#' run a correlation matrix PCA, set to TRUE. Scaling and centering are accomplished
#' manually before the PCA is run, which I think might be slow to run for really large
#' files. That part is untested, and it may be quicker to run it over a big.matrix--
#' currently this just runs over a regular matrix before converting afterwards to
#' a big matrix.
#' @param axes How many axes from the PCA to retain and return.
#'
#' @details It can be nearly impossible to run a standard base PCA over the
#' environmental variables in a set of eBird files. This function uses functions from
#' the data.table, bigmemory and bigpca packages to run a PCA. For small files it is
#' slower than a base R PCA, but it theoretically can handle very large inputs that
#' would normally crash R. I have a version of this file that runs in parallel, but not sure we need
#' it, and may swamp the RAM. Assess whether worth including parallel option at some
#' point. There is a major assumption built into the 'comparisons' list above, which
#' is that the files in read.wd begin with the species' names, with underscores
#' between the genus and species.
#'
#' @return Nothing to the workspace. Saves PCA results to write.wd.
#'
#' @export
#'
#' @importFrom dplyr bind_rows
#' @importFrom data.table data.table
#' @importFrom bigmemory as.big.matrix
#' @importFrom bigpca big.PCA

bigPCA <- function(comparisons, read.wd, write.wd, columns, scale.center=FALSE, axes)
{
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
			return.loadings=TRUE)
		
		#combine with the species names
		pca <- data.table::data.table(species=bound$species, tempPCA$PCs)
		
		#save to file under name of comparison i
		outName <- paste(names(comparisons)[i], "_pca.csv", sep="")
		data.table::fwrite(pca, file=paste(write.wd, outName, sep="/"),
			row.names=FALSE)
	}
}
