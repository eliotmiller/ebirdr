#' Calculate overlaps between all pairs of a hypervolume list
#'
#' Find all pairwise intersections, unions, etc., between elements in a hypervolume list.
#'
#' @param hv.list Object of class 'HypervolumeList'.
#' @param what Options are "jaccard" or "sorensen".
#' @param cores The number of cores to use for parallel processing. 
#'
#' @details Makes the assumption that the hv.list was loaded in with loadHypervolumes,
#' and that the Name elements in the list are of the form 'Genus_species.RDS'. Function
#' only calculates the set of {i,j}, not {j,i}. Because of the way
#' hypervolume_set works, these values can change slightly, so some precision is
#' lost here. Prints progress to screen.
#'
#' @return Matrix with rownames and colnames corresponding to the input hv.list, and
#' the elements filled in according to the specified 'what'.
#'
#' @export
#'
#' @references Blonder, B., C. Lamanna, C. Violle, and B. J. Enquist. 2014.
#' The n-dimensional hypervolume. Global Ecology and Biogeography 23:595-609.

hypervolumeOverlaps <- function(hv.list, what, cores)
{
 	#because hypervolume is just a suggests, use this to ensure people have it installed
 	if(!requireNamespace("hypervolume", quietly = TRUE))
 	{
 		stop("The hypervolume package is needed for this. Install it.", call. = FALSE)
 	}

	registerDoParallel(cores)
	
	#define j here to avoid notes during devtools check
	j <- 1

	#set up a blank matrix, give it names
	overlaps <- matrix(nrow=length(hv.list@HVList), ncol=length(hv.list@HVList))
	
	#pull the names from the hv.list
	tempNames <- sub(".RDS", "", unlist(lapply(hv.list@HVList, function(x) x@Name)))
	rownames(overlaps) <- tempNames
	colnames(overlaps) <- tempNames

	for(i in 1:length(hv.list@HVList))
	{
		#concatenate the results of each inner loop into a vector
		toInsert <- foreach(j = 1:i, .combine='c') %dopar%
		{
			#find the various sets of i with j
			sets <- hypervolume::hypervolume_set(hv.list@HVList[[i]],
				hv.list@HVList[[j]], verbose=FALSE, check.memory=FALSE)

			#calculate overlap statistics
			temp <- hypervolume::hypervolume_overlap_statistics(sets)

			#set the relevant element in the matrix to the requested overlap stat
			temp[what]
		}

		#now set that concatenated vector to be the correct elements in overlaps
		overlaps[i,1:length(toInsert)] <- toInsert

		#print the progress
		numerator <- sum(!is.na(overlaps))
		denominator <- ((1+dim(overlaps)[1])*dim(overlaps)[1])/2
		print(numerator/denominator)
	}

	overlaps
}
