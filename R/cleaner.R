#' Function to clean eBird data files
#'
#' Filter eBird records to only those that meet a specified set of criteria.
#'
#' @param arguments Character vector of arguments to which the existing eBird records will
#' be filtered. Records that match these arguments will be kept. See examples below.
#' @param keep Character vector of column names in the existing eBird records that you
#' would like to be passed along/kept in the cleaned files.
#' @param group.id The name of the group ID column in the existing eBird records. Usually
#' (always?) simply 'GROUP.ID'. Required if you would like to remove all but one duplicated
#' group checklist. Note that it should be possible to remove the unique.filter argument
#' and, if group.id is missing, simply assume unique.filter is FALSE. However, because
#' the cleaning function operates through a foreach loop, need to do some fancy coding to
#' ensure the group.id argument properly gets "exported into the parallel foreach
#' environment". This solution seems to work.
#' @param unique.filter Whether or not to invoke the uniqueFilter function. Default is
#' TRUE, meaning that all but one checklist from shared group checklists will be removed.
#' Setting to FALSE means that all (duplicated) group checklists will be kept.
#' @param read.wd The path to the read working directory, where the existing eBird records
#' are housed. The files should be in csv format, and only files you intend to be cleaned
#' should be in the directory. The read.wd cannot contain subdirectories or any
#' non-csv file types.
#' @param write.wd The path to the write working directory. Cleaned files will be written
#' to this directory as csv files. The phrase "_cleaned" is automatically appended to the
#' file name, so the original eBird records will not be overwritten even if read and write
#' working directories are the same.
#' @param cores The number of cores to use for parallel processing. This package currently
#' requires your computer to be able to handle at, the bare minimum, installation of the
#' various parallel processing packages. A forthcoming feature will allow you to specify
#' 'seq' here, and would mean ebirdr could be run even if you can't install the parallel
#' processing software.
#' @param ... Additional arguments to be passed to lower level functions. For instance,
#' could be used to pass sub.id information to uniqueFilter().
#' results. Alternatively (and perhaps preferably), either "plot" or "richness" can be
#' provided and, assuming that concatenation option was specified in the multiLinker runs,
#' performance results will be summarized just over that specified option.
#' 
#' @details This function should be fairly flexible. Pass it a set of arguments, and only
#' records that meet those requirements will be kept. Tell it which columns you want to
#' come along, and only those columns will be kept. Currently, this function writes the
#' results to file. It saves into the current R session a list of character vectors,
#' where each element in the list refers to a file in the write.wd, and the elements within
#' the character vector summarize the unique SUBNATIONAL1_CODEs that are found in each
#' cleaned file. Useful for generating absences, e.g. by querying on the resulting list
#' with the ebirdQuery type set to 'absences'. Room for improvement in terms of the output
#' of this function, e.g. would be nice to make it so you can specifiy what you want to
#' get saved into the active R session.
#'
#' @return A list of character vectors of unique SUBNATIONAL1_CODEs that each species
#' occurs in.
#'
#' @export
#'
#' @importFrom data.table fwrite fread
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel
#'
#' @references Team eBird.
#'
#' @examples
#' data(ex)
#' dim(ex)
#'
#' #create an empty directory and save the ex object into it as "ex.csv"
#' dir.create("delete")
#' write.csv(ex, paste(getwd(), "delete", "ex.csv", sep="/"), row.names=FALSE)
#' #then change your working directory to be this new directory, and run
#' tempLocs <- cleaner(
#'    arguments=c("VALID == '1'", "PROTOCOL_NAME != 'Historical'"),
#'	  keep=c("LOC_ID", "SUBNATIONAL1_CODE", "LATITUDE", "LONGITUDE", "SUB_ID",
#'		"CREATION_DT", "PROTOCOL_NAME", "DURATION_HRS", "ALL_OBS_REPORTED",
#'		"OBS_DT", "EFFORT_HRS_ATLEAST", "EFFORT_HRS_ATMOST", "EFFORT_DISTANCE_KM",
#'		"EFFORT_DISTANCE_KM_ATLEAST", "EFFORT_DISTANCE_KM_ATMOST", "NUM_OBSERVERS",
#'		"SCI_NAME", "HOW_MANY_ATLEAST", "HOW_MANY_ATMOST", "REVIEWED", "VALID"),
#'	  group.id="GROUP_ID", read.wd=paste(getwd(), "delete", sep="/"),
#'    write.wd=paste(getwd(), "delete", sep="/"), cores=1)
#'
#' #load the cleaned file in
#' ex2 <- read.csv(paste(getwd(), "delete", "ex_cleaned.csv", sep="/"))
#'
#' #many of the records were culled. data.frame went from 80 rows to 52, and we
#' #only kept 21 of 28 columns.
#' dim(ex2)
#'
#' #a list of all unique SUBNATIONAL1_CODEs in the cleaned file was saved into
#' #the active R session
#' tempLocs

cleaner <- function(arguments, keep, group.id, unique.filter=TRUE,
	read.wd, write.wd, cores, ...)
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
	outFiles <- sub(".csv", "_cleaned.csv", allFiles)
	
	#go into a foreach loop where it reads in the csv file, gets rid of duplicate entries,
	#subsets to the columns of interest and saves out a csv of the cleaned file, and also
	#outputs all unique subnational codes for that species	
	output <- foreach(i = 1:length(allFiles)) %dopar%
	{
		#load in a single file in the list of files
		temp <- as.data.frame(fread(paste(read.wd, allFiles[i], sep="/")))
	
		#invoke uniqueFilter() if set to TRUE
		if(unique.filter)
		{
			temp <- uniqueFilter(temp, group.id, ...)
		}
		
		#define this here and call at end, will save into output then
		toCall <- unique(temp$SUBNATIONAL1_CODE)
		
		#loop through the arguments and subset the single file down to conditions
		#specified in arguments
		for(j in 1:length(arguments))
		{
			#this is here for legacy reasons, but having major issues with the NAs in
			#the distances
			#temp <- temp[eval(parse(text=paste("temp", arguments[j], sep=""))),]
			temp <- subset(temp, eval(parse(text=paste(arguments[j]))))
		}

		#skip to next species if there are 1 or fewer presence records
		if(dim(temp)[1] < 1)
		{
			return(NA)
		}
		
		#subset to the columns of interest only
		temp <- temp[,keep]
				
		#save out the subsetted file to the write.wd.
		fwrite(temp, file=paste(write.wd, outFiles[i], sep="/"), row.names=FALSE)
		
		toCall
	}

	#give names to this and save out
	names(output) <- sub(".csv", "", allFiles)
	output
}
