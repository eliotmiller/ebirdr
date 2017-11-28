#' Function to clean a data frame of eBird observations
#'
#' Filter eBird records to only those that meet a specified set of criteria.
#'
#' @param ebird.data Dataframe of eBird records.
#' @param arguments Character vector of arguments to which the existing eBird records will
#' be filtered. Records that match these arguments will be kept.
#' Essentially this should be interpreted as a series of AND statements. In order to use OR
#' statements, add the pipe to one of the comma-separated elements of the vector. See
#' examples below.
#' @param keep Character vector of column names in the existing eBird records that you
#' would like to be passed along/kept in the cleaned files. Can be set to 'keep.all' in
#' order to keep all columns.
#' @param group.id The name of the group ID column in the existing eBird records. Usually
#' (always?) simply 'GROUP.ID'. Required if you would like to remove all but one duplicated
#' group checklist.
#' @param unique.filter Whether or not to invoke the uniqueFilter function. Default is
#' TRUE, meaning that all but one checklist from shared group checklists will be removed.
#' Setting to FALSE means that all (duplicated) group checklists will be kept.
#' @param ... Additional arguments to be passed to lower level functions. For instance,
#' could be used to pass sub.id information to uniqueFilter().
#' 
#' @details This function should be fairly flexible. Pass it a set of arguments, and only
#' records that meet those requirements will be kept. Tell it which columns you want to
#' come along, and only those columns will be kept. Use the names of your data frame to
#' keep all columns in the cleaned data frame. If you want to clean a
#' number of different eBird data frames with the same set of arguments, consider using
#' the multiCleaner function, which saves the results directly to file.
#'
#' @return A cleaned data frame of eBird records.
#'
#' @export
#'
#' @importFrom data.table fwrite fread
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @importFrom methods new
#' @importFrom utils write.csv
#'
#' @references Team eBird.
#'
#' @examples
#' data(ex)
#' dim(ex)
#'
#' #run the cleaner function
#' cleaned <- cleaner(ebird.data=ex, arguments=c("VALID == '1'",
#' "PROTOCOL_NAME == 'eBird - Stationary Count' |
#'   PROTOCOL_NAME == 'eBird - Traveling Count' & EFFORT_DISTANCE_KM <= 5 |
#'   PROTOCOL_NAME == 'PriMig - Pri Mig Banding Protocol' |
#'   PROTOCOL_NAME == 'eBird - Exhaustive Area Count' & DURATION_HRS < 10"),
#'   keep=names(ex), unique.filter=TRUE)
#'
#' #many of the records were culled.
#' dim(cleaned)

cleaner <- function(ebird.data, arguments, keep, group.id, unique.filter=TRUE, ...)
{	
	#invoke uniqueFilter() if set to TRUE
	if(unique.filter)
	{
		temp <- uniqueFilter(ebird.data, group.id, ...)
	}
				
	#loop through the arguments and subset to conditions specified in arguments
	for(i in 1:length(arguments))
	{
		#this is here for legacy reasons, but having major issues with the NAs in
		#the distances
		#temp <- temp[eval(parse(text=paste("temp", arguments[j], sep=""))),]
		temp <- subset(ebird.data, eval(parse(text=paste(arguments[i]))))
	}

	#if keep is set to keep.all, set keep equal to all column names
	if(keep == "keep.all")
	{
		keep <- names(temp)
	}

	temp <- temp[,keep]
	
	temp
}
