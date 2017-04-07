#' Filter out group checklists.
#'
#' Function to remove all but one of a set of eBird group checklists.
#'
#' @param dat The original eBird data.frame to filter.
#' @param group.id The name of the group ID column, usually "GROUP.ID".
#' @param sub.id The name of the sub ID column, or otherwise. Experimental,
#' have not really tested, but in theory can be used to sort dat so that, for
#' instance, only the first input of a set of group checklists is kept. 
#' 
#' @details This function is called internally by cleaner(), but is also
#' exported here, as it may prove generally useful to some. Note that the results
#' are saved into the active R session, as opposed to written to file like the
#' cleaner() function.
#'
#' @return A filtered data.frame. 
#'
#' @export
#'
#' @references Team eBird.
#'
#' @examples
#' #not run
#' #results <- readIn()
#' #summ <- sesIndiv(results)
#' #examp <- nullPerformance(summ)

uniqueFilter <- function(dat, group.id, sub.id)
{
	#assign a unique group.id to every row that is missing a group identifier
	toGenerate <- length(dat[,group.id][dat[,group.id]==""])

	#previously you ran this like below hashed out, but got errors recently. here for
	#legacy sake	
	#dat[,group.id][dat[,group.id]==""] <- paste("delete", 1:toGenerate, sep="")
	dat[,group.id][is.na(dat[,group.id])] <- paste("delete", 1:toGenerate, sep="")

	#now if sub.id is provided, sort according to that	
	if(!missing(sub.id))
	{
		dat <- dat[order(dat[,sub.id]),]
	}
	
	#subset to non-duplicated group.ids
	dat <- dat[!duplicated(dat[,group.id]),]
	
	#delete the group IDs you made up and return
	dat[,group.id][grep("delete", dat[,group.id])] <- ""
	
	dat
}
