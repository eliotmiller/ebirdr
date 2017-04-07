#' Directly query the eBird database
#'
#' Function to query the eBird database via SQL commands.
#'
#' @param connection The result of a properly formatted object from estConn().
#' @param type One of either "sp", "absences", or "all.spp". If set to sp, then will query
#' based on six-letter eBird codes. If set to "absences" or "all.spp", then will query on
#' SUBNATIONAL1_CODEs.
#' @param query.list A list of things to query, where the type of thing is defined by the
#' type argument. In general, these are lists of character vectors, where the name of the
#' element in that list defines the name of the csv file into which the records will be
#' saved, and the elements in the character vector define what will be queried. There is
#' some chance the underlying query functions are stupidly written, e.g. "absences"
#' might actually just need a character vector, not in a list. Requires some checking,
#' Eliot currently forgets how he left things.
#' @param write.wd The path to the write working directory. Queried data will be saved
#' here as csv files.
#' 
#' @details This function is only useful if you have permission to run it. Lots of room
#' to improve the possible types of queries. A temporary solution is to write
#' specialized queries and add these to the utils.R function, then modify the possible
#' inputs to this function. A more general long-term solution would allow people to input
#' SQL queries directly into this function. Would be particularly smart to institute some
#' sanity checks to said SQL queries if we went that route. 
#'
#' @return A list of character vectors of unique SUBNATIONAL1_CODEs that each species
#' occurs in.
#'
#' @export
#'
#' @importFrom ROracle dbGetQuery
#' @importFrom data.table fwrite
#'
#' @references Team eBird.
#'
#' @examples
#' #not run
#' #results <- readIn()
#' #summ <- sesIndiv(results)
#' #examp <- nullPerformance(summ)

ebirdQuery <- function(connection, type, query.list, write.wd)
{
	if(missing(write.wd))
	{
		stop("Set your working directory")
	}
	
	#this will check if the elements in query.list have names
	if(length(names(query.list))!=length(query.list))
	{
		stop("Make sure the query.list is properly named")
	}

	#if type=sp, then passing in species to query. for each element in the query list
	if(type=="sp")
	{
		for(i in 1:length(query.list))
		{
			#use the queryPaster function to create an SQL query for all eBird taxa
			#contained within that (Jetz) species concept
			temp <- dbGetQuery(conn=connection, statement=spQueryPaster(query.list[[i]]))
		
			#remove duplicate records for instances where you called both a specific
			#subspecies and its parent. you screwed up and ran it wrong originally. that
			#way is commented out below. should have done as follows
			temp <- temp[!duplicated(temp$SUB_ID),]
			#temp <- temp[temp$SUB_ID %in% unique(temp$SUB_ID),]

			#save out as RDS file. first create the file name
			saveName <- paste(names(query.list)[i], "_presences", ".csv", sep="")
			fwrite(temp, paste(write.wd, saveName, sep="/"), row.names=FALSE)
		}
	}
	
	#if type=absences, then passing in locations to query. for each element
	else if(type=="absences")
	{
		for(i in 1:length(query.list))
		#for(i in 1:length(query.list))
		{
			#use the queryPaster function to create an SQL query for that location
			temp <- dbGetQuery(conn=connection, statement=absQueryPaster(query.list[i]))
		
			#save out as csv file. first create the file name
			saveName <- paste(names(query.list)[i], "_poss_locs", ".csv", sep="")
			fwrite(temp, paste(write.wd, saveName, sep="/"), row.names=FALSE)
		}	
	}
	#if type=absences, then passing in locations to query. for each element
	else if(type=="all.spp")
	{
		for(i in 1:length(query.list))
		#for(i in 1:length(query.list))
		{
			#use the queryPaster function to create an SQL query for that location
			temp <- dbGetQuery(conn=connection, statement=allQueryPaster(query.list[i]))
		
			#save out as csv file. first create the file name
			saveName <- paste(query.list[i], "_allRecords", ".csv", sep="")
			fwrite(temp, paste(write.wd, saveName, sep="/"), row.names=FALSE)
		}	
	}
	else
	{
		stop("type must be set to sp, absences, or all.spp")
	}	
}
