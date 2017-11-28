#' Extract environmental data
#'
#' Extract and associate environmental data from GIS layers with eBird records.
#'
#' @param ebird.data Dataframe of eBird records.
#' @param env.wd Working directory where the environmental data is currently stored.
#' It would be very easy to make this able to download environmental data, set the
#' resolution you want in the data, and to use other forms
#' of environmental data besides bioclim, but those things are currently hardwired in.
#' It won't be much more work to make this more flexible when we're ready to take that
#' step.
#' @param keep Character vector of column names in the existing eBird records that you
#' would like to be passed along/kept in the cleaned files. Specify names(ebird.data),
#' (but substitute the actual name of your data frame, see example) to keep all columns.
#' @param longitude The name of the column in the eBird data files where the longitude
#' data is kept.
#' @param latitude The name of the column in the eBird data files where the latitude
#' data is kept.
#' @param cut.incomplete Default is TRUE. If a point is missing environmental data,
#' will remove that point before saving out the file.
#'
#' @details There is a lot of room to make this function more flexible. Currently it
#' assumes you have bioclim data in a folder already on your computer at a resolution of
#' 2.5 minutes. There is no reason the function cannot be generalized to download bioclim
#' data at whatever resolution you prefer, and there is no reason it cannot be generalized
#' to extract data from other GIS layers.
#'
#' @return Saves data files with associated environmental data to the write.wd, returns
#' a data frame summarizing sample sizes to the workspace.
#'
#' @export
#'
#' @importFrom raster getData extract
#' @importFrom stats complete.cases runif sd

extractEnv <- function(ebird.data, env.wd, keep, longitude, latitude, cut.incomplete=TRUE)
{
	#load the environmental data
	envData <- raster::getData(name="worldclim", var="bio", res=2.5,
		download=FALSE, path=env.wd)
	
	#pull the environmental data for the records
	env <- raster::extract(x=envData, y=ebird.data[,c(longitude, latitude)])
		
	#subset records to just the columns you want to keep. could probably get rid
	#or this step and speed the function up considerably. have it in here in case
	#people get far into an analysis and are sick of how large their files are,
	#and want to cut off extraneous columns
	ebird.data <- ebird.data[,keep]
		
	#bind the records to the climate data
	results <- data.frame(ebird.data, env)
		
	#if complete cases is TRUE, cut any cases where the env data is incomplete
	if(cut.incomplete)
	{
		#identify the columns that contain the env data in toSave
		lastCol <- dim(results)[2]
		firstCol <- lastCol - dim(env)[2] + 1
		results <- results[complete.cases(results[,firstCol:lastCol]),]
	}

	results
}
