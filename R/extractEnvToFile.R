#' Extract environmental data
#'
#' Extract and associate environmental data from GIS layers with eBird records.
#'
#' @param env.wd Working directory where the environmental data is currently stored.
#' It would be very easy to make this able to download environmental data, set the
#' resolution you want in the data, and to use other forms
#' of environmental data besides bioclim, but those things are currently hardwired in.
#' It won't be much more work to make this more flexible when we're ready to take that
#' step.
#' @param ebird.wd Working directory where the eBird records are stored.
#' @param write.wd Working directory where the eBird records + environmental data will
#' be saved to.
#' @param keep Character vector of column names in the existing eBird records that you
#' would like to be passed along/kept in the cleaned files. Can be set to 'keep.all',
#' in which case all columns in the eBird data files will be kept.
#' @param longitude The name of the column in the eBird data files where the longitude
#' data is kept.
#' @param latitude The name of the column in the eBird data files where the latitude
#' data is kept.
#' @param cut.incomplete Default is TRUE. If a point is missing environmental data,
#' will remove that point before saving out the file.
#' @param cores The number of cores to use for parallel processing. This package currently
#' requires your computer to be able to handle at, the bare minimum, installation of the
#' various parallel processing packages. A forthcoming feature will allow you to specify
#' 'seq' here, and would mean ebirdr could be run even if you can't install the parallel
#' processing software.
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

extractEnvToFile <- function(env.wd, ebird.wd, write.wd, keep, longitude,
	latitude, cut.incomplete=TRUE, cores)
{
	registerDoParallel(cores)
	
	#load the environmental data
	envData <- raster::getData(name="worldclim", var="bio", res=2.5,
		download=FALSE, path=env.wd)
	
	#create a character vector listing all ebird files
	allFiles <- list.files(path=ebird.wd)

	results <- foreach(i = 1:length(allFiles), .combine=c) %dopar%
	{
		#read in the presences for species i
		records <- as.data.frame(fread(paste(ebird.wd, allFiles[i], sep="/")))
		
		#if keep was set to 'keep.all', set keep to the col names of the file
		if(keep=="keep.all")
		{
			keep <- names(records)
		}

		#pull the environmental data for the records
		env <- raster::extract(x=envData, y=records[,c(longitude, latitude)])
		
		#subset records to just the columns you want to keep. could probably get rid
		#or this step and speed the function up considerably. have it in here in case
		#people get far into an analysis and are sick of how large their files are,
		#and want to cut off extraneous columns
		records <- records[,keep]
		
		#bind the records to the climate data
		toSave <- data.frame(records, env)
		
		#if complete cases is TRUE, cut any cases where the env data is incomplete
		if(cut.incomplete)
		{
			#identify the columns that contain the env data in toSave
			lastCol <- dim(toSave)[2]
			firstCol <- lastCol - dim(env)[2] + 1
			toSave <- toSave[complete.cases(toSave[,firstCol:lastCol]),]
		}

		#save out csv
		outName <- sub(".csv", "_withenv.csv", allFiles[i])
		fwrite(toSave, file=paste(write.wd, outName, sep="/"), row.names=FALSE)
		
		#calculate sample size
		dim(toSave)[1]
	}
	
	#going to make a big assumption here that the first two words of each file are the
	#genus, species of the files in question, and that there's an underscore between
	splitUp <- strsplit(allFiles, "_")

	results <- data.frame(species=paste(lapply(splitUp, "[", 1),
		lapply(splitUp, "[", 2), sep="_"), results)

	results
}
