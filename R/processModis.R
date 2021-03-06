#' Process MODIS files from Google Earth Engine
#'
#' Combine MODIS files and split into species-level files.
#'
#' @param species.col Character vector specifying the column that contains the species
#' names. Will be used to split files. IMPORTANT: ebirdr generally uses file names as
#' organizational/naming structure. It's possible that the species names do not match
#' the file names, for instance if the analysis is using a different taxonomy than
#' eBird. If this is the case, and if the raw MODIS files contain the OBS_ID, the this
#' argument can be set to NULL, and a 'obs.table' passed instead. See below.
#' @param obs.table Data frame with 'OBS_ID' as the first column, and the
#' 'correct.species' that
#' observation belongs to as the second column. If this approach is used, then
#' 'correct.species' must be one of the values in keep. This argument can be missing
#' if species.col is provided.
#' @param read.wd Path to the read directory, where the files to be prepped are found.
#' @param write.wd Path to the write directory.
#' @param aux.wd Path to directory where the sample size summary file will be saved.
#' @param keep Character vector of columns to keep in the output files.
#' @param drop Should points that were not assigned complete MODIS values be dropped?
#' Default is TRUE.
#' @param cores How many cores to use for parallel processing.
#'
#' @details This function works by first binding all the files together in read.wd,
#' then dropping to all but the specified columns in keep, optionally dropping
#' incomplete records, then splitting into species-level files and saving out.
#'
#' @return Nothing to workspace. Saves out two files. First to aux.wd is a
#' data frame summarizing per species sample sizes. Next are
#' species-level csv files with matched MODIS data. Spaces in the species names will
#' be removed.
#'
#' @export
#'
#' @importFrom plyr rbind.fill.matrix
#' @importFrom sp coordinates proj4string CRS
#' @importFrom rgdal writeOGR
#' @importFrom parallel mclapply
#'
#' @references Team eBird.
#'

processModis <- function(species.col, obs.table, read.wd, write.wd, aux.wd,
	keep, drop, cores)
{
	#list all the files in read.wd
	allFiles <- list.files(path=read.wd)

	loaded <- parallel::mclapply(as.list(paste(read.wd, allFiles, sep="/")),
		data.table::fread, mc.cores=cores)

	#rbind all the files together, then convert to a data frame
	bound <- as.data.frame(dplyr::bind_rows(loaded))

	#if species.col is NULL, merge in the obs.table
	if(is.null(species.col))
	{
		bound <- merge(bound, obs.table)

		#define species.col as correct.species
		species.col <- "correct.species"
	}

	#drop to columns you want
	bound <- bound[,keep]

	#if drop is TRUE, cut to complete cases
	if(drop)
	{
		bound <- bound[complete.cases(bound),]
	}
	
	#split these into species-level files
	splitUp <- split(bound, bound[,species.col])

	#replace any spaces in the names of these files with underscores
	names(splitUp) <- sub(" ", "_", names(splitUp))

	#record the number of records in the files here
	temp <- lapply(splitUp, dim)
	records <- unlist(lapply(temp, "[", 1))

	records <- data.frame(species=names(records), records=records)

	#save this data frame out
	write.csv(records, paste(aux.wd, "modisSampleSize.csv", sep="/"),
		row.names=FALSE)

	#generate output filenames for modis files
	outnames <- paste(names(splitUp), "_modis.csv", sep="")

	parallel::mclapply(seq_along(splitUp), function(x)
		data.table::fwrite(splitUp[[x]], paste(write.wd, outnames[x], sep="/")),
		mc.cores=cores)
}
