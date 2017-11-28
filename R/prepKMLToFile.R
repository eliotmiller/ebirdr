#' Convert eBird records to KML
#'
#' Prep multiple eBird files into a single KML file.
#'
#' @param include Character vector of the columns to include in the prepped KML file.
#' Ideally, these should include a single column or combination of columns that 
#' can be used as a unique identifier to match KML records back to csv records later.
#' Also, this function is currently programmed in a rather inflexible manner, and one
#' of the columns needs to be a date, to be specified exactly with the date.col arg.
#' @param sql.date Default is FALSE. Set to TRUE if the date is the complex character
#' string that comes directly out of eBIRD SQL database, and will force into suitable
#' format. Otherwise takes date as is.
#' @param date.col The name of the date column to use for querying downstream. Likely
#' that some uses of KML files wouldn't need a date column. We should revise this
#' function to be more flexible, and not require a date.
#' @param coords Character vector in form c('longitude','latitude'), specifying the
#' names of the columns where the longitude and latitude are contained in the eBird
#' records.
#' @param early.date The earliest permissible date to retain in the dataset. Provide
#' as a properly formatted Date object.
#' @param late.date The latest permissible date to retain in the dataset. Provide as
#' a properly formatted Date object.
#' @param handle.early Set to to 'drop' to exclude records from before early.date.
#' Otherwise, set to a year (i.e. a numeric vector of length one, e.g. '2001'). All
#' records from before early.date will assume the same day and month, but the year will
#' be changed to the provided value. There is a caveat here that if the record in
#' question is from February 29th, it will automatically be shifted to February 28th.
#' @param handle.late Set to to 'drop' to exclude records from after late.date.
#' Otherwise, set to a year (i.e. a numeric vector of length one, e.g. '2001'). All
#' records from after late.date will assume the same day and month, but the year will
#' be changed to the provided value. There is a caveat here that if the record in
#' question is from February 29th, it will automatically be shifted to February 28th.
#' @param schema Not entirely sure what this does. Ends up getting assigned to a field
#' called 'schemaURL' in the resulting KML file.
#' @param read.wd Path to the read directory, where the files to be prepped are found.
#' All files in the directory will be bound together and turned into a single KML.
#' @param write.wd Path to the write directory. CRITICAL: DO NOT USE THE ~ SYMBOL
#' IN YOUR WRITE.WD OR THE FUNCTION WILL FAIL WITH A MYSTERIOUS ERROR.
#' @param cores How many cores to use for parallel processing.
#'
#' @details This function is rather inflexible and currently intended to convert a
#' set of eBird records from csv files into a single KML file. That file can then
#' be loaded into Google as a Fusion Table, which is then fed into a Google Earth
#' Engine script written by Matt Strimas-Mackey. This script can match records by
#' date and location to spatial data, e.g., MODIS reflectances. There currently is
#' no version of this function that doesn't write results directly to file. For
#' reasons I do not understand, this function generates a warning at the end that
#' seems like it can be ignored.
#'
#' @return Nothing to the workspace. A file titled output.kml is written to the
#' write.wd.
#'
#' @export
#'
#' @importFrom plyr rbind.fill.matrix
#' @importFrom sp coordinates proj4string CRS
#' @importFrom rgdal writeOGR
#'
#' @references Team eBird.
#'
#' @examples
#' #NOT RUN. this will prep a series of CSV files for querying with the MODIS/MCD43A4
#' #dataset. useful for subsequent querying with a Google Earth Engine script by
#' #Matt Strimas-Mackey.
#' #prepKML(include=c("SUB_ID", "SCI_NAME", "OBS_DT", "LONGITUDE", "LATITUDE"),
#'  #date.col="OBS_DT", coords=c("LONGITUDE","LATITUDE"),
#'  #early.date=as.Date("2000-02-18"), late.date=as.Date("2017-03-22"),
#'  #handle.early="2001", handle.late="2016", schema="test",
#'  #read.wd="~/thinned", write.wd="~/kml", cores=6)

prepKMLToFile <- function(include, sql.date=FALSE, date.col, coords, early.date,
  late.date, handle.early, handle.late, schema, read.wd, write.wd, cores)
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

  #go into a foreach loop where it loads the files, cuts them down to the
  #include columns, then rbinds them all together, and converts to a KML
  toBind <- foreach(i = 1:length(allFiles)) %dopar%
  {
    #load in a single file in the list of files
    temp <- as.data.frame(fread(paste(read.wd, allFiles[i], sep="/")))

    #cut the file down to the columns to include
    temp[,include]
  }

  #bind all the files together into a single large data frame
  bound <- dplyr::bind_rows(toBind)

  #check whether there is already a column titled date. if so, throw an
  #error and stop here
  if(exists("date", bound))
  {
    stop("the eBird records already contain a column titled 'date'")
  }

  #if sql date is TRUE, force to a useful format with this hidden fxn.
  #force to character here to make it possible to use substr fxn below
  else if(sql.date)
  {
  	bound$date <- ebirdDate(bound[,date.col])
  }

  #otherwise, just make a date column as is
  else
  {
    bound$date <- bound[,date.col]
  }

  #if handle.early is set to drop, exclude all dates from before early.date
  if(handle.early == "drop")
  {
    bound <- bound[bound$date >= early.date,]
  }

  #if handle.early is a number, replace the year for records that are before
  #early.date with the value in handle.early
  else
  {
    #this is the month and day of all records before early.date
    tempM <- substr(bound$date[bound$date < early.date], 6, 7)
    tempD <- substr(bound$date[bound$date < early.date], 9, 10)

    #if the length of either of those is 0, then there are no dates before
    #early date, and don't need to do any of the rest here
    if(length(tempM) > 0)
    {
      #here's a quick check to ensure that any dates from Feb 29th from before
      #early date are shifted to Feb 28th
      tempDM <- paste(tempM, tempD, sep="-")
      tempD[tempDM == "02-29"] <- "28"

      #paste these along with the new year and replace the values in bound
      newDate <- as.Date(paste(handle.early, tempM, tempD, sep="-"))
      bound$date[bound$date < early.date] <- newDate
    }
  }

  #if handle.late is set to drop, exclude all dates from after late.date
  if(handle.late == "drop")
  {
    bound <- bound[bound$date <= late.date,]
  }

  #else replace the year for records that are after late.date
  else
  {
    tempM <- substr(bound$date[bound$date > late.date], 6, 7)
    tempD <- substr(bound$date[bound$date > late.date], 9, 10)

    #if the length of either of those is 0, then there are no dates before
    #early date, and don't need to do any of the rest here
    if(length(tempM) > 0)
    {
      tempDM <- paste(tempM, tempD, sep="-")
      tempD[tempDM == "02-29"] <- "28"
      newDate <- as.Date(paste(handle.late, tempM, tempD, sep="-"))
      bound$date[bound$date > late.date] <- newDate
    }
  }

  #set coordinates on the bound object
  sp::coordinates(bound) <- coords
  sp::proj4string(bound) <- sp::CRS("+proj=longlat +datum=WGS84")

  rgdal::writeOGR(obj=bound,
    dsn=paste(write.wd, "output.kml", sep="/"), layer=schema, driver="KML")
}
