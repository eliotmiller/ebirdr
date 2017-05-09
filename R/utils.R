#this really random line of code makes R not throw a warning with respect to the non-
#standard foreach loops in some of the functions
globalVariables("i")

#utility function to convert eBird date/time fields from SQL queries into standard
#R date format

ebirdDate <- function(x)
{
	year <- substr(x, start=1, stop=4)
	month <- substr(x, start=6, stop=7)
	day <- substr(x, start=9, stop=10)
	output <- as.Date(paste(year, month, day, sep="-"))
	output
}
