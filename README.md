# ebirdr
## An eBird data manipulation package

ebirdr is an in-development R package for manipulating and analyzing eBird data. Eventually we intend to release the package, but more testing and development are needed.

#### What does it do?
ebirdr currently contains a minimal set of functions. These include the ability to filter all but one group record, clean eBird records to only those that match a given set of arguments, spatially thin eBird records (stratified spatial sampling), extract climate values for eBird records, run PCAs over large datasets (e.g., climate data for a large set of species), derive multivariate hypervolumes, and prep KML files for MODIS queries.

#### How do I use it?
Here are a few examples of how to use ebirdr. These examples use functions that work within the active R session. Much of the power of ebirdr comes from functions that perform the same calculation over multiple files, writing the results directly to the hard drive. These examples are not currently documented here.

```r
# Start the ebirdr package
library(ebirdr)

# Load up the example dataset
data(ex)

# This function removes all but one of a set of group checklists
dim(ex)
dim(uniqueFilter(ex, "GROUP_IDENTIFIER"))

# Cull the records to only those that meet a specific set of criteria
cleaned <- cleaner(ebird.data=ex, arguments=c("VALID == '1'",
	"PROTOCOL_NAME == 'eBird - Stationary Count' |
	PROTOCOL_NAME == 'eBird - Traveling Count' & EFFORT_DISTANCE_KM <= 5 |
	PROTOCOL_NAME == 'PriMig - Pri Mig Banding Protocol' |
	PROTOCOL_NAME == 'eBird - Exhaustive Area Count' & DURATION_HRS < 10"),
	keep="keep.all", unique.filter=TRUE)

# Many of the records were culled.
dim(cleaned)
```

#### How do I get it?
library(devtools)
install_github("eliotmiller/ebirdr")
library(ebirdr)
```