# ebirdr
## An eBird data manipulation package

This is currently a private repo. It is an in-development R package for manipulating and analyzing eBird data. We intend to release it for public use after additional development and testing. 

#### What does it do?
ebirdr currently contains a minimal set of functions. These include the ability to filter all but one group record, clean eBird records to only those that match a given set of arguments, spatially thin eBird records (stratified spatial sampling), extract climate values for eBird records, run PCAs over variables (e.g., climate), derive multivariate hypervolumes, and prep KML files for MODIS queries.

#### How do I use it?
Here are a few examples of how to use ebirdr. I took the output of Matt Strimas-Mackey's auk examples, split them into separate species (Golden-winged and Blue-winged Warblers), replaced the spaces in the names columns, and saved as separated csv files. Put those csv files in a folder, and the example begins there. Adjust your directory names accordingly.

```r
# Navigate to the folder containing your awk-derived Golden-winged and Blue-winged Warbler files
# Start the ebirdr package
setwd("~/Documents/Writing and Other Work/Post-doc Work/ebirdr devel/orig")
library(ebirdr)

# Load them in, plot locations to get a sense of data density
gwwa <- as.data.frame(data.table::fread("Vermivora_chrysoptera.csv"))
bwwa <- as.data.frame(data.table::fread("Vermivora_cyanoptera.csv"))

plot(gwwa$LATITUDE~gwwa$LONGITUDE, cex=0.3)
plot(bwwa$LATITUDE~bwwa$LONGITUDE, cex=0.3)

# This function removes all but one of a set of group checklists
dim(gwwa)
dim(uniqueFilter(gwwa, "GROUP_IDENTIFIER"))

# First example. Cull the records to only those that meet a specific set of criteria
system.time(cleaner(
	arguments=c(
		"PROTOCOL_TYPE != 'Historical'",
		"EFFORT_DISTANCE_KM <= 1 |
			PROTOCOL_TYPE == 'eBird - Stationary Count' |
			PROTOCOL_TYPE == 'eBird - Exhaustive Area Count' |
			PROTOCOL_TYPE == 'eBird Random Location Count' |
			PROTOCOL_TYPE == 'PriMig - Pri Mig Banding Protocol'"),
	keep=c("GLOBAL_UNIQUE_IDENTIFIER","COMMON_NAME", "LOCALITY_ID", "LATITUDE", "LONGITUDE",
		"OBSERVATION_DATE", "SAMPLING_EVENT_IDENTIFIER"),
	group.id="GROUP_IDENTIFIER",
	unique.filter=TRUE,
	read.wd="~/Documents/Writing and Other Work/Post-doc Work/ebirdr devel/orig",
	write.wd="~/Documents/Writing and Other Work/Post-doc Work/ebirdr devel/cleaned",
	cores=2))

# Load the cleaned points back in and see how they look
setwd("~/Documents/Writing and Other Work/Post-doc Work/ebirdr devel/cleaned")
gwwaClean <- as.data.frame(data.table::fread("Vermivora_chrysoptera_cleaned.csv"))
bwwaClean <- as.data.frame(data.table::fread("Vermivora_cyanoptera_cleaned.csv"))

dim(gwwaClean)
head(gwwaClean)

# Second example. Spatially downsample these points
spThinner(xxx="extent", yyy="extent", lat.col="LATITUDE", long.col="LONGITUDE",
	nx=100, ny=100, size=30,
	read.wd="~/Documents/Writing and Other Work/Post-doc Work/ebirdr devel/cleaned",
	write.wd="~/Documents/Writing and Other Work/Post-doc Work/ebirdr devel/thinned",
	cores=2)

# Load the thinned points back in, plot to show the spatial stratification
library(maps)
setwd("~/Documents/Writing and Other Work/Post-doc Work/ebirdr devel/thinned")
gwwaThin <- as.data.frame(data.table::fread("Vermivora_chrysoptera_cleaned_thinned.csv"))
bwwaThin <- as.data.frame(data.table::fread("Vermivora_cyanoptera_cleaned_thinned.csv"))

# Plot the original data in blue
plot(bwwa$LONGITUDE, bwwa$LATITUDE, cex = 0.1, pch = 20, col = "blue", xlab = "Longitude",
	ylab = "Latitude", main = "Blue-winged Warbler records")
map("world", lwd=0.5, add=TRUE)
map("state", lwd=0.5, add=TRUE)	

# Now add the sampled data in red
points(bwwaThin$LONGITUDE, bwwaThin$LATITUDE, pch = 20, cex = 0.25, col = "red")

# Third example. Query environmental data for the remaining points
system.time(extractEnv(env.wd="~/Documents/Writing and Other Work/Post-doc Work/Dominance&Distributions/data",
	ebird.wd="~/Documents/Writing and Other Work/Post-doc Work/ebirdr devel/thinned",
	write.wd="~/Documents/Writing and Other Work/Post-doc Work/ebirdr devel/wEnv",
	keep="keep.all", longitude="LONGITUDE", latitude="LATITUDE", cores=2))

# Fourth example. Run a big PCA over the climate data. First prep the comparison for analysis
temp <- strsplit(list.files("~/Documents/Writing and Other Work/Post-doc Work/ebirdr devel/wEnv"), "_")
warblers <- paste(lapply(temp, "[", 1), lapply(temp, "[", 2), sep="_")
warblers <- list(warblers)
names(warblers) <- "warblers"

# Note that we cut bio7 here, because it is just bio5-bio6.
system.time(bigPCA(comparisons=warblers,
	read.wd="~/Documents/Writing and Other Work/Post-doc Work/ebirdr devel/wEnv",
	write.wd="~/Documents/Writing and Other Work/Post-doc Work/ebirdr devel/clim_pcas",
	aux.wd="~/Documents/Writing and Other Work/Post-doc Work/ebirdr devel/clim_pca_summaries",
	columns=paste("bio", c(1:6,8:19), sep=""), scale.center=TRUE, axes=3))

# Fifth example. Derive the climate hypervolumes for each species
system.time(hypervolumes(bandwidth.arg="silverman", species.col="species",
	trait.cols=c("PC1", "PC2"),
	read.wd="~/Documents/Writing and Other Work/Post-doc Work/ebirdr devel/clim_pcas",
	write.wd="~/Documents/Writing and Other Work/Post-doc Work/ebirdr devel/clim_hypervolumes",
	cores=4))

# Load the hypervolumes you just calculated
hvs <- loadHypervolumes("~/Documents/Writing and Other Work/Post-doc Work/ebirdr devel/clim_hypervolumes")
plot(hvs)

# Sixth example. Prep KML files to query MODIS data and to facilitate plotting in Google Earth
prepKML(include=c("SAMPLING_EVENT_IDENTIFIER", "COMMON_NAME", "OBSERVATION_DATE", "LONGITUDE", "LATITUDE"),
	sql.date=FALSE, date.col="OBSERVATION_DATE", coords=c("LONGITUDE","LATITUDE"),
	early.date=as.Date("2000-02-18"), late.date=as.Date("2017-03-22"), handle.early="drop",
	handle.late="drop", schema="test",
	read.wd="/Users/eliotmiller/Documents/Writing and Other Work/Post-doc Work/ebirdr devel/thinned",
	write.wd="/Users/eliotmiller/Documents/Writing and Other Work/Post-doc Work/ebirdr devel/kml", cores=2)
```

#### How do I get it?
First ask for permission from Eliot to become a contributor to ebirdr. Then get an authorization token and install like this:
```r
library(devtools)
install_github("eliotmiller/ebirdr", auth_token="yourtokenhere")
library(ebirdr)
```