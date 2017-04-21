#' Random lines of eBird data.
#'
#' A dataset containing random lines of eBird data from multiple species.
#'
#' @format A data frame with 80 rows and 28 variables:
#' \describe{
#'   \item{LOC_ID}{location ID}
#'   \item{SUBNATIONAL1_CODE}{the code at broad spatial scales}
#'   \item{SUBNATIONAL2_CODE}{the code at finer spatial scales}
#'   \item{LATITUDE}{latitude of the record}
#'   \item{LONGITUDE}{longitude of the record}
#'   \item{SUB_ID}{the ID of the entire checklist}
#'   \item{OBS_ID}{the ID of the specific observation}
#'   \item{GROUP_ID}{the group ID if it is a shared checlist}
#'   \item{CREATION_DT}{the date the list was entered}
#'   \item{PROTOCOL_ID}{the ID number of the eBird protocol used}
#'   \item{PROTOCOL_NAME}{the name of the eBird protocol used}
#'   \item{USER_ID}{the ID number of the user}
#'   \item{DURATION_HRS}{how long birds were recorded}
#'   \item{ALL_OBS_REPORTED}{whether all birds observed are being reported}
#'   \item{OBS_DT}{the date the observations were made}
#'   \item{EFFORT_HRS_ATLEAST}{time-related effort variables}
#'   \item{EFFORT_HRS_ATMOST}{time-related effort variables}
#'   \item{EFFORT_DISTANCE_KM}{distance-related effort variables}
#'   \item{EFFORT_DISTANCE_KM_ATLEAST}{distance-related effort variables}
#'   \item{EFFORT_DISTANCE_KM_ATMOST}{distance-related effort variables}
#'   \item{NUM_OBSERVERS}{the number of observers involved with the list}
#'   \item{ORIG_SPECIES_CODE}{the six-letter eBird code used}
#'   \item{SCI_NAME}{the eBird-specific scientific name of the species}
#'   \item{HOW_MANY_ATLEAST}{how many indivduals were seen}
#'   \item{HOW_MANY_ATMOST}{how many indivduals were seen}
#'   \item{REVIEWED}{whether the observation was reviewed by a moderator}
#'   \item{COMMENTS}{comments from that particular OBS_ID}
#'   \item{VALID}{whether the observation is considered valid}
#'   ...
#' }
#' @source \url{http://ebird.org/}
"ex"