% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_by_age.R
\name{get_by_age}
\alias{get_by_age}
\title{Return occurrences for all taxa within a specified time range.}
\usage{
get_by_age(x, ageunit = "Mya", timerule = "contain", timebuffer = "20",
  ...)
}
\arguments{
\item{x}{An (optional) age range \code{c(agemin, agemax)}.}

\item{ageunit}{The age units for the age range.  By default \code{"ma"} (millions of years ago), accepts \code{"kya"} (thousands of years ago) and \code{"ybp"} (years before present).}

\item{timerule}{Resolve time overlap using the appropriate rule: \code{contain} (records are fully within range), \code{major} (records at least 50\% within), \code{overlap} (records overlap), or \code{buffer} (fully contained, with a buffer of - default - 20\%).}

\item{timebuffer}{(optional) for use with \code{timerule == "buffer"}, the temporal buffer around the defined age range in \code{x}.}

\item{...}{Other parameters to be passed into the API, described at \url{https://training.paleobiodb.org/comp1.0}.}
}
\value{
More details on the use of these parameters can be obtained from
 \url{https://training.paleobiodb.org/comp1.0/}.

 A list of class `occurrence` and `list`.  The list is composed of two elements:

 \item{ \code{records} }{The complete listing of taxon occurrences.}
 \item{ \code{meta}  }{Metadata for the search.}

 The \code{records} object is a \code{data.frame}
 \item{ \code{collection_name}  }{Site or collection unit name for the record.}
 \item{ \code{lng}  }{Collection site longitude.}
 \item{ \code{lat}  }{Collection site latitude.}
 \item{ \code{accepted_name}  }{The taxon name.}
 \item{ \code{max_age}  }{The oldest of all sample ages (in calendar years before present) in the dataset.}
 \item{ \code{min_age}  }{The youngest of all sample ages (in calendar years before present) in the dataset.}
 \item{ \code{age_unit}  }{The units for age (by default "Mya").}
 \item{ \code{database}  }{The database from which the record was obtained.}
 \item{ \code{occurrence_no}  }{The numeric ID for the record within the parent database.}
 \item{ \code{dataset_no}  }{For records within Neotoma, the numeric ID of the dataset from which the sample was obtained.}
 \item{ \code{accepted_no}  }{The numeric identifier for the taxon name from the parent database.}
 \item{ \code{collection_no}  }{The numeric identifier for the collection within the parent database.}
 \item{ \code{country}  }{The country within which the sample is found (if known).}
 \item{ \code{state}  }{The state (when known) within the identified country.}
}
\description{
A wrapper for the Composite API, returning all records from both databases.
}
\examples{
\dontrun{
# Search for sites with "Canis" fossils.
holocene_snap <- get_by_age(x = c(12000, 11500), ageunit = "ybp", timerule = "contain", base_name = "")

# Limit searches to North America (undocumented use of \\code{bbox})

}
}
\author{
Simon J. Goring \email{goring@wisc.edu}
}
\references{
EarthLife Consortium: http://earthlifeconsortium.org/
API Reference:  https://training.paleobiodb.org/comp1.0
}
\keyword{IO}
\keyword{connection}

