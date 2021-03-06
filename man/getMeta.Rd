% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createTD.R
\name{getMeta}
\alias{getMeta}
\alias{setMeta}
\title{Get and set metadata for TD objects}
\usage{
getMeta(TD)

setMeta(TD, meta)
}
\arguments{
\item{TD}{An object of class TD.}

\item{meta}{A data.frame containing metadata.}
}
\description{
Functions for extracting and adding metadata for objects of class TD.\cr\cr
\code{getMeta} extracts a data.frame with location, date, design, latitude,
longitude, plot width and plot length for all trials in TD.\cr\cr
\code{setMeta} adds metadata from a data.frame to an object of class TD. See
details for the specifications of the data.frame.\cr\cr
The most common use case is extracting metadata from a TD object, modifying
the content and then adding it back to the TD object.\cr
Information in the metadata of a TD object is used in plotting functions
(e.g. latitude and longitude for a map plot) and when fitting models on the
data (the trial design).
}
\details{
When setting metadata, metadata has to be a data.frame with rownames
corresponding to the trials in \code{TD}. The data.frame should contain one
or more of the following columns:
\describe{
\item{trLocation}{The location of the trial. Used as default name when
creating plots and summaries.}
\item{trDate}{The date of the trial.}
\item{trDesign}{The design of the trial. One of "none" (no (known) design),
"ibd" (incomplete-block design), "res.ibd" (resolvable incomplete-block
design), "rcbd" (randomized complete block design),
"rowcol" (row-column design) or "res.rowcol" (resolvable row-column design).
Used when fitting models.}
\item{trLat}{The latitude of the trial on a scale of -90 to 90. Used when
plotting the trials on a map.}
\item{trLong}{The longitude of the trial on a scale of -180 to 180. Used
when plotting the trials on a map.}
\item{trPlWidth}{The width of the plot. Used in combination with trPlLength
to determine the size of the plots in a layout plot of a trial.}
\item{trPlLength}{The length of the plot. Used in combination with
trPlWidth to determine the size of the plots in a layout plot of a trial.}
}
The values of the metadata of TD will be set to the values in the
corresponding column in \code{meta}. Existing values will be overwritten,
but \code{NA} will be ignored so setting a value to \code{NA} won't result
in accidentally removing it.
}
\examples{
data("dropsRaw")

## Create a TD object.
dropsTD <- createTD(data = dropsRaw[dropsRaw$year == 2012, ],
                    genotype = "Variety_ID",
                    trial = "Experiment",
                    loc = "Site",
                    repId = "Replicate",
                    subBlock = "block",
                    rowCoord = "Row",
                    colCoord = "Column",
                    trLat = "Lat",
                    trLong = "Long")

## Get meta data from dropsTD.
(dropsMeta <- getMeta(dropsTD))

## Add trial date to meta data.
dropsMeta$trDate <- as.Date(rep("010112", times = 5), "\%d\%m\%y")

## Add back meta data to wheatTD.
dropsTD <- setMeta(dropsTD,
                   dropsMeta)

}
\seealso{
Other functions for TD objects: 
\code{\link{TD}},
\code{\link{plot.TD}()},
\code{\link{summary.TD}()}
}
\concept{functions for TD objects}
