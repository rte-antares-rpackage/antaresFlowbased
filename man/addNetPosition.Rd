% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addNetPosition.R
\name{addNetPosition}
\alias{addNetPosition}
\title{addNetPosition}
\usage{
addNetPosition(data, opts = antaresRead::simOptions(), inAreas = c("be",
  "de", "fr", "nl"), adq = FALSE, newName = "_CWE")
}
\arguments{
\item{data}{\code{antaresDataList} read with readAntares, containing both areas and links tables.}

\item{opts}{\code{list} of simulation parameters returned by the function \link{setSimulationPath}: this represents the 
directory of the Antares study. The default path is indicated by \code{antaresRead::simOptions()}.}

\item{inAreas}{\code{character} lists of areas belonging to the perimeter, whose exchanges are going to be taken into account in the balance
calculation. All links connecting two areas in this list are taken into account, the links connected to only one area (or none) are ignored.
By default, the list is c("be", "de", "fr", "nl").}

\item{adq}{\code{boolean} calculates the net positions of the areas based on post-adequacy patch results. By default, the value is FALSE.}

\item{newName}{\code{character} Added suffix to the calculated column's name. By default, the value is "_CWE".}
}
\description{
This function calculates the Net Position (the balance) of areas within a bigger group of areas, 
based on the exchanged flows in the indicated perimeter.
By default, the function calculates the Net Position within the CWE area and creates a new column called Balance_CWE.
}
\examples{
\dontrun{
opts <- antaresRead::setSimulationPath("D:/Users/titorobe/Desktop/antaresStudy", 2)
data <- readAntares(area = "all", links = "all", mcYears = 1)

##Add the net positions in the CWE area
data <- addNetPosition(data, opts, adq = FALSE)

##Add the net positions in an area containing CWE+AT
data <- addNetPosition(data, opts, adq = FALSE,
 inAreas = c("be", "de", "fr", "nl", "at"), newName = "_CWEAt")

}

}
