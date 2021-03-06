% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotMAP.R
\name{plotMAP}
\alias{plotMAP}
\title{Plotting trips from a central-placve foraging species}
\usage{
plotMAP(
  allbirds,
  pmap = FALSE,
  path = NULL,
  w = 12,
  h = 12,
  units = "cm",
  ColLong = NULL,
  ColLat = NULL
)
}
\arguments{
\item{pmap}{logical. Should every plot be plotted in your active session? Depending of the number of single trip you have, this might generate a lot of plots}

\item{path}{path indicating where the plots should be saved}

\item{w}{width of the plot. Unit to select in the \emph{units} argument. Passing to ggsave() function}

\item{h}{height of the plot. Unit to select in the \emph{units} argument. Passing to ggsave() function}

\item{units}{unit to consider for plotting e.g. c("mm", "cm"). Passing to ggsave() function}

\item{ColLat}{vector indicating the latitude of the colony and have the same length as your data frame. If nothing is passed to that
argument, the function will automatically take the first longitude of the trip as the colony's latitude}

\item{df}{data frame containing all the trips. For now, passing only CPFMove object}

\item{ColLonga}{vector indicating the longitude of the colony and have the same length as your data frame. If nothing is passed to that
argument, the function will automatically take the first longitude of the trip as the colony's longitude}
}
\value{
This function returns a number equal as the number of unique trips in your dataset. If the argument \strong{pmap} is \emph{TRUE}
the plots will also be returned in R
}
\description{
This function was developped for giving a quick visual on your trips. Land basemap and bathymetry is also
render automatically when calling the function
}
\details{
This function has been designed orignally to facilitate the visual inspection of various filtering parameters on the actual trips. Howerver,
we plan to make this function more general as soon as possible. This function use mainly the plotting function from
the package [\link{ggOceanMaps}](https://mikkovihtakari.github.io/ggOceanMaps/)
}
\examples{
\dontrun{
path <- c("C:/Users/philip/Desktop/test")
test <- plotMAP(f, pmap = F, path = path, ColLong = f$ColLong, ColLat = f$ColLat)
}
}
\keyword{central-place-forager}
\keyword{colony}
\keyword{cpf}
\keyword{delineating}
\keyword{movement}
\keyword{movement-ecology}
\keyword{trips}
