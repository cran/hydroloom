% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_attribute_topology.R
\name{make_attribute_topology}
\alias{make_attribute_topology}
\alias{make_attribute_topology.data.frame}
\alias{make_attribute_topology.hy}
\title{Make Attribute Topology}
\usage{
make_attribute_topology(x, min_distance)

\method{make_attribute_topology}{data.frame}(x, min_distance)

\method{make_attribute_topology}{hy}(x, min_distance)
}
\arguments{
\item{x}{data.frame network compatible with \link{hydroloom_names}.}

\item{min_distance}{numeric distance in units compatible with the units of
the projection of \code{lines}. If no nodes are found within this distance, no
connection will be returned.}
}
\value{
data.frame with id and toid
}
\description{
given a set of lines with starting and ending nodes that
form a geometric network, construct an attribute topology.
}
\details{
If a \code{future} plan is set up, node distance calculations will be
applied using future workers.
}
\examples{

x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

y <- dplyr::select(x, COMID)

y <- sf::st_transform(y, 5070)

z <- make_attribute_topology(y, 10)

x <- add_toids(hy(x), return_dendritic = FALSE)

x[x$id == x$id[1],]$toid
z[z$COMID == x$id[1],]$toid

}
