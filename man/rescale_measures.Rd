% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{rescale_measures}
\alias{rescale_measures}
\title{Rescale Aggregate id Measure to id Measure}
\usage{
rescale_measures(measure, from, to)
}
\arguments{
\item{measure}{numeric aggregate measure between 0 and 100}

\item{from}{numeric from-measure relative to the aggregate}

\item{to}{numeric to-measure relative to the aggregate}
}
\value{
numeric rescaled measure
}
\description{
Given a aggregate id (e.g. reachcode) measure and the from and
to measure for a id (e.g. comid flowline), returns the measure along the
id line. This is a utility specific to the NHDPlus data model where
many comid flowlines make up a single reachcode / reach. "Measures"
are typically referenced to reaches. Flowlines have a stated from-measure /
to-measure. In some cases it is useful to rescale the measure such that it
is relative only to the flowline.

from is downstream -- 0 is the outlet
to is upstream -- 100 is the inlet
}
\examples{
rescale_measures(40, 0, 50)
rescale_measures(60, 50, 100)

}
