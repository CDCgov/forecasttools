% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hub_utils.R
\name{gather_hub_target_data}
\alias{gather_hub_target_data}
\title{Gather target truth data from a forecast hub.}
\usage{
gather_hub_target_data(
  hub_path,
  target_data_rel_path = fs::path("target-data", "target-hospital-admissions.csv")
)
}
\arguments{
\item{hub_path}{Local path to hub.}

\item{target_data_rel_path}{The path to the target
data file relative to forecast hub root directory.
Defaults to the path in the FluSight Forecast Hub.}
}
\value{
Table of target data
}
\description{
Gather target truth data from a forecast hub.
}
