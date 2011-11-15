\name{rweather}
\alias{rweather}
\title{Retrieve weather forecast for a given location using Google Weather API}
\usage{
  rweather(address = "Trieste", message = FALSE)
}
\arguments{
  \item{address}{A character containing the address of the
  location of interest}

  \item{message}{If TRUE (default FALSE) the function
  returns a message summarizing the weather forecast for
  the location of interest}
}
\value{
  A list containing: \itemize{ \item forecast_information
  for the given the given location \item current_conditions
  for the given the given location \item forecast
  conditions (3 days) for the given the given location }
}
\description{
  This function will get the weather forecast conditions
  for a given location
}
\examples{
library("RWeather")
rweather(address="Basovizza", message=TRUE)
}

