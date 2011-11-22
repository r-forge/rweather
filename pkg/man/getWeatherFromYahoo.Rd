\name{getWeatherFromYahoo}
\alias{getWeatherFromYahoo}
\title{Retrieve weather forecast for a given location using Yahoo API}
\usage{
  getWeatherFromYahoo(location_id = "US", units = "metric",
    message = FALSE)
}
\arguments{
  \item{location_id}{}

  \item{units}{A character either 'metric' (default) for
  metric units or '' for non metric units}

  \item{message}{If TRUE (default FALSE) the function
  returns a message summarizing the weather forecast for
  the location of interest}
}
\value{
  A data.frame containing the weather information for the
  given location
}
\description{
  This function will get the weather forecast conditions
  for a given location
}
\examples{
library("RWeather")
getWeatherFromYahoo("US")
}

