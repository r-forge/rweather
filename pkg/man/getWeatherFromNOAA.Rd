\name{getWeatherFromNOAA}
\alias{getWeatherFromNOAA}
\title{Retrieve weather report from NOAA: National Oceanic and Atmospheric Administration (United States)}
\usage{
  getWeatherFromNOAA(station_id = "KJFK", message = FALSE)
}
\arguments{
  \item{station_id}{the ID of the weather station near the
  necessary location To find your station ID, open
  http://www.weather.gov/xml/current_obs/seek.php?state=az&Find=Find
  select the desired state and retrieve the Observation
  Location: the station_id is the string within the ()
  parentheses.}

  \item{message}{If TRUE (default FALSE) the function
  returns a message summarizing the weather forecast for
  the location of interest}
}
\value{
  A data.frame containing the weather information for the
  given station
}
\description{
  This function will get the weather forecast conditions
  for a given station
}
\examples{
\dontrun{
library("RWeather")
getWeatherFromNOAA(station_id="KJFK") # current observation for New York
}
}

