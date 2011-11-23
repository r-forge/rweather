\name{getWeatherFromNOAA}
\alias{getWeatherFromNOAA}
\title{Retrieve weather report from NOAA: National Oceanic and Atmospheric Administration (United States)}
\usage{
  getWeatherFromNOAA(station_id = "KJFK", message = FALSE)
}
\arguments{
  \item{station_id}{the ID of the weather station near the
  necessary location To find your station ID, perform the
  following steps: \itemize{ \item Open this URL:
  http://www.weather.gov/xml/current_obs/seek.php?state=az&Find=Find
  \item Select the necessary state state. Click 'Find'.
  \item Find the necessary station in the 'Observation
  Location' column.  \item The station ID is in the URL for
  the weather page for that station. } For example if the
  weather page is
  http://weather.noaa.gov/weather/current/KPEO.html -- the
  station ID is KPEO.}

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
library("RWeather")
getWeatherFromNOAA(station_id="KJFK") # current observation for New York
}

