\name{getWeatherFromGoogle}
\alias{getWeatherFromGoogle}
\title{Retrieve weather forecast for a given location using Google Weather API}
\usage{
  getWeatherFromGoogle(location_id = "Trieste",
    language = "en", message = FALSE)
}
\arguments{
  \item{location_id}{A character containing a zip code
  (10001); city name, state (weather=woodland,PA); city
  name, country (weather=london, england);
  latitude/longitude(weather=,,,30670000,104019996) or
  possibly other.}

  \item{language}{A character containing the ISO 639
  2-letters code for the selected language (default 'en')}

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
\dontrun{
library("RWeather")
getWeatherFromGoogle(location_id="Basovizza", message=TRUE)
}
}

