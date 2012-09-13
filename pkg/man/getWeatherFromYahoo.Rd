\name{getWeatherFromYahoo}
\alias{getWeatherFromYahoo}
\title{Retrieve weather forecast for a given location using Yahoo API}
\usage{
  getWeatherFromYahoo(location_id = "10001",
    units = "metric", message = FALSE)
}
\arguments{
  \item{location_id}{A five digit US zip code or location
  ID. To find your location ID, browse or search for your
  city from the Weather home
  page(http://weather.yahoo.com/) The weather ID is in the
  URL for the forecast page for that city. You can also get
  the location ID by entering your zip code on the home
  page. For example, if you search for Los Angeles on the
  Weather home page, the forecast page for that city is
  http://weather.yahoo.com/forecast/USCA0638.html. The
  location ID is USCA0638.}

  \item{units}{A character either 'metric' (default) for
  metric units or '' for non metric units}

  \item{message}{If TRUE (default FALSE) the function
  returns a message summarizing the weather forecast for
  the location of interest}
}
\value{
  A list containing a list with the weather information for
  the given location and a data.frame with the forecasts
}
\description{
  This function will get the weather forecast conditions
  for a given location
}
\examples{
\dontrun{
library("RWeather")
getWeatherFromYahoo("10001")
}
}

