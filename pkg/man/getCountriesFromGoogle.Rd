\name{getCountriesFromGoogle}
\alias{getCountriesFromGoogle}
\title{Retrieve countries and cities lists using Google Weather APIs}
\usage{
  getCountriesFromGoogle(country = NULL)
}
\arguments{
  \item{country}{Either NULL or a character containing a
  2-letters code for the selected country (e.g. 'JP')}
}
\value{
  A data.frame containing either the names and iso_codes of
  the countries exposed by the APIs or the names, latitudes
  and longitudes of the cities of the selected country
}
\description{
  This function will get countries and cities lists from
  Google Weather API
}
\examples{
library("RWeather")
getCountriesFromGoogle()
getCountriesFromGoogle(country="JP")
}

