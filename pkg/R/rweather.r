#' Retrieve weather forecast for a given location using Google Weather API
#'
#' This function will get the weather forecast conditions for a given location
#'
#' @param location_id A character containing a zip code (10001); city name, state (weather=woodland,PA); 
#' city name, country (weather=london, england);  latitude/longitude(weather=,,,30670000,104019996) or possibly other.
#'
#' @param language A character containing the ISO 639 2-letters code for the selected language (default 'en')
#'
#' @param message If TRUE (default FALSE) the function returns a message summarizing the weather forecast for the location of interest
#'
#' @return A list containing: \itemize{
#' \item forecast_information for the given the given location
#' \item current_conditions for the given the given location
#' \item forecast conditions (3 days) for the given the given location
#' }
#'
#' @export
#'
#' @examples
#'
#' library("RWeather")
#' getWeatherFromGoogle(location_id="Basovizza", message=TRUE)
#'
getWeatherFromGoogle <- function( location_id = "Trieste", language="en", message = FALSE ){
	# load the XML feeds for the Google Weather API
	weather.url = paste( "http://www.google.com/ig/api?weather=", URLencode(location_id), "&hl=", language, sep="" )
	xml = xmlTreeParse(weather.url, useInternalNodes=TRUE) # to get the xml data for the given location
	# basic error check to see if we can get the current weather condition for the given location.
	if( !is.null(xmlToList(xml)$weather$problem_cause) ) stop("Couldn't determine this location!\n")
	#
	forecast_information <- data.frame( 
		city=xpathSApply(xml,"//xml_api_reply/weather/forecast_information/city",xmlGetAttr,"data"),
		postal_code=xpathSApply(xml,"//xml_api_reply/weather/forecast_information/postal_code",xmlGetAttr,"data"),
		forecast_date=xpathSApply(xml,"//xml_api_reply/weather/forecast_information/forecast_date",xmlGetAttr,"data"),
		stringsAsFactors = FALSE 
	)
	current_conditions <- data.frame( 
		condition=xpathSApply(xml,"//xml_api_reply/weather/current_conditions/condition",xmlGetAttr,"data"),
		temp_f=xpathSApply(xml,"//xml_api_reply/weather/current_conditions/temp_f",xmlGetAttr,"data"),
		temp_c=xpathSApply(xml,"//xml_api_reply/weather/current_conditions/temp_c",xmlGetAttr,"data"),
		humidity=xpathSApply(xml,"//xml_api_reply/weather/current_conditions/humidity",xmlGetAttr,"data"),
		icon=paste("http://google.com", xpathSApply(xml,"//xml_api_reply/weather/current_conditions/icon",xmlGetAttr,"data"), sep=""),
		wind_condition=xpathSApply(xml,"//xml_api_reply/weather/current_conditions/wind_condition",xmlGetAttr,"data"),
		stringsAsFactors = FALSE
	)
	forecast_conditions <- data.frame( 
		day_of_week=xpathSApply(xml,"//xml_api_reply/weather/forecast_conditions/day_of_week",xmlGetAttr,"data"),
		low=xpathSApply(xml,"//xml_api_reply/weather/forecast_conditions/low",xmlGetAttr,"data"),
		high=xpathSApply(xml,"//xml_api_reply/weather/forecast_conditions/high",xmlGetAttr,"data"),
		icon=paste("http://google.com", xpathSApply(xml,"//xml_api_reply/weather/forecast_conditions/icon",xmlGetAttr,"data"), sep=""),
		condition=xpathSApply(xml,"//xml_api_reply/weather/forecast_conditions/condition",xmlGetAttr,"data"),
		stringsAsFactors = FALSE
	)
	output <- list(
		forecast_information=forecast_information,
		current_conditions=current_conditions,
		forecast_conditions=forecast_conditions
	)
	# message
	# is the current weather the same as the forecast? if not display the forecast
	if(identical(current_conditions$condition,forecast_conditions$condition[1])) forecast.msg =""
	else forecast.msg = paste("But the forecast says ", tolower(forecast_conditions$condition[1]), ". ", sep="")
	# if the temp in degrees c is below 20 i.e. cold
	if(as.numeric(current_conditions$temp_c) < 20) tmp.msg ="If you're going outside i'd wrap up warm. "
	# if the temp in degrees c is over 21 i.e. Warm / Hot
	else if(as.numeric(current_conditions$temp_c) >= 20) tmp.msg ="You should be ok without warm clothes today. "
	# check to see if there is rain or storms forecast
	if( length(grep("rain",current_conditions$icon[1], value=F))|
	    length(grep("storm",current_conditions$icon[1], value=F))|
	    length(grep("rain",forecast_conditions$icon[1], value=F))|
	    length(grep("storm",forecast_conditions$icon[1], value=F))) {
		storm.msg = "But don't forget to take an umbrella!"
	} else storm.msg =""
	info.msg <- paste( "Weather summary for ", forecast_information$city, ":\n",
		"The weather in ", forecast_information$city, " is ", tolower(current_conditions$condition) , ". ",
		forecast.msg, "The temperature is currently ", current_conditions$temp_c, "\u00B0C (",
		current_conditions$temp_f,"\u00B0F).\n", tmp.msg, current_conditions$humidity, ". ",storm.msg, "\n", sep="" 
	)	
	if (message) {
		cat(info.msg)
		return( invisible(output) )
	} else return( output )
}

#' Retrieve countries and cities lists using Google Weather APIs
#'
#' This function will get countries and cities lists from Google Weather API
#'
#' @param country Either NULL or a character containing a 2-letters code for the selected country (e.g. 'JP')
#'
#' @return A data.frame containing either the names and iso_codes of the countries exposed by the APIs
#' or the names, latitudes and longitudes of the cities of the selected country
#'
#' @export
#'
#' @examples
#'
#' library("RWeather")
#' getCountries()
#' getCountries(country="JP")
#'
getCountriesFromGoogle <- function( country=NULL ){
	if(is.null(country)){
		# weather.url = paste( "http://www.google.com/ig/countries?output=xml&hl=", language, sep="" )
		weather.url = "http://www.google.com/ig/countries?output=xml"
		xml = xmlTreeParse(weather.url, useInternalNodes=TRUE)
		countries <- data.frame( 
			name=xpathSApply(xml,"//xml_api_reply/countries/country/name",xmlGetAttr,"data"),
			iso_code=xpathSApply(xml,"//xml_api_reply/countries/country/iso_code",xmlGetAttr,"data"),
			stringsAsFactors = FALSE
		)
		output <- countries
	} else {
		# weather.url= paste( "http://www.google.com/ig/cities?output=xml&country=", country, "&hl=", language, sep="" )
		weather.url = paste( "http://www.google.com/ig/cities?output=xml&country=", country, sep="" )
		xml = xmlTreeParse(weather.url, useInternalNodes=TRUE)
		cities <- data.frame( 
			name=xpathSApply(xml,"//xml_api_reply/cities/city/name",xmlGetAttr,"data"),
			latitude_e6=xpathSApply(xml,"//xml_api_reply/cities/city/latitude_e6",xmlGetAttr,"data"),
			longitude_e6=xpathSApply(xml,"//xml_api_reply/cities/city/longitude_e6",xmlGetAttr,"data"),
			stringsAsFactors = FALSE
		)
		output <- cities
	}
	return(output)
}
#------------------------------------------------------------------------------
#'
#' Retrieve weather forecast for a given location using Yahoo API
#'
#' This function will get the weather forecast conditions for a given location
#'
#' @param location_id 
#'
#' @param units A character either 'metric' (default) for metric units or '' for non metric units
#'
#' @param message If TRUE (default FALSE) the function returns a message summarizing the weather forecast for the location of interest
#'
#' @return A data.frame containing the weather information for the given location
#'
#' @export
#'
#' @examples
#'
#' library("RWeather")
#' getWeatherFromYahoo("US")
#'
getWeatherFromYahoo <- function( location_id ="US" , units = "metric", message = FALSE ){
	# YAHOO_WEATHER_URL    = 'http://xml.weather.yahoo.com/forecastrss?p=%s&u=%s'
	# YAHOO_WEATHER_NS     = 'http://xml.weather.yahoo.com/ns/rss/1.0'
	# load the RSS feeds for the Yahoo API
	weather.url = paste( "http://xml.weather.yahoo.com/forecastrss?p=", URLencode(location_id), "&u=", units, sep="" )
	xml = xmlTreeParse(weather.url, useInternalNodes=TRUE) # to get the xml data for the given location
	channel <- data.frame( 
		location=xpathSApply(xml,"//channel/title", xmlValue),
		# latitude=xpathSApply(xml,"//current_observation/latitude", xmlValue),
		# longitude=xpathSApply(xml,"//current_observation/longitude", xmlValue),
		# observation_time=xpathSApply(xml,"//current_observation/observation_time", xmlValue),
		# condition=xpathSApply(xml,"//current_observation/weather", xmlValue),
		# temp_f=xpathSApply(xml,"//current_observation/temp_f", xmlValue),
		# temp_c=xpathSApply(xml,"//current_observation/temp_c", xmlValue),
		# humidity=xpathSApply(xml,"//current_observation/relative_humidity", xmlValue),
		# wind=xpathSApply(xml,"//current_observation/wind_string", xmlValue),
		# pressure=xpathSApply(xml,"//current_observation/pressure_string", xmlValue),
		# stringsAsFactors = FALSE 
	)
	return(channel)
}
#------------------------------------------------------------------------------
#'
#' Retrieve weather report from NOAA: National Oceanic and Atmospheric Administration (United States) 
#'
#' This function will get the weather forecast conditions for a given station
#'
#' @param station_id
#'
#' @param message If TRUE (default FALSE) the function returns a message summarizing the weather forecast for the location of interest
#'
#' @return A data.frame containing the weather information for the given station 
#'
#' @export
#'
#' @examples
#'
#' library("RWeather")
#' getWeatherFromNOAA(station_id="KJFK") # current observation for New York
#'
getWeatherFromNOAA <- function( station_id ="KJFK", message = FALSE ){
	# load the XML feeds for the NOAA
	weather.url = paste( "http://www.weather.gov/xml/current_obs/", URLencode(station_id), ".xml", sep="" )
	xml = xmlTreeParse(weather.url, useInternalNodes=TRUE) # to get the xml data for the given location
	current_observation <- data.frame( 
		location=xpathSApply(xml,"//current_observation/location", xmlValue),
		latitude=xpathSApply(xml,"//current_observation/latitude", xmlValue),
		longitude=xpathSApply(xml,"//current_observation/longitude", xmlValue),
		observation_time=xpathSApply(xml,"//current_observation/observation_time", xmlValue),
		condition=xpathSApply(xml,"//current_observation/weather", xmlValue),
		temp_f=xpathSApply(xml,"//current_observation/temp_f", xmlValue),
		temp_c=xpathSApply(xml,"//current_observation/temp_c", xmlValue),
		humidity=xpathSApply(xml,"//current_observation/relative_humidity", xmlValue),
		wind=xpathSApply(xml,"//current_observation/wind_string", xmlValue),
		pressure=xpathSApply(xml,"//current_observation/pressure_string", xmlValue),
		stringsAsFactors = FALSE 
	)
	# message
	# if the temp in degrees c is below 20 i.e. cold
	if(as.numeric(current_observation$temp_c) < 20) tmp.msg ="If you're going outside i'd wrap up warm. "
	# if the temp in degrees c is over 21 i.e. Warm / Hot
	else if(as.numeric(current_observation$temp_c) >= 20) tmp.msg ="You should be ok without warm clothes today. "
	# check to see if there is rain or storms forecast
	if( length(grep("rain", tolower(current_observation$condition), value=F))|
	    length(grep("storm", tolower(current_observation$condition), value=F)) ){
		storm.msg = "But don't forget to take an umbrella!"
	} else storm.msg =""
	info.msg <- paste( "Weather summary for ", current_observation$location, ":\n",
		"The weather in ", current_observation$location, " is ", tolower(current_observation$condition) , ". ",
		"The temperature is currently ", current_observation$temp_c, "\u00B0C (",
		current_observation$temp_f,"\u00B0F).\n", tmp.msg, current_observation$humidity, ". ", storm.msg, "\n", sep="" 
	)
	if (message) {
		cat(info.msg)
		return( invisible( current_observation ) )
	} else return( current_observation )
}

