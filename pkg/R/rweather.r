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
# getWeatherFromGoogle <- function( location_id = "Trieste", language="en", message = FALSE ){
# 	# load the XML feeds for the Google Weather API
# 	weather.url = paste( "http://www.google.com/ig/api?weather=", URLencode(location_id), "&hl=", language, sep="" )
# 	xml = xmlTreeParse(weather.url, useInternalNodes=TRUE) # to get the xml data for the given location
# 	# basic error check to see if we can get the current weather condition for the given location.
# 	if( !is.null(xmlToList(xml)$weather$problem_cause) ) stop("Couldn't determine this location!\n")
# 	#
# 	forecast_information <- data.frame( 
# 		city=xpathSApply(xml,"//xml_api_reply/weather/forecast_information/city",xmlGetAttr,"data"),
# 		postal_code=xpathSApply(xml,"//xml_api_reply/weather/forecast_information/postal_code",xmlGetAttr,"data"),
# 		forecast_date=xpathSApply(xml,"//xml_api_reply/weather/forecast_information/forecast_date",xmlGetAttr,"data"),
# 		stringsAsFactors = FALSE 
# 	)
# 	current_conditions <- data.frame( 
# 		condition=xpathSApply(xml,"//xml_api_reply/weather/current_conditions/condition",xmlGetAttr,"data"),
# 		temp_f=xpathSApply(xml,"//xml_api_reply/weather/current_conditions/temp_f",xmlGetAttr,"data"),
# 		temp_c=xpathSApply(xml,"//xml_api_reply/weather/current_conditions/temp_c",xmlGetAttr,"data"),
# 		humidity=xpathSApply(xml,"//xml_api_reply/weather/current_conditions/humidity",xmlGetAttr,"data"),
# 		icon=paste("http://google.com", xpathSApply(xml,"//xml_api_reply/weather/current_conditions/icon",xmlGetAttr,"data"), sep=""),
# 		wind_condition=xpathSApply(xml,"//xml_api_reply/weather/current_conditions/wind_condition",xmlGetAttr,"data"),
# 		stringsAsFactors = FALSE
# 	)
# 	forecast_conditions <- data.frame( 
# 		day_of_week=xpathSApply(xml,"//xml_api_reply/weather/forecast_conditions/day_of_week",xmlGetAttr,"data"),
# 		low=xpathSApply(xml,"//xml_api_reply/weather/forecast_conditions/low",xmlGetAttr,"data"),
# 		high=xpathSApply(xml,"//xml_api_reply/weather/forecast_conditions/high",xmlGetAttr,"data"),
# 		icon=paste("http://google.com", xpathSApply(xml,"//xml_api_reply/weather/forecast_conditions/icon",xmlGetAttr,"data"), sep=""),
# 		condition=xpathSApply(xml,"//xml_api_reply/weather/forecast_conditions/condition",xmlGetAttr,"data"),
# 		stringsAsFactors = FALSE
# 	)
# 	output <- list(
# 		forecast_information=forecast_information,
# 		current_conditions=current_conditions,
# 		forecast_conditions=forecast_conditions
# 	)
# 	# message
# 	# is the current weather the same as the forecast? if not display the forecast
# 	if(identical(current_conditions$condition,forecast_conditions$condition[1])) forecast.msg =""
# 	else forecast.msg = paste("But the forecast says ", tolower(forecast_conditions$condition[1]), ". ", sep="")
# 	# if the temp in degrees c is below 20 i.e. cold
# 	if(as.numeric(current_conditions$temp_c) < 20) tmp.msg ="If you're going outside i'd wrap up warm. "
# 	# if the temp in degrees c is over 21 i.e. Warm / Hot
# 	else if(as.numeric(current_conditions$temp_c) >= 20) tmp.msg ="You should be ok without warm clothes today. "
# 	# check to see if there is rain or storms forecast
# 	if( length(grep("rain",current_conditions$icon[1], value=F))|
# 	    length(grep("storm",current_conditions$icon[1], value=F))|
# 	    length(grep("rain",forecast_conditions$icon[1], value=F))|
# 	    length(grep("storm",forecast_conditions$icon[1], value=F))) {
# 		storm.msg = "But don't forget to take an umbrella!"
# 	} else storm.msg =""
# 	info.msg <- paste( "Weather summary for ", forecast_information$city, ":\n",
# 		"The weather in ", forecast_information$city, " is ", tolower(current_conditions$condition) , ". ",
# 		forecast.msg, "The temperature is currently ", current_conditions$temp_c, "\u00B0C (",
# 		current_conditions$temp_f,"\u00B0F).\n", tmp.msg, current_conditions$humidity, ". ",storm.msg, "\n", sep="" 
# 	)	
# 	if (message) {
# 		cat(info.msg)
# 		return( invisible(output) )
# 	} else return( output )
# }
#
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
#' getCountriesFromGoogle()
#' getCountriesFromGoogle(country="JP")
#'
# getCountriesFromGoogle <- function( country=NULL ){
# 	if(is.null(country)){
# 		# weather.url = paste( "http://www.google.com/ig/countries?output=xml&hl=", language, sep="" )
# 		weather.url = "http://www.google.com/ig/countries?output=xml"
# 		xml = xmlTreeParse(weather.url, useInternalNodes=TRUE)
# 		countries <- data.frame( 
# 			name=xpathSApply(xml,"//xml_api_reply/countries/country/name",xmlGetAttr,"data"),
# 			iso_code=xpathSApply(xml,"//xml_api_reply/countries/country/iso_code",xmlGetAttr,"data"),
# 			stringsAsFactors = FALSE
# 		)
# 		output <- countries
# 	} else {
# 		# weather.url= paste( "http://www.google.com/ig/cities?output=xml&country=", country, "&hl=", language, sep="" )
# 		weather.url = paste( "http://www.google.com/ig/cities?output=xml&country=", country, sep="" )
# 		xml = xmlTreeParse(weather.url, useInternalNodes=TRUE)
# 		cities <- data.frame( 
# 			name=xpathSApply(xml,"//xml_api_reply/cities/city/name",xmlGetAttr,"data"),
# 			latitude_e6=xpathSApply(xml,"//xml_api_reply/cities/city/latitude_e6",xmlGetAttr,"data"),
# 			longitude_e6=xpathSApply(xml,"//xml_api_reply/cities/city/longitude_e6",xmlGetAttr,"data"),
# 			stringsAsFactors = FALSE
# 		)
# 		output <- cities
# 	}
# 	return(output)
# }
#------------------------------------------------------------------------------
#'
#' Retrieve weather forecast for a given location using Yahoo API
#'
#' This function will get the weather forecast conditions for a given location
#'
#' @param location_id A five digit US zip code or location ID. To find your location ID, 
#' browse or search for your city from the Weather home page(http://weather.yahoo.com/)
#' The weather ID is in the URL for the forecast page for that city. You can also get 
#' the location ID by entering your zip code on the home page. For example, 
#' if you search for Los Angeles on the Weather home page, the forecast page for that city is
#' http://weather.yahoo.com/forecast/USCA0638.html. The location ID is USCA0638.
#'
#' @param units A character either 'metric' (default) for metric units or '' for non metric units
#'
#' @param message If TRUE (default FALSE) the function returns a message summarizing the weather forecast for the location of interest
#'
#' @return A list containing a list with the weather information for the given location and 
#' a data.frame with the forecasts
#'
#' @export
#'
#' @examples
#'
#' library("RWeather")
#' getWeatherFromYahoo("10001")
#'
getWeatherFromYahoo <- function( location_id ="10001" , units = "metric", message = FALSE ){
	# load the RSS feeds for the Yahoo API
	weather.url = paste( "http://xml.weather.yahoo.com/forecastrss?p=", URLencode(location_id), "&u=", units, sep="" )
	xml = xmlTreeParse(weather.url, useInternalNodes=TRUE) # to get the xml data for the given location
	current_condition <- list( 
		location = data.frame(
			city=xpathSApply(xml,"//channel/yweather:location ", xmlGetAttr, "city"),
			region=xpathSApply(xml,"//channel/yweather:location ", xmlGetAttr, "region"),
			country=xpathSApply(xml,"//channel/yweather:location ", xmlGetAttr, "country"),
			stringsAsFactors = FALSE
		),
		units = data.frame(
			temperature=xpathSApply(xml,"//channel/yweather:units ", xmlGetAttr, "temperature"),
			distance=xpathSApply(xml,"//channel/yweather:units ", xmlGetAttr, "distance"),
			pressure=xpathSApply(xml,"//channel/yweather:units ", xmlGetAttr, "pressure"),
			speed=xpathSApply(xml,"//channel/yweather:units ", xmlGetAttr, "speed"),
			stringsAsFactors = FALSE
		),
		wind = data.frame(
			chill=xpathSApply(xml,"//channel/yweather:wind ", xmlGetAttr, "chill"),
			direction=xpathSApply(xml,"//channel/yweather:wind ", xmlGetAttr, "direction"),
			speed=xpathSApply(xml,"//channel/yweather:wind ", xmlGetAttr, "speed"),
			stringsAsFactors = FALSE
		),
		atmosphere = data.frame(
			humidity=xpathSApply(xml,"//channel/yweather:atmosphere ", xmlGetAttr, "humidity"),
			visibility=xpathSApply(xml,"//channel/yweather:atmosphere ", xmlGetAttr, "visibility"),
			pressure=xpathSApply(xml,"//channel/yweather:atmosphere ", xmlGetAttr, "pressure"),
			rising=xpathSApply(xml,"//channel/yweather:atmosphere ", xmlGetAttr, "rising"),
			stringsAsFactors = FALSE
		),
		astronomy = data.frame(
			sunrise=xpathSApply(xml,"//channel/yweather:astronomy ", xmlGetAttr, "sunrise"),
			sunset=xpathSApply(xml,"//channel/yweather:astronomy ", xmlGetAttr, "sunset"),
			stringsAsFactors = FALSE
		),
		condition = data.frame(
			text=xpathSApply(xml,"//channel/item/yweather:condition  ", xmlGetAttr, "text"),
			code=xpathSApply(xml,"//channel/item/yweather:condition  ", xmlGetAttr, "code"),
			temp=xpathSApply(xml,"//channel/item/yweather:condition  ", xmlGetAttr, "temp"),
			date=xpathSApply(xml,"//channel/item/yweather:condition  ", xmlGetAttr, "date"),
			stringsAsFactors = FALSE
		)
	)
	forecasts<- data.frame( 
		day=xpathSApply(xml,"//channel/item/yweather:forecast ", xmlGetAttr, "day"),
		date=xpathSApply(xml,"//channel/item/yweather:forecast ", xmlGetAttr, "date"),
		low=xpathSApply(xml,"//channel/item/yweather:forecast ", xmlGetAttr, "low"),
		high=xpathSApply(xml,"//channel/item/yweather:forecast ", xmlGetAttr, "high"),
		condition=xpathSApply(xml,"//channel/item/yweather:forecast ", xmlGetAttr, "text"),
		stringsAsFactors = FALSE
	)
	output <- list(current_condition=current_condition, forecasts=forecasts)
	# message
	# if the temp in degrees c is below 20 i.e. cold
	if(as.numeric(current_condition$condition$temp) < 68) tmp.msg ="If you're going outside i'd wrap up warm. "
	# if the temp in degrees c is over 21 i.e. Warm / Hot
	else if(as.numeric(current_condition$condition$temp) >= 68) tmp.msg ="You should be ok without warm clothes today. "
	# check to see if there is rain or storms forecast
	if( length(grep("rain", tolower(current_condition$condition$text), value=F))|
	    length(grep("storm", tolower(current_condition$condition$text), value=F)) ){
		storm.msg = "But don't forget to take an umbrella!\n"
	} else storm.msg ="\n"
	info.msg <- paste( "Weather summary for ", current_condition$location$city, ":\n",
		"The weather in ", current_condition$location$city, " is ", tolower(current_condition$condition$text) , ". ",
		"The temperature is currently ", current_condition$condition$temp,"\u00B0F.\n", tmp.msg,". Humidity is ", 
		current_condition$atmosphere$humidity, "%. ", storm.msg, sep="" 
	)
	if (message) {
		cat(info.msg)
		return( invisible( output ) )
	} else return( output )
}
#------------------------------------------------------------------------------
#'
#' Retrieve weather report from NOAA: National Oceanic and Atmospheric Administration (United States) 
#'
#' This function will get the weather forecast conditions for a given station
#'
#' @param station_id the ID of the weather station near the necessary location
#' To find your station ID, open http://www.weather.gov/xml/current_obs/seek.php?state=az&Find=Find
#' select the desired state and retrieve the Observation Location: 
#' the station_id is the string within the () parentheses.
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
		current_observation$temp_f,"\u00B0F).\n", tmp.msg, "Humidity: ",current_observation$humidity, "%. ", storm.msg, "\n", sep="" 
	)
	if (message) {
		cat(info.msg)
		return( invisible( current_observation ) )
	} else return( current_observation )
}

