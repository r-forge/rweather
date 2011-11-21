#' Retrieve weather forecast for a given location using Google Weather API
#'
#' This function will get the weather forecast conditions for a given location
#'
#' @param address A character containing the address of the location of interest
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
#' rweather(address="Basovizza", message=TRUE)
#'
rweather <- function(address = "Trieste", language="en", message = FALSE){
	# load the XML feeds for the Google Weather API
	url = paste( "http://www.google.com/ig/api?weather=", URLencode(address), "&hl=", language, sep="" )
	xml = xmlTreeParse(url, useInternalNodes=TRUE) # to get the xml data for the given location
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
		"The weather in ", forecast_information$postal_code, " is ", tolower(current_conditions$condition) , ". ",
		forecast.msg, "The temperature is currently ", current_conditions$temp_c, "\u00B0C (",
		current_conditions$temp_f,"\u00B0F).\n", tmp.msg, current_conditions$humidity, ". ",storm.msg, "\n", sep="" 
	)	
	if (message) {
		cat(info.msg)
		return( invisible(output) )
	} else return( output )
}
