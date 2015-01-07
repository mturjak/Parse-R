##
##  parse.R
##
##  R interface for parse.com
##

##
# list of required packages
req.packages <- c(

	"utils", 	# URLencode()
	"RCurl",	# getURL()
	"jsonlite"	# toJSON() and fromJSON()

)

##
# check the required packages against installed packages and install if missing
# Cran mirror set to GWDG Goettingen - change to the one closest to you (list of mirrors: http://cran.r-project.org/mirrors.html)
new.packages <- req.packages[!(req.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://ftp5.gwdg.de/pub/misc/cran/")

##
# quietly load required packages
invisible(lapply(req.packages, library, ch=T))

##
# helper function for calling getURL{RCurl}
parseCurlHelper <- function(objectURI = "users", customrequest = "GET", httpheader = NULL, body = NULL, verbose = FALSE) {
	args <- list(
		paste(PARSE_API_URL,objectURI, sep=""),
		httpheader = c(
			"X-Parse-Application-Id" = APP_ID,
			"X-Parse-REST-API-Key" = REST_API_KEY,
			httpheader),
		customrequest = customrequest,
		verbose = verbose
	)
	if(!is.null(body)) {
		args <- c(args, postfields = body)
	}
	return(
		fromJSON(
			tryCatch(
				do.call(getURL, args),
				error = function(e) paste("{\"error\" : \"", gsub("([.])|[^[:alnum:] ]", "\\1", toString(e)), "\"}")
			)
		)
	)
}

uploadHelper <- function(fileName, verbose = FALSE) {
	args <- list(
		  uri = paste(PARSE_API_URL,"files/",fileName, sep=""),
			httpheader = c(
				"X-Parse-Application-Id" = APP_ID,
				"X-Parse-REST-API-Key" = REST_API_KEY),
			customrequest = "DELETE",
			verbose = verbose,
			postfields = data
	)
	return(
		fromJSON(
			tryCatch(
				do.call(getURL, args),
				error = function(e) paste("{\"error\" : \"", gsub("([.])|[^[:alnum:] ]", "\\1", toString(e)), "\"}")
			)
		)
	)
}

##
# initiation of the main parse.com object
# parse.com = list(

		##
		# required config constants (find them on https://parse.com/apps under your app's Settings > Keys)
		APP_ID = "YOUR_APPLICATION_ID_HERE"
		REST_API_KEY = "YOUR_REST_API_KEY"
		PARSE_API_URL = "https://api.parse.com/1/"

		##
		# when transforming dataframes to JSON you need to add a header name for column holding row names
		# (parse.com accepts only lower and upper case letters)
		ROW_NAMES_TO_JSON = "rowName"

		##
		# parse.com User class
		parseUser = list(

				# list all users
				list = parseCurlHelper,

				# get user with objectId
				get = function(uid) {
						return(
								parseCurlHelper(
										paste("users", uid, sep="/")
								)
						)
				}
		)
		#class(parseUser) = "ParseUser"

		##
		# parse.com general class
		parseObject = list(

				# list all objects of a class
				list = function(className) {
						return(
								parseCurlHelper(
										paste("classes", className, sep="/")
								)
						)
				},

				# get object by objectId
				get = function(className, objectId) {
						return(
								parseCurlHelper(
										paste("classes", className, objectId, sep="/")
								)
						)
				},

				# create new object
				post = function(className, body, method = "POST") {
						return(
								parseCurlHelper(
										paste("classes", className, sep="/"),
										method,
										httpheader = c(
												"Content-type" = "application/json; charset=utf-8" ),
										gsub("(\"_row\")", paste("\"", ROW_NAMES_TO_JSON, "\"", sep=""),
											gsub("^(\\[)|(\\])$","",toJSON(body))
										)
								)
						)
				},

				# update object
				put = function(className, objectId, body) {
						return(
								post(paste(className, objectId, sep="/"), body, "PUT")
						)
				}
		)
		#class(object) <- "ParseObject"
#)


################################################################

##
# test
parseUnitTest = list(
		parseUser = list(
				list = parseUser$list(),
				get = parseUser$get(list$results$objectId[1])
		),
		parseObject = list(
				post = parseObject$post("Car", mtcars[1,]),
				list = parseObject$list("Car"),
				get = parseObject$get("Car", list$results$objectId[1])
		)
)

##
# test on data.ftame
for(i in 1:nrow(mtcars))
{
		parseObject$post(
				"Car",
				mtcars[i,]
		)
}
