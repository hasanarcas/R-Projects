library("httr")
library("jsonlite")
api_key <- "QNiqK7NtFFA9SCe4O84PiAVTPg5SKTLxbs538Dzn"

HEAD <- get(paste("https://api.nasa.gov/planetary/apod/", api_key, sep=""))

