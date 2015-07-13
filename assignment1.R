csv.hotspot.2014 <- read.csv("LocationHotspot2014.csv")
csv.hotspot.2015 <- read.csv("LocationHotspot2015.csv")

csv.weather <- (function(){
  x <- read.csv("RainWind.csv")
  x[x$Year==2014|x$Year==2015,]
})()
csv.weather$Date <- as.Date(paste(csv.weather$Year, csv.weather$Month, csv.weather$Day), format = "%Y %m %d")
#http://unixhelp.ed.ac.uk/CGI/man-cgi?date
#%U -> week number of year, with Sunday as first day of week (00..53)
csv.weather$Week<-as.numeric(format(csv.weather$Date,"%U"))+1

# unique(csv.weather[as.numeric(csv.weather$Solar_Radiation_Mjm2)==NA,])
# csv.weather[csv.weather$Rainfall_mm=="Trace",]

# R factor to numeric conversion
# http://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-an-integer-numeric-without-a-loss-of-information
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

wc <- sapply(csv.weather, class)
causes.factor <- function(x, x.name){
  retval <- NA
  if(x=="factor"){
    x.isna <- is.na(as.numeric(as.character.factor((csv.weather[[x.name]]))))
    retval <- as.character.factor(unique(csv.weather[[x.name]][x.isna]))
  }
  # list causing it to append with name back again     
  list(retval)
}

# mapply is used since sapply, lapply cannot pass names into function
causes.vals<-mapply(causes.factor, x=wc, x.name=names(wc))

#data cleanup
csv.weather$Rainfall_mm.fix <- (function(){
  fix <- as.numeric(as.character.factor(csv.weather$Rainfall_mm))
  ifelse(test = (is.na(fix) | fix < 0), yes = 0, no = fix)
})()
csv.weather$Wind_mean_24Hr[csv.weather$Wind_mean_24Hr<0] <- 0
csv.weather$Humidity.fix <- (function(){
  fix <- as.numeric(as.character.factor(csv.weather$Humidity))
  ifelse(test = (is.na(fix)), yes = 0, no = fix)
})()
csv.weather$Solar_Radiation_Mjm2.fix <- (function(){
  fix <- as.numeric(as.character.factor(csv.weather$Solar_Radiation_Mjm2))
  ifelse(test = (is.na(fix) | fix < 0), yes = 0, no = fix)
})()