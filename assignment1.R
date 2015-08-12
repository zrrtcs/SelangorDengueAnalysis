# install.packages("stringr", dependencies=TRUE)
library(stringr)
setwd("/Users/pzagmatic/Zevelopment/SESAMasters/TMA7021 Data Mining/tutorial/Assignment1")

csv.hotspot.2014 <- (function(){
  x <- read.csv("LocationHotspot2014.csv")
  x[,"State"] <- toupper(x[,"State"])
  x[x$State=="SELANGOR",]
})()
csv.hotspot.2015 <- (function(){
  x <- read.csv("LocationHotspot2015.csv")
  x[,"State"] <- toupper(x[,"State"])
  x[x$State=="SELANGOR",]
})()

weekno.20140901 <- as.integer(format(as.Date("2014-09-01", format = "%Y-%m-%d"), "%U"))
weekno.20150430 <- as.integer(format(as.Date("2015-04-30", format = "%Y-%m-%d"), "%U"))

# combining the two csv files containing the dengue hotspot data
# from 1st September 2014 till 30th April 2015
csv.hotspot <- 
  rbind(csv.hotspot.2014[csv.hotspot.2014$Week >= weekno.20140901,], 
        csv.hotspot.2015[csv.hotspot.2015$Week <= weekno.20150430,])

csv.weather <- (function(){
  x <- read.csv("RainWindPJSubangKLIA.csv") #   x <- read.csv("RainWind.csv")
  stnno <- list(PJ=48648, Subang=48647, KLIA=48650)
  # filter by stnno
  x <- x[x$Stnno == stnno[["PJ"]],]
  # filter by year   
  x <- x[x$Year==2014|x$Year==2015,]
  x$Date <- as.Date(paste(x$Year, x$Month, x$Day), format = "%Y %m %d")
  x$Week <- as.numeric(format(x$Date,"%W"))+1
  # filter between 20140901-20150430
  x[x$Date >= as.Date("2014-09-01") & x$Date <= as.Date("2015-04-30"),]
})()

# csv.weather$Date <- as.Date(paste(csv.weather$Year, csv.weather$Month, csv.weather$Day), format = "%Y %m %d")
#http://unixhelp.ed.ac.uk/CGI/man-cgi?date
#%U -> week number of year, with Sunday as first day of week (00..53)
# csv.weather$Week <- ?as.numeric(format(csv.weather$Date,"%U"))+1

#[START]data cleanup
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
csv.hotspot$Location <- sapply(csv.hotspot$Location, function(x){
  x <- gsub(pattern = "\\s",replacement = "_", perl=T, x = x)
  x <- gsub(pattern = "[()\\[\\]{}]",replacement = "", perl=T, x = x)
  x <- gsub(pattern = "[-\\/\\\\&,]",replacement = ",", perl=T, x = x)
  as.factor(x)
})

csv.hotspot$Area <- sapply(csv.hotspot$Area, function(x){
#   unique(csv.hotspot$Area)
  x <- str_trim(x, side = "both")
  x <- gsub(pattern = "\\s",replacement = "_", perl=T, x = x)
  x <- ifelse(x == "Petalilng" || x == "Petalng", "Petaling", x)
  x <- ifelse(x == "K.Selangor", "Kuala Selangor", x)
  x <- ifelse(x == "K.Langat", "Kuala Langat", x)
  as.factor(x)
})

csv.hotspot$Area <- apply(csv.hotspot, 1, function(hotspot){
  x <- hotspot["Area"]
  x <- str_trim(x, side = "both")
  x <- gsub(pattern = "\\s",replacement = "_", perl=T, x = x)
  x <- ifelse(x == "Petalilng" || x == "Petalng", "Petaling", x)
  x <- ifelse(x == "K.Selangor", "KualaSelangor", x)
  x <- ifelse(x == "K.Langat", "KualaLangat", x)
  x <- if(x == "Selangor"){
    if(hotspot["Location"] %in% c("Seri_Pristana_Fasa_1", "Sri_Kembangan_7")) {
      "Petaling"
    }
    else if(hotspot["Location"] %in% c("Jalan_5,1,10_Taman_West_Country", "Appt_Taman_Anggerik_Villa_2")) { 
      "Hulu_Langat" 
    }
  } else{x}
  as.factor(x)
})
csv.hotspot<-csv.hotspot[!(csv.hotspot$Area %in% c("Barat_Daya", "K.Kangsar")),]
#[END]data cleanup

# library(dplyr)
# csv.weather.YW <- csv.weather %>% group_by(Year, Week)
# inner_join(
#   csv.weather.YW %>% summarise(Wind_mean_weekly = mean(Wind_mean_24Hr)),
#   csv.weather.YW %>% summarise(Rainfall_mm_mean_weekly = mean(Rainfall_mm.fix)),
#   csv.weather.YW %>% summarise(Humidity_mean_weekly = mean(Humidity.fix)),
#   csv.weather.YW %>% summarise(Solar_Radiation_Mjm2_mean_weekly = mean(Solar_Radiation_Mjm2.fix))
# )

listyearweek <- list(Year=csv.weather$Year, Week=csv.weather$Week)  
Wind_mean_weekly = aggregate(csv.weather$Wind_mean_24Hr, listyearweek, mean, na.rm=T, simplify = T)
Rainfall_mm_mean_weekly =  aggregate(csv.weather$Rainfall_mm.fix, listyearweek, mean, na.rm=T)
Humidity_mean_weekly = aggregate(csv.weather$Humidity.fix, listyearweek, mean, na.rm=T)
Solar_Radiation_Mjm2_mean_weekly =  aggregate(csv.weather$Solar_Radiation_Mjm2.fix, listyearweek, mean, na.rm=T)

names(Wind_mean_weekly)[3] = "Wind_mean_weekly"
names(Rainfall_mm_mean_weekly)[3]  = "Rainfall_mm_mean_weekly"
names(Humidity_mean_weekly)[3] = "Humidity_mean_weekly"
names(Solar_Radiation_Mjm2_mean_weekly)[3] = "Solar_Radiation_Mjm2_mean_weekly"
csv.denguedata.2014_2015 <- Reduce(
  function(x,y) {
    merge(x,y, by = c("Year","Week"))
  },
  (list(csv.hotspot, Wind_mean_weekly, Rainfall_mm_mean_weekly, Humidity_mean_weekly, Solar_Radiation_Mjm2_mean_weekly))
)

#write dengue_data_2014-2015.csv
write.csv(csv.denguedata.2014_2015, "dengue_data_2014-2015.csv")

View(mukimselangorpop)
mukimselangorpop <- (function(){
  x<-read.csv("mukim_selangor_pop01.csv", header = F)
  names(x) <- c("Area", "Population")
  x$Area 
  
  x <- x[x$Population!="",]
  x$Population <-apply(x, MARGIN = 1, function(y){
    as.numeric(gsub(y["Population"], pattern = "[ ,]", replacement = ""))
  })
  
})()
# open mukim selangor pop csv file


csv.denguedata.2014_2015_pop <- Reduce(
  function(x,y) {
    merge(x,y, by = c("Year","Week"))
  },
  (list(csv.hotspot, Wind_mean_weekly, Rainfall_mm_mean_weekly, Humidity_mean_weekly, Solar_Radiation_Mjm2_mean_weekly))
)

str(csv.denguedata.2014_2015_pop)
csv.denguedata.2014_2015_pop <- Reduce(
  function(x,y) {
    merge(x,y, by = c("Year","Week"))
  },
  (list(csv.hotspot, Wind_mean_weekly, Rainfall_mm_mean_weekly, Humidity_mean_weekly, Solar_Radiation_Mjm2_mean_weekly))
)

csv.denguedata.2014_2015_pop

#ADMINISTRATION

# Create a new column in dengue_data_2014_2015.csv to include population information.

# LIBRARY loading
# ggplot library
library(ggplot2)

ggplot()

# # R factor to numeric conversion
# # http://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-an-integer-numeric-without-a-loss-of-information
# as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
# 
# wc <- sapply(csv.weather, class)
# causes.factor <- function(x, x.name){
#   retval <- NA
#   if(x=="factor"){
#     x.isna <- is.na(as.numeric(as.character.factor((csv.weather[[x.name]]))))
#     retval <- as.character.factor(unique(csv.weather[[x.name]][x.isna]))
#   }
#   # list causing it to append with name back again     
#   list(retval)
# }
# 
# # mapply is used since sapply, lapply cannot pass names into function
# causes.vals<-mapply(causes.factor, x=wc, x.name=names(wc))
# 
