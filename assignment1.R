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

csv.load.weather <- function(csvfile="RainWindPJSubangKLIA.csv", station="PJ"){
  x <- read.csv(csvfile) #   x <- read.csv("RainWind.csv")
  stnno <- list(PJ=48648, Subang=48647, KLIA=48650)
  # filter by stnno
  x <- x[x$Stnno == stnno[[station]],]
  # filter by year   
  x <- x[x$Year==2014|x$Year==2015,]
  x$Date <- as.Date(paste(x$Year, x$Month, x$Day), format = "%Y %m %d")
  x$Week <- as.numeric(format(x$Date,"%W"))+1
  # filter between 20140901-20150430
  x[x$Date >= as.Date("2014-09-01") & x$Date <= as.Date("2015-04-30"),]
}
csv.weather <- csv.load.weather()

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
summary(csv.weather)
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
csv.hotspot$Total_Kes <- (function(){
  x <- as.numeric(csv.hotspot$Total_Kes)
  x <- ifelse(is.na(x), yes = 0, no = x)
})()
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
Wind_mean_weekly <- aggregate(csv.weather$Wind_mean_24Hr, listyearweek, mean, na.rm=T, simplify = T)
Rainfall_mm_mean_weekly <-  aggregate(csv.weather$Rainfall_mm.fix, listyearweek, mean, na.rm=T)
Humidity_mean_weekly <- aggregate(csv.weather$Humidity.fix, listyearweek, mean, na.rm=T)
Solar_Radiation_Mjm2_mean_weekly <-  aggregate(csv.weather$Solar_Radiation_Mjm2.fix, listyearweek, mean, na.rm=T)

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

csv.denguedata.2014_2015 <- Reduce(
  function(x,y) {
    merge(x,y, by = c("Year","Week"))
  },
  (list(csv.hotspot, Wind_mean_weekly, Rainfall_mm_mean_weekly, Humidity_mean_weekly, Solar_Radiation_Mjm2_mean_weekly))
)

# open mukim selangor pop csv file
selangorpop <- (function(){
  x<-read.csv("mukim_selangor_poptot.filtered.csv", header = F)
  names(x) <- c("Area", "Area_Population")  
  x <- x[x$Area_Population!="",]
  isMukim <- grepl("[a-z]", x$Area, ignore.case = F)
  x$Area <- str_trim(x$Area)
  x <- x[!isMukim,]
  x <- x[x$Area!="JUMLAH",]
  
  x$Area_Population <-apply(x, MARGIN = 1, function(y){
    as.numeric(gsub(y["Area_Population"], pattern = "[ ,]", replacement = ""))
  })
  
  x$Area <- sapply(x$Area, function(y){  
    y<-gsub("ULU ","HULU ",y)
    y<-gsub(" ", "_", str_to_title(y))
  })
  x 
})()

csv.denguedata.2014_2015_pop <- Reduce(
  function(x,y) {
    merge(x,y, by = "Area")
  },
  (list(csv.denguedata.2014_2015, selangorpop))
)

#write dengue_data_2014-2015.csv
write.csv(csv.denguedata.2014_2015_pop, "dengue_data_2014-2015.csv")

names(csv.denguedata.2014_2015_pop)
outbreak_by_area_weekly <- (function(){
  x <-aggregate(csv.denguedata.2014_2015_pop$Total_Kes, 
                by=list(Year=csv.denguedata.2014_2015_pop$Year, Week=csv.denguedata.2014_2015_pop$Week, 
                Area = csv.denguedata.2014_2015_pop$Area), FUN="sum", na.rm=TRUE)
  names(x)[4] <- "Total.Area.Case"
  x
})()


#ADMINISTRATION

# Create a new column in dengue_data_2014_2015.csv to include population information.

library(data.table)



area_pop <- unique(data.table(csv.denguedata.2014_2015_pop)[,Area,Area_Population][order(-Area_Population)])




outbreak_by_area_weekly.dt <- data.table(outbreak_by_area_weekly)[,Year.Week:=paste(Year,Week) ]

outbreak_by_area_weekly.dt.with.casepopratio <- (function(){
  setkey(area_pop, Area)
  setkey(outbreak_by_area_weekly.dt, Area)
  x <- merge(area_pop,outbreak_by_area_weekly.dt)
  x[,Case.Population.Ratio:=Total.Area.Case/Area_Population*100]
  x[order(-Case.Population.Ratio)]
})()

# LIBRARY loading
# ggplot library
library(ggplot2)

# ggplot(data=outbreak_by_area_weekly.dt, aes(x=Year.Week, y=Total.Area.Case, fill=Area)) +
#   geom_bar(stat="identity", position=position_dodge()) +
#   coord_flip()


for(yw in outbreak_by_area_weekly.dt[,Year.Week]){
  xxx <- outbreak_by_area_weekly.dt[Year.Week==yw]  
  ggplot(data=xxx, aes(x=Area, y=Total.Area.Case)) +
    geom_bar(stat="identity", position=position_dodge())
}
  ggplot(data=outbreak_by_area_weekly.dt, aes(x=Year.Week, y=Total.Area.Case, fill=Area)) +
    geom_bar(stat="identity", position=position_dodge()) +
    coord_flip()

ggplot(data=outbreak_by_area_weekly.dt, aes(x=Year.Week, y=Total.Area.Case, fill=Area)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip()

ggplot(area_pop, aes(x=Area, y=Area_Population)) +
  geom_bar(stat="identity", position=position_dodge())

ggplot(data=outbreak_by_area_weekly.dt.with.casepopratio, aes(x=Year.Week, y=Case.Population.Ratio, fill=Area)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip()