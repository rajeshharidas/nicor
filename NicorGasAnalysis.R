if (!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")

if (!require(caret))
  install.packages("caret", repos = "http://cran.us.r-project.org")

if (!require(data.table))
  install.packages("data.table", repos = "http://cran.us.r-project.org")

if (!require(dplyr))
  install.packages("dplyr", repos = "http://cran.us.r-project.org")

if (!require(gridExtra))
  install.packages("gridExtra", repos = "http://cran.us.r-project.org")

if (!require(kableExtra))
  install.packages("kableExtra", repos = "http://cran.us.r-project.org")

if (!require(lubridate))
  install.packages("lubridate", repos = "http://cran.us.r-project.org")

if (!require(epiDisplay))
  install.packages("epiDisplay")

if (!require(dygraphs))
  install.packages("dygraphs")

if (!require(imputeMissings))
  install.packages("imputeMissings")

if (!require(rcompanion))
  install.packages("rcompanion")

library(tidyverse)
library(caret)
library(data.table)
library(dplyr)
library(gridExtra)
library(kableExtra)
library(epiDisplay)

library(lubridate)
library(ggridges)
library(reshape2)

library(dygraphs)
library(xts)
library(htmlwidgets)
library(imputeMissings)
library(rcompanion)

#RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Download the temperature data from google nest
dl <- tempfile()

download.file("https://raw.githubusercontent.com/rajeshharidas/nicor/main/temperaturedata.csv",
              dl)

temperaturedata <- read_delim(
  dl,
  delim = ",",
  col_names = c(
    "timeofcapture",
    "humidity",
    "hvaccycleon",
    "mode",
    "month",
    "temperature",
    "timeoftarget"
  ),
  na = c("", "NA"),
  quoted_na = TRUE,
  comment = "",
  trim_ws = FALSE,
  skip = 1,
  progress = show_progress(),
  skip_empty_rows = TRUE
)

#Down load the real time nest event data for the periods (2018-2021)
dl1 <- tempfile()

download.file("https://raw.githubusercontent.com/rajeshharidas/nicor/main/nesteventdata.csv",
              dl1)

nestdata <- read_delim(
  dl1,
  delim = ",",
  col_names = c("eventid","timeofevent","traitkey","traitvalue"),
  na = c("", "NA"),
  quoted_na = TRUE,
  comment = "",
  trim_ws = FALSE,
  skip = 1,
  progress = show_progress(),
  skip_empty_rows = TRUE
)

#Download the nicor gas usage data from nicor bill
dl2 <- tempfile()

download.file("https://raw.githubusercontent.com/rajeshharidas/nicor/main/nicorgasdata.csv",
              dl2)

nicorgasusage <- read_delim(
  dl2,
  delim = ",",
  col_names = c("readingdate","ccfs","daysused","meterreading","readingtype"),
  na = c("", "NA"),
  quoted_na = TRUE,
  comment = "",
  trim_ws = FALSE,
  skip = 1,
  progress = show_progress(),
  skip_empty_rows = TRUE
)

#Download the nicor gas bill data from nicor bill
dl3 <- tempfile()

download.file("https://raw.githubusercontent.com/rajeshharidas/nicor/main/nicorinvdata.csv",
              dl3)

nicorinvdata <- read_delim(
  dl3,
  delim = ",",
  col_names = c("billdate","currentcharges","distcharge","naturalgascost"),
  na = c("", "NA"),
  quoted_na = TRUE,
  comment = "",
  trim_ws = FALSE,
  skip = 1,
  progress = show_progress(),
  skip_empty_rows = TRUE
)

#Download the electric reading data from city of naperville
dl4 <- tempfile()

download.file("https://raw.githubusercontent.com/rajeshharidas/nicor/main/electricdata.csv",
              dl4)

electricdata <- read_delim(
  dl4,
  delim = ",",
  col_names = c("readingdate","reading","readingbegin"),
  na = c("", "NA"),
  quoted_na = TRUE,
  comment = "",
  trim_ws = FALSE,
  skip = 1,
  progress = show_progress(),
  skip_empty_rows = TRUE
)

#Download the latest sensor data from google takeout. This has data from March 2021 - Dec 2021
dl5 <- tempfile()

download.file("https://raw.githubusercontent.com/rajeshharidas/nicor/main/SensorData.csv",
              dl5)

sensordata <- read_delim(
  dl5,
  delim = ",",
  col_names = c("date","time","humidity","temp"),
  na = c("", "NA"),
  quoted_na = TRUE,
  comment = "",
  trim_ws = FALSE,
  skip = 1,
  progress = show_progress(),
  skip_empty_rows = TRUE
)

#Download the weather data for 2018-2021
dl6 <- tempfile()

download.file("https://raw.githubusercontent.com/rajeshharidas/nicor/main/noaadata.csv",
              dl6)

noaadata <- read_delim(
  dl6,
  delim = ",",
  col_names = c("station","date","prcp","snow","snwd","tavg","tmax","tmin","tobs"),
  na = c("", "NA"),
  quoted_na = TRUE,
  comment = "",
  trim_ws = FALSE,
  skip = 1,
  progress = show_progress(),
  skip_empty_rows = TRUE
)

#Data after March 2021 - Does not include nest real-time heating/cooling events
sensordata <- sensordata %>% 
  mutate(timeofcapture=ymd_hms(str_c(date,time,sep=" ")),temperature=temp) %>% 
  filter (timeofcapture >= '2021-03-14 00:00:00') %>% 
  filter (!is.na(temperature) & !is.na(humidity)) %>%
  dplyr::select (-temp,-time) %>% 
  dplyr::select(timeofcapture,date,temperature,humidity) %>%
  arrange(timeofcapture)

#Remove NAs from temperature data. 
#get the date for comparisons
#select only 4 properties and arrange in ascending order of time-series
temperaturedata <- temperaturedata %>% 
  filter(!is.na(temperature) & !is.na(humidity)) %>%
  mutate (date = as.Date(timeofcapture)) %>%
  dplyr::select (timeofcapture,date,temperature,humidity) %>%
  arrange(timeofcapture)

#add Fahrenheit data as the original data is captured in Celsius
temperaturedata <- temperaturedata %>% mutate (ftemperature = (temperature * 1.8) + 32)
sensordata <- sensordata %>% mutate (ftemperature = (temperature * 1.8) + 32)
#add date to the nest event data for comparison
nestdata <- nestdata %>% mutate (date=as.Date(timeofevent))

#summarize the number of times the furnace or the A/C went 'ON' during the heating vs cooling periods
nestreportdata <- nestdata %>% group_by (date) %>% 
  summarize(ontimes=mean(traitvalue == 'ON'),coolingtimes=mean(traitvalue == 'COOLING'),heatingtimes=mean(traitvalue == 'HEATING'))

#summarize the average temps and humidity for the period for which we have nest real-time data
avgweather <- temperaturedata %>% group_by(date) %>% 
  summarize(avgtemp=mean(ftemperature),avghumidity=mean(humidity))

#join temp data with real-time event data keeping only rows for which we have real time events
nestreportdata <- nestreportdata %>% left_join(avgweather,by="date")

#impute missing values for the temp and humidity using median/mean
nestreportdata <- impute(data.frame(nestreportdata),flag=TRUE)

#convert the date format in the NOAA weather data
noaadata <- noaadata %>% mutate(date = mdy(date))

#calculate the mean temps for the each date and sort in ascending order of time-series
noaadata <- noaadata %>% group_by(date) %>% 
  summarize(avgtmax=mean(tmax),avgtmin=mean(tmin)) %>% 
  arrange(date)

#left join the weather center data with nest even data to get the on times, thermostat temps and weather temps outside
nestreportdata <- nestreportdata %>% left_join(noaadata,by="date")

#if the weather data is missing impute them with average temps. could introduce error
nestreportdata <- nestreportdata %>% 
  mutate(avgtmax = ifelse(is.na(avgtmax),avgtemp,avgtmax), avgtmin= ifelse(is.na(avgtmin),avgtemp,avgtmin))

#do a combined plot for the temps and humidity
 tempplot <- temperaturedata %>% ggplot(aes(timeofcapture, temperature,col='red')) +
      geom_line() + scale_y_continuous(trans = "log2") + scale_x_continuous()
 humplot <- temperaturedata %>% ggplot(aes(timeofcapture, humidity,col='blue')) +
       geom_line() + scale_y_continuous(trans = "log2") + scale_x_continuous()
 grid.arrange(tempplot, humplot,  nrow=2)
 
 #do a melt plot for the Celsius, Fahrenheit, and humidity
 df_melt <- melt(temperaturedata[, c("timeofcapture", "temperature","ftemperature", "humidity")], id="timeofcapture")  # melt by date. 
 gg <- ggplot(df_melt, aes(x=timeofcapture))  # setup
 gg + geom_line(aes(y=value, color=variable), size=1) + scale_color_discrete(name="Legend") 

 #plot a dynamic interactive graph for temps and humidity (change this for different plots) 
 df_xts <- xts(x = temperaturedata$humidity, order.by = temperaturedata$timeofcapture)
 dg <- dygraph(df_xts) %>%
       dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = TRUE, colors="red") %>%
       dyRangeSelector() %>%
       dyCrosshair(direction = "vertical") %>%
       dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
       dyRoller(rollPeriod = 1)
 dg
 
 #save the plot as html  
 saveWidget(dg, file=paste0( getwd(), "/dygraphshum.html"))
 
 #Objective of the below exercise is to predict the number of times the furnace or a/c turns on during winter vs other seasons

 #set seed so the results are repeatable 
 set.seed(1996,sample.kind="Rounding")
 
 #get winter specific data  from the nest data. For our exercise winter data is when heating and on times coincide.
 #some times heating is on during fall weather or early spring when temps are still low outside
 winterreport <- nestreportdata %>% filter(ontimes > 0 & heatingtimes > 0)
 
 #create a 70/30 train/test set split
 test_index <- createDataPartition(winterreport$ontimes, times = 1, p = 0.3, list = FALSE)
 
 train_set <- winterreport %>% slice(-test_index)
 test_set <- winterreport %>% slice(test_index)
 
 #perfom a linear regression predicting on times from thermostat data and weather data
 nestfitwinter <- glm(ontimes ~ avgtemp+avghumidity+avgtmax+avgtmin, data=train_set)
 #predict using the test set
 nestwinterpred <- predict(nestfitwinter,test_set)
 #calculate the RMSE for the winter prediction
 winterRMSE <- RMSE(test_set$ontimes,nestwinterpred)
 winterRMSE
 
 #Predict and plot the heating times against the outside weather and inside home nest settings
 model.1 <- glm(heatingtimes ~ avgtmax, data=winterreport, family="Gamma")
 model.2 <- glm(heatingtimes ~ avgtmin, data=winterreport, family="Gamma")
 model.3 <- glm(heatingtimes ~ avgtemp, data=winterreport, family="Gamma")
 model.4 <- glm(heatingtimes ~ avghumidity, data=winterreport, family="Gamma")
 winterAccuracy <- accuracy(list(model.1,model.2,model.3,model.4),plotit=TRUE, digits=3)
 winterAccuracy
 
 plotPredy(data  = winterreport,
           x     = avgtmax,
           y     = heatingtimes,
           model = model.1,
           xlab  = "avgtmax",
           ylab  = "heatingtimes")
 
 #set seed so the results are repeatable 
 set.seed(1996,sample.kind="Rounding")
 
 #get non-winter specific data  from the nest data. For our exercise non-winter data is when cooling and on times coincide.
 #some times cooling is on during fall weather when temps are still hot/humid outside - very low occurence
 summerreport <- nestreportdata %>% filter(ontimes > 0 & coolingtimes > 0)
 
 #create a 70/30 train/test set split
 test_index <- createDataPartition(summerreport$ontimes, times = 1, p = 0.3, list = FALSE)
 
 train_set <- summerreport %>% slice(-test_index)
 test_set <- summerreport %>% slice(test_index)
 
 #perfom a linear regression predicting on times from thermostat data and weather data
 nestfitsummer <- glm(ontimes ~ avgtemp+avghumidity+avgtmax+avgtmin, data=train_set)
 #predict using the test set
 nestsummerpred <- predict(nestfitsummer,test_set)
 #calculate the RMSE for the non-winter prediction
 summerRMSE <- RMSE(test_set$ontimes,nestsummerpred)
 summerRMSE
 
 #Predict and plot the on times against the outside weather and inside home nest settings
 model.1 <- glm(ontimes ~ avgtmax, data=summerreport, family="Gamma")
 model.2 <- glm(ontimes ~ avgtmin, data=summerreport, family="Gamma")
 model.3 <- glm(ontimes ~ avgtemp, data=summerreport, family="Gamma")
 model.4 <- glm(ontimes ~ avghumidity, data=summerreport, family="Gamma")
 summerAccuracy <- accuracy(list(model.1,model.2,model.3,model.4),plotit=TRUE, digits=3)
 summerAccuracy
 
 plotPredy(data  = summerreport,
           x     = avgtmax,
           y     = ontimes,
           model = model.1,
           xlab  = "avgtmax",
           ylab  = "ontimes")
 
 
 #The objective of the following exercise is to predict the optimal thermostat settings based on bill data during
 #winter and non-winter usage periods
 
 #make date a common field
 nicorgasusage <- nicorgasusage %>% mutate(date=readingdate)
 nicorinvdata <- nicorinvdata %>% mutate(date=billdate)
 
 #join invoice and usage data
 nicorbill <- nicorgasusage %>% left_join(nicorinvdata,by="date") %>% arrange(date)
 #bills are for the previous 28-32 days. Add a label monthyear for that
 nicorbill <- nicorbill %>% mutate(month=month(date),year=year(date),monthyear=ifelse(month == 1,str_c(12,year-1),str_c(month-1,year)))
 
 #join temperature data with weather data and take and average of the data by monthyear label
 usagewithweather <- temperaturedata %>% 
   left_join(noaadata,by="date") %>% 
   mutate(monthyear=str_c(month(date),year(date)),year=year(date),month=month(date)) %>% 
   group_by(monthyear) %>%
   summarize(myavgtemp=mean(ftemperature),myavghumidity=mean(humidity),myavgtmax=mean(avgtmax),myavgtmin=mean(avgtmin)) %>% 
   arrange(monthyear)
 
 #impute any missing weather data
 usagewithweather <- impute(data.frame(usagewithweather),flag=TRUE)
 
 #temperature data has only data till end of march
 nicorbillanalysis <- nicorbill %>% left_join(usagewithweather,by="monthyear") %>% filter(date < '2021-03-31')
 
 #create a 70/30 train/test set split
 test_index <- createDataPartition(nicorbillanalysis$myavgtemp, times = 1, p = 0.3, list = FALSE)
 
 train_set <- nicorbillanalysis %>% slice(-test_index)
 test_set <- nicorbillanalysis %>% slice(test_index)
 
 #run the glm model to compute the avg temp setting on thermostat for the weather
 estimatednestsetting <- glm(myavgtemp ~ myavgtmax+myavgtmin+myavghumidity+daysused+currentcharges+ccfs, data=train_set)
 #predict the avg temp setting using test set
 estimatednestsettingpred <- predict(estimatednestsetting,test_set)
 #compute the RMSE
 nestRMSE <- RMSE(test_set$myavgtemp,estimatednestsettingpred)
 nestRMSE
 
 #compute the models for different features
 n.model.1 <- glm(myavgtemp ~ myavgtmax, data=nicorbillanalysis, family="Gamma")
 n.model.2 <- glm(myavgtemp ~ myavgtmin, data=nicorbillanalysis, family="Gamma")
 n.model.3 <- glm(myavgtemp ~ myavghumidity, data=nicorbillanalysis, family="Gamma")
 n.model.4 <- glm(myavgtemp ~ daysused, data=nicorbillanalysis, family="Gamma")
 n.model.5 <- glm(myavgtemp ~ currentcharges, data=nicorbillanalysis, family="Gamma")
 n.model.6 <- glm(myavgtemp ~ ccfs, data=nicorbillanalysis, family="Gamma")
 #compare the accuracy and RMSE
 nestAccuracy <- accuracy(list(n.model.1,n.model.2,n.model.3,n.model.4,n.model.5,n.model.6),plotit=TRUE, digits=3)
 nestAccuracy
 
 #test the model with the new dataset for 03/2021 - 11/2021 data
 #join temperature data with weather data and take and average of the data by monthyear label
 usagewithweather2 <- sensordata %>%
   left_join(noaadata,by="date") %>% filter(date > '2021-03-15')  %>%
   mutate(monthyear=str_c(month(date),year(date)),year=year(date),month=month(date)) %>% 
   group_by(monthyear) %>%
   summarize(myavgtemp=mean(ftemperature),myavghumidity=mean(humidity),myavgtmax=mean(avgtmax),myavgtmin=mean(avgtmin)) %>% 
   arrange(monthyear)
 
 #impute any missing weather data
 usagewithweather2 <- impute(data.frame(usagewithweather2),flag=TRUE)
 
 #temperature data has only data till end of march
 nicorbillanalysis2 <- nicorbill %>% left_join(usagewithweather2,by="monthyear") %>% filter(date > '2021-03-31')
 
 estimatednestsettingpred2 <- predict(estimatednestsetting,nicorbillanalysis2)
 nestRMSE2 <- RMSE(nicorbillanalysis2$myavgtemp,estimatednestsettingpred2)
 nestRMSE2
 
#plot the predictions for the second set
 n.model.1 <- glm(myavgtemp ~ myavgtmax, data=nicorbillanalysis2, family="Gamma")
 n.model.2 <- glm(myavgtemp ~ myavgtmin, data=nicorbillanalysis2, family="Gamma")
 n.model.3 <- glm(myavgtemp ~ myavghumidity, data=nicorbillanalysis2, family="Gamma")
 n.model.4 <- glm(myavgtemp ~ daysused, data=nicorbillanalysis2, family="Gamma")
 n.model.5 <- glm(myavgtemp ~ currentcharges, data=nicorbillanalysis2, family="Gamma")
 #compare the accuracy and RMSE
 nestAccuracy2 <- accuracy(list(n.model.1,n.model.2,n.model.3,n.model.4,n.model.5),plotit=TRUE, digits=3)
 nestAccuracy2
 
 #Temperature diff test
 tempdiffanalysis <- nestreportdata %>% left_join(nicorbill,by="monthyear") 
 tempdiffanalysis <- tempdiffanalysis %>% filter(!is.na(readingdate))
 tempdiffanalysis <- tempdiffanalysis %>% mutate(tempdiff=avgtmax-avgtmin)
 tempdiffanalysis <- tempdiffanalysis %>% mutate(tempdiff = ifelse (tempdiff < 0,-tempdiff,tempdiff))
 
 tempdifffit <- glm(avgtemp ~ tempdiff,family="Gamma",data=tempdiffanalysis)
 accuracy(list(tempdifffit),plotit=TRUE, digits=3)