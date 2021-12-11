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

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

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

sensordata <- sensordata %>% 
  mutate(timeofcapture=ymd_hms(str_c(date,time,sep=" ")),temperature=temp) %>% 
  filter (timeofcapture >= '2021-03-14 00:00:00') %>% 
  filter (!is.na(temperature) & !is.na(humidity)) %>%
  dplyr::select (-temp,-time) %>% 
  dplyr::select(timeofcapture,date,temperature,humidity) %>%
  arrange(timeofcapture)

temperaturedata <- temperaturedata %>% 
  filter(!is.na(temperature) & !is.na(humidity)) %>%
  mutate (date = as.Date(timeofcapture)) %>%
  dplyr::select (timeofcapture,date,temperature,humidity) %>%
  arrange(timeofcapture)

temperaturedata <- temperaturedata %>% mutate (ftemperature = (temperature * 1.8) + 32)
sensordata <- sensordata %>% mutate (ftemperature = (temperature * 1.8) + 32)
nestdata <- nestdata %>% mutate (date=as.Date(timeofevent))

nestreportdata <- nestdata %>% group_by (date) %>% 
  summarize(ontimes=mean(traitvalue == 'ON'),coolingtimes=mean(traitvalue == 'COOLING'),heatingtimes=mean(traitvalue == 'HEATING'))

avgweather <- temperaturedata %>% group_by(date) %>% 
  summarize(avgtemp=mean(ftemperature),avghumidity=mean(humidity))

nestreportdata <- nestreportdata %>% left_join(avgweather,by="date")

nestreportdata <- impute(data.frame(nestreportdata),flag=TRUE)

noaadata <- noaadata %>% mutate(date = mdy(date))

noaadata <- noaadata %>% group_by(date) %>% 
  summarize(avgtmax=mean(tmax),avgtmin=mean(tmin)) %>% 
  arrange(date)

nestreportdata <- nestreportdata %>% left_join(noaadata,by="date")

nestreportdata <- nestreportdata %>% 
  mutate(avgtmax = ifelse(is.na(avgtmax),avgtemp,avgtmax), avgtmin= ifelse(is.na(avgtmin),avgtemp,avgtmin))

 tempplot <- temperaturedata %>% ggplot(aes(timeofcapture, temperature,col='red')) +
      geom_line() + scale_y_continuous(trans = "log2") + scale_x_continuous()
 humplot <- temperaturedata %>% ggplot(aes(timeofcapture, humidity,col='blue')) +
       geom_line() + scale_y_continuous(trans = "log2") + scale_x_continuous()
 grid.arrange(tempplot, humplot,  nrow=2)
 
 
 df_melt <- melt(temperaturedata[, c("timeofcapture", "temperature","ftemperature", "humidity")], id="timeofcapture")  # melt by date. 
 gg <- ggplot(df_melt, aes(x=timeofcapture))  # setup
 gg + geom_line(aes(y=value, color=variable), size=1) + scale_color_discrete(name="Legend") 

  
 df_xts <- xts(x = temperaturedata$humidity, order.by = temperaturedata$timeofcapture)
 dg <- dygraph(df_xts) %>%
       dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = TRUE, colors="red") %>%
       dyRangeSelector() %>%
       dyCrosshair(direction = "vertical") %>%
       dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
       dyRoller(rollPeriod = 1)
 dg
  
 saveWidget(dg, file=paste0( getwd(), "/dygraphshum.html"))
 
 set.seed(1996,sample.kind="Rounding")
 winterreport <- nestreportdata %>% filter(ontimes > 0 & heatingtimes > 0)
 
 test_index <- createDataPartition(winterreport$ontimes, times = 1, p = 0.3, list = FALSE)
 
 train_set <- winterreport %>% slice(-test_index)
 test_set <- winterreport %>% slice(test_index)
 
 nestfitwinter <- lm(ontimes ~ avgtemp+avghumidity+avgtmax+avgtmin, data=train_set)
 nestwinterpred <- predict(nestfitwinter,test_set)
 RMSE(test_set$ontimes,nestwinterpred)
 
 model.1 <- glm(heatingtimes ~ avgtmax, data=winterreport, family="Gamma")
 model.2 <- glm(heatingtimes ~ avgtmin, data=winterreport, family="Gamma")
 model.3 <- glm(heatingtimes ~ avgtemp, data=winterreport, family="Gamma")
 model.4 <- glm(heatingtimes ~ avghumidity, data=winterreport, family="Gamma")
 accuracy(list(model.1,model.2,model.3,model.4),plotit=TRUE, digits=3)
 
 plotPredy(data  = winterreport,
           x     = avgtmax,
           y     = heatingtimes,
           model = model.1,
           xlab  = "avgtmax",
           ylab  = "heatingtimes")
 
 
 set.seed(1996,sample.kind="Rounding")
 summerreport <- nestreportdata %>% filter(ontimes > 0 & coolingtimes > 0)
 
 test_index <- createDataPartition(summerreport$ontimes, times = 1, p = 0.3, list = FALSE)
 
 train_set <- summerreport %>% slice(-test_index)
 test_set <- summerreport %>% slice(test_index)
 
 nestfitsummer <- lm(ontimes ~ avgtemp+avghumidity+avgtmax+avgtmin, data=train_set)
 nestsummerpred <- predict(nestfitsummer,test_set)
 RMSE(test_set$ontimes,nestsummerpred)
 
 model.1 <- glm(ontimes ~ avgtmax, data=summerreport, family="Gamma")
 model.2 <- glm(ontimes ~ avgtmin, data=summerreport, family="Gamma")
 model.3 <- glm(ontimes ~ avgtemp, data=summerreport, family="Gamma")
 model.4 <- glm(ontimes ~ avghumidity, data=summerreport, family="Gamma")
 accuracy(list(model.1,model.2,model.3,model.4),plotit=TRUE, digits=3)
 
 plotPredy(data  = summerreport,
           x     = avgtmax,
           y     = ontimes,
           model = model.1,
           xlab  = "avgtmax",
           ylab  = "ontimes")
 