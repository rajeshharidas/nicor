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
  summarize(heatingtimes=sum(traitvalue == 'HEATING'),coolingtimes=sum(traitvalue == 'COOLING'))

nestreportdata <- nestreportdata %>% left_join(avgweather,by="date")

nestreportdata <- impute(data.frame(nestreportdata),flag=TRUE)

avgweather <- temperaturedata %>% group_by(date) %>% 
  summarize(avgtemp=mean(ftemperature),avghumidity=mean(humidity))

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