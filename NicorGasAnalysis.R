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


library(tidyverse)
library(caret)
library(data.table)
library(dplyr)
library(gridExtra)
library(kableExtra)
library(epiDisplay)

library(lubridate)

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
  dplyr::select (-temp,-date,-time) %>% 
  dplyr::select(timeofcapture,temperature,humidity) %>%
  arrange(timeofcapture)

temperaturedata <- temperaturedata %>% 
  filter(!is.na(temperature) & !is.na(humidity)) %>%
  dplyr::select (timeofcapture,temperature,humidity) %>%
  arrange(timeofcapture)