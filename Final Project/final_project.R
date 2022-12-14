# final project - map of inflation across the U.S. States
library(tidyverse)
library(lubridate)

# import the data: it's all from FRED
# national inflation data
usdf <- read.csv("https://raw.githubusercontent.com/kyndall-brown/homeworkcode/main/inflationdata/national-inflation-data.csv")
# metropolitan statistical area personal income data
msadf <- read.csv("https://raw.githubusercontent.com/kyndall-brown/homeworkcode/main/inflationdata/per-capita-personal-income-msa.csv")
# state personal income data
statedf <- read.csv("https://raw.githubusercontent.com/kyndall-brown/homeworkcode/main/inflationdata/per-capita-personal-income-states.csv")

# data cleaning
msadf[msadf == "#DIV/0!"] <- NA
statedf[statedf == "#DIV/0!"] <- NA
dmy(usdf$observation_date)
ymd(statedf$observation_date)
# drop states observations with no matching national inflation #s
statedf <- statedf %>%
  filter(ymd(observation_date) >= dmy(usdf$observation_date[[1]]))

# create empty dataframe for the 50 states
statesmapdf <- data.frame(
  observation_date = character(),
  state_name = character(),
  personal_income = character(),
  personal_yoy = character()
)
# fill dataframe by iterating through list of states
for (i in 1:50) {
  tempdf <- data.frame (
    observation_date = statedf$observation_date,
    state_name = state.name[i],
    personal_income = statedf[i+1],
    personal_yoy = statedf[i+51]
  )
  # rename the columns to be the same as statesmapdf
  colnames(tempdf) <- c("observation_date", "state_name", "personal_income",
                        "personal_yoy")
  # bind them onto the bottom of statesmapdf
  statesmapdf <- rbind(statesmapdf, tempdf)
}

for (i in 1:50)