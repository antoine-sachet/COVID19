today <- Sys.Date()  
startDate <- as.Date("2020-01-22")
allDays <- as.numeric(today-startDate)

getTodaysColumn <- function() {
  which(
    grepl(gsub("^X0","X", format(today - 1, "X%m.%e.%y")), names(Cases_USA),1) |
      grepl(gsub("^X0","X", format(today - 1, "X%m.%e.%Y")), names(Cases_USA),1)
  )
}

if (!exists("Cases_USA") || length(getTodaysColumn()) == 0) {
  source("ImportCovidData.R")
  if (!exists("Cases_USA") || length(getTodaysColumn()) == 0) {
    stop("Couldn't import the data for today")
  }
}

# USA Files: Cases_USA, Deaths_USA, Population_USA
# Global Files: Cases_Global, Deaths_Global, Population_Global

Cases_USA  <- Cases_USA[ , seq(getTodaysColumn())]
Deaths_USA <- Deaths_USA[ , seq(getTodaysColumn())]

projection <- 7 # Project forward just 7 days
asymptomatic <- 10  # Number of asymptomatic patients per symptomatic patient
endDate <- startDate + allDays + projection - 1
Dates <- seq(from = startDate, to = endDate, by = 1)
extendedDates <- seq(from = startDate, to = startDate + allDays + 60, by = 1)