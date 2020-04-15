suppressPackageStartupMessages({
  library(ggplot2)
  library(openxlsx)
  library(officer)
  library(rvg)
  library(usmap)
  library(httr)
  library(cowplot)
  library(dplyr)
  library(bfw)
  library(rnaturalearth)
  library(sf)
})

source("model.R", local = TRUE)
source("plot.R", local = TRUE)

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
  source("ImportCovidData.R", local = TRUE)
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
bootstrapsN <- 100

timestamp <- format(Sys.time(), format = "%Y-%m-%d")
pptx <- read_pptx("Template.pptx")
master <- "Office Theme"
slideNumber <- 1
pptxfileName <- file.path("analyses", paste0("Steve's COVID Analysis.", timestamp, ".pptx"))
if (file.exists(pptxfileName)) {
  file.remove(pptxfileName)
}
while(file.exists(pptxfileName)) {
  message("Close the open PowerPoint File \"", pptxfileName, "\"\n")
  Sys.sleep(3)
  file.remove(pptxfileName)
}

# Disclaimer slide
DISCLAIMER <-
  c(
    "This is not confidential and can be freely shared. The code is available at https://github.com/StevenLShafer/COVID19/.",
    "",
    "This is my analysis, not Stanford's.",
    "",
    "Data sources:",
    "       USA Data:     https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv.",
    "       Global Data:  https://github.com/CSSEGISandData/COVID-19. This is the Johns Hopkins data repository.",
    "",
    "Nate Silver has an excellent write-up on the potential problems of the data (see https://fivethirtyeight.com/features/coronavirus-case-counts-are-meaningless/). He is right, of course, but these data are all we have. Also, he does not address the fact that the data are consistent with the expections. Specifically, the increases are initially log-linear, but then \"flatten\" as expected when public health policies are implemented. We would not see this if the data were just random noise.",
    "",
    "Models:",
    "       Primary (applied to the red dots on the graph): log(y) = intercept + (peak - intercept) * (1 - exp(k * time))",
    "       Log linear: log(Y) = intercept + slope * time, used to compute doubling time (0.693/slope).",
    "",
    "The number printed on the graph is the projection for a week from today. Doubling times are calculated for 5 day windows.",
    "",
    "The idiosyncratic locations are where Pamela and I have family or friends, or are locations requested by friends. I'm happy to add other regions. Also, I'm happy to add people to the blind CC distribution list. Just let me know.",
    "",
    "Please send any questions to steven.shafer@stanford.edu.",
    "",
    "Stay safe, well, and kind."
  )

pptx <- add_slide(pptx, layout = "Title and Text", master = master)
pptx <- ph_with(pptx, value = "Caveats and Comments",  location = ph_location_type("title") )
pptx <- ph_with(pptx, value = "", location = ph_location_type("body"))
pptx <- ph_add_text(pptx, str = paste(DISCLAIMER, collapse = "\n"), style = fp_text(font.size = 10))
pptx <- ph_with(pptx, value = slideNumber, location = ph_location_type("sldNum"))
slideNumber <- slideNumber + 1

# Add a slide to the powerpoint file and increment the slide number
nextSlide <- function (ggObject, Title) {
  suppressWarnings(
    print(ggObject)
  )
  
  pptx <- add_slide(pptx, layout = "Title and Content", master = master)
  pptx <- ph_with(pptx, value = Title, location = ph_location_type("title"))
  suppressWarnings(
    pptx <<- ph_with(pptx, value = dml(ggobj = ggObject), location = ph_location_type("body"))
  )
  pptx <<- ph_with(pptx, value = slideNumber, location = ph_location_type("sldNum"))
  slideNumber <<- slideNumber + 1
}

# US Data
Country <- "United States of America"
County <- NULL
State <- NULL
logStart <- "2020-02-29"
logEnd   <- "2020-03-22"
Title <- "USA"
weight <- 1
plotPred(Country = "United States of America", Title = "USA", logStart = "2020-02-28", logEnd = "2020-03-21")
plotPred(County = "New York County", Title = "New York City", logStart = "2020-03-02", logEnd = "2020-03-21", 
         weight = 2)
plotPred(State = "CA", Title = "California", logStart = "2020-03-02", logEnd = "2020-03-25", weight = 0)
plotPred(County = c("Santa Clara County", "San Mateo County"), Title = "Santa Clara and San Mateo", 
         logStart = "2020-03-02", logEnd = "2020-03-20", weight = 1)
plotPred(County = "San Francisco County", Title = "San Francisco", logStart = "2020-03-07", logEnd = "2020-03-27")
plotPred(County = "San Luis Obispo County", Title = "San Luis Obispo", logStart = "2020-03-16", logEnd = "2020-03-25")
plotPred(County = "King County", State = "WA", Title = "King County (Seattle)", logStart = "2020-02-29", logEnd = "2020-03-10", weight=0)
plotPred(County = "Los Angeles County", State = "CA", Title = "Los Angeles", logStart = "2020-03-04", 
         logEnd = "2020-03-27", weight = 1.5)
plotPred(County = "Multnomah County", Title = "Multnomah County (Portland)", 
         logStart = "2020-03-16", logEnd = "2020-03-31")
plotPred(County = "Westchester County", Title = "Westchester County", logStart = "2020-03-15", 
         logEnd = "2020-03-26", weight = 1)
plotPred(County = "Alameda County", Title = "Alameda County", logStart = "2020-03-05", logEnd = "2020-03-24")
plotPred(County = c("Santa Clara County", "San Mateo County", "San Francisco County", "Marin County", "Napa County", "Solano County", "Sonoma County"), 
         Title = "Bay Area", logStart = "2020-03-02", logEnd = "2020-03-23")
plotPred(County = "De Soto Parish", Title = "De Soto Parish, Louisiana", 
         logStart = "2020-03-22", logEnd = "2020-03-25")
plotPred(County = "Bergen County", Title = "Bergen County", logStart = "2020-03-14", 
         logEnd = "2020-03-29", weight = 1)
plotPred(State = "DC", Title = "Washington DC", logStart = "2020-03-14", 
         logEnd = "2020-04-03", weight = 1.5)
plotPred(County = "Dallas County", State = "TX", Title = "Dallas Texas", 
         logStart = "2020-03-10", logEnd = "2020-03-25")
plotPred(County = "Collin County", State = "TX", Title = "Collin Texas", 
         logStart = "2020-03-19", logEnd = "2020-03-26")
plotPred(County = "Harris County", State = "TX", Title = "Harris County, Texas", 
         logStart = "2020-03-20", logEnd = "2020-04-07",weight = 1)
plotPred(County = "McLean County", State = "IL", Title = "McLean County, Illinois", logStart = "2020-03-20",
         logEnd = "2020-04-02")
plotPred(County = "Cook County", State = "IL", Title = "Cook County, Illinois", logStart = "2020-03-06", logEnd = "2020-03-20")
plotPred(County = "Suffolk County", State = "MA", Title = "Suffolk County (Boston)", logStart = "2020-03-10", 
         logEnd = "2020-04-05", weight = 2)
plotPred(State = "UT", Title = "Utah (State)", logStart = "2020-03-02", logEnd = "2020-03-18")
plotPred(County = "Utah County", Title = "Utah County", logStart = "2020-03-02", 
         logEnd = "2020-04-02", weight=1)
plotPred(County = "Polk County", State = "IA", Title = "Polk County, Iowa", 
         logStart = "2020-03-02", logEnd = "2020-04-04")
plotPred(County = "Oakland County", State = "MI", Title = "Oakland County, Michigan", 
         logStart = "2020-03-02", logEnd = "2020-03-27", weight = 1)
plotPred(State = "HI", Title = "Hawaii", logStart = "2020-03-02", logEnd = "2020-03-22")
plotPred(County = "City of St. Louis", Title = "St. Louis (City)", logStart = "2020-03-02", logEnd = "2020-03-25")
plotPred(County = "St. Louis County", Title = "St. Louis (County)", 
         logStart = "2020-03-02", logEnd = "2020-03-31")
plotPred(County = "Baltimore City", Title = "Baltimore (City)", logStart = "2020-03-02", logEnd = "2020-03-25")
plotPred(County = "Durham County", Title = "Durham County", 
         logStart = "2020-03-17", logEnd = "2020-04-03")
plotPred(County="Miami-Dade County", Title = "Miami-Dade",
         logStart = "2020-03-02", logEnd="2020-03-28")
plotPred(State="FL", Title = "Florida",
         logStart = "2020-03-02",logEnd="2020-04-01")
plotPred(State="SD", Title = "South Dakota",
         logStart = "2020-03-02",logEnd="2020-04-02")
plotPred(State="MT", Title = "Montana",
         logStart = "2020-03-02",logEnd="2020-04-02", weight = 0)
plotPred(State="IA", Title = "Iowa",
         logStart = "2020-03-02",logEnd="2020-04-02", weight = 0)
# States with and without statewide public health restrictions
NoOrders <- c("ND", "SD", "NE", "WY","UT","OK","IA","AR")
plotPred(State=NoOrders, Title = "States without statewide orders",
         logStart = "2020-03-02",logEnd="2020-04-02", weight = 0)
plotPred(State=states$abbreviation[!states$abbreviation %in% NoOrders], 
         Title = "States with statewide orders",
         logStart = "2020-03-02",logEnd="2020-04-02", weight = 0)

# Last week doubling times by state
STATES <- data.frame(
  abbr = states$abbreviation,
  state = states$state,
  population = 0,
  slope = 0,
  intercept = 0,
  peak = 0,
  k = 0,
  stringsAsFactors = FALSE
)
last <- ncol(Cases_USA)
X <- 1:5
first <- last - 4

for (i in 1:nrow(STATES))
{
  STATES$population[i] <- sum(Population_USA$Population[Population_USA$State == STATES$state[i]], na.rm = TRUE)    
  Y <- log(
    colSums(Cases_USA[
      Cases_USA$State == STATES$abbr[i],
      first:last], na.rm=TRUE)
  )
  fit <- lm(Y ~ X)$coefficients
  STATES$intercept[i] <- fit[1]
  STATES$slope[i] <- fit[2]
  if (STATES$slope[i] < 0.01) STATES$slope[i] <- 0.01 
  fit <- covid_fit(Y, log(STATES$population[i] / (1 +  asymptomatic)))
  # Peak can't get higher than 70% of state population
  STATES$intercept[i] <- fit[1]
  STATES$peak[i] <- fit[2]
  STATES$k[i] <- fit[3]
}

STATES$dtime <- 0.693/STATES$slope
STATES$dtime[STATES$dtime > 25] <- 25
STATES$dtime[STATES$dtime < 2] <- 2
STATES$PredWeek  <- exp(STATES$intercept + (STATES$peak - STATES$intercept) * (1-exp(-STATES$k *  7))) / STATES$population * 100
STATES$Pred30Day <- exp(STATES$intercept + (STATES$peak - STATES$intercept) * (1-exp(-STATES$k * 30))) / STATES$population * 100

# USA Rate
USAPopulation <- sum(STATES$population)
Y <- log(colSums(Cases_USA[, first:last], na.rm=TRUE))
fit <- lm(Y ~ X)$coefficients
intercept <- fit[1]
slope <- fit[2]
USA <- 0.693/slope
USAFit <- covid_fit(Y, log(USAPopulation))
USAPredWeek   <- exp(USAFit[1] + (USAFit[2] - USAFit[1]) * (1-exp(-USAFit[3] *  7))) / USAPopulation * 100
USAPred30Day  <- exp(USAFit[1] + (USAFit[2] - USAFit[1]) * (1-exp(-USAFit[3] * 30))) / USAPopulation * 100

# By doubling time
STATES$state <- factor(STATES$state, levels = STATES$state[order(STATES$dtime)], ordered = TRUE)
Title <- paste("Five day estimation of doubling time", Sys.Date())
ggObject <- ggplot(STATES, aes(x=state, y=dtime)) +
  geom_col(fill = "brown", color="black", width=.5) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  labs(
    title = Title,
    y = "Doubling Time",
    x = "State"
  ) +
  annotate("segment",x = 0.5, xend = 51.5, y = USA, yend = USA) +
  annotate("text", label=paste("Overall US:", round(USA,1), "days"), x = 1, y=USA, hjust=0, vjust = -0.5)
nextSlide(ggObject, "Doubling Time")

ggObject <- plot_usmap(data = STATES, values = "dtime", color = "black") +
  scale_fill_continuous(
    low = "red", high = "white", name = "Doubling Time", label = scales::comma
  ) + theme(legend.position = "right") +
  labs(
    title = Title
  )
nextSlide(ggObject, "Doubling Time")

# Population Percent in five days
STATES$state <- factor(STATES$state, levels = STATES$state[order(STATES$PredWeek)], ordered = TRUE)
Title <- paste("Projected cases for", endDate)
ggObject <- ggplot(STATES, aes(x=state, y=PredWeek)) +
  geom_col(fill = "brown", color="black", width=.5) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  #  scale_y_continuous(breaks = c(0:5)) +
  labs(
    title = Title,
    y = "Percent population in 7 days",
    x = "State"
  ) +
  annotate("segment",x = 0.5, xend = 51.5, y = USAPredWeek, yend = USAPredWeek) +
  annotate("text", label=paste("Overall US:", round(USAPredWeek,1), "percent"), x = 1, y=USAPredWeek, hjust=0, vjust = -0.5)
nextSlide(ggObject, "All states in 7 days")

ggObject <- plot_usmap(data = STATES, values = "PredWeek", color = "black") +
  scale_fill_continuous(
    low = "white", high = "red", name = "Percent", label = scales::comma
  ) + theme(legend.position = "right") +
  labs(
    title = Title
  )
nextSlide(ggObject, "Percent population in 7 days")

# Population Percent in 30 days
STATES$state <- factor(STATES$state, levels = STATES$state[order(STATES$Pred30Day)], ordered = TRUE)
Title <- paste("Projection for", endDate)
ggObject <- ggplot(STATES, aes(x=state, y=Pred30Day)) +
  geom_col(fill = "brown", color="black", width=.5) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  #  scale_y_continuous(breaks = c(0:5)) +
  labs(
    title = Title,
    y = "Percent population in 30 days",
    x = "State"
  ) +
  annotate("segment",x = 0.5, xend = 51.5, y = USAPred30Day, yend = USAPred30Day) +
  annotate("text", label=paste("Overall US:", round(USAPred30Day,1), "percent"), x = 1, y=USAPred30Day, hjust=0, vjust = -0.5)
nextSlide(ggObject, "Percent population in 30 days")

ggObject <- plot_usmap(data = STATES, values = "Pred30Day", color = "black") +
  scale_fill_continuous(
    low = "white", high = "red", name = "Percent", label = scales::comma
  ) + theme(legend.position = "right") +
  labs(
    title = Title
  )
nextSlide(ggObject, "Percent population in 30 days")

#USA Map of Cases by County
temp <- Counties
CROWS <- match(temp$FIPS, Cases_USA$CountyFIPS)
temp$Cases <- log(Cases_USA[CROWS,ncol(Cases_USA)])
temp$Cases[is.na(temp$Cases)] <- 0
Title <- paste("Distribution of Reported Cases as of", today)
ggObject <- plot_usmap(regions = "counties") +
  geom_point(data = temp, aes(x = LONGITUDE.1, y=LATITUDE.1, size = Cases, color = Cases, alpha = Cases)) +
  scale_color_gradient(low="white",high="red") +
  scale_size(range = c(0, 4)) + 
  scale_alpha(range = c(0, 1)) + 
  theme(legend.position = "none") +
  labs(
    title = Title
  )
nextSlide(ggObject, "Current US Cases by County")

#USA Map of Deaths by County
temp <- Counties
CROWS <- match(temp$FIPS, Deaths_USA$CountyFIPS)
temp$Deaths <- log(Deaths_USA[CROWS,ncol(Deaths_USA)])
temp$Deaths[is.na(temp$Deaths)] <- 0
Title <- paste("Distribution of Deaths as of", today)
ggObject <- plot_usmap(regions = "counties") +
  geom_point(data = temp, aes(x = LONGITUDE.1, y=LATITUDE.1, size = Deaths, color = Deaths, alpha = Deaths)) +
  scale_color_gradient(low="white",high="red") +
  scale_size(range = c(0, 4)) + 
  scale_alpha(range = c(0, 1)) + 
  theme(legend.position = "none") +
  labs(
    title = Title
  )
nextSlide(ggObject, "Current US Deaths by County")




# Worldwide
plotPred(Country = "Italy", Title = "Italy", logStart = "2020-02-22", logEnd = "2020-03-13", weight = 0)
plotPred(Country = "Spain", Title = "Spain", logStart = "2020-02-25", 
         logEnd = "2020-03-21")
plotPred(Country = "France", Title = "France", logStart = "2020-02-27", 
         logEnd = "2020-04-04", weight = 0)
plotPred(Country = "Portugal", Title = "Portugal", 
         logStart = "2020-03-03", logEnd = "2020-03-31")
plotPred(Country = "Sweden", Title = "Sweden", logStart = "2020-02-28", 
         logEnd = "2020-03-15", weight=0)
plotPred(Country = "Netherlands", Title = "Netherlands", logStart = "2020-02-29", 
         logEnd = "2020-03-27")
plotPred(Country = "England", Title = "United Kingdom", 
         logStart = "2020-02-26", logEnd = "2020-03-31", weight = 2)
plotPred(Country = "South Africa", Title = "South Africa", logStart = "2020-03-08", logEnd = "2020-03-27", weight = 0)
plotPred(Country = "Brazil", Title = "Brazil", logStart = "2020-03-09", 
         logEnd = "2020-03-23", weight = 0)
plotPred(Country = "Paraguay", Title = "Paraguay", logStart = "2020-03-09", logEnd = "2020-03-22", weight = 0)
plotPred(Country = "Rwanda", Title = "Rwanda", 
         logStart = "2020-03-12", logEnd = "2020-03-26")
plotPred(Country = "Canada", Title = "Canada", logStart = "2020-03-10", logEnd = "2020-03-22")
plotPred(Country = "Australia", Title = "Australia", logStart = "2020-03-10", 
         logEnd = "2020-03-20", weight=0)
plotPred(Country = "Germany", Title = "Germany", logStart = "2020-02-26", 
         logEnd = "2020-03-26", weight=0)
plotPred(Country = "Switzerland", Title = "Switzerland", logStart = "2020-03-10", 
         logEnd = "2020-03-23", weight=1)
plotPred(Country = "Israel", Title = "Israel", logStart = "2020-02-27", logEnd = "2020-03-25", weight=0)
plotPred(Country = "Russia", Title = "Russia", logStart = "2020-03-11", logEnd = "2020-04-02", weight=1)
plotPred(Country = "India", Title = "India", logStart = "2020-03-06", logEnd = "2020-03-25", weight=0)
plotPred(Country = "Japan", Title = "Japan", logStart = "2020-02-20", 
         logEnd = "2020-04-02", weight=1)
plotPred(Country = "Mexico", Title = "Mexico", logStart = "2020-02-20", 
         logEnd = "2020-03-30", weight=0)

# World Map of Cases
CROWS <- match(worldmap$geounit, Cases_Global$Country)
worldmap$cases <- Cases_Global[CROWS, ncol(Cases_Global)]
worldmap$lcases <- log(worldmap$cases)
CROWS <- match(worldmap$geounit, Deaths_Global$Country)
worldmap$deaths <- Deaths_Global[CROWS, ncol(Deaths_Global)]
worldmap$ldeaths <- log(worldmap$deaths)

worldmap <- worldmap[!is.na(worldmap$lcases),]

ggObject <-   ggplot() + 
  geom_sf(data = worldmap, color="#00013E", aes(fill=lcases)) +
  scale_fill_gradient(low="#FFCFCF",high="#C00000") +
  labs(
    title = paste("Total Cases as of", Sys.Date())
  ) +
  theme(
    panel.background = element_rect(
      fill = "#00013E"
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  )
nextSlide(ggObject, "Worldwide Distribution of Cases")

ggObject <-   ggplot() + 
  geom_sf(data = worldmap, color="#00013E", aes(fill=ldeaths)) +
  scale_fill_gradient(low="#FFCFCF",high="#C00000") +
  labs(
    title = paste("Total Deaths as of", Sys.Date())
  ) +
  theme(
    panel.background = element_rect(
      fill = "#00013E"
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  )
nextSlide(ggObject, "Worldwide Distribution of Deaths")

# Doubling Time by Country
doubling_Global <- data.frame(
  Country = Cases_Global$Country,
  doublingTime = NA
)

cols <- (ncol(Cases_Global)-4):ncol(Cases_Global)
X <- 1:5
for (i in 1:nrow(doubling_Global))
{
  if (Cases_Global[i,cols[1]] > 0)
  {
    Y <- unlist(log(Cases_Global[i,cols]))
    doubling_Global$doublingTime[i] <- 0.693 / lm(Y ~ X)$coefficients[2]
  }
}
doubling_Global <- doubling_Global[!is.na(doubling_Global$doublingTime),]
doubling_Global <- doubling_Global[!is.infinite(doubling_Global$doublingTime),]
doubling_Global$doublingTime[doubling_Global$doublingTime < 0] <- 0
doubling_Global$doublingTime[doubling_Global$doublingTime > 30] <- 30

CROWS <- match(worldmap$geounit, doubling_Global$Country)
worldmap$doublingTime <- doubling_Global$doublingTime[CROWS]

ggObject <-   ggplot() + 
  geom_sf(data = worldmap, color="#00013E", aes(fill=doublingTime)) +
  scale_fill_gradient(high="#FFCFCF",low="#C00000") +
  labs(
    title = paste("Doubling Time as of", Sys.Date())
  ) +
  theme(
    panel.background = element_rect(
      fill = "#00013E"
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #     legend.position = "none",
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  )
nextSlide(ggObject, "Worldwide Doubling Time")

doubling_Global$Country <- factor(doubling_Global$Country, levels = doubling_Global$Country[order(doubling_Global$doublingTime)], ordered = TRUE)
Title <- paste("Doubling Time over the last 5 days as of", Sys.Date())

ggObject <- ggplot(doubling_Global[doubling_Global$doublingTime <= 14,], aes(x=Country, y=doublingTime)) +
  geom_col(fill = "brown", color="black", width=.5) +
  coord_cartesian(
    ylim = c(0,14)) +
  scale_y_continuous(breaks = c(2,4,5,8,10,12,14)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, size = 8)) +
  labs(
    title = Title,
    y = "Doubling Time",
    x = "Country"
  )
nextSlide(ggObject, "Doubling Time (last 5 days)")

ggObject <- ggplot(doubling_Global[doubling_Global$doublingTime > 14,], aes(x=Country, y=doublingTime)) +
  geom_col(fill = "brown", color="black", width=.5) +
  coord_cartesian(
    ylim = c(14,28)) +
  scale_y_continuous(breaks = c(14, 16, 18, 20, 22, 24, 26, 28)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, size = 8)) +
  labs(
    title = Title,
    y = "Doubling Time",
    x = "Country"
  )
nextSlide(ggObject, "Doubling Time (last 5 days)")

print(pptx, target = pptxfileName)
