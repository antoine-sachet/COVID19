library(ggplot2)
library(openxlsx)
library(officer)
library(rvg)
library(usmap)
remove(list=ls())

# Load Data
# Data source:

SOURCE <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"
allData <- read.csv(SOURCE)

# Check if updated
if (
  tail(names(allData),1) ==
  gsub(" ","", gsub("^X0","X", format(Sys.Date()-1, "X%m.%e.%y"))) |
  tail(names(allData),1) ==
  gsub(" ","", gsub("^X0","X", format(Sys.Date()-1, "X%m.%e.%Y")))
)
{

Projection <- 7 # Project forward just 7 days
allDays <- ncol(allData)-4
startDate <- as.Date("2020-01-22")
endDate <- startDate + allDays + Projection - 1
Dates <- seq(from = startDate, to=endDate, by = 1)

setwd("g:/projects/covid")
TIMESTAMP <- format(Sys.time(), format = "%Y-%m-%d")
DATE <- format(Sys.Date(), "%m/%d/%y")

PPTX <- read_pptx("Template.pptx")
MASTER <- "Office Theme"
SLIDE <- 1
pptxfileName <- paste0("Steve's COVID Analysis.", TIMESTAMP, ".pptx")

# Disclaimer slide
DISCLAIMER <-
  c(
    "1. These are not confidential. You can freely share this output freely. The code is available at https://github.com/StevenLShafer/COVID19/.",
    "",
    "2. This is my analysis, not Stanford's analysis. My understanding is that Stanford's internal analysis, done to plan resource allocation at Stanford, shows substantially longer doubling times. This is reassuring for those of us working at Stanford.",
    "",
    "3. The data are from usafacts.org. Here is the file: https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv. The data are collected from state and county public health departments.",
    "",
    "4.	The model for the recent data is an asymptotic rise to peak: exp(intercept + peak * (1-exp(-k * delta time))).",
    "",
    "5. The number printed on the graph is the projection for a week from today. Because that is only a week out, it is probably not far off.",
    "",
    "6. The \"peak\" in the caption is the estimated total peak at time = infinity from the model. I have very little confidence because it is a projection well into the future, and very sensitive to small errors in that data.",
    "",
    "7.	The idiosyncratic locations chosen for the analysis reflect where Pamela and I have friends and family. I'm happy to add other regions. Also, I'm happy to add people to the blind CC distribution list. Just let me know.",
    "",
    "8. I'm not sure why Georgia is missing from the USA maps at the end. Likely a problem with the R library being used. I'm looking into it.",
    "",
    "9. Send any questions to steven.shafer@stanford.edu."
  )

PPTX <- add_slide(PPTX, layout = "Title and Text", master = MASTER)
PPTX <- ph_with(PPTX, value = "Caveats and Comments",  location = ph_location_type("title") )
PPTX <- ph_with(PPTX, value = "", location = ph_location_type("body"))
PPTX <- ph_add_text(PPTX, str = paste(DISCLAIMER, collapse = "\n"), style = fp_text(font.size = 11))
PPTX <- ph_with(PPTX, value = SLIDE, location = ph_location_type("sldNum"))
SLIDE <- SLIDE + 1

nextSlide <- function (PLOT, Title)
  {
  suppressWarnings(
    print(PLOT)
  )

  PPTX <- add_slide(PPTX, layout = "Title and Content", master = MASTER)
  PPTX <- ph_with(PPTX, value = Title, location = ph_location_type("title"))
  suppressWarnings(
    PPTX <<- ph_with(PPTX, value = dml(ggobj = PLOT), location = ph_location_type("body"))
  )
  PPTX <<- ph_with(PPTX, value = SLIDE, location = ph_location_type("sldNum"))
  SLIDE <<- SLIDE + 1
}


plotPred <- function(Cases, Title, logStart, logEnd)
{
  DATA <- data.frame(
    Date = Dates,
    Actual = c(colSums(allData[Cases, c(5:ncol(allData))],na.rm=TRUE), rep(NA, Projection)),
    Phase = "",
    stringsAsFactors = FALSE
  )

  start <- which(DATA$Date == as.Date(logStart))
  end   <-  which(DATA$Date == as.Date(logEnd))
  DATA$Phase[1:(start-1)] <- "Pre log linear"
  DATA$Phase[start:(end-1)] <- "Log linear"
  DATA$Phase[end:nrow(DATA)] <- "Current"
  DATA$Predicted.LL <- DATA$Predicted.SD <- NA
  DATA$Actual[DATA$Actual == 0 | is.na(DATA$Actual)] <- NA

  # Log linear phase
  Y <- log(DATA$Actual[DATA$Phase == "Log linear"])
  X <- 1:length(Y)
  coefs <- lm(Y ~ X)$coefficients
  Intercept <- coefs[1]
  slope <- coefs[2]
  X <- start:nrow(DATA)
  DATA$Predicted.LL[X] <- exp(Intercept + 1:length(X) * slope)

  # Current
  use <- DATA$Date >= as.Date(logEnd) & !is.na(DATA$Actual)
  Y <- log(DATA$Actual[use])

  covid_fn <- function(par, Y)
  {
    # par <- intercept, peak, k
    return(par[1] + par[2] * (1-exp(-par[3] * seq(0, length.out=length(Y)))))
  }

  obj <- function(par, Y)
  {
    fit <- covid_fn(par, Y)
    return(sum((Y-fit)^2))
  }

  fit <- optim(
    c(Y[1], Y[1]+2, 0.5),
    obj,
    Y = Y,
    method = "L-BFGS-B",
    lower = c(Y[1]/2, 0.1, 0.05),
    upper = c(17, 20, 5)
  )$par

  X <- end:nrow(DATA)
  DATA$Predicted.SD[X] <- exp(covid_fn(fit, c(Y, rep(NA,7))))
  WeekPrediction <- tail(DATA$Predicted.SD, 1)

  caption <- paste0("Initial doubling: ", sprintf("%0.1f", 0.693/slope), " days, Predicted peak: ", prettyNum(round(exp(fit[1] + fit[2]) , 0),big.mark = ",", scientific = FALSE))

  DATA$Phase <- factor(as.character(DATA$Phase), levels=c("Pre log linear","Log linear","Current"), ordered = TRUE)
  PLOT <- ggplot(DATA, aes(x = Date, y = Actual, color = Phase)) +
    geom_point(size = 2) +
    coord_cartesian(ylim = c(1,10000000), expand = TRUE, clip = "on") +
    geom_line(size = 1, aes(y=Predicted.LL), color="blue") +
    geom_line(size = 1, aes(y=Predicted.SD), color="red") +
    labs(
      title = paste("Steve's", Title,"Projection as of", Sys.Date()),
      y = "Actual (points) / Predicted (line)",
      caption = caption
    ) +
    scale_color_manual(values = c("forestgreen","blue","red", "brown")) +
    scale_x_date(
      date_breaks = "7 days",
      date_labels = "%b %d"
    ) +
    scale_y_log10(
      breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000,10000000),
      labels = c("1", "10","100","1,000","10,000","100,000","1,000,000", "10,000,000")
    ) +
    theme(axis.text.x=element_text(angle=60, hjust=1))+
    annotation_logticks() +
    theme(
      panel.grid.minor = element_blank(),
      legend.key = element_rect(fill = NA)
    ) +
    annotate(
      geom = "point",
      x = endDate,
      y = WeekPrediction,
      shape = 8
    ) +
    annotate(
      geom = "text",
      x = endDate,
      y = WeekPrediction * 1.4,
      hjust = 0.5,
      vjust = 0,
      label = prettyNum(round(WeekPrediction, 0), big.mark = ",", scientific = FALSE),
      size = 3
    )

  nextSlide(PLOT, Title)
}


# US Data
Cases <- rep(TRUE, nrow(allData))
logStart <- "2020-02-29"
logEnd   <- "2020-03-22"
Title <- "USA"
plotPred(Cases, "USA", "2020-02-28", "2020-03-21")

# New York City
Cases <- grep("New York", allData$County.Name)
logStart <- "2020-03-02"
logEnd   <- "2020-03-21"
Title <- "New York City"
plotPred(Cases, "New York City", "2020-03-02", "2020-03-21")

# Santa Clara and San Mateo
Cases <- allData$County.Name == "Santa Clara County" |
  allData$County.Name == "San Mateo County"
plotPred(Cases, "Santa Clara and San Mateo", "2020-03-02", "2020-03-18")

# San Francisco
Cases <-  allData$County.Name == "San Francisco County"
plotPred(Cases, "San Francisco", "2020-03-07", "2020-03-27")

# Seattle
Cases <-  allData$County.Name == "King County" & allData$State == "WA"
plotPred(Cases, "King County (Seattle)", "2020-02-29", "2020-03-12")

# Portland
Cases <- allData$County.Name == "Multnomah County"
plotPred(Cases, "Multnomah County (Portland)", "2020-03-16", "2020-03-23")

# Westchester County
Cases <- allData$County.Name == "Westchester County"
plotPred(Cases, "Westchester County", "2020-03-15", "2020-03-24")

# Alameda
Cases <- allData$County.Name == "Alameda County"
plotPred(Cases, "Alameda County", "2020-03-05", "2020-03-24")

# Orleans Parish
Cases <- allData$County.Name == "Orleans Parish"
plotPred(Cases, "Orleans Parish", "2020-03-14", "2020-03-21")

# Bergen County
Cases <- allData$County.Name == "Bergen County"
plotPred(Cases, "Bergen County", "2020-03-14", "2020-03-21")

# Bay Area (Alameda, Contra Costa, Marin, Napa, San Mateo, Santa Clara, Solano, Sonoma, and San Francisco)
Cases <- allData$County.Name == "Santa Clara County" |
  allData$County.Name == "San Mateo County" |
  allData$County.Name == "San Francisco County" |
  allData$County.Name == "Marin County" |
  allData$County.Name == "Napa County" |
  allData$County.Name == "Solano County" |
  allData$County.Name == "Sonoma County"
plotPred(Cases, "Bay Area", "2020-03-02", "2020-03-12")

Cases <- allData$State == "CA"
plotPred(Cases, "California", "2020-03-02", "2020-03-20")

Cases <- allData$State == "UT"
plotPred(Cases, "Utah", "2020-03-02", "2020-03-20")

# Last week doubling times by state
states <- as.character(unique(allData$State))
last <- ncol(allData)
X <- 1:7
first <- last - 6
DATA <- data.frame(
  abbr = states,
  dtime = 0,
  slope = 0,
  intercept = 0
)
for (i in 1:length(states))
{
  Y <- colSums(allData[
    allData$State == states[i],
    first:last], na.rm=TRUE)
  fit <- lm(log(Y) ~ X)$coefficients
  DATA$intercept[i] <- fit[1]
  DATA$slope[i] <- fit[2]
}
DATA$dtime <- 0.693/DATA$slope

# Add full state name
SOURCE <- "https://worldpopulationreview.com/static/states/abbr-name.csv"
namesState <- read.csv(SOURCE, header=FALSE, stringsAsFactors = FALSE)
names(namesState) <- c("Abbreviation","Full")
namesState$Full[namesState$Abbreviation == "DC"] <- "District of Columbia" # needs a lower case "of"
CROWS <- match(DATA$abbr, namesState$Abbreviation)
DATA$full <- namesState$Full[CROWS]

# Add State Population
SOURCE <- "http://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv"
populationState <- read.csv(SOURCE, stringsAsFactors = FALSE)[6:56,c("NAME", "POPESTIMATE2019")]
CROWS <- match(DATA$full, populationState$NAME)
DATA$Population <- populationState$POPESTIMATE2019[CROWS]

# Predict levels in 1 week (14 days from the start of the function, which was a week ago)
DATA$Prediction <- exp(DATA$intercept + DATA$slope*14)
DATA$Per10000 <- DATA$Prediction / DATA$Population * 10000

# USA Rate
Y <- colSums(allData[, first:last], na.rm=TRUE)
fit <- lm(log(Y) ~ X)$coefficients
intercept <- fit[1]
slope <- fit[2]
USA <- 0.693/slope
Prediction <- exp(intercept + slope * 14)
Per10000 <- sum(DATA$Prediction) / sum(DATA$Population) * 10000
# By doubling time
DATA$abbr <- factor(DATA$abbr, levels = DATA$abbr[order(DATA$dtime)], ordered = TRUE)
DATA$full <- factor(DATA$full, levels = DATA$full[order(DATA$dtime)], ordered = TRUE)
Title <- paste("Steve's 7 day estimation of doubling time", Sys.Date())
PLOT <- ggplot(DATA, aes(x=full, y=dtime)) +
  geom_col(fill = "brown", color="black", width=.5) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  labs(
    title = Title,
      y = "Doubling Time",
      x = "State"
    ) +
  annotate("segment",x = 0.5, xend = 51.5, y = USA, yend = USA) +
  annotate("text", label=paste("Overall US:", round(USA,1), "days"), x = 1, y=USA, hjust=0, vjust = -0.5)

nextSlide(PLOT, "Doubling Time")

DATA$state <- DATA$abbr

PLOT <- plot_usmap(data = DATA, values = "dtime", color = "black") +
  scale_fill_continuous(
    low = "red", high = "white", name = "Doubling Time", label = scales::comma
  ) + theme(legend.position = "right") +
  labs(
    title = Title
  )
nextSlide(PLOT, "Doubling Time")

# Projected cases per 10000
DATA$abbr <- factor(DATA$abbr, levels = DATA$abbr[order(DATA$Per10000)], ordered = TRUE)
DATA$full <- factor(DATA$full, levels = DATA$full[order(DATA$Per10000)], ordered = TRUE)
Title <- paste("Steve's projection for", endDate)
PLOT <- ggplot(DATA, aes(x=full, y=Per10000)) +
  geom_col(fill = "brown", color="black", width=.5) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
#  scale_y_continuous(breaks = c(0:5)) +
  labs(
    title = Title,
    y = "Cases per 10,000 in 1 week",
    x = "State"
  ) +
  annotate("segment",x = 0.5, xend = 51.5, y = Per10000, yend = Per10000) +
  annotate("text", label=paste("Overall US:", round(Per10000,1), "cases"), x = 1, y=Per10000, hjust=0, vjust = -0.5)

nextSlide(PLOT, "All states in 1 week")


PLOT <- plot_usmap(data = DATA, values = "Per10000", color = "black") +
  scale_fill_continuous(
    low = "white", high = "red", name = "Cases Per 10000", label = scales::comma
  ) + theme(legend.position = "right") +
  labs(
    title = Title
  )

nextSlide(PLOT, "Projected Cases per 10,000")

print(PPTX, target = pptxfileName)
} else {
  cat("usafacts.org not updated yet\n")
}
