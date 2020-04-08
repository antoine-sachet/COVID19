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
  
  remove(list=ls())
  setwd("g:/projects/covid")
  today <- Sys.Date()  
  startDate <- as.Date("2020-01-22")
  allDays <- as.numeric(today-startDate)
  
  source("ImportCovidData.R")
  # USA Files: Cases_USA, Deaths_USA, Population_USA
  # Global Files: Cases_Global, Deaths_Global, Population_Global
  
  # Check if updated
  if (
    tail(names(Cases_USA),1) ==
    gsub(" ","", gsub("^X0","X", format(today - 1, "X%m.%e.%y"))) |
    tail(names(Deaths_USA),1) ==
    gsub(" ","", gsub("^X0","X", format(today - 1, "X%m.%e.%Y")))
  )
  {

  projection <- 7 # Project forward just 7 days
  asymptomatic <- 10  # Number of asymptomatic patients per symptomatic patient
  endDate <- startDate + allDays + projection - 1
  Dates <- seq(from = startDate, to=endDate, by = 1)
  extendedDates <- seq(from = startDate, to=startDate + allDays + 60, by = 1)
  bootstrapsN <- 100

  timestamp <- format(Sys.time(), format = "%Y-%m-%d")
  pptx <- read_pptx("Template.pptx")
  master <- "Office Theme"
  slideNumber <- 1
  pptxfileName <- paste0("Steve's COVID Analysis.", timestamp, ".pptx")
  if (file.exists(pptxfileName))
    file.remove(pptxfileName)
  
  while(file.exists(pptxfileName))
  {
    cat("Close the open PowerPoint File\n")
    file.remove(pptxfileName)
  }
  
  
  # Disclaimer slide
  DISCLAIMER <-
    c(
      "This is not confidential and can be freely shared. The code is available at https://github.com/StevenLShafer/COVID19/.",
      "",
      "This is my analysis, not Stanford's. My understanding is that Stanford's internal analysis, done to plan resource allocation at Stanford, shows substantially longer doubling times. This is reassuring for those of us working at Stanford.",
      "",
      "Data sources:",
      "       USA Data:     https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv.",
      "       Global Data:  https://github.com/CSSEGISandData/COVID-19. This is the Johns Hopkins data repository.",
      "",
      "Nate Silver has an excellent write-up on the potential problems of the data (see https://fivethirtyeight.com/features/coronavirus-case-counts-are-meaningless/). He is right, of course, but these data are all we have. Also, he does not address the fact that the data are consistent with the expections. Specifically, the increases are initially log-linear, but then \"flatten\" as expected when public health policies are implemented. We would not see this if the data were just random noise.",
      "",
      "Models:",
      "       Log linear phase: log(Y) = intercept + slope * time",
      "       Asymptotic phase: log(y) = intercept + peak * (1 - exp(k * time))",
      "",
      "The number printed on the graph is the projection for a week from today. Because that is only a week out, it is probably not far off. The \"peak\" in the caption is the estimated total peak at time = infinity from the model. I have very little confidence because it is a projection well into the future, and very sensitive to small errors in that data.",
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

  nextSlide <- function (ggObject, Title)
    {
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
  
closest <- function(a, b)
{
  which(abs(a-b) == min(abs(a-b)))[1]
}

covid_fn <- function(par, N)
{
  # par <- intercept, peak, k
  return(par[1] + (par[2] - par[1]) * (1-exp(-par[3] * 0:(N-1))))
}

covid_obj <- function(par, Y)
{
  return(sum((Y-covid_fn(par, length(Y)))^2))
}

covid_fit <-  function(Y, maxCases)
{
  return(
    optim(
      c(Y[1],4, 0.8),
      covid_obj,
      Y = Y,
    method = "L-BFGS-B",
    lower = c(Y[1] - 1, Y[1] - 1, 0.01),
    upper = c(Y[1] + 1, maxCases, 0.693)
    )$par
  )
}

covid_obj_bootstrap <- function(par, intercept, Y, maxCases)
{
  return(sum((Y-covid_fn(c(intercept, par), length(Y)))^2))
}

covid_bootstrap <-  function(Y, initial, maxCases)
{
  results <- data.frame(
    peak = rep(NA, bootstrapsN),
    k = rep(NA, bootstrapsN)
  )
  for (i in 1: bootstrapsN)
  {
    fit <- 
      optim(
        c(initial[2], initial[3]),
        covid_obj_bootstrap,
        intercept = initial[1],
        Y = sample(Y, length(Y), replace=TRUE),
        method = "L-BFGS-B",
        lower = c(initial[1], 0.01),
        upper = c(maxCases, 0.693)
      )$par
    results$peak[i] <- fit[1]
    results$k[i] <- fit[2]
  }
  return(results)
}

plotPred <- function(
  County = NULL, 
  State = NULL, 
  Country = NULL,
  Title, 
  logStart, 
  logEnd
  )
  {
  
  # USA Data
  if (!is.null(County) | !is.null(State))
  {
    if(is.null(County))
    {
      useCounty <- rep(TRUE, nrow(Cases_USA))
    } else {
      useCounty <- Cases_USA$County.Name %in% County
    }
    if(is.null(State))
    {
      useState <- rep(TRUE, nrow(Cases_USA))
    } else {
      useState <- Cases_USA$State %in% State
    }
    
    use <- useCounty & useState

    CASES <- data.frame(
      Date = Dates,
      Actual = c(colSums(Cases_USA[use, c(5:ncol(Cases_USA))],na.rm=TRUE), rep(NA, projection)),
      Phase = "",
      Predicted.LL = NA,
      Predicted.SD = NA,
      stringsAsFactors = FALSE
    )
    
    DEATHS <- data.frame(
      Date = Dates,
      Actual = c(colSums(Deaths_USA[use, c(5:ncol(Deaths_USA))],na.rm=TRUE), rep(NA, projection)),
      Phase = "Deaths",
      Predicted.LL = NA,
      Predicted.SD = NA,
      stringsAsFactors = FALSE
    )
    FIPS <- Cases_USA$CountyFIPS[use]
    maxCases <- sum(Population_USA$Population[Population_USA$CountyFIPS %in% FIPS], na.rm=TRUE) / (1 + asymptomatic)
  }
  
  # Global data
  if (!is.null(Country))
  {
    use <- Cases_Global$Country %in% Country
    CASES <- data.frame(
      Date = Dates,
      Actual = c(colSums(Cases_Global[use, c(2:ncol(Cases_Global))],na.rm=TRUE), rep(NA, projection)),
      Phase = "",
      Predicted.LL = NA,
      Predicted.SD = NA,
      stringsAsFactors = FALSE
    )
    
    DEATHS <- data.frame(
      Date = Dates,
      Actual = c(colSums(Deaths_Global[use, c(2:ncol(Deaths_Global))],na.rm=TRUE), rep(NA, projection)),
      Phase = "Deaths",
      Predicted.LL = NA,
      Predicted.SD = NA,
      stringsAsFactors = FALSE
    )
    maxCases <- sum(Population_Global$Population[Population_Global$Country %in% Country], na.rm=TRUE) / (1 + asymptomatic)
  }
  
  # Cumulative case numbers and deaths cannot drop
  for (i in nrow(CASES):2)
  {
    if(!is.na(CASES$Actual[i]))
      if (CASES$Actual[i-1] > CASES$Actual[i]) CASES$Actual[i-1] <- CASES$Actual[i]
      if(!is.na(DEATHS$Actual[i]))
        if (DEATHS$Actual[i-1] > DEATHS$Actual[i]) DEATHS$Actual[i-1] <- DEATHS$Actual[i]
  }

    start <- which(CASES$Date == as.Date(logStart))
    end   <-  which(CASES$Date == as.Date(logEnd))
    CASES$Phase[1:(start-1)] <- "Pre log linear"
    CASES$Phase[start:(end-1)] <- "Log linear"
    CASES$Phase[end:nrow(CASES)] <- "Current"
    CASES$Actual[CASES$Actual == 0 | is.na(CASES$Actual)] <- NA
    DEATHS$Actual[DEATHS$Actual == 0 | is.na(DEATHS$Actual)] <- NA
    maxCases <- log(maxCases)

    # Log linear phase
    Y <- log(CASES$Actual[CASES$Phase == "Log linear"])
    X <- 1:length(Y)
    coefs <- lm(Y ~ X)$coefficients
    Intercept <- coefs[1]
    slope <- coefs[2]
    X <- start:nrow(CASES)
    CASES$Predicted.LL[X] <- exp(Intercept + 1:length(X) * slope)

    # Current
    use <- CASES$Date >= as.Date(logEnd) & !is.na(CASES$Actual)
    Y <- log(CASES$Actual[use])
    fit <- covid_fit(Y, maxCases)
    
    # Bootstrap
    results <- covid_bootstrap(Y, fit, maxCases)
    
    X <- end:nrow(CASES)
    L <- length(X)
    CASES$Predicted.SD[X] <- exp(covid_fn(fit, L))
    WeekPrediction <- tail(CASES$Predicted.SD, 1)
    if (sum(DEATHS$Actual, na.rm = TRUE) > 0)
    {
      DATA <- rbind(CASES, DEATHS)
    } else {
      DATA <- CASES
    }

    caption <- paste0("Initial doubling: ", sprintf("%0.1f", 0.693/slope), " days, Predicted peak: ", prettyNum(round(exp(fit[2]) , 0),big.mark = ",", scientific = FALSE))

    DATA$Phase <- factor(as.character(DATA$Phase), levels=c("Pre log linear","Log linear","Current", "Deaths"), ordered = TRUE)
  #  DEATHS$Phase <- factor(as.character(DEATHS$Phase), levels=c("Pre log linear","Log linear","Current", "Deaths"), ordered = TRUE)
    ggObject <-
      ggplot(DATA, aes(x = Date, y = Actual, color = Phase)) +
      geom_point(size = 2, na.rm = TRUE) +
      coord_cartesian(ylim = c(1,10000000), expand = TRUE, clip = "on") +
      geom_line(data=DATA[DATA$Phase != "Deaths",], size = 1, aes(y=Predicted.LL), color="blue", na.rm = TRUE) +
      geom_line(data=DATA[DATA$Phase != "Deaths",], size = 1, aes(y=Predicted.SD), color="red", na.rm = TRUE) +
      labs(
        title = paste("Steve's", Title,"projection as of", Sys.Date()),
        y = "Actual (points) / Predicted (line)",
        caption = caption
      ) +
      scale_color_manual(values = c("forestgreen","blue","red", "black")) +
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

    DELTA <- data.frame(
      Date = extendedDates,
      Total = 0,
      Delta = 0
    )

    for (i in 2:nrow(CASES))
    {
      switch(
        CASES$Phase[i],
        'Pre log linear' = DELTA$Total[i] <- CASES$Actual[i],
        'Log linear'     = DELTA$Total[i] <- CASES$Predicted.LL[i],
        )
    }
    X <- end:nrow(DELTA)
    L <- length(X)
    DELTA$Total[X] <- exp(covid_fn(fit, L))

    # Cumulative case numbers cannot drop
    N <- nrow(DELTA)
    DELTA$Total[is.na(DELTA$Total)] <- 0
    for (i in N:2)
    {
      if (DELTA$Total[i-1] > DELTA$Total[i]) DELTA$Total[i-1] <- DELTA$Total[i]
    }


    DELTA$Delta[2:N] <- DELTA$Total[2:N] - DELTA$Total[1:(N-1)]
    DELTA$Source <- "Reported"
    DELTA$Source[DELTA$Date > today] <- "Predicted"
    DELTA$Source <- factor(DELTA$Source, levels = c("Reported","Predicted"), ordered = TRUE)
    
    Reported <- closest(DELTA$Delta[DELTA$Date <= today], max(DELTA$Delta[DELTA$Date <= today])/2)
    Predicted <- closest(DELTA$Delta[DELTA$Date > today], (max(DELTA$Delta[DELTA$Date > today])+min(DELTA$Delta[DELTA$Date > today]))/2) + allDays
    
    ggObject2 <- ggplot(DELTA, aes(x=Date, y = Delta)) + 
      geom_point(data = DELTA[DELTA$Date <= today,], size = 0.8, na.rm = TRUE, show.legend = FALSE, color = "black") +
      geom_line(data = DELTA[DELTA$Date > today,], size = 0.8, na.rm = TRUE, show.legend = FALSE, color = "brown") +
      scale_color_manual(values = c("black","brown")) +
      labs(
        y = "New Cases / Day"
      ) +
      scale_y_continuous(
        labels = scales::comma_format(big.mark = ',',
                                    decimal.mark = '.')) +
      theme(
        axis.text=element_text(size=6),
        axis.title=element_text(size=8)      ) + 
      annotate("segment",x = today, xend = today, y = 0, yend = max(DELTA$Delta), color = "blue") +
      annotate(
        "text", 
        label = "Reported", 
        color = "black", 
        x = DELTA$Date[Reported] - 1, 
        y = DELTA$Delta[Reported],
        hjust = 1.2, 
        vjust = -0.2, 
        size = 2
      ) +
      annotate(
        "text", 
        label = "Predicted", 
        color = "brown", 
        x = DELTA$Date[Predicted] + 1, 
        y = DELTA$Delta[Predicted],
        hjust = -0.2, 
        vjust = -0.2,
        size = 2
      )
    
  if (DELTA$Delta[DELTA$Date == today] > max(DELTA$Delta)/2)
  {
    ggObject2 <- ggObject2 +
      annotate(
        "text", 
        label = "Today", 
        color = "blue", 
        x = today, 
        y = 0,
        hjust = -0.1, 
        vjust = -0.5,
        angle = 90,
        size = 2
      )
  } else {
    ggObject2 <- ggObject2 +
      annotate(
        "text", 
        label = "Today", 
        color = "blue", 
        x = today, 
        y = max(DELTA$Delta),
        hjust = 1.1, 
        vjust = -0.5,
        angle = 90,
        size = 2
      )
    
  }
      
    ggObject3 <-  ggdraw() +
      draw_plot(ggObject) +
      draw_plot(ggObject2, x = 0.145, y = .605, width = .29, height = .28)

    nextSlide(ggObject3, Title)
  }

  # US Data
  Country <- "United States of America"
  logStart <- "2020-02-29"
  logEnd   <- "2020-03-22"
  Title <- "USA"
  plotPred(Country = "United States of America", Title = "USA", logStart = "2020-02-28", logEnd = "2020-03-21")
  plotPred(County = "New York County", Title = "New York City", logStart = "2020-03-02", logEnd = "2020-03-21")
  plotPred(County = c("Santa Clara County", "San Mateo County"), Title = "Santa Clara and San Mateo", logStart = "2020-03-02", logEnd = "2020-03-18")
  plotPred(County = "San Francisco County", Title = "San Francisco", logStart = "2020-03-07", logEnd = "2020-03-25")
  plotPred(County = "San Luis Obispo County", Title = "San Luis Obispo", logStart = "2020-03-16", logEnd = "2020-03-25")
  plotPred(County = "King County", State = "WA", Title = "King County (Seattle)", logStart = "2020-02-29", logEnd = "2020-03-10")
  plotPred(County = "Multnomah County", Title = "Multnomah County (Portland)", logStart = "2020-03-16", logEnd = "2020-03-23")
  plotPred(County = "Westchester County", Title = "Westchester County", logStart = "2020-03-15", logEnd = "2020-03-24")
  plotPred(County = "Alameda County", Title = "Alameda County", logStart = "2020-03-05", logEnd = "2020-03-24")
  plotPred(County = c("Santa Clara County", "San Mateo County", "San Francisco County", "Marin County", "Napa County", "Solano County", "Sonoma County"), Title = "Bay Area", logStart = "2020-03-02", logEnd = "2020-03-12")
  plotPred(State = "CA", Title = "California", logStart = "2020-03-02", logEnd = "2020-03-20")
  plotPred(County = "De Soto Parish", Title = "De Soto Parish, Louisiana", logStart = "2020-03-22", logEnd = "2020-03-25")
  plotPred(County = "Bergen County", Title = "Bergen County", logStart = "2020-03-14", logEnd = "2020-03-20")
  plotPred(County = "Dallas County", State = "TX", Title = "Dallas Texas", logStart = "2020-03-10", logEnd = "2020-03-20")
  plotPred(County = "McLean County", State = "IL", Title = "McLean County, Illinois", logStart = "2020-03-20", logEnd = "2020-03-23")
  plotPred(County = "Cook County", State = "IL", Title = "Cook County, Illinois", logStart = "2020-03-06", logEnd = "2020-03-20")
  plotPred(County = "Suffolk County", State = "MA", Title = "Suffolk County (Boston)", logStart = "2020-03-10", logEnd = "2020-03-24")
  plotPred(State = "UT", Title = "Utah (State)", logStart = "2020-03-02", logEnd = "2020-03-18")
  plotPred(County = "Utah County", Title = "Utah County", logStart = "2020-03-02", logEnd = "2020-03-20")
  plotPred(County = "Polk County", State = "IA", Title = "Polk County, Iowa", logStart = "2020-03-02", logEnd = "2020-03-20")

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
  X <- 1:7
  first <- last - 6
  
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
  fit <- covid_fit(Y, log(STATES$population[i] / (1 +  asymptomatic)))
  # Peak can't get higher than 70% of state population
  STATES$intercept[i] <- fit[1]
  STATES$peak[i] <- fit[2]
  STATES$k[i] <- fit[3]
}
  
STATES$dtime <- 0.693/STATES$slope
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
Title <- paste("Steve's 7 day estimation of doubling time", Sys.Date())
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

# Population Percent in 7 days
STATES$state <- factor(STATES$state, levels = STATES$state[order(STATES$PredWeek)], ordered = TRUE)
Title <- paste("Steve's projection for", endDate)
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
Title <- paste("Steve's projection for", endDate)
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
CROWS <- match(Counties$FIPS, Cases_USA$CountyFIPS)
Counties$Cases <- log(Cases_USA[CROWS,ncol(Cases_USA)])
Counties$Cases[is.na(Counties$Cases)] <- 0
Title <- paste("Distribution of Reported Cases as of", today)
ggObject <- plot_usmap(regions = "counties") +
  geom_point(data = Counties, aes(x = LONGITUDE.1, y=LATITUDE.1, size = Cases, color = Cases, alpha = Cases)) +
  scale_color_gradient(low="white",high="red") +
  scale_size(range = c(0, 4)) + 
  scale_alpha(range = c(0, 1)) + 
  theme(legend.position = "none") +
 labs(
    title = Title
  )
nextSlide(ggObject, "Current US by County")

# Worldwide
plotPred(Country = "Italy", Title = "Italy", logStart = "2020-02-22", logEnd = "2020-03-10")
plotPred(Country = "Spain", Title = "Spain", logStart = "2020-02-25", logEnd = "2020-03-13")
plotPred(Country = "France", Title = "France", logStart = "2020-02-27", logEnd = "2020-03-10")
plotPred(Country = "Portugal", Title = "Portugal", logStart = "2020-03-03", logEnd = "2020-03-17")
plotPred(Country = "Sweden", Title = "Sweden", logStart = "2020-02-28", logEnd = "2020-03-10")
plotPred(Country = "Netherlands", Title = "Netherlands", logStart = "2020-02-29", logEnd = "2020-03-06")
plotPred(Country = "England", Title = "United Kingdom", logStart = "2020-02-26", logEnd = "2020-03-20")
plotPred(Country = "South Africa", Title = "South Africa", logStart = "2020-03-08", logEnd = "2020-03-22")
plotPred(Country = "Brazil", Title = "Brazil", logStart = "2020-03-08", logEnd = "2020-03-20")
plotPred(Country = "Rwanda", Title = "Rwanda", logStart = "2020-03-12", logEnd = "2020-03-19")

# World Map of Cases
CROWS <- match(worldmap$geounit, Cases_Global$Country)
worldmap$cases <- Cases_Global[CROWS, ncol(Cases_Global)]
worldmap$lcases <- log(worldmap$cases)

worldmap <- worldmap[!is.na(worldmap$lcases),]
  
ggObject <-   ggplot() + 
    geom_sf(data = worldmap, color="#00013E", aes(fill=lcases)) +
    scale_fill_gradient(low="#FFCFCF",high="#C00000") +
    # coord_sf(
    #   crs = st_crs('+proj=gall'),
    #   xlim = c(-11168658, 11168658),
    #   ylim = c(-6600000, 8500000),
    #   expand = FALSE
    # )+
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
# ggpubr::ggexport(
#   ggObject,
#   filename = "Worldmap.png", 
#   resolution = 72,
#   height = 144,
#   width = 240,
#   pointsize = 4,
#   verbose = FALSE
# )
# pngfileName <- gsub(".png","001.png",pngfileName) #Weird!
# print(ggObject)
# 
# pptx <- add_slide(pptx, layout = "Title and Content", master = master)
# pptx <- ph_with(pptx, value = "Worldwide Distribution of cases", location = ph_location_type("title"))
# pptx <- ph_with(pptx, external_img(src = "worldmap001.png"), location = ph_location_type("body"))
# pptx <- ph_with(pptx, value = slideNumber, location = ph_location_type("sldNum"))
nextSlide(ggObject, "Worldwide Distribution of Cases")

print(pptx, target = pptxfileName)
  } else {
    cat("usafacts.org not updated yet\n")
  }
