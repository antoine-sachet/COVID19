library(ggplot2)
library(cowplot)

closest <- function(a, b) {
  which(abs(a-b) == min(abs(a-b), na.rm=TRUE))[1]
}

covid_fn <- function(par, N) {
  # par <- intercept, peak, k
  return(par[1] + (par[2] - par[1]) * (1-exp(-par[3] * 0:(N-1))))
}

covid_obj <- function(par, Y, weight) {
  return(
    sum(
      (Y-covid_fn(par, length(Y)))^2 * (1:length(Y))^weight
    )
  )
}

covid_fit <-  function(Y, maxCases, weight = 1) {
  Y1 <- tail(Y,5)
  X1 <- 1:5
  slope <- lm(Y1 ~ X1)$coefficients[2]
  return(
    optim(
      c(Y[1],Y[1] + 2, slope),
      covid_obj,
      Y = Y,
      weight = weight,
      method = "L-BFGS-B",
      lower = c(Y[1] - 1, Y[1] - 1, 0.01),
      upper = c(Y[1] + 1, maxCases, 0.693)
    )$par
  )
}

plotPred <- function(
  County = NULL, 
  State = NULL, 
  Country = NULL,
  Title, 
  logStart, 
  logEnd,
  weight = 1,
  addToPPT = TRUE
)
{
  # USA Data
  if (!is.null(County) || !is.null(State))
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
      Predicted = NA,
      stringsAsFactors = FALSE
    )
    
    DEATHS <- data.frame(
      Date = Dates,
      Actual = c(colSums(Deaths_USA[use, c(5:ncol(Deaths_USA))],na.rm=TRUE), rep(NA, projection)),
      Phase = "Deaths",
      Predicted = NA,
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
      Predicted = NA,
      stringsAsFactors = FALSE
    )
    
    DEATHS <- data.frame(
      Date = Dates,
      Actual = c(colSums(Deaths_Global[use, c(2:ncol(Deaths_Global))],na.rm=TRUE), rep(NA, projection)),
      Phase = "Deaths",
      Predicted = NA,
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
  
  # Current
  use <- CASES$Date >= as.Date(logEnd) & !is.na(CASES$Actual)
  Y <- log(CASES$Actual[use])
  fit <- covid_fit(Y, maxCases, weight)
  
  # Last 5 days
  Y <- log(CASES$Actual[CASES$Date > today - 6 & CASES$Date < today])
  if (Y[1] < Y[5])
  {
    X <- 1:length(Y)
    coefs <- lm(Y ~ X)$coefficients
    last5Doubling <- sprintf("%0.1f", 0.693 / coefs[2]) 
  } else {
    last5Doubling <- "flat (no change)"
  }
  
  # Prediction over next week
  X <- end:nrow(CASES)
  L <- length(X)
  CASES$Predicted[X] <- exp(covid_fn(fit, L))
  WeekPrediction <- tail(CASES$Predicted, 1)
  if (sum(DEATHS$Actual, na.rm = TRUE) > 0)
  {
    DATA <- rbind(CASES, DEATHS)
  } else {
    DATA <- CASES
  }
  
  caption <- paste0("Doubling time over past 5 days: ", last5Doubling) 
  
  DATA$Phase <- factor(as.character(DATA$Phase), levels=c("Pre log linear","Log linear","Current", "Deaths"), ordered = TRUE)
  #  DEATHS$Phase <- factor(as.character(DEATHS$Phase), levels=c("Pre log linear","Log linear","Current", "Deaths"), ordered = TRUE)
  ggObject <-
    ggplot(DATA, aes(x = Date, y = Actual, color = Phase)) +
    geom_point(size = 2, na.rm = TRUE) +
    coord_cartesian(ylim = c(1,10000000), expand = TRUE, clip = "on") +
    geom_line(data=DATA[DATA$Phase != "Deaths",], size = 1, aes(y=Predicted), color="red", na.rm = TRUE) +
    labs(
      title = paste(Title,"projection as of", Sys.Date()),
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
  
  INSET <- data.frame(
    Date = extendedDates,
    Total = 0,
    Delta = 0,
    doublingTime = NA, 
    stringsAsFactors = FALSE
  )
  
  for (i in 2:nrow(CASES))
  {
    INSET$Total[i] <- CASES$Actual[i]
  }
  X <- end:nrow(INSET)
  L <- length(X)
  INSET$Total[X] <- exp(covid_fn(fit, L))
  
  # Cumulative case numbers cannot drop, needed because of merging reported cases with predicted cases.
  N <- nrow(INSET)
  INSET$Total[is.na(INSET$Total)] <- 0
  for (i in N:2)
  {
    if (INSET$Total[i-1] > INSET$Total[i]) INSET$Total[i-1] <- INSET$Total[i]
  }
  INSET$Total[is.na(INSET$Total)] <- 0
  
  # Calculate Doubling Times
  X <- 1:5
  for (i in 40:N)
  {
    if (INSET$Total[i-4] > 0)
    {
      Y <- log(INSET$Total[(i-4):i])
      INSET$doublingTime[i] <- 0.693 / lm(Y ~ X)$coefficients[2]
    }
  }
  INSET$doublingTime[INSET$doublingTime > 30] <- 30.123456 # No point in showing a doubling time > 30 days. This is a unique placeholder so other calculation can occur
  INSET$doublingTime[INSET$doublingTime < 0] <- 0
  
  INSET$Delta[2:N] <- INSET$Total[2:N] - INSET$Total[1:(N-1)]
  INSET$Source <- "Reported"
  INSET$Source[INSET$Date > today] <- "Predicted"
  
  Reported <- closest(INSET$Delta[INSET$Date <= today], max(INSET$Delta[INSET$Date <= today])/2)
  Predicted <- closest(INSET$Delta[INSET$Date > today], (max(INSET$Delta[INSET$Date > today])+min(INSET$Delta[INSET$Date > today]))/2) + allDays
  
  topINSET <- cbind(INSET[,c("Date", "Delta","Source")], "New Cases / Day")
  bottomINSET <- cbind(INSET[,c("Date", "doublingTime","Source")],"Doubling Time")
  bottomINSET$Source[INSET$Source == "Reported"] <- "Calculated"
  names(topINSET) <- names(bottomINSET) <- c("Date","Y","Source","Wrap")
  
  Reported <- 
    closest(
      topINSET$Y[topINSET$Date <= today], 
      max(topINSET$Y[topINSET$Date <= today], na.rm=TRUE)/2)
  Calculated <- 
    closest(
      bottomINSET$Y[bottomINSET$Date <= today], 
      max(bottomINSET$Y[bottomINSET$Date <= today], na.rm=TRUE)/2)+ nrow(topINSET)
  Predicted1 <- 
    closest(
      topINSET$Y[topINSET$Date > today], 
      (
        max(topINSET$Y[topINSET$Date > today], na.rm=TRUE)+
          min(topINSET$Y[topINSET$Date > today], na.rm=TRUE)
      )/2) + allDays 
  Predicted2 <- 
    closest(
      bottomINSET$Y[bottomINSET$Date > today], 
      (
        max(bottomINSET$Y[bottomINSET$Date > today], na.rm=TRUE)+
          min(bottomINSET$Y[bottomINSET$Date > today], na.rm=TRUE)
      )/2) + allDays + nrow(topINSET)
  
  INSET <- rbind(topINSET, bottomINSET)
  
  ann_text <- data.frame(
    Date = c(INSET$Date[Reported] - 1, INSET$Date[Predicted1] +1, INSET$Date[Calculated] - 1, INSET$Date[Predicted2] +1),
    Y =    c(INSET$Y[Reported], INSET$Y[Predicted1], INSET$Y[Calculated],INSET$Y[Predicted2]),
    Text = c("Reported","Predicted", "Calculated", "Predicted"),
    hjust = c(1.2, -0.2, 1.2, -0.2),
    vjust = c(-0.2, -0.2, -0.2, -0.2),
    color = c("black","brown","black","brown"),
    Wrap = c("New Cases / Day", "New Cases / Day", "Doubling Time","Doubling Time"),
    stringsAsFactors = FALSE
  )
  today_line <- data.frame(
    Date = c(today + 0.5, today + 0.5, today + 0.5, today + 0.5),
    Y = c(0, max(INSET$Y, na.rm = TRUE), 0, 30),
    Wrap = c("New Cases / Day", "New Cases / Day", "Doubling Time","Doubling Time")
  )
  
  today_text <- data.frame(
    Date = c(today, today), 
    Y =    c(0, 0), 
    Text = c("today","today"),
    hjust = c(-0.5, -0.5),
    vjust = c(-0.5, -0.5),
    Wrap = c("New Cases / Day", "Doubling Time"),
    stringsAsFactors = FALSE
  )
  
  if (topINSET$Y[topINSET$Date == today] < max(topINSET$Y)/2)
  {
    today_text$Y[1] <- max(topINSET$Y)
    today_text$hjust[1] <- 1.5
  }
  
  if (bottomINSET$Y[bottomINSET$Date == today] < 15)
  {
    today_text$Y[2] <- 30
    today_text$hjust[2] <- 1.5
  }
  
  INSET$Wrap <- factor(INSET$Wrap, c("New Cases / Day", "Doubling Time"), ordered = TRUE)
  ann_text$Wrap <- factor(ann_text$Wrap, c("New Cases / Day", "Doubling Time"), ordered = TRUE)
  today_line$Wrap <- factor(today_line$Wrap, c("New Cases / Day", "Doubling Time"), ordered = TRUE)
  today_text$Wrap <- factor(today_text$Wrap, c("New Cases / Day", "Doubling Time"), ordered = TRUE)
  INSET$Y[INSET$Y == 30.123456] <- NA  # Get rid of values > 30
  
  ggObject2 <- 
    ggplot(INSET, aes(x=Date, y = Y)) + 
    geom_point(data = INSET[INSET$Date <= today,], size = 0.7, na.rm = TRUE, show.legend = FALSE, color = "black") +
    geom_line(data = INSET[INSET$Date > today,], size = 0.8, na.rm = TRUE, show.legend = FALSE, color = "brown") +
    scale_color_manual(values = c("black","brown")) +
    labs(
      y = "New Cases / Day"
    ) +
    labs(y=NULL) +
    scale_y_continuous(
      labels = scales::comma_format(big.mark = ',',
                                    decimal.mark = '.')) +
    geom_text(data = ann_text, aes(label = Text, hjust = hjust, vjust = vjust, color = color), size = 2.5, show.legend = FALSE) +
    geom_text(data = today_text, aes(label = Text, hjust = hjust, vjust = vjust), color="blue", angle = 90, size=2, show.legend = FALSE) +
    geom_line(data = today_line, color = "blue") +
    theme(
      axis.text=element_text(size=6),
      axis.title=element_text(size=8),
      strip.background = element_blank(),
      strip.placement = "outside" 
    ) + 
    facet_grid(
      Wrap ~ .,
      scales="free_y",
      switch = "y",
      shrink=FALSE
    )
  
  ggObject3 <-  ggdraw() +
    draw_plot(ggObject) +
    draw_plot(ggObject2, x = 0.12, y = .405, width = .29, height = .50)
  
  if (addToPPT) {
    nextSlide(ggObject3, Title)
  } else {
    ggObject3
  }
}
