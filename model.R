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