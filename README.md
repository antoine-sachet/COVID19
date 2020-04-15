# Tracking the COVID-19 pandemic

## Caveats / Comments

This is not confidential. The analyses and R program code and can be freely shared.

This is my analysis, not Stanford's. 

## Run it yourself

You can run the analysis yourself in one of two ways:

1. You can create a PowerPoint with many analysis graphs by running the `covid.update.R` script.
2. You can run an interactive Shiny application that allows you to choose a location and some parameters to graph by opening the `app.R` file in the RStudio IDE and clicking the "Run App" button.

## Data sources:

- [USA Data](https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv)
- [Global Data](https://github.com/CSSEGISandData/COVID-19) This is the Johns Hopkins data repository

## Description

Nate Silver has an excellent write-up on the potential problems of the data (see https://fivethirtyeight.com/features/coronavirus-case-counts-are-meaningless/). He is right, of course, but these data are all we have. Also, he does not address the fact that the data are consistent with the expectations. Specifically, the increases are initially log-linear, but then "flatten" as expected when public health policies are implemented. We would not see this if the data were just random noise.

Asymptotic Model: log(y) = intercept + (peak - intercept) * (1 - exp(k * time)). The constant k describes the rate of approach to steady state, such that 0.693/k is the half-time to steady state. 

Not surprisingly, the model is wrong. As Yogi Berra said, "It's tough to make predictions, especially about the future." The model is hugely sensitive to exactly what data points are chosen, and the predicted peak can change by an order of magnitude based on the starting estimate. Additionally, the model assumes that everything will stay the same. Everything will not stay the same. Testing is ramping up. More and more patients are being tested, which will result in picking up more asymptomatic infections and reducing the apparent mortality. Additionally, social distancing may be reduced to mitigate the economic consequences. Even if the model were perfect, it could not anticipate changes changes in policy. The prediction that is a week out is likely quite close. Predictions of peak case numbers over the next week or so are also likely accurate. Beyond that, it's mostly a guess.

The idiosyncratic locations are where Pamela and I have family or friends, or are locations requested by friends. I'm happy to add other regions. Also, I'm happy to add people to the blind CC distribution list. Just let me know.

Please send any questions to steven.shafer@stanford.edu.

Steven L. Shafer, MD
