library(shiny)

source("plot.R")
source("loadData.R")

US_NAME <- "United States of America"

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("country", "Country",
                  sort(as.character(unique(Cases_Global$Country))),
                  US_NAME),
      conditionalPanel(
        paste0("input.country == '", US_NAME, "'"),
        selectInput("state", "State (optional)", 
                    c("<Entire country>" = "_all_", sort(as.character(unique(Cases_USA$State)))),
                    NULL)
      ),
      conditionalPanel(
        paste0("input.country == '", US_NAME, "' && input.state != '_all_'"),
        selectInput("county", "County (optional)",
                    NULL, NULL)
      ),
      dateInput("logStart", "Start of the log linear phase", 
                min = "2020-01-22", max = today - 7, value = "2020-03-01"),
      uiOutput("logEndUI")
    ),
    mainPanel(
      width = 9,
      plotOutput("plot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$state, {
    if (input$state == "_all_") {
      return()
    }
    counties <- subset(Cases_USA, State == input$state)$County.Name
    counties <- sort(as.character(unique(counties)))
    updateSelectInput(session, "county", choices = c("<Entire state>" = "_all_", counties))
  })
  
  output$logEndUI <- renderUI({
    dateInput("logEnd", "Start of the regression", 
              min = input$logStart + 1, max = today - 3,
              value = input$logStart + 1)
  })

  plot <- reactive({
    if (is.null(input$logEnd)) {
      return(NULL)
    }
    
    plot <- try(silent = TRUE, {
      country <- input$country
      state <- NULL
      county <- NULL
      if (country == US_NAME) {
        if (input$state != "_all_") {
          # set Country to NULL because the plot function can't handle 
          # country and state/county being set
          country <- NULL
          state <- input$state
          if (input$county != "_all_") {
            county <- input$county
          }
        }
      }
      title <- paste(c(country, state, county), collapse = "/") 
      
      plotPred(Country = country, State = state, County = county,
               Title = title, logStart = input$logStart, logEnd = input$logEnd, 
               addToPPT = FALSE)
    })
    
    if ("try-error" %in% class(plot)) {
      return(NULL)
    }
    plot
  })
  
  output$plot <- renderPlot({
    plot()
  })
}

shinyApp(ui, server)