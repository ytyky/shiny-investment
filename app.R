library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  
  # title panel
  titlePanel("shiny investing scenario"),
  
  fluidRow(
    # sidebar
    column(3,
           sliderInput(inputId = "initial",
                       label = "Initial Amount",
                       min = 1,
                       max = 10000,
                       value = 1000),
           sliderInput(inputId = "annual_contrib",
                       label = "Annual Conrtibution",
                       min = 0,
                       max = 5000,
                       value = 200),
           sliderInput(inputId = "annual_growth",
                       label = "Annual Growth Rate (in %)",
                       min = 0,
                       max = 20,
                       value = 3,
                       step = 0.1)
    ),
    
    column(3,
           sliderInput(inputId = "high_yield_annual_rate",
                       label = "High Yield Annual Rate (in %)",
                       min = 0,
                       max = 20,
                       value = 2,
                       step = 0.1),
           sliderInput(inputId = "fixed_income_annual_rate",
                       label = "Fixed Income annual rate (in %)",
                       min = 0,
                       max = 20,
                       value = 5,
                       step = 0.1),
           sliderInput(inputId = "US_Equity_annual_rate",
                       label = "US Equity annual rate (in %)",
                       min = 0,
                       max = 20,
                       value = 10,
                       step = 0.1)
           
    ),
    
    column(3,
           sliderInput(inputId = "high_yield_vol",
                       label = "High Yield volatility (in %)",
                       min = 0,
                       max = 20,
                       value = 0.1,
                       step = 0.1),
           sliderInput(inputId = "fixed_income_vol",
                       label = "Fixed Income volatility (in %)",
                       min = 0,
                       max = 20,
                       value = 4.5,
                       step = 0.1),
           sliderInput(inputId = "US_vol",
                       label = "US Equity volatility (in %)",
                       min = 0,
                       max = 20,
                       value = 15,
                       step = 0.1)
    ),
    
    column(3,
           sliderInput(inputId = "years",
                       label = "Years)",
                       min = 0,
                       max = 50,
                       value = 20),
           numericInput("seed",
                        label = "Random Seed",
                        value = 12345),
           selectInput("facet",
                       label = "Facet?",
                       choices = c("Yes", "No"))
    )
    
    
  ),
  
  # main panel
  mainPanel("Timelines",
            plotOutput("financial_model", width = "150%", height = "350px")          
  )
  
  
)

server <- function(input, output) {
  output$financial_model <- renderPlot({
    set.seed(input$seed)
    #' @title money return model
    #' @description calculates money returns
    #' @param initial initial money
    #' @param mu rate (%)
    #' @param sigma volatility (%)
    #' @param period time period (years)
    #' @param contrib yearly contribution
    #' @param g annual growth rate (%)
    #' @return future value
    money_returns <- function(initial, mu, sigma, period, contrib, g) {
      mu = mu/100
      sigma = sigma/100
      g = g/100
      year = c(0:period)
      amt = rep(0, period+1)
      for (i in year) {
        if (i == 0) {
          amt[i + 1] = initial
        } else {
          r = rnorm(1, mu, sigma)
          amt[i + 1] = amt[i]*(1 + r) + contrib*(1+g)^(i-1)
        }
      }
      return(amt)
    }
    
    high_yield <- money_returns(input$initial,
                                input$high_yield_annual_rate,
                                input$high_yield_vol,
                                input$years,
                                input$annual_contrib,
                                input$annual_growth)
    us_bonds <- money_returns(input$initial,
                              input$fixed_income_annual_rate,
                              input$fixed_income_vol,
                              input$years,
                              input$annual_contrib,
                              input$annual_growth)
    us_stocks <- money_returns(input$initial,
                               input$US_Equity_annual_rate,
                               input$US_vol,
                               input$years,
                               input$annual_contrib,
                               input$annual_growth)
    
    
    # intergrate 3 investing models
    years <- 0:input$years
    period <- length(years)
    type = c(rep("high_yield", period), rep("us_bonds", period), rep("us_stocks", period))
    
    model <- data.frame(year = rep(years,3),
                        type = type,
                        value =c(high_yield,us_bonds,us_stocks))
    
    df <- ggplot(model, aes(x = year, y = value, colour = type))
    if (input$facet == "Yes") {
      df + geom_line() +
        facet_wrap(~ type) +
        geom_point() +
        geom_area(aes(fill = type), alpha = 0.5) +
        ggtitle("investing in 3 different models")
    } else {
      df + geom_line() +
        ggtitle("investing in 3 different models")
    }
    
  }, width = "auto")
}


shinyApp(ui, server)