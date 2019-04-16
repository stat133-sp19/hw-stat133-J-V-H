# Savings and Annuity Simulator 
# Joseph Hernandez
# UC Berkeley 
# Statistics 133 
# ---------------------------------------------------
library(shiny)
library(ggplot2)
library(reshape2)

# Functions 

# no annuity 
# ---------------------------------------------------

#' @title: Future Value 
#' @description: Future value of an investment, ignoring inflation, ceteris paribus
#' @param: amount, rate, years 
#' @return: fv
future_value <- function(amount,rate,years){
  fv <- (amount)*((1 + rate)^(years))
  return(fv)
}

# annuity
# ---------------------------------------------------

#' @title: Future Value of an Annuity  
#' @description: Future value of an annuity, ignoring inflation, with a fixed amount deposited 
#' @param: contrib, rate, years 
#' @return: fva
annuity <- function(contrib,rate,years){
  multiple <- ((1 + rate)^(years))
  fva <- (contrib)*(((multiple)-1)/rate)
  return(fva)
}

# growing annuity
# ---------------------------------------------------

#' @title: Future Value of a Growing Annuity  
#' @description: Future value of an annuity, ignoring inflation, with an amount deposited that increases by a growth rate 
#' @param: contrib, rate, growth, years 
#' @return: fvga
growing_annuity <- function(contrib,rate,growth,years){
  multiple <- ((1 + rate)^(years))
  gmultiple <- ((1 + growth)^(years))
  fvga <- (contrib)*(((multiple)-(gmultiple))/(rate-growth))
  return(fvga)
}

# ---------------------------------------------------
# UI Code 
# ---------------------------------------------------

ui <- fluidPage(
   titlePanel("Savings and Annuity Simulator"),
   h3("Stat 133 - Workout02"),
   fluidRow(
     column(4,
         sliderInput("amount",
                     "Initial Amount:",
                     min = 0,
                     max = 100000,
                     step = 500,
                     value = 1000)),
     column(4,
         sliderInput("rate",
                     "Return Rate (in %):",
                     min = 0,
                     max = 20,
                     step = 0.1,
                     value = 5)),
     column(4,
         sliderInput("years",
                     "Number of Years:",
                     min = 0,
                     max = 50,
                     step = 1,
                     value = 20)),
     column(4,
         sliderInput("contrib",
                     "Annual Contribution:",
                     min = 0,
                     max = 50000,
                     step = 500,
                     value = 2000)),
     column(4,
          sliderInput("grow",
                     "Growth Rate (in %):",
                     min = 0,
                     max = 20,
                     step = 0.1,
                     value = 2)),
     column(4,
         selectInput("facet",
                     "Facet?",
                     choices = c("No","Yes"))
      )),
   mainPanel(
     h4("Timeline"),
     plotOutput("plot"),
     h4("Balanaces"),
     verbatimTextOutput("table")
    )
   )
# ---------------------------------------------------
# Server Code
# ---------------------------------------------------

server <- function(input, output) {
  library(ggplot2)
  library(reshape2)
  modalities <- reactive({
    modalities <- data.frame(
      years = (0:input$years),
      no_contrib = rep(input$amount, input$years + 1),
      fixed_contrib = rep(input$amount, input$years + 1),
      growing_contrib = rep(input$amount, input$years + 1)
    )
    for(i in 2:(input$years+1)){
      modalities$no_contrib[i] <- future_value(amount = input$amount, 
                                                 rate = (input$rate/100), 
                                                 years = (i-1))
      modalities$fixed_contrib[i] <- future_value(amount = input$amount, 
                                                    rate = (input$rate/100), 
                                                    years = (i-1)) + 
        annuity(contrib = input$contrib, 
                rate = (input$rate/100), 
                years = (i-1))
      modalities$growing_contrib[i] <- future_value(amount = input$amount, 
                                                      rate = (input$rate/100), 
                                                      years = (i-1)) + 
        growing_annuity(contrib = input$contrib, 
                        rate = (input$rate/100), 
                        growth = (input$grow/100), 
                        years = (i-1))
    }
    return(modalities)
  })
  
   output$plot <- renderPlot({
     melt_modalities <- melt(data=modalities(), id.vars = "years")
     graph <- ggplot(data=melt_modalities, aes(x=years, y=value)) + 
        geom_line(aes(colour=variable)) +
        ggtitle("Savings Simulation Timeline: Future Value, Annuity, Growing Annuity") + 
        xlab("Years") + 
        ylab("Value (USD $)") + 
        theme_minimal()
     if (input$facet == "No"){
       graph + scale_fill_manual(name="Simulation Type", labels(c("No Contribution", "Fixed Contribution", "Growing Contribution")))
     } else {
       graph + facet_wrap(~variable) + geom_area(alpha = 0.6, aes(fill=variable))
     }
   })
   output$table <- renderPrint(print(modalities(), print.gap = 2))
}
shinyApp(ui = ui, server = server)

