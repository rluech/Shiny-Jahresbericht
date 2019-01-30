

library(shiny)
library(shinydashboard)
load("A1.rdata")
source("A1.r")

# --- server ------------------------------------------------------------------

server <- function(input, output) {
  
  # --- activity --------------------------------------------------------------
  
  output$A1.act.plot.DS <- renderPlot({
    A1.act.plot(
      data  = A1.act.data$DS[, input$A1.act, drop = FALSE],
      color = A1.act.color[input$A1.act],
      stacked = input$A1.act.stacked
      )
  })
  output$A1.act.plot.SR <- renderPlot({
    A1.act.plot(
      data  = A1.act.data$SR[, input$A1.act, drop = FALSE],
      color = A1.act.color[input$A1.act],
      stacked = input$A1.act.stacked
      )
  })
  output$A1.act.plot.SI <- renderPlot({
    A1.act.plot(
      data  = A1.act.data$SI[, input$A1.act, drop = FALSE],
      color = A1.act.color[input$A1.act],
      stacked = input$A1.act.stacked
      )
  })
  
  # --- indoor outdoor --------------------------------------------------------
  
  output$A1.out.plot.DS <- renderPlot({
    A1.out.plot(
    data  = A1.out.data$DS[, input$A1.out, drop = FALSE],
    color = A1.out.color[input$A1.out]
    )
  })
  output$A1.out.plot.SR <- renderPlot({
    A1.out.plot(
      data  = A1.out.data$SR[, input$A1.out, drop = FALSE],
      color = A1.out.color[input$A1.out]
    )
  })
  output$A1.out.plot.SI <- renderPlot({
    A1.out.plot(
      data  = A1.out.data$SI[, input$A1.out, drop = FALSE],
      color = A1.out.color[input$A1.out]
    )
  })
  
}

# shinyApp(ui, server)