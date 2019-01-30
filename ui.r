
library(shiny)
library(shinydashboard)
load("A1.rdata")
source("A1.r")

# --- helper ------------------------------------------------------------------

sidebarstyile <- "text-align:center;color:#FFA319;font-size:100%"
chapter <- list(
  `Cross Media` = c("Cross Media",""),
  `TV` = c("TV: Day","TV: Week","TV: Years","TV: Demografics"),
  `Radio` = c("Radio: Day","Radio: Week","Radio: Years","Radio: Demografics")
  )

# --- header ------------------------------------------------------------------

header <- dashboardHeader(title = "Jahresbericht")

# --- sidebar -----------------------------------------------------------------

sidebar <- dashboardSidebar(

  selectInput(
    "chapter", 
    label = em("Select chapter", style = sidebarstyile),
    chapter,
    selected = names(chapter)[1]
    )

)

# --- body --------------------------------------------------------------------

body <- dashboardBody(
  
  fluidRow(
    
    tabBox(
      title = "Cross Media",
      id = "tabset.act", width = 8, height = "550px",
      tabPanel("Deutschschweiz", plotOutput("A1.act.plot.DS")),
      tabPanel("Suissromande",   plotOutput("A1.act.plot.SR")),
      tabPanel("Svizeraitalia",  plotOutput("A1.act.plot.SI"))
    ),
    box(width = 2, title = "", height = "550px",
        checkboxGroupInput("A1.act.stacked", "plot", c("stacked" = TRUE)),
        checkboxGroupInput("A1.act", "activity", 
                           selected = names(A1.act.color),
                           choiceNames = names(A1.act.color),
                           choiceValues = names(A1.act.color))
        ),
    
    tabBox(
      title = "Indoor Outdoor",
      id = "tabset.out", width = 8, 
      tabPanel("Deutschschweiz", plotOutput("A1.out.plot.DS")),
      tabPanel("Suissromande",   plotOutput("A1.out.plot.SR")),
      tabPanel("Svizeraitalia",  plotOutput("A1.out.plot.SI"))
    ),
    box(width = 2, title = "", height = "550px",
        checkboxGroupInput("A1.out", "activity", 
                           selected = names(A1.out.color),
                           choiceNames = names(A1.out.color),
                           choiceValues = names(A1.out.color))
    )
  )
)

# --- page --------------------------------------------------------------------

dashboardPage(header, sidebar, body)
