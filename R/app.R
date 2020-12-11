#' Show coverage graphs and maps on a Shiny dashboard.
#'
#' @param wdir Path to directory where data are (directory with Status_yyyymmdd subdirectories)
#' @import shiny
#' @import shinydashboard
#' @return Open local Shiny dashboard. Exit by closing dashboad window.
#' @export
ShowCov <- function(wdir) {

  # Clean Environment
  # rm(list=ls())

  # library(shiny)
  # library(shinydashboard)

  # Read and prepare data
  df <- NULL
  for (f in list.files(wdir, "Status")) {
    # print(f)
    df <- rbind(df, readRDS(paste0(wdir, "/", f, "/Data/NUTSVacMonth.RDS")), fill = TRUE)
  }
  df$StatusDate <- as.Date(df$StatusDate)

  ### Header ###
  header <- dashboardHeader(title = span("Measles vaccination coverage", style = "color:#009999; font-size: 28px"))

  ### Sidebar ###
  sidebar <- dashboardSidebar(
    selectizeInput("dose",
                   label = "Select dose:",
                   choices = NULL,
                   multiple = TRUE,
                   options = list(maxItems = 1)),
    selectizeInput("nuts",
                   label="Select NUTS:",
                   choices = NULL,
                   multiple = TRUE,
                   options = list(maxItems = 1)),
    selectizeInput("BC",
                   label="Select Birth cohort:",
                   choices = NULL,
                   multiple = TRUE,
                   options = list(maxItems = 1))
  )

  ### Body ###
  body <- dashboardBody(
    fluidRow(
      tabBox(id = "tabmenu", width = 12,
             tabPanel("Coverage by birth cohort", icon = icon("home","fa-lg"),
                      fluidRow(
                        column(width = 12,
                               plotOutput("CovBC", height = "500px"),#%>%withSpinner(color="#009999"),
                               sliderInput("StatusDateCovBC", "StatusDate:",
                                           min = min(df$StatusDate), max = max(df$StatusDate),
                                           value = max(df$StatusDate))
                        )
                      )
             ),
             tabPanel("Coverage by age", icon = icon("home","fa-lg"),
                      fluidRow(
                        column(width = 12,
                               plotOutput("CovAge", height = "500px"),#%>%withSpinner(color="#009999"),
                               sliderInput("StatusDateCovAge", "StatusDate:",
                                           min = min(df$StatusDate), max = max(df$StatusDate),
                                           value = max(df$StatusDate))
                        )
                      )
             ),
             tabPanel("Coverage map", icon = icon("home","fa-lg"),
                      fluidRow(
                        column(width = 12,
                               plotOutput("CovMap", height = "500px"),#%>%withSpinner(color="#009999"),
                               sliderInput("StatusDateCovMap", "StatusDate:",
                                           min = min(df$StatusDate), max = max(df$StatusDate),
                                           value = max(df$StatusDate))
                        )
                      )
             )
      )
    )
  )

  ui <- dashboardPage(header, sidebar, body, skin = "blue")


  ############################################################
  # Server
  ############################################################


  server <- function(input, output, session) {

    observe({
      updateSelectizeInput(session, "nuts",
                           choices = as.list(unique(c(unique(substr(df$nuts,1,2)), unique(substr(df$nuts,1,3)), unique(substr(df$nuts,1,4)), unique(substr(df$nuts,1,5))))),
                           server = TRUE,
                           selected = substr(df$nuts[1],1,2))
      updateSelectizeInput(session, "dose", choices = as.list(unique(df$dose)),
                           server = TRUE,
                           selected = df$dose[1])
      updateSelectizeInput(session, "BC", choices = as.list(unique(df$birthyear)),
                           server = TRUE,
                           selected = df$birthyear[1])
    })
    observeEvent(c(input$dose, input$nuts) , {
      output$CovBC <- renderPlot({
        MCVCovLoc::VacCovBC(
          wdir = wdir,
          StatusDate = max(df[df$StatusDate <= input$StatusDateCovBC,]$StatusDate),
          NUTS = input$nuts,
          dose = input$dose
        )
      })
    })
    observeEvent(c(input$dose, input$nuts) , {
      output$CovAge <- renderPlot({
        MCVCovLoc::VacCovAge(
          wdir = wdir,
          StatusDate = max(df[df$StatusDate <= input$StatusDateCovAge,]$StatusDate),
          NUTS = input$nuts,
          dose = input$dose
        )
      })
    })
    observeEvent(c(input$dose, input$BC) , {
      output$CovMap <- renderPlot({
        MCVCovLoc::MapCov(
          wdir = wdir,
          StatusDate = max(df[df$StatusDate <= input$StatusDateCovMap,]$StatusDate),
          NUTS = input$nuts,
          BirthCohort = input$BC,
          dose = input$dose
        )
      })
    })
  }

  # Run
  shinyApp(ui, server)
}
