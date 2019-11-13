library(shiny)
library(data.table)

# Define UI for miles per gallon application
ui <- fluidPage(
  headerPanel("HADS and NDI Mixed Model"), 
  sidebarPanel(
    p("Select the HADS Depression score at baseline"),
    numericInput(inputId = "hads_depr", label = "HADS Depression score", 1, min=0, max=20, step=1),
    p("Select the HADS Anxiety score at baseline"),
    numericInput(inputId = "hads_anx", label = "HADS Anxiety score", 1,  min=0, max=20, step=1),
    #p("Select the total HADS score"),
    #numericInput(inputId = "hads_tot", label = "HADS total score", 1,  min=0, max=40, step=1),
    p("Select the NDI at baseline"),
    numericInput(inputId = "NDI_0", label = "NDI at baseline", 1,  min=0, max=100, step=1)),


   # p("Select the time moment"),
  #  checkboxGroupInput(inputId = "time",label='time', c(52,104), selected=NULL)),
    
  mainPanel(
    textOutput("case_depr"),
    textOutput("case_anx"),
    #plotOutput("plot1"),
    textOutput("PredVal1"),
    textOutput("PredVal2"),
    plotOutput("plot2")
    
    #verbatimTextOutput(outputId = "time")
    #verbatimTextOutput(outputId = "DepPrint")
    #plotOutput("hist")
  )
)