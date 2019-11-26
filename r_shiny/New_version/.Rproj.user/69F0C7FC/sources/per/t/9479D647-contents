library(shiny)
library(data.table)

# Define UI for miles per gallon application
ui <- fluidPage(
  headerPanel("Functional recovery after ACD, ACDF or ACDF"), 
  sidebarPanel(
    #p("Select the HADS Depression score at baseline (between 0-21)"),
    #numericInput(inputId = "hads_depr", label = "HADS Depression score at baseline", 1, min=0, max=21, 
      #           step=1),
    sliderInput("hads_depr", "Select HADS depression at baseline:",
                min = 0, max = 21,
                value = 10),
    #p("Select the HADS Anxiety score at baseline (between 0-21)"),
    #numericInput(inputId = "hads_anx", label = "HADS Anxiety score at baseline", 1,  min=0, max=21, 
    #             step=1),
    sliderInput("hads_anx", "Select HADS anxiety at baseline:",
                min = 0, max = 21,
                value = 10),
    #p("Select the total HADS score"),
    #numericInput(inputId = "hads_tot", label = "HADS total score", 1,  min=0, max=40, step=1),
    #p("Select the NDI score at baseline (between 0-100)"),
    #numericInput(inputId = "NDI_0", label = "NDI at baseline", 1,  min=0, max=100, step=1)),
    sliderInput("NDI_0", "Select NDI at baseline:",
              min = 0, max = 100,
              value = 50)),


   # p("Select the time moment"),
  #  checkboxGroupInput(inputId = "time",label='time', c(52,104), selected=NULL)),
    
  mainPanel(

    plotOutput("plot2"),
    h6("Disclaimer: The blue lines visualizes the average functional recovery (NDI) of 
       patients after neck surgery, based on the data from the NECK-trail. Therefore, it does
       take the HADS anxiety and depression scores after 52 and 104 weeks in consideration. 
       The gray interval around it is a 95 % confidence interval. Whereas, the red line is 
       the predicted functional recovery of a new patient, based only on its NDI & HADS 
       anxiety and depression scores at baseline."),
    h4("Below the predicted NDI:"),
    #plotOutput("plot1"),
    textOutput("PredVal1"),
    textOutput("PredVal2"),
    h4("HADS dicotomized at baseline:"),
    textOutput("case_depr"),
    textOutput("case_anx")
    #verbatimTextOutput(outputId = "time")
    #verbatimTextOutput(outputId = "DepPrint")
    #plotOutput("hist")
  )
)