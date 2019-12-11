library(shiny)
library(data.table)

ui <- fluidPage(
  headerPanel("Functional recovery after ACD, ACDF or ACDF"), 
  sidebarPanel(

    sliderInput("hads_depr", "Select HADS depression at baseline:",
                min = 0, max = 21,
                value = 10),

    sliderInput("hads_anx", "Select HADS anxiety at baseline:",
                min = 0, max = 21,
                value = 10),
   
    sliderInput("NDI_0", "Select NDI at baseline:",
                min = 0, max = 100,
                value = 50)),
  
  

  mainPanel(
    
    plotOutput("plot2"),
    h6("Disclaimer: The blue lines visualizes the average functional recovery (NDI) of 
       patients after neck surgery, based on the data from the NECK-trail. Therefore, it does
       take the HADS anxiety and depression scores after 52 and 104 weeks in consideration. 
       The gray interval around it is a 95 % confidence interval. Whereas, the red line is 
       the predicted functional recovery of a new patient, based only on its NDI & HADS 
       anxiety and depression scores at baseline."),
    h4("Below the predicted NDI:"),
    textOutput("PredVal1"),
    textOutput("PredVal2")
    #h4("HADS dicotomized at baseline:"),
    #textOutput("case_depr"),
    #textOutput("case_anx")
    )
  )