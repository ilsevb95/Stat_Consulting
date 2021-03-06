rm(list = ls())
library(shiny)
library(dplyr)
library(ggplot2)
library(lme4)
library(tidyverse)
library(nlme)
library(emmeans)
library(car)
library(lattice)

##Data preperation 
df_long <- read.csv("data_final_2019-11-05.csv", header=TRUE)
df_prediction <- read.csv("data_final_prediction_2019-11-05.csv")
df_long <- df_long %>% 
  select(id, ndi, time, hads_depr, hads_anx) %>%
  mutate(ndi = as.numeric(ndi), time = as.numeric(time),
         hads_anx = as.numeric(hads_anx), hads_depr = as.numeric(hads_depr), 
         time_fct = as.factor(time))



model_final <- lmer(ndi1_2 ~ ndi0 + hads0_tot+  time_fct + (1|id), 
                    data = df_prediction, 
                    REML = T) 

model_final_emm <- lme4::lmer(ndi ~ time_fct + hads_depr + hads_anx + (1|id), REML= T, 
                              data = df_long)

refgrid <-  ref_grid(model_final_emm)

df_emmeans <-  data.frame(summary(refgrid))
df_emmeans$time <- as.numeric(as.character(df_emmeans$time_fct))
df_emmeans$lwr <- df_emmeans$prediction - 1.96*df_emmeans$SE
df_emmeans$upr <- df_emmeans$prediction + 1.96*df_emmeans$SE



theme <- theme(panel.background = element_blank(),
               panel.grid.major = element_line(colour = "darkgrey", size=0.5),
               panel.grid.minor = element_line(colour = "grey", 
                                               size=.25, 
                                               linetype = "dashed"),
               panel.border = element_blank(),
               axis.line.x = element_line(colour = "black", 
                                          size=0.5, 
                                          lineend = "butt"),
               axis.line.y = element_line(colour = "black", 
                                          size=0.5),
               axis.text=element_text(size=15),
               axis.title=element_text(size=22),
               plot.title = element_text(size = 22),
               strip.text = element_text(size = 15),
               legend.title = element_blank())



shinyServer(function(input, output) {
  hads_anx <- reactive({input$hads_anx})
  hads_depr <- reactive({input$hads_depr})
  hads_tot <- reactive({hads_anx()+hads_depr()})
  pred1 <- reactive({round(ifelse(predict(model_final,newdata=
                                            data.frame(ndi0= input$NDI_0 , hads0_tot= hads_tot(),
                                                       time_fct = 52,id=100), 
                                          allow.new.levels=TRUE, interval="predict") < 0, 0, 
                                  predict(model_final,newdata=
                                            data.frame(ndi0= input$NDI_0 , hads0_tot=hads_tot(),
                                                       time_fct = 52,id=100), 
                                          allow.new.levels=TRUE, interval="predict")))})

  pred2 <- reactive({ifelse(round(predict(model_final,newdata=
                                            data.frame(ndi0= input$NDI_0 , hads0_tot=hads_tot(),
                                                       time_fct = 104,id=100), 
                                          allow.new.levels=TRUE)) <0, 0, 
                            round(predict(model_final,newdata=
                                            data.frame(ndi0= input$NDI_0 , hads0_tot=hads_tot(),
                                                       time_fct = 104,id=100), 
                                          allow.new.levels=TRUE,  interval="predict")))})

  
  case_anxiety <- reactive( if (input$hads_anx<8){
    "Non case"}else if(input$hads_anx>10){"Case"}
    else{"Doubtful case"})
  case_depression <- reactive( if (input$hads_depr<8){
    "Non case"}else if(input$hads_depr>10){"Case"}
    else{"Doubtful case"})
  output$case_anx <- renderText({paste("Level of Anxiety at baseline:", case_anxiety())})
  output$case_depr <- renderText({paste("Level of Depression at baseline:",case_depression())})
  output$PredVal1 <- renderText({paste("Predicted NDI after 1 year:",pred1())})
  output$PredVal2 <- renderText({paste("Predicted NDI after 2 years:",pred2())})
  
  output$plot1 <- renderPlot({
    
    plot(c(52,104),c(as.numeric(pred1()),as.numeric(pred2())), xlab = "Time (weeks)", 
         ylab = "NDI predicted", 
         type='l')
  })
  
  output$plot2 <- renderPlot({
    
    ggplot() + geom_point() + 
      geom_line(data=df_emmeans, aes(x=time, y=prediction), col = "blue", size = 2) +
      geom_line(aes(x=c(0,52,104),y=c(input$NDI_0,as.numeric(pred1()),as.numeric(pred2()))), 
                col= "red", size=2)+
      geom_point(aes(x=c(0,52,104),y=c(input$NDI_0,as.numeric(pred1()),as.numeric(pred2()))), 
                 col= "red", size=4) +
      geom_point(data = df_emmeans, aes(x = time, y = prediction), col = "blue", size = 4) +
      geom_ribbon(data=df_emmeans, aes(x=time,ymin= lwr, ymax= upr), alpha=0.3) + 
      theme +
      xlab("Time (weeks)") + ylab("NDI") + ylim(c(0,100))
  })
})

