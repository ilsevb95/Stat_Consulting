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


df_long<- read.csv("data_final_2019-11-04.csv", header=TRUE)
df_long <- df_long %>% 
  mutate(hads_tot = hads_depr + hads_anx) %>%
  select(id, ndi, time, hads_tot) %>%
  mutate(ndi = as.numeric(ndi), time = as.numeric(time),
         hads_tot = as.numeric(hads_tot), time_fct = as.factor(time))


df_time0 <- df_long[df_long$time == 0, ]
df_time0 <- select(df_time0, id, ndi, hads_tot)
colnames(df_time0) <-  c("id", "ndi0", "hads0")
# Create new dataframe with HADS and NDI at 52 and 104 weeks
df_time1_2 <- df_long[df_long$time != 0, ]
df_time1_2 <- select(df_time1_2, id, ndi, time)
colnames(df_time1_2) <-  c("id", "ndi1_2", "time")
# Merge dataframes
df_prediction <- merge(df_time1_2, df_time0, by = "id")


model_final <- lmer(ndi1_2 ~ ndi0 + hads0 +  time + (1|id), data = df_prediction, REML = T) 

#model_final <- lme4::lmer(ndi ~ time_fct + hads_tot + (1|id), REML= T, data = df_long)
model_final_emm <- lme4::lmer(ndi ~ time_fct + hads_tot + (1|id), REML= T, data = df_long)

refgrid <-  ref_grid(model_final_emm)

df_emmeans <-  data.frame(summary(refgrid))
df_emmeans$time <- as.numeric(as.character(df_emmeans$time_fct))
df_emmeans$lwr <- df_emmeans$prediction - 1.96*df_emmeans$SE
df_emmeans$upr <- df_emmeans$prediction + 1.96*df_emmeans$SE



theme <-  theme(#panel.background = element_rect(fill = "lightgrey", colour = "white", size = 4),
  axis.text=element_text(size=15),
  axis.title=element_text(size=22),
  plot.title = element_text(size = 22),
  strip.text = element_text(size = 15),
  legend.title = element_blank())


shinyServer(function(input, output) {
  #output$time=input$time
  hads_tot <- reactive({input$hads_anx+input$hads_depr})
  pred1 <- reactive({predict(model_final,newdata=
                               data.frame(ndi0= input$NDI_0 , hads0=hads_tot(),time = 52,id=100), allow.new.levels=TRUE, interval="predict")})
  pred2 <- reactive({predict(model_final,newdata=
                               data.frame(ndi0= input$NDI_0 , hads0=hads_tot(),time = 104,id=100), allow.new.levels=TRUE)})
  
  case_anxiety <- reactive( if (input$hads_anx<8){
    "Non case"}else if(input$hads_anx>10){"Case"}
    else{"Doubtful case"})
  case_depression <- reactive( if (input$hads_depr<8){
    "Non case"}else if(input$hads_depr>10){"Case"}
    else{"Doubtful case"})
  output$case_anx <- renderText({paste("Level of Anxiety:",case_anxiety())})
  output$case_depr <- renderText({paste("Level of Depression:",case_depression())})
  output$PredVal1 <- renderText({paste("Predicted NDI after 1 year:",pred1())})
  output$PredVal2 <- renderText({paste("Predicted NDI after 2 years:",pred2())})
  
  #output$PredVal <- renderText(pred)
  #x <- c(as.numeric(pred1),as.numeric(pred2))
  #y <- c(52,104)
  output$plot1 <- renderPlot({
    
    plot(c(52,104),c(as.numeric(pred1()),as.numeric(pred2())), xlab = "NDI Predicted", ylab = "Time", type='l')
  })
  
  output$plot2 <- renderPlot({
    
    ggplot() + geom_point() + 
      geom_line(data=df_emmeans, aes(x=time, y=prediction), col = "blue", size = 2) +
      geom_line(aes(x=c(0,52,104),y=c(input$NDI_0,as.numeric(pred1()),as.numeric(pred2()))), col= "red", size=2)+
      #geom_point(data = data, aes(x = time, y = ndi, col = id, size = 0.1)) +
      geom_ribbon(data=df_emmeans, aes(x=time,ymin= lwr, ymax= upr), alpha=0.3) + 
      theme(legend.position = "none") + theme + 
      ggtitle("Estimated marginal mean of NDI over time") +
      xlab("Predicted NDI")
  })
  })

#data = df_emmeans, aes(x = time, y = prediction )
