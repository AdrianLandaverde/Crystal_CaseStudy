library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(corrplot)
library(rsconnect)

colorStatus="success"

df_energy= read.csv("energy__data-2.csv",fileEncoding="UTF-8-BOM")
df_energy$Day=floor((((df_energy$Hour-1)/24)+1))
df_energy$energy_consumpt_2005[is.na(df_energy$energy_consumpt_2005)]<- mean(df_energy$energy_consumpt_2005, na.rm=TRUE)
df_energy$energy_consumpt_2006[is.na(df_energy$energy_consumpt_2006)]<- mean(df_energy$energy_consumpt_2006, na.rm=TRUE)

df_energy2005= df_energy[,c(1,2,4,5,8)]
df_energy2005$Date=as.Date(df_energy$Day-1,"2005-01-01")

df_energy2006= df_energy[,c(1,3,6,7,8)]
df_energy2006$Date=as.Date(df_energy$Day-1,"2006-01-01")

df_energy2005= df_energy2005[df_energy2005$Hour<=8760,]
df_energy2006= df_energy2006[df_energy2006$Hour<=8760,]

df_energy2005$Day= strtoi(format(df_energy2005$Date,"%j"),base=10)
df_energy2005$DayOfMonth= strtoi(format(df_energy2005$Date,"%d"),base=10)
df_energy2005$DayOfWeek= format(df_energy2005$Date,"%u")

df_energy2005$Week= strtoi(format(df_energy2005$Date,"%W"),base=10)
df_energy2005$WeekOfMonth=ceiling(as.numeric(format(df_energy2005$Date, "%d")) / 7)

df_energy2005$Month=strtoi(format(df_energy2005$Date,"%m"),base=10)

df_energy2005$HourOfDay= (df_energy2005$Hour)-(df_energy2005$Day-1)*24
df_energy2005$HourOfWeek= (df_energy2005$Hour+(24*5))-(df_energy2005$Week)*(7*24)

df_energy2006$Day= strtoi(format(df_energy2006$Date,"%j"),base=10)
df_energy2006$DayOfMonth= strtoi(format(df_energy2006$Date,"%d"),base=10)
df_energy2006$DayOfWeek= format(df_energy2006$Date,"%u")

df_energy2006$Week= strtoi(format(df_energy2006$Date,"%W"),base=10)
df_energy2006$WeekOfMonth=ceiling(as.numeric(format(df_energy2006$Date, "%d")) / 7)

df_energy2006$Month=strtoi(format(df_energy2006$Date,"%m"),base=10)

df_energy2006$HourOfDay= (df_energy2006$Hour)-(df_energy2006$Day-1)*24
df_energy2006$HourOfWeek= (df_energy2006$Hour+(24*5))-(df_energy2006$Week)*(7*24)

sidebar <- dashboardSidebar(
  width=300,
  sidebarMenu(
    hr(),
    menuItem(h3("Behaviour in Time"), tabName = "Time",
             selectInput(inputId = ("TP_Time"), label = h4("Time Lapse"), 
                         choices = list("Hour"="Hour", "Day"="Day",
                                        "Week"="Week", "Month"="Month"), 
                         selected = "Day"),
             br(),
             
             selectInput(inputId = ("TP_Variable"), label = h4("Variable"), 
                         choices = list("Energy" = "energy_consumpt_", 
                                        "Temperature" = "full_temp_", 
                                        "Humidity" = "full_humid_"),
                         selected="energy_consumpt_"),
             br(),
             selectInput(inputId = ("TP_SubTime"), label = h4("Sub-period Lapse"), 
                         choices = list("Hour of the Day" = "HourOfDay", 
                                        "Hour of the Week" = "HourOfWeek", 
                                        "Day of the Week" = "DayOfWeek",
                                        "Day of the Month"= "DayOfMonth",
                                        "Week of the Month"="WeekOfMonth"),
                         selected="HourOfDay"),
             sliderInput("TP_Range", label = h4("Sub-Period Quantile Range"), min = 0, 
                         max = 100, value = c(0, 100))),
    
    menuItem(h3("Distribution Plot"), tabName = "Time",
             selectInput(inputId = ("Dis_Time"), label = h4("Time Lapse"), 
                         choices = list("Hour"="Hour", "Day"="Day",
                                        "Week"="Week", "Month"="Month"), 
                         selected = "Day"),
             br(),
             
             selectInput(inputId = ("Dis_Variable"), label = h4("Variable"), 
                         choices = list("Energy" = "energy_consumpt_", 
                                        "Temperature" = "full_temp_", 
                                        "Humidity" = "full_humid_"),
                         selected="energy_consumpt_"),
             br(),
             sliderInput("Dis_Range", label = h4("Quantile Range"), min = 0, 
                         max = 100, value = c(0, 100))),
    menuItem(h3("Correlation"), tabName = "Time",
             selectInput(inputId = ("Corr_Time"), label = h4("Time Lapse"), 
                         choices = list("Hour"="Hour", "Day"="Day",
                                        "Week"="Week", "Month"="Month",
                                        "Hour of the Day" = "HourOfDay", 
                                        "Hour of the Week" = "HourOfWeek", 
                                        "Day of the Week" = "DayOfWeek",
                                        "Day of the Month"= "DayOfMonth",
                                        "Week of the Month"="WeekOfMonth"), 
                         selected = "Day"),
             
             selectInput(inputId = ("Corr_Var1"),
                         label = h4("Variable 1"), 
                         choices = list("Energy" = "energy_consumpt_", 
                                        "Temperature" = "full_temp_", 
                                        "Humidity" = "full_humid_"), 
                         selected = "full_humid_"),
             
             selectInput(inputId = ("Corr_Var2"), label = h4("Variable 2"), 
                         choices = list("Energy" = "energy_consumpt_", 
                                        "Temperature" = "full_temp_", 
                                        "Humidity" = "full_humid_"), 
                         selected = "energy_consumpt_"),
             
             selectInput(inputId = ("Corr_Year"), label = h4("Years"), 
                         choices = list("2005" = 1, 
                                        "2006" = 2, 
                                        "Both" = 3), 
                         selected = 3)),hr()
    
  )
)

body <- dashboardBody(
  h1("Behaviour in Time"),
  fluidRow(
    column(width=12,
           box(title = ("Period of the Year"), width = 12,
               status = colorStatus, solidHeader = TRUE, collapsible = TRUE,
               column(width = 12,plotOutput("TimePlot"))),
           
           box(title = ("Subperiod of the Year"), width = 12,
               status = colorStatus, solidHeader = TRUE, collapsible = TRUE,
               column(width = 12,plotOutput("SubTimePlot")))
           )
    ),
  h1("Distribution"),
    fluidRow(
    column(width=12,
           box(title = ("Distribution Histogram"), width = 6,
               status = colorStatus, solidHeader = TRUE, collapsible = TRUE,
               column(width = 12,plotOutput("DistHist"))),
           
           box(title = ("Distribution Box Plot"), width = 6,
               status = colorStatus, solidHeader = TRUE, collapsible = TRUE,
               column(width = 12,plotOutput("DistBox")))
           )
    ),
  h1("Correlation"),
  fluidRow(
    column(width=12,
           box(title = ("Correlation Matrix"), width = 6,
               status = colorStatus, solidHeader = TRUE, collapsible = TRUE,
               column(width = 12,plotOutput("Correlation"))),
           
           box(title = ("Correlation of 2 Variables"), width = 6,
               status = colorStatus, solidHeader = TRUE, collapsible = TRUE,
               column(width = 12,plotOutput("Correlation2")))
    )
  )
  )

header<- dashboardHeader(title="Data Analysis", titleWidth = 300)

ui <- dashboardPage(header, sidebar,body, skin = "green")

server <- function(input, output) {
  
  output$TimePlot<- renderPlot({
    time<- input$TP_Time 
    variable<- input$TP_Variable
    
    
    names(df_energy2005)[names(df_energy2005)==time]<- "Time"
    names(df_energy2006)[names(df_energy2006)==time]<- "Time"
    
    
    df_energyTemp2005<- df_energy2005 %>% group_by(Time) %>%
      summarise(energy_consumpt_2005= mean(energy_consumpt_2005),
                full_temp_2005= mean(full_temp_2005),
                full_humid_2005= mean(full_humid_2005))
    
    df_energyTemp2006<- df_energy2006 %>% group_by(Time) %>%
      summarise(energy_consumpt_2006= mean(energy_consumpt_2006),
                full_temp_2006= mean(full_temp_2006),
                full_humid_2006= mean(full_humid_2006))
    
    df_energyF<- merge(x = df_energyTemp2005, y = df_energyTemp2006, all = TRUE)
    
    
    names(df_energyF)[names(df_energyF)==paste(variable,"2005",sep="")]<- "Variable2005"
    names(df_energyF)[names(df_energyF)==paste(variable,"2006",sep="")]<- "Variable2006"
    
    
    if(variable=="energy_consumpt_"){
      variable="Energy"
    } else if(variable=="full_temp_"){
      variable="Temperature"
    } else {
      variable="Humidity"
    }
    
    if(time!="Hour"){
      variable= paste("Mean",variable)
    }
    
    ggplot(df_energyF, aes(x=Time)) +
      geom_line(aes(y=Variable2005,colour = "2005")) +
      geom_line(aes(y=Variable2006,colour = "2006")) +
      labs(x = time, y=variable,
           title = paste(variable,"per",time))
    }
  )
  
  output$DistBox<- renderPlot({
    
    time<- input$Dis_Time 
    variable<- input$Dis_Variable
    rangeQ<- input$Dis_Range
    
    
    names(df_energy2005)[names(df_energy2005)==time]<- "Time"
    names(df_energy2006)[names(df_energy2006)==time]<- "Time"
    
    
    df_energyTemp2005<- df_energy2005 %>% group_by(Time) %>%
      summarise(energy_consumpt_2005= mean(energy_consumpt_2005),
                full_temp_2005= mean(full_temp_2005),
                full_humid_2005= mean(full_humid_2005))
    
    df_energyTemp2006<- df_energy2006 %>% group_by(Time) %>%
      summarise(energy_consumpt_2006= mean(energy_consumpt_2006),
                full_temp_2006= mean(full_temp_2006),
                full_humid_2006= mean(full_humid_2006))
    
    df_energyF<- merge(x = df_energyTemp2005, y = df_energyTemp2006, all = TRUE)
    
    
    names(df_energyF)[names(df_energyF)==paste(variable,"2005",sep="")]<- "Variable2005"
    names(df_energyF)[names(df_energyF)==paste(variable,"2006",sep="")]<- "Variable2006"
    
    
    
    df_energyFHist<- data.frame(Time= c(df_energyF$Time,df_energyF$Time),
                                Variable= c(df_energyF$Variable2005,df_energyF$Variable2006),
                                Year= c(rep("2005",dim(df_energyF)[1]),rep("2006",dim(df_energyF)[1])))
    
    if(variable=="energy_consumpt_"){
      variable="Energy"
    } else if(variable=="full_temp_"){
      variable="Temperature"
    } else {
      variable="Humidity"
    }
    
    if(time!="Hour"){
      variable= paste("Mean",variable)
    }
    
    ggplot(df_energyFHist, aes(x=Year, y=Variable)) + 
      geom_boxplot(aes(fill=Year))+
      scale_y_continuous(limits = quantile(df_energyFHist$Variable, c(rangeQ[1]/100, rangeQ[2]/100)))+
      labs(x = "Year", y=variable,
           title = paste(variable,"per",time))
  })
  
  output$DistHist<- renderPlot({
    
    time<- input$Dis_Time 
    variable<- input$Dis_Variable
    rangeQ<- input$Dis_Range
    
    
    names(df_energy2005)[names(df_energy2005)==time]<- "Time"
    names(df_energy2006)[names(df_energy2006)==time]<- "Time"
    
    
    df_energyTemp2005<- df_energy2005 %>% group_by(Time) %>%
      summarise(energy_consumpt_2005= mean(energy_consumpt_2005),
                full_temp_2005= mean(full_temp_2005),
                full_humid_2005= mean(full_humid_2005))
    
    df_energyTemp2006<- df_energy2006 %>% group_by(Time) %>%
      summarise(energy_consumpt_2006= mean(energy_consumpt_2006),
                full_temp_2006= mean(full_temp_2006),
                full_humid_2006= mean(full_humid_2006))
    
    df_energyF<- merge(x = df_energyTemp2005, y = df_energyTemp2006, all = TRUE)
    
    
    names(df_energyF)[names(df_energyF)==paste(variable,"2005",sep="")]<- "Variable2005"
    names(df_energyF)[names(df_energyF)==paste(variable,"2006",sep="")]<- "Variable2006"
    
    
    
    df_energyFHist<- data.frame(Time= c(df_energyF$Time,df_energyF$Time),
                                Variable= c(df_energyF$Variable2005,df_energyF$Variable2006),
                                Year= c(rep("2005",dim(df_energyF)[1]),rep("2006",dim(df_energyF)[1])))
    
    if(variable=="energy_consumpt_"){
      variable="Energy"
    } else if(variable=="full_temp_"){
      variable="Temperature"
    } else {
      variable="Humidity"
    }
    
    if(time!="Hour"){
      variable= paste("Mean",variable)
    }
    
    ggplot(df_energyFHist,aes(x=Variable,fill=Year)) + 
      geom_histogram(alpha = 0.5) +
      scale_fill_manual(name="Year",values=c("red","blue"),labels=c("2005","2006"))+
      labs(x = variable, title = paste("Distribution of",variable,"per",time))
  })
  
  output$SubTimePlot<- renderPlot({
    subPeriod<- input$TP_SubTime
    variable<- input$TP_Variable
    rangeQ<- input$TP_Range
    
    if(subPeriod=="HourOfDay"){
      period="Hour of the Day"
      names(df_energy2005)[names(df_energy2005)==subPeriod]<- "SubPeriod"
      names(df_energy2005)[names(df_energy2005)=="Day"]<- "Period"
      
      names(df_energy2006)[names(df_energy2006)==subPeriod]<- "SubPeriod"
      names(df_energy2006)[names(df_energy2006)=="Day"]<- "Period"
    
      } else if(subPeriod=="HourOfWeek"){
        period="Hour of the Week"
        names(df_energy2005)[names(df_energy2005)==subPeriod]<- "SubPeriod"
        names(df_energy2005)[names(df_energy2005)=="Week"]<- "Period"
        
        names(df_energy2006)[names(df_energy2006)==subPeriod]<- "SubPeriod"
        names(df_energy2006)[names(df_energy2006)=="Week"]<- "Period"
    
      }else if(subPeriod=="DayOfWeek"){
        period="Day of the Week"
        names(df_energy2005)[names(df_energy2005)==subPeriod]<- "SubPeriod"
        names(df_energy2005)[names(df_energy2005)=="Week"]<- "Period"
        
        names(df_energy2006)[names(df_energy2006)==subPeriod]<- "SubPeriod"
        names(df_energy2006)[names(df_energy2006)=="Week"]<- "Period"
      
      } else if(subPeriod=="DayOfMonth"){
        period="Day of the Month"
        names(df_energy2005)[names(df_energy2005)==subPeriod]<- "SubPeriod"
        names(df_energy2005)[names(df_energy2005)=="Month"]<- "Period"
        
        names(df_energy2006)[names(df_energy2006)==subPeriod]<- "SubPeriod"
        names(df_energy2006)[names(df_energy2006)=="Month"]<- "Period"
      
      } else if(subPeriod=="WeekOfMonth"){
        period="Week of the Month"
        names(df_energy2005)[names(df_energy2005)==subPeriod]<- "SubPeriod"
        names(df_energy2005)[names(df_energy2005)=="Month"]<- "Period"
        
        names(df_energy2006)[names(df_energy2006)==subPeriod]<- "SubPeriod"
        names(df_energy2006)[names(df_energy2006)=="Month"]<- "Period"
      }
    
    df_energyTemp2005<- df_energy2005 %>% group_by(SubPeriod,Period) %>%
      summarise(energy_consumpt_2005= mean(energy_consumpt_2005),
                full_temp_2005= mean(full_temp_2005),
                full_humid_2005= mean(full_humid_2005))
    
    df_energyTemp2006<- df_energy2006 %>% group_by(SubPeriod,Period) %>%
      summarise(energy_consumpt_2006= mean(energy_consumpt_2006),
                full_temp_2006= mean(full_temp_2006),
                full_humid_2006= mean(full_humid_2006))
    
    names(df_energyTemp2005)[names(df_energyTemp2005)==paste(variable,"2005",sep="")]<- "Variable2005"
    names(df_energyTemp2006)[names(df_energyTemp2006)==paste(variable,"2006",sep="")]<- "Variable2006"
    
    
    df_energyFSubHist<- data.frame(Period= c(df_energyTemp2005$SubPeriod,df_energyTemp2006$SubPeriod),
                                   Variable= c(df_energyTemp2005$Variable2005, 
                                             df_energyTemp2006$Variable2006),
                                   Year= c(rep("2005",dim(df_energyTemp2005)[1]),
                                           rep("2006",dim(df_energyTemp2006)[1])))
    
    df_energyFSubHist$Period <- sprintf("%02d", as.numeric(df_energyFSubHist$Period))
    
    if(variable=="energy_consumpt_"){
      variable="Energy"
    } else if(variable=="full_temp_"){
      variable="Temperature"
    } else {
      variable="Humidity"
    }
    
    ggplot(df_energyFSubHist, aes(x=Period, y=Variable)) + 
      geom_boxplot(aes(fill=Period))+
      scale_y_continuous(limits = quantile(df_energyFSubHist$Variable, c(rangeQ[1]/100, rangeQ[2]/100)))+
      labs(x = period, y=variable,
           title = paste("Distribution of",variable,"per",period))
  })
  
  output$Correlation<- renderPlot({
    time<- input$Corr_Time 
    
    names(df_energy2005)[names(df_energy2005)==time]<- "Time"
    names(df_energy2006)[names(df_energy2006)==time]<- "Time"
    
    
    df_energyTemp2005<- df_energy2005 %>% group_by(Time) %>%
      summarise(energy_consumpt_2005= mean(energy_consumpt_2005),
                full_temp_2005= mean(full_temp_2005),
                full_humid_2005= mean(full_humid_2005))
    
    df_energyTemp2006<- df_energy2006 %>% group_by(Time) %>%
      summarise(energy_consumpt_2006= mean(energy_consumpt_2006),
                full_temp_2006= mean(full_temp_2006),
                full_humid_2006= mean(full_humid_2006))
    
    df_energyF<- merge(x = df_energyTemp2005, y = df_energyTemp2006, all = TRUE)
    
    
    colnames(df_energyF)<- c("Time","Energy_2005","Temp_2005","Humid_2005","Energy_2006","Temp_2006","Humid_2006")
    
  
    corrplot(cor(select(df_energyF,-Time)), method="number",order = 'AOE')
  })
  
  output$Correlation2<- renderPlot({
    time<- input$Corr_Time 
    var1<- input$Corr_Var1
    var2<- input$Corr_Var2
    
    names(df_energy2005)[names(df_energy2005)==time]<- "Time"
    names(df_energy2006)[names(df_energy2006)==time]<- "Time"
    
    df_energyTemp2005<- df_energy2005 %>% group_by(Time) %>%
      summarise(energy_consumpt_2005= mean(energy_consumpt_2005),
                full_temp_2005= mean(full_temp_2005),
                full_humid_2005= mean(full_humid_2005))
    
    df_energyTemp2006<- df_energy2006 %>% group_by(Time) %>%
      summarise(energy_consumpt_2006= mean(energy_consumpt_2006),
                full_temp_2006= mean(full_temp_2006),
                full_humid_2006= mean(full_humid_2006))
    
    df_energyF<- merge(x = df_energyTemp2005, y = df_energyTemp2006, all = TRUE)
    
    names(df_energyF)[names(df_energyF)==paste(var1,"2005",sep="")]<- "Var1_2005"
    names(df_energyF)[names(df_energyF)==paste(var2,"2005",sep="")]<- "Var2_2005"
    names(df_energyF)[names(df_energyF)==paste(var1,"2006",sep="")]<- "Var1_2006"
    names(df_energyF)[names(df_energyF)==paste(var2,"2006",sep="")]<- "Var2_2006"
    
    if(var1=="energy_consumpt_"){
      var1="Energy"
    } else if(var1=="full_temp_"){
      var1="Temperature"
    } else {
      var1="Humidity"
    }
    
    if(var2=="energy_consumpt_"){
      var2="Energy"
    } else if(var2=="full_temp_"){
      var2="Temperature"
    } else {
      var2="Humidity"
    }
    
    year<- input$Corr_Year
    if(year==1){
      ggplot(df_energyF) +
        geom_line(aes(x=Var1_2005,y=Var2_2005,colour = "2005")) +
        labs(x = var1, y=var2,
             title = paste(variable,"per",time))  
    } else if(year==2){
      ggplot(df_energyF) +
        geom_line(aes(x=Var1_2006,y=Var2_2006,colour = "2006")) +
        labs(x = var1, y=var2,
             title = paste(variable,"per",time))  
    } else{
      ggplot(df_energyF) +
        geom_line(aes(x=Var1_2005,y=Var2_2005,colour = "2005")) +
        geom_line(aes(x=Var1_2006,y=Var2_2006,colour = "2006")) +
        labs(x = var1, y=var2,
             title = paste("Correlation in ",time))  
    }
    
  })
}


shinyApp(ui = ui, server = server)