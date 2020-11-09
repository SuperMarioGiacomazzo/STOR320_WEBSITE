library(shiny)
library(tidyverse)
library(broom)
library(purrr)
library(gapminder)

gapminder2=gapminder
names(gapminder2)=c("Country","Continent","Year","Life Expectancy","Population","GDP")

shinyServer(function(input, output) {
  
  #Build Input Based Off Country Names 
  output$OUTcountry <- renderUI({
      selectizeInput(inputId='INcountry',
                  label="Available Countries",
                  choices=gapminder2$Country,
                  selected="United States",
                  multiple=TRUE,
                  options=list())
  })
  
  #Build Input Based Off Variable 
  output$OUTvariable <- renderUI({
    radioButtons(inputId='INvariable',
                label="Select Variable",
                choices=names(gapminder2)[4:6])
  })
  
  #Display the Options Selected By the User
  output$OUTsynopsis1<-renderText({
    expr=paste("User:",input$name)
  })
  output$OUTsynopsis2<-renderText({
    expr=paste("Countries Selected:",paste(input$INcountry,collapse=", "))
  })
  output$OUTsynopsis3<-renderText({
    expr=paste("Variable for Analysis:",input$INvariable)
  })
  
  
  #Part 1: Create a Table Previewing Data
  output$OUTpreview<-renderTable({
    gapminder2 %>% 
      select(Country,Continent,Year,input$INvariable)%>% 
      filter(Country %in% input$INcountry) %>%
      arrange(Year)
  })
  
  #Part 2: Create a Table Summarizing Data by Country
  output$OUTsummary<-renderTable({
    gapminder2 %>% 
      select(Country,Continent,Year,input$INvariable)%>% 
      filter(Country %in% input$INcountry) %>%
      arrange(Year)%>%
      group_by(Country) %>%
      summarize(N=n(),
            MIN=min(get(input$INvariable)),
            Q1=quantile(get(input$INvariable),0.25),
            Q2=quantile(get(input$INvariable),0.5),
            Q3=quantile(get(input$INvariable),0.75),
            MAX=max(get(input$INvariable)),
            CHANGE=MAX-MIN,
            MEAN=mean(get(input$INvariable)),
            SD=sd(get(input$INvariable))
      )
  })
  
  #Part 3: Create a Graphic Showing Trends
  output$OUTtrendvar<-renderText({
    expr=paste("Trend Comparison for",input$INvariable)
  })
  
  output$OUTtrendplot<-renderPlot({
    gapminder2 %>% 
      select(Country,Continent,Year,input$INvariable)%>% 
      filter(Country %in% input$INcountry) %>%
      arrange(Year)%>%
      ggplot(aes(x=Year,y=get(input$INvariable))) +
      geom_line(aes(color=Country),size=input$width)+
      ylab(input$INvariable) +
      theme_minimal()
  })
  
  
  
  
})
