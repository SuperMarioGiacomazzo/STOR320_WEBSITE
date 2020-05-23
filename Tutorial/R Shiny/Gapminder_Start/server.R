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
  
})
