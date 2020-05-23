#------------------------------------------------------------------------------#
# Coursera Data Products Class Project
# Kristen Borchert
# Burpee Calculator
# server.R
#------------------------------------------------------------------------------#

library(tidyverse)


#------------------------------------------------------------------------------#
# Read in the beer calorie data
#------------------------------------------------------------------------------#

beer_data <- read_csv("beerdata.csv")
# A tibble: 46 × 4
#                               Beer Calories Carbs AlcoholPercent
#                              <chr>    <int> <dbl>          <dbl>
# 1                     Amstel Light       95   5.0            3.5
# 2                     Anchor Steam      155  16.0            4.9
# 3    Anheuser-Busch Ice Pale Lager      171  12.5            5.9
# 4  Anheuser-Busch Light Pale Lager       95   3.2            4.1
# 5                   Beck's Pilsner      138   9.0            5.0
# 6             Beck's Premier Light       63   3.8            2.3
# 7                        Budweiser      145  10.6            5.0
# 8                        Bud Light      110   6.6            4.2
# 9                 Budweiser Select       99   3.1            4.3
# 10             Budweiser Select 55       55   1.8            2.4
# ... with 36 more rows


#------------------------------------------------------------------------------#
# Calculate calories burned per burpee
#------------------------------------------------------------------------------#

get_burpee_calories <- function(weight_lb) {
    
    # Assumptions
    #     1) Each Burpee consumes 3.5 ml of O2 per kg of weight.
    #     2) 1L of oxygen consumed = 5 kcals
    
    # Convert weight in lbs to kg
    weight_kg <- (weight_lb / 2.2046)
    
    # Calculate calories burned per Burpee
    burpee_calories <- weight_kg * 3.5/1000 * 5
    
    return(burpee_calories)
    
}


#------------------------------------------------------------------------------#
# Calculate total calories consumed from beer
#------------------------------------------------------------------------------#

get_beer_calories <- function(beer, quantity) {
    
    # Retrieve calories for selected beer
    beer_calories <- beer_data %>% 
        filter(Beer == beer) %>% 
        select(Calories) %>% 
        unlist(., use.names = FALSE)
    
    # Calculate total calories consumed
    total_calories <- beer_calories * quantity
    
    return(total_calories)
    
}


#------------------------------------------------------------------------------#
# Calculate number of burpees to offset beer consumption
#------------------------------------------------------------------------------#

calc_n_burpees <- function(weight_lb, beer, quantity){
    
    burpee_cal <- get_burpee_calories(weight_lb)
    beer_cal <- get_beer_calories(beer, quantity)
    n_burpees <- round(beer_cal / burpee_cal)
    
    return(n_burpees)
    
}


#------------------------------------------------------------------------------#
# Shiny server
#------------------------------------------------------------------------------#

shinyServer(
    function(input, output) {
        
        output$beer_selector <- renderUI({
            selectInput(inputId = 'beer', 
                        label = "Type of Beer Consumed:",
                        choices = beer_data$Beer)
        })
        
        output$weight <- renderText(
            paste({input$weight}, "lbs")
        )
        
        output$burpee_calories <- renderText({
            get_burpee_calories(input$weight)
        })
        
        output$beer_calories <- renderText({
            if(!is.null(input$beer)){
                get_beer_calories(input$beer, input$quantity)
            }
        })
        
        output$n_burpees <- renderText({
            if(!is.null(input$beer)){
                calc_n_burpees(input$weight, input$beer, input$quantity)
            }
        })
        
    }
)













