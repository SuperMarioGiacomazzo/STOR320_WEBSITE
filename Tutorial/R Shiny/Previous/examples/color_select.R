library(shiny)

color_list <- list(red = c("tomato", "violetred", "firebrick"),
                   green = c("forestgreen", "chartreuse", "olivedrab"),
                   blue = c("dodgerblue", "navy", "torquoise"))

ui <- fluidPage(
    
    radioButtons(inputId = "color_family",
                 label = "Choose a color family",
                 choices = c("red", "green", "blue")),
    
    uiOutput(outputId = "color_selector"),
    
    verbatimTextOutput(outputId = "color_chosen")
    
)

server <- function(input, output) {
    
    output$color_selector <- renderUI({
        
        selectInput(inputId = "color",
                    label = "Select color",
                    choices = color_list[input$color_family])
        
    })
        
    output$color_chosen <- renderText({
        paste("You chose this color:", input$color)
    })
    
    
}

shinyApp(ui = ui, server = server)



