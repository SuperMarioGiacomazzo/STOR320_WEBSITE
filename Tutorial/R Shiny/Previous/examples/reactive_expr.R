library(shiny)

ui <- fluidPage(
    
    sliderInput(inputId = "rate",
                label = "Enter rate",
                min = 0,
                max = 1, 
                value = 0.5),
    
    textOutput(outputId = "pct_std"),
    
    textOutput(outputId = "pct_half")
    
)

server <- function(input, output) {
    
    my_percent <- reactive({
        input$rate * 100
    })
    
    my_half_percent <- reactive({
        my_percent() / 2
    })
    
    output$pct_std <- renderText({
        paste0("This is my percent: ", my_percent(), "%")
    })
    
    output$pct_half <- renderText({
        paste0("This is my percent by half: ", my_half_percent(), "%")
    })
    
}

shinyApp(ui = ui, server = server)



