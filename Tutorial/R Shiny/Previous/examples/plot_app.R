library(shiny)
library(ggplot2)

ui <- fluidPage(
    
    titlePanel("Hello Shiny!"),
    
    # Sidebar with a slider input for the number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "bins",
                        label = "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput(outputId = "distPlot")
        )
    )
    
)

server <- function(input, output) {
    
    # Expression that generates a histogram. The expression is
    # wrapped in a call to renderPlot to indicate that:
    #
    #  1) It is "reactive" and therefore should be automatically
    #     re-executed when inputs change
    #  2) Its output type is a plot
    
    output$distPlot <- renderPlot({
        
        ggplot(faithful, aes(x = waiting)) +
            geom_histogram(bins = input$bins, colour = "white")
        
    })
    
}

shinyApp(ui = ui, server = server)