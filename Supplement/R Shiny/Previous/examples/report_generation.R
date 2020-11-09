library(shiny)
library(rmarkdown)

ui <- fluidPage(
    
    textInput(inputId = 'user_name',
              label = 'Name'),
    
    radioButtons(inputId = 'format', 
                 label = 'Run Report Format', 
                 c('PDF', 'HTML', 'Word'),
                 inline = TRUE),
    
    downloadButton('generate_report')
    
)

server <- function(input, output) {
    
    output$generate_report <- downloadHandler(
        
        filename = function() {
            
            paste0("my_report", ".",
                   switch(
                       input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
                   ))
            
        },
        
        content = function(file) {
            
            markdownFile <- 'report.Rmd'
            src <- normalizePath(markdownFile)

            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, markdownFile, overwrite = TRUE)

            out <- render(markdownFile, switch(
                input$format,
                PDF = pdf_document(), HTML = html_document(), Word = word_document()
            ))
            file.rename(out, file)
            
        }
    )
    
}

shinyApp(ui = ui, server = server)
