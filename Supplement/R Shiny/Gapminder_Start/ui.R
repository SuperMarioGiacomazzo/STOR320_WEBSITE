library(shiny)

css <- HTML(
  "h4{
                text-indent: 20px;
  }"
)

shinyUI(
  fluidPage(
      
      tags$head(tags$style(css)),
    
      #Name of Shiny App
      titlePanel("Analysis of Gapminder Data"),
      
      #Specifies the Type of Layout
      sidebarLayout(
        
        #Part of "sidebarLayout"
        sidebarPanel(
          helpText("Instructions: Select Countries of Interest for 
                   Comparison and Analysis For Key Variables "),
          br(),
          #Input to Enter Name
          textInput(inputId="name",label="First Name"),
          
          #Input to Select Country
          uiOutput(outputId="OUTcountry"),
          
          #Input to Select Variable
          uiOutput(outputId="OUTvariable"),
          
          #Submit Button For Updates
          submitButton("Stay Woke!")
        ),
        
        #Part of "sidebarLayout"
        mainPanel("",
            
          #Splits Main Panel Output Using Tabs         
          tabsetPanel(
            
            #First Tab Gives a Preview of the Data Selected
            tabPanel("Summary",
              h2("Synopsis"),
              br(),
              h4(textOutput("OUTsynopsis1")),
              h4(textOutput("OUTsynopsis2")),
              h4(textOutput("OUTsynopsis3")),
              br(),
              
              h2("Data Selected"),
              br(),
              
              br(),
              
              h2("Country Comparison"),
              br(),
              
              br()
              
            ),
            tabPanel("Graphics"
                     
            )
            
          )
          
        )
        
      )
      
  )
  
)
