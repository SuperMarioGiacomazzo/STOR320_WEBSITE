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
          
          #Part 3:Line Width for Trend Graphic
          sliderInput(inputId="width",
                      label="Width of Trend Lines",
                      min=1,max=3,value=1,step=1),
          
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
              #1: Print Data for Desired 
              #   Countries and Variable
              tableOutput("OUTpreview"),
              br(),
              
              h2("Country Comparison"),
              br(),
              #2: Print Summary
              tableOutput("OUTsummary"),
              br()
              
            ),
            tabPanel("Graphics",
              
              #3:Print Trend Graphic       
              h2(textOutput("OUTtrendvar")),
              br(),
              plotOutput("OUTtrendplot"),
              br()
                     
            )
          )
        )
      )
  )
)
