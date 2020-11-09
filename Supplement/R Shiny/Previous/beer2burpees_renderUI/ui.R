#------------------------------------------------------------------------------#
# Coursera Data Products Class Project
# Kristen Borchert
# Burpee Calculator
# ui.R
#------------------------------------------------------------------------------#

shinyUI(
    fluidPage(
        titlePanel("Beer to Burpee Calculator"),
        sidebarLayout(
            sidebarPanel(
                p("This app calculates the number of burpees (an exercise) to perform to offset beer consumption. 
              Simply enter your weight in lbs, the number of beers you have consumed and the type of beer 
              you consumed."),
                br(),
                h4("Enter Your Weight and Beer Consumption Here"),
                numericInput(inputId = 'weight', "Input your Weight (lbs)", 0, min = 100, max = 250, step = 10),
                numericInput(inputId = 'quantity', "Number of Glasses (12 oz.)", 1, min = 1, max = 10, step = 1),
                uiOutput(outputId = "beer_selector"),
                submitButton('Submit')
            ),
            mainPanel("",
                      h3('Calories per Burpee'),
                      h4('For your weight of...'),
                      verbatimTextOutput("weight"),
                      h4('You burn this many calories per Burpee:'),
                      verbatimTextOutput("burpee_calories"),
                      h3('Calories Consumed'),
                      h4('Your beer(s) cost you this many calories:'),
                      verbatimTextOutput("beer_calories"),
                      h4('Number of Burpees needed to offset beer consumption:'),
                      verbatimTextOutput("n_burpees"),
                      h3('How to Perform Burpees:'),
                      img(src = "BurpeeImage.png", height = 300, width = 700),
                      p("Sources:"),
                      p("1) Burpee calculations:'Burpee Equivalents:  Understanding Junk Food in terms of Your Favorite Exercise'
                    by Dr. Jeff Godin, found at: http://blog.spartanrace.com/burpee-equivalents-understanding-junk-food."),
                      p("2) Beer Calorie Info: http://www.fitsugar.com/Calories-Popular-Beers-1504697"),
                      p("3) Burpee Image: http://crossfit317.com/1-31-14-wod-burpees-snatches-o-my")
            )
        )
    )
)