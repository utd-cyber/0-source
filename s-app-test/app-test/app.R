# install.packages("shiny")

library(shiny)



# Define UI
ui <- fluidPage(
    titlePanel("Simple Shiny App"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("num", 
                        "Choose a number", 
                        value = 25, min = 1, max = 100)
        ),
        mainPanel(
            textOutput("numberText")
        )
    )
)

# Define server logic
server <- function(input, output) {
    output$numberText <- renderText({
        paste("You selected:", input$num)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
