library(shiny)
library(lime)

# Define UI
ui <- fluidPage(
  titlePanel("SMS Spam Classification with LIME"),
  sidebarLayout(
    sidebarPanel(
      textInput("text_input", "Enter SMS Text:", "Your SMS text here"),
      actionButton("explain", "Classify and Explain")
    ),
    mainPanel(
      tableOutput("explanation")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$explain, {
    req(input$text_input)
    
    # Prepare the input for prediction
    new_data <- tibble(text = input$text_input)
    
    # Predict using the fitted workflow
    prediction <- predict(sms_fit, new_data, type = "prob")
    
    # Create an explanation using lime
    explainer <- lime(as.data.frame(sms_train), sms_fit)
    explanation <- explain(as.data.frame(new_data), explainer, n_labels = 1, n_features = 5)
    
    # Render the explanation
    output$explanation <- renderTable({
      as.data.frame(explanation)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
