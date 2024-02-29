install.packages("tidytext")
install.packages("tm")
install.packages("tm")

library(tidytext)
library(tm)

# Function for sentiment analysis
perform_sentiment_analysis <- function(input_text) {
  # Ensure the text is in UTF-8 encoding
  input_text <- iconv(input_text, to = "UTF-8")
  
  # Convert the text to a corpus
  corpus <- VCorpus(VectorSource(input_text))
  
  # Clean up the text: remove punctuation, numbers, whitespace, tolower, etc.
  corpus_clean <- tm_map(corpus, content_transformer(tolower))
  corpus_clean <- tm_map(corpus_clean, removePunctuation)
  corpus_clean <- tm_map(corpus_clean, removeNumbers)
  corpus_clean <- tm_map(corpus_clean, removeWords, stopwords("en"))
  corpus_clean <- tm_map(corpus_clean, stripWhitespace)
  
  # Convert the corpus to a data frame
  text_df <- data.frame(text = unlist(sapply(corpus_clean, `[`, "content")), stringsAsFactors = FALSE)
  
  # Tokenize the text into words
  tidy_text <- text_df %>%
    unnest_tokens(word, text)
  
  # Get sentiment scores for each word
  sentiment_scores <- tidy_text %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE)
  
  return(sentiment_scores)
}





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
