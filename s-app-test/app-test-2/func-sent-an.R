
library(shiny)
library(tidytext)
library(tm)
library(dplyr)

input_text <- 'his script includes the UI definition, the server logic for sentiment analysis, and the text highlighting functionality. ~> After downloading, you can run this script in R to launch your Shiny app. If you encounter any issues or have further questions, feel free to ask! ​ ​'

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
    inner_join(get_sentiments("bing"), by = "word") %>%
    group_by(word) %>%
    summarize(sentiment = first(sentiment), n = n()) %>%
    ungroup()
  
  return(sentiment_scores)
}

# UI definition
ui <- fluidPage(
  titlePanel("Sentiment Analysis App"),
  sidebarLayout(
    sidebarPanel(
      textInput("text", "Enter Text:", "")
    ),
    mainPanel(
      htmlOutput("result")
    )
  )
)

# Server logic
server <- function(input, output) {
  output$result <- renderUI({
    # Retrieve input text
    input_text <- input$text
    
    # Perform sentiment analysis
    if (nchar(input_text) > 0) {
      sentiment_scores <- perform_sentiment_analysis(input_text)
      
      # Prepare a string with HTML tags for color highlighting
      highlighted_text <- input_text
      words <- unlist(strsplit(input_text, " "))
      for (word in words) {
        if (word %in% sentiment_scores$word) {
          color <- ifelse(sentiment_scores[sentiment_scores$word == word, "sentiment"] == "positive", "green", "red")
          highlighted_word <- sprintf("<span style='color: %s;'>%s</span>", color, word)
          highlighted_text <- gsub(paste0("\b", word, "\b"), highlighted_word, highlighted_text, fixed = TRUE)
          
        
        }
      }
      
      # Return the processed text as HTML
      HTML(highlighted_text)
      highlighted_text_2 <- "his script includes the UI definition, the server logic for sentiment analysis <span style='color:red> bad</span>'"
      
      HTML(highlighted_text_2)
    } else {
      "Please enter some text to analyze."
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
