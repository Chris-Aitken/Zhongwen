
# load necessary packages
library(shiny)
library(readxl)
library(dplyr)
library(glue)

# declare path to app files locally
path_to_project <- "~/Documents/Personal/Chinese/Zhongwen"

# load vocabulary with translations
vocab <- read_excel(glue("{path_to_project}/Data/vocabulary.xlsx"))

# function for getting a random entry from voab list
get_random_entry <- function(df, col) {
  df %>%
    sample_n(1) %>%
    pull({{ col }})
}

# user interface
ui <- fluidPage(
  fluidRow(
    column(
      4,
      textOutput("vocab_test_prompt"),
      actionButton("get_new_question", "Generate!"),
      offset = 4
    )
  ),
  dataTableOutput("vocab_table")
)

# backend server logic
server <- function(input, output, session) {
  
  # generate new test question
  test_question <- reactive({
                     input$get_new_question
                     get_random_entry(vocab, mandarin)
                   })
  
  # output test prompt
  output$vocab_test_prompt <- renderText(test_question())
  
  # full vocabulary
  output$vocab_table <- renderDataTable(
                          select(vocab, -lesson),
                          options = list(pageLength = 10)
                        )
  
}

# run it
shinyApp(ui, server)

