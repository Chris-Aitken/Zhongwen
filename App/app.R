
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
      6,
      offset = 3,
      h1(textOutput("vocab_test_prompt")),
      fluidRow(
        column(
          8,
          textInput("vocab_test_input", label = NULL)
        ),
        column(
          4,
          actionButton("submit_response", "Submit")
        )
      ),
      column(
        4,
        offset = 1,
        actionButton("get_new_question", "Generate New Question")
      )
    )
  ),
  #dataTableOutput("vocab_table")
)

# backend server logic
server <- function(input, output, session) {
  
  # clear test prompt box if new question generated
  observeEvent(
    input$get_new_question, {
      updateTextInput(inputId = "vocab_test_input", label = NULL, value = "")
    }
  )
  
  # generate new test question
  gen_test_question <- reactive({
                         input$get_new_question
                         get_random_entry(vocab, mandarin)
                       })
  
  # output test prompt
  output$vocab_test_prompt <- renderText(gen_test_question())
  
  # full vocabulary
  output$vocab_table <- renderDataTable(
                          select(vocab, -lesson),
                          options = list(pageLength = 10)
                        )
  
}

# run it
shinyApp(ui, server)

