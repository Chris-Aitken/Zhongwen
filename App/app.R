
# load necessary packages
library(shiny)
library(shinyFeedback)
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(glue)

# declare path to app files locally
path_to_project <- "~/Documents/Personal/Chinese/Zhongwen"

# load vocabulary with translations
vocab <- read_excel(glue("{path_to_project}/Data/vocabulary.xlsx"))

# function for removing explanatory notes from translations for evaluations
strip_notes <- function(text_in) {
  text_in %>%
    gsub("\\(\\w+\\)", "", ., ignore.case = TRUE) %>%
    gsub("\\s{2,}", " ", .) %>%
    trimws()
}

# function for getting a random entry from voab list
get_random_entry <- function(df, prompt_col, response_col) {
  df %>%
    sample_n(size = 1) %>%
    select({{ prompt_col }}, {{ response_col }}) %>%
    drop_na() %>%
    rename(prompt = {{ prompt_col }}, response = {{ response_col }}) %>%
    mutate(response = strip_notes(response))
}

# work out if all elements of a list match
all_same <- function(list_in) {
  length(unique(list_in)) == 1
}

# function for checking strings of text match (approximately)
check_equal <- function(expected, val_to_check) {
  expected <- strsplit(expected, split = ",") %>% unlist()
  all_inputs <- list(expected = expected, val_to_check = val_to_check)
  any(grepl(val_to_check, expected, ignore.case = TRUE))
}

# user interface
ui <- fluidPage(
  
  # set up html etc for feedback mechanism
  useShinyFeedback(),
  
  # layout of basic quiz
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
      ),
    )
  ),
  
  # vocabulary
  #dataTableOutput("vocab_table")
  h1(textOutput("correctness"))
)

# backend server logic
server <- function(input, output, session) {
  
  # generate new test question
  gen_test_question <- reactive({
                         input$get_new_question
                         vocab %>% 
                           get_random_entry(
                             prompt_col = "mandarin",
                             response_col = "english"
                           )
                       })
  
  # output test prompt
  output$vocab_test_prompt <- renderText(gen_test_question()$prompt)
  
  # assess if answer is correct
  check_answer_correct <- eventReactive(
                            input$submit_response, {
                              gen_test_question()$response == input$vocab_test_input
                            }
                          )
  
  # out text for testing
  output$correctness <- renderText(check_answer_correct())

  # report whether user is correct or missed the mark
  observeEvent(
    input$submit_response, {

      # show praise if guess is okay
      feedbackSuccess(
        inputId = "vocab_test_input",
        show = check_answer_correct(),
        text = "Nice job!"
      )

      # show issue if problem
      feedbackWarning(
          inputId = "vocab_test_input",
          show = isFALSE(check_answer_correct()),
          text = gen_test_question()$response,
          icon = NULL
      )

    }
  )
  
  # clear test prompt box if new question generated
  observeEvent(
    input$get_new_question, {
      updateTextInput(inputId = "vocab_test_input", label = NULL, value = "")
      hideFeedback("vocab_test_input")
    }
  )
  
  # full vocabulary
  output$vocab_table <- renderDataTable(
                          select(vocab, -lesson),
                          options = list(pageLength = 10)
                        )
  
}

# run it
shinyApp(ui, server)

