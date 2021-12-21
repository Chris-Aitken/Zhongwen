
# load necessary packages
library(shiny)
library(shinyFeedback)
library(shinyWidgets)
library(bslib)
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
    drop_na({{ prompt_col }}, {{ response_col }}) %>%
    sample_n(size = 1) %>%
    select({{ prompt_col }}, {{ response_col }}) %>%
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
  
  # set theme
  theme = bs_theme(version = 5),
  
  # set up html etc for feedback mechanism
  useShinyFeedback(),
  
  # options for basic quiz
  fluidRow(
    column(
      4,
      radioGroupButtons(
        "sampling_type",
        "After question",
        selected = "with_replacement",
        individual = TRUE,
        justified = TRUE,
        choices = c(
          "Put word back" = "with_replacement",
          "Remove from deck" = "without_replacement"
        )
      )
    ),
    column(
      4,
      radioGroupButtons(
        "prompt_type",
        "Prompt",
        selected = "mandarin",
        individual = TRUE,
        justified = TRUE,
        choices = c(
          "Pīnyīn" = "pinyin",
          "中文" = "mandarin",
          "English" = "english"
        )
      )
    ),
    column(
      4,
      radioGroupButtons(
        "response_type",
        "Response",
        selected = "english",
        individual = TRUE,
        justified = TRUE,
        choices = c(
          "Pīnyīn" = "pinyin",
          "中文" = "mandarin",
          "English" = "english"
        )
      )
    )
  ),
  
  # layout of quiz itself
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
        6,
        offset = 1,
        actionButton("get_new_question", "Generate New Question")
      ),
    )
  ),
  
  # vocabulary
  # dataTableOutput("vocab_table")

)

# backend server logic
server <- function(input, output, session) {
  
  # generate new test question
  gen_test_question <- reactive({
                         input$get_new_question
                         vocab %>% 
                           get_random_entry(
                             prompt_col = input$prompt_type,
                             response_col = input$response_type
                           )
                       })
  
  # output test prompt
  output$vocab_test_prompt <- renderText(gen_test_question()$prompt)
  
  # assess if answer is correct
  check_answer_correct <- eventReactive(
                            input$submit_response, {
                              check_equal(
                                gen_test_question()$response,
                                input$vocab_test_input
                              )
                            }
                          )

  # report whether user is correct or missed the mark
  observeEvent(
    input$submit_response, {

      # don't show feedback if nothing has been entered
      req(input$vocab_test_input)
      
      # remove any feedback currently displayed
      hideFeedback("vocab_test_input")
      
      # code for handling feedback
      if (isTRUE(check_answer_correct())) {
        
        # show priase if answer is okay (approx)
        showFeedback(
          inputId = "vocab_test_input",
          color = "#5cb85c",
          icon = shiny::icon("ok", lib = "glyphicon"),
          text = "Nice job!"
        )
        
      } else if (isFALSE(check_answer_correct())) {
        
        # show answer otherwise
        showFeedback(
          inputId = "vocab_test_input",
          color = "#F89406",
          text = sprintf("Answer: %s", gen_test_question()$response),
          icon = NULL
        )
        
      }

    }
  )
  
  # clear test prompt box + feedback if new question generated
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

