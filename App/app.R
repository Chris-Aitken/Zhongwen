
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

# chapters of book vocabulary is from
lesson_nums <- unique(vocab$lesson)

# initialise empty df to add previously-served questions to
previous_question_entries <- vocab %>% slice(0)

# function for removing explanatory notes from translations for evaluations
strip_notes <- function(text_in) {
  text_in %>%
    gsub("\\(\\w+\\)", "", ., ignore.case = TRUE) %>%
    gsub("\\s{2,}", " ", .) %>%
    trimws()
}

# function for getting a random entry from voab list
get_random_entry <- function(df, prompt_col, response_col, lesson_selection, sampling_type, exclude_df) {
  
  # check we have at least some lessons selected, set to all if not
  # then replace "all" flag with all lesson nums
  lesson_selection <- ifelse(is.null(lesson_selection), "all", lesson_selection) %>%
                      {ifelse(. == "all", lesson_nums, lesson_selection)}
  
  # change sampling type flag to logical for below
  no_replacement <- ifelse(sampling_type == "without_replacement", TRUE, FALSE)
  
  # focus only on lessons user cares about
  df <- df %>%
        filter(lesson %in% {{ lesson_selection }})
  
  # if user has requested that entries from previous questions be removed, remove them
  if (no_replacement) {
    df <- df %>%
          anti_join(previous_question_entries, by = {{ prompt_col }})
  }
  
  # get entry for current question
  df <- df %>%
        rename(prompt = {{ prompt_col }}, response = {{ response_col }}) %>%
        select(prompt, response) %>%
        drop_na(prompt, response) %>%
        sample_n(size = 1) %>%
        mutate(response = strip_notes(response))
  
}

# function for checking strings of text match (approximately)
check_equal <- function(expected, val_to_check) {
  expected <- strsplit(expected, split = ",") %>% unlist()
  all_inputs <- list(expected = expected, val_to_check = val_to_check)
  any(grepl(glue("^( )*{val_to_check}( )*$"), expected, ignore.case = TRUE))
}

# user interface
ui <- fluidPage(
  
  # set theme
  theme = bs_theme(version = 3),
  
  # set up html etc for feedback mechanism
  useShinyFeedback(),
  
  # add some vertical space
  br(),
  
  # options for basic quiz
  fluidRow(
    
    # hidden options in dropdown
    column(
      1,
      br(),
      dropdownButton(
        
        # set id for dropdown
        inputId = "settings_dropdown",
        
        # title inside dropdown
        h2("Settings"),
        br(),
        
        # first option - sample with replacement?
        radioGroupButtons(
          "sampling_type",
          div(icon("random"), "Sampling"),
          selected = "with_replacement",
          individual = TRUE,
          justified = TRUE,
          choices = c(
            "Put word back" = "with_replacement",
            "Remove from deck" = "without_replacement"
          )
        ),
        
        # second option - restrict vocabulary to specific lesson?
        multiInput(
          "lessons_to_include",
          div(icon("filter"), "Lessons to include"),
          selected = "all",
          choiceNames = c("All", glue("Lesson: {lesson_nums}")),
          choiceValues = c("all", list(lesson_nums))
        ),
        
        # options for styling of dropdown
        tooltip = tooltipOptions(title = "Additional options"),
        icon = icon("cog"),
        width = "300px"
        
      )
    ),
    
    # which language should prompt be presented in?
    column(
      4,
      offset = 1,
      radioGroupButtons(
        "prompt_type",
        "Prompt",
        selected = "mandarin",
        individual = TRUE,
        justified = TRUE,
        choices = c(
          "中文" = "mandarin",
          "Pīnyīn" = "pinyin",
          "English" = "english"
        )
      )
    ),
    
    # which language should we expect the response to be in?
    column(
      4,
      radioGroupButtons(
        "response_type",
        "Response",
        selected = "english",
        individual = TRUE,
        justified = TRUE,
        choices = c(
          "中文" = "mandarin",
          "Pīnyīn" = "pinyin",
          "English" = "english"
        )
      )
    )
    
  # close off options
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
          actionButton("submit_response", label = "Submit", icon = icon("arrow-circle-right"))
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
  
  # check that at least one lesson type selected by user; choose all if not
  observeEvent(
    input$settings_dropdown, {
      if (is.null(input$lessons_to_include)) {
        updateMultiInput(session = session, inputId = "lessons_to_include", selected = "all")
      }
    }
  )
  
  # generate new test question (not eventReactive, because we want dependency on options, too)
  gen_test_question <- reactive({
                         input$get_new_question
                         vocab %>% 
                           get_random_entry(
                             prompt_col = input$prompt_type,
                             response_col = input$response_type,
                             lesson_selection = input$lessons_to_include,
                             sampling_type = input$sampling_type,
                             exclude_df = previous_question_entries
                           )
                       })
  
  # output test prompt
  output$vocab_test_prompt <- renderText(gen_test_question()$prompt)
  
  # create tracker at start; if options set to remove words already presented & answered, track them
  observeEvent(
    input$submit_response, {
      if (input$sampling_type == "without_replacement") {
        previous_question_entries <<- bind_rows(
                                        previous_question_entries,
                                        gen_test_question() %>%
                                          rename(
                                            !!input$prompt_type := prompt,
                                            !!input$response_type := response
                                          )
                                      ) %>%
                                      distinct_all()
      }
    }
  )
  
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
  
  # clear test prompt box + feedback if new question generated or other settings change
  observeEvent({
      input$get_new_question
      input$prompt_type
      input$response_type
      input$sampling_type
      input$lessons_to_include
    }, {
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

