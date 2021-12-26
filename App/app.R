
# load necessary packages
library(shiny)
library(shinyalert)
library(shinyWidgets)
library(shinyjs)
library(bslib)
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(glue)

# declare path to app files locally
path_to_project <- "~/Documents/Personal/Chinese/Zhongwen"

# load vocabulary with translations
vocab <- glue("{path_to_project}/Data/vocabulary.xlsx") %>%
         read_excel()

# chapters of book vocabulary is from
lesson_nums <- unique(vocab$lesson)

# initialise empty df to add previously-served questions to
previous_question_entries <- vocab %>% slice(0)

# different choices for languages to test
language_choices <- c("中文" = "mandarin",
                      "Pīnyīn" = "pinyin",
                      "English" = "english")

# function for removing explanatory notes from translations for evaluations
strip_notes <- function(text_in) {
  text_in %>%
    gsub("\\(\\w+\\)", "", ., ignore.case = TRUE) %>%
    gsub("\\s{2,}", " ", .) %>%
    trimws()
}

# function for getting a random entry from vocab list
get_random_entry <- function(df, prompt_col, response_col, lesson_selection, sampling_type, exclude_df) {
  
  # check we have at least some lessons selected, set to all if not
  # then replace "all" flag with all lesson nums
  lesson_selection <- ifelse(is.null(lesson_selection), "all", lesson_selection) %>%
                      {ifelse(. == "all", lesson_nums, lesson_selection)}
  
  # change sampling type flag to logical for below
  no_replacement <- ifelse(sampling_type == "without_replacement", TRUE, FALSE)
  
  # focus only on lessons user cares about
  df_ed <- df %>%
           filter(lesson %in% {{ lesson_selection }})
  
  # if user has requested that entries from previous questions be removed, remove them
  if (no_replacement) {
    df_ed <- df_ed %>%
             anti_join(previous_question_entries, by = {{ prompt_col }})
  }
  
  # remove entries for which one of prompt or response missing + rename
  df_ed <- df_ed %>%
           rename(prompt = {{ prompt_col }}, correct_response = {{ response_col }}) %>%
           drop_na(prompt, correct_response)
  
  # if we've exhuasted all vocab from selected set and aren't using replacement, report all done
  if (nrow(df_ed) == 0) {
    
    # return report
    tibble(prompt = "All done!", correct_response = "")
    
  } else {
    
    # otherwise, get entry for current question
    df_ed %>%
      sample_n(size = 1) %>%
      mutate(correct_response = strip_notes(correct_response))
    
  }
  
}

# function for checking strings of text match (approximately)
check_equal <- function(expected, val_to_check) {
  expected <- strsplit(expected, split = ",") %>% unlist()
  any(grepl(glue("^( )*{val_to_check}( )*$"), expected, ignore.case = TRUE))
}

# function for turning off/on individual radio button inputs
disable_radio_options <- function(radio_input_id, options, disable_if_equal_to) {
  
  # start option counter
  option_num <- 0
  
  # cycle through radio button options
  for (option in options) {
  
    # selector for option we want to change
    toggling_selector <- glue("#{radio_input_id} button:eq({option_num})")
    
    # toggle availability of option
    if (option == disable_if_equal_to) {
      disable(selector = toggling_selector)
    } else {
      enable(selector = toggling_selector)
    }
    
    # update counter
    option_num <- option_num + 1
    
  }
  
}

# user interface
ui <- fluidPage(
  
  # set theme
  theme = bs_theme(version = 3),
  
  # set up necessary additional details
  useShinyjs(),
  
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
          div(icon("random"), "After question"),
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
        choices = language_choices
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
        choices = language_choices
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
  
  # ensure that user can't select response language as prompt language
  observeEvent(
    input$prompt_type, {
      disable_radio_options(
        radio_input_id = "response_type",
        options = language_choices,
        disable_if_equal_to = input$prompt_type
      )
    }
  )
  
  # now the reverse: ensure user can't select prompt language as response language
  observeEvent(
    input$response_type, {
      disable_radio_options(
        radio_input_id = "prompt_type",
        options = language_choices,
        disable_if_equal_to = input$response_type
      )
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
  
  # if options set to remove words already presented & answered, track them
  observeEvent(
    input$submit_response, {
      
      # only do following if user has explicitly requested it
      if (input$sampling_type == "without_replacement") {
        
        # if done with all available questions
        if (gen_test_question()$prompt == "All done!") {
          
          # start tracker again
          previous_question_entries <<- previous_question_entries %>% 
                                        slice(0)
          
        # continue tracking if not 
        } else {
          
          # add current question to list of previous questions
          previous_question_entries <<- bind_rows(
                                          previous_question_entries,
                                          gen_test_question() %>%
                                            rename(
                                              !!input$prompt_type := prompt,
                                              !!input$response_type := correct_response
                                            )
                                        ) %>%
                                        distinct_all()
          
        }
      }
    }
  )
  
  # stop user from submitting response if prompt lets user know bank is exhausted
  # (if sampling without replacement)
  observe({
    if (gen_test_question()$prompt == "All done!") {
      disable("vocab_test_input")
    } else {
      enable("vocab_test_input")
    }
  })
  
  # assess if answer is correct
  check_answer_correct <- eventReactive(
                            input$submit_response, {
                              check_equal(
                                gen_test_question()$correct_response,
                                input$vocab_test_input
                              )
                            }
                          )

  # report whether user is correct or missed the mark
  observeEvent(
    input$submit_response, {

      # don't show feedback if nothing has been entered
      req(input$vocab_test_input)
      
      # get full data row
      full_question_entry <<- gen_test_question() %>%
                              rename(
                                !!input$prompt_type := prompt,
                                !!input$response_type := correct_response
                              )
      
      # code for handling feedback
      if (isTRUE(check_answer_correct())) {
        
        # show priase if answer is okay (approx)
        shinyalert(
          title = "Correct!",
          size = "xs", 
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          html = FALSE,
          type = "success",
          showConfirmButton = TRUE,
          confirmButtonText = "Close",
          showCancelButton = FALSE,
          timer = 10000,
          animation = TRUE,
          inputId = "alert_correct_answer",
          text = sprintf(
                   "'%s' (%s) translates to '%s'",
                   full_question_entry$mandarin,
                   full_question_entry$pinyin,
                   full_question_entry$english
                 )
        )
        
      } else if (isFALSE(check_answer_correct())) {
        
        # show answer otherwise
        shinyalert(
          title = "Not quite!",
          size = "xs", 
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          html = FALSE,
          type = "warning",
          showConfirmButton = TRUE,
          confirmButtonText = "Close",
          showCancelButton = FALSE,
          timer = 10000,
          imageUrl = "",
          animation = TRUE,
          inputId = "alert_incorrect_answer",
          text = sprintf(
            "'%s' (%s) translates to '%s'",
            full_question_entry$mandarin,
            full_question_entry$pinyin,
            full_question_entry$english
          )
        )
        
      }

    }
  )
  
  # after feddback is displayed, move onto next question
  observeEvent({
      input$alert_correct_answer | input$alert_incorrect_answer
    }, {
      click("get_new_question")
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

