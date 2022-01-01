
# load necessary packages
library(shiny)
library(shinyalert)
library(shinyWidgets)
library(shinyjs)
library(reactable)
library(bslib)
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(glue)
library(lubridate)
library(htmltools)

# declare path to app files locally
path_to_project <- "~/Documents/Personal/Chinese/Zhongwen"

# load vocabulary with translations
vocab <- glue("{path_to_project}/Data/vocabulary.xlsx") %>%
         read_excel() %>%
         mutate(entry_id = row_number())

# chapters of book vocabulary is from
lesson_nums <- unique(vocab$lesson)

# named lesson choices
lesson_choices <- c("all", list(lesson_nums)) %>%
                  setNames(c("All", glue("Lesson {lesson_nums}")))

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

# function for processing lesson selections
process_lesson_selection <- function(lesson_selection) {
  ifelse(is.null(lesson_selection), "all", lesson_selection) %>%
    {ifelse(. == "all", lesson_nums, lesson_selection)}
}

# function for getting a random entry from vocab list
get_random_entry <- function(df, prompt_col, response_col, lesson_selection, sampling_type, exclude_df) {
  
  # check we have at least some lessons selected, set to all if not
  # then replace "all" flag with all lesson nums
  lesson_selection <- process_lesson_selection(lesson_selection)
  
  # change sampling type flag to logical for below
  no_replacement <- ifelse(sampling_type == "without_replacement", TRUE, FALSE)
  
  # focus only on lessons user cares about
  df_ed <- df %>%
           filter(lesson %in% {{ lesson_selection }})
  
  # if user has requested that entries from previous questions be removed, remove them
  if (no_replacement) {
    df_ed <- df_ed %>%
             anti_join(exclude_df, by = "entry_id")
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
  expected <- unlist(strsplit(expected, split = ","))
  val_to_check <- gsub("[[:punct:]]", "", val_to_check)
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

# function for rounding (down) to nearest multiple
round_to <- function(num, multiple_of) {
  floor(num / multiple_of) * multiple_of
}

# function for removing catch-all category if multiple present
remove_catch_all_category <- function(elements, name) {
  elements[elements != name]
}

# user interface
ui <- fluidPage(
  
  # set theme
  theme = bs_theme(version = 4, bootswatch = "journal"),
  
  # some quick inline css
  tags$head(
    tags$style(
      HTML('
         .navbar {
           padding: 0.5rem 0rem;
           border-bottom: 1px solid rgba(0,0,0,0.3) !important;
         }
         
         .navbar.navbar-default {
           border: none
         }
         
         .navbar-nav > li {
           padding-left:10px;
           padding-right:10px;
         }
         
         .navbar-light .navbar-toggler, .navbar-light .navbar-toggle, .navbar.navbar-default .navbar-toggler, .navbar.navbar-default .navbar-toggle {
           border-color: rgba(0,0,0,0.2);
         }
      
         .dropdown-toggle::after {
           display: none;
         }
         
         #dropdown-menu-settings_dropdown {
           box-shadow: 0 50px 100px rgba(50,50,93,.1),0 15px 35px rgba(50,50,93,.15),0 5px 15px rgba(0,0,0,.1);
         }
         
         .form-group {
           text-align: center;
         }
         
         #vocab_test_prompt {
           font-size: 2.5em !important;
           text-align: center;
         }
         
         h2 {
           text-align: center;
         }
         
         div.sweet-alert p {
           font-size: 22px;
         }
         
         button.confirm {
           background-color: rgb(220,112,105) !important;
         }
         
         .sweet-alert .sa-icon.sa-success {
           border-color: rgb(220,112,105) !important;
         }
         
         .sweet-alert .sa-icon.sa-success .sa-line {
           background-color: rgb(220,112,105) !important;
         }
         
         .sweet-alert .sa-icon.sa-success .sa-placeholder {
           border: 4px solid rgba(220,112,105,.4) !important;
         }
         
         .sweet-alert .sa-icon.sa-error {
           border-color: rgba(220,112,105,.4) !important;
         }
         
         .sweet-alert .sa-icon.sa-error .sa-line {
           background-color: rgb(220,112,105) !important;
         }
         
         .rt-search {
           margin-bottom: 10pt;
           align-self: start;
         }
         
         .header:hover {
           background-color: #eee;
         }
         
         .bar-cell {
           display: flex;
           align-items: center;
         }
        
         .number {
           font-family: "Fira Mono", Consolas, Monaco, monospace;
           font-size: 13.5px;
           white-space: pre;
         }
        
         .bar-chart {
           flex-grow: 1;
           margin-left: 6px;
           height: 14px;
         }
         
         .bar-chart-background-present {
           background-color: #e1e1e1
         }
         
         .bar-chart-background-missing {
           background-color: #f5f5f5
         }
        
         .bar {
           height: 100%;
         }
      ')
    )
  ),
  
  # add js script to check dimensions of browser for vocab table
  tags$head(
    tags$script('
        var dimension = [0, 0];
        $(document).on("shiny:connected", function(e) {
            dimension[0] = window.innerWidth;
            dimension[1] = window.innerHeight;
            Shiny.onInputChange("dimension", dimension);
        });
        $(window).resize(function(e) {
            dimension[0] = window.innerWidth;
            dimension[1] = window.innerHeight;
            Shiny.onInputChange("dimension", dimension);
        });
    ')
  ),
  
  # set up necessary additional details
  useShinyjs(),
  
  # create multi-page structure
  navbarPage(
    "当代中文 – Revision Tool",
    collapsible = TRUE,
    id = "page_nav_menu",
    
    # home page
    tabPanel(
      "Home",
      value = "home"
    ),
    
    # test page
    tabPanel(
      "Test Vocabulary Recall",
      value = "vocab_test",
  
      # add some vertical space
      tags$hr(style = "height:5px; visibility:hidden;"),
      
      # options for basic quiz
      fluidRow(
        
        # hidden options in dropdown
        column(
          1,
          br(),
            dropdownButton(
              
              # set id for dropdown, settings etc
              inputId = "settings_dropdown",
              
              # title inside dropdown
              h2("Settings"),
              br(),
              
              # first option - show score?
              radioGroupButtons(
                "show_score_choice",
                div(icon("award"), "Performance"),
                selected = TRUE,
                individual = TRUE,
                justified = TRUE,
                choices = c(
                  "Show score" = TRUE,
                  "Hide score" = FALSE
                )
              ),
              
              # second option - sample with replacement?
              radioGroupButtons(
                "sampling_type",
                div(icon("random"), "After question"),
                selected = "with_replacement",
                individual = TRUE,
                justified = TRUE,
                choices = c(
                  "Put word back" = "with_replacement",
                  "Remove word" = "without_replacement"
                )
              ),
              
              # third option - restrict vocabulary to specific lesson?
              multiInput(
                "test_lessons_to_include",
                div(icon("filter"), "Lessons to include"),
                selected = "all",
                choiceNames = names(lesson_choices),
                choiceValues = unname(lesson_choices)
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
          br(),
          br(),
            fluidRow(
              column(
                12,
                div(
                  style = "display:inline-block;vertical-align:top;",
                  textInput(
                    "vocab_test_input",
                    label = NULL, 
                    placeholder = "Enter response"
                  )
                ),
                div(
                  style = "display:inline-block;vertical-align:top;",
                  actionButton("submit_response", label = "Submit")
                ),
                align = "center"
              )
            ),
          br(),
          column(
            12,
            actionButton("get_new_question", "Generate New Question"),
            align = "center"
          )
        )
      ),
      
      # score for quiz
      tags$hr(style = "height:60px; visibility:hidden;"),
      fluidRow(
        column(
          12,
          hidden(
            div(
              id = "vocab_test_score_box",
              textOutput("score")
            )
          ),
          align = "center"
        )
      ),
      
      # add vertical whitespace
      br(),
      br()
    
    # close off test page
    ),
    
    # new page for full vocabulary
    tabPanel(
      "Full Vocabulary",
      value = "vocab_table",
      
      # selection of lessons to restrict vocab to
      br(),
      fluidRow(
        column(
          12,
          pickerInput(
            "full_vocab_lessons_to_include",
            div(icon("filter"), "Lessons to include"),
            multiple = TRUE,
            selected = "all",
            options = list(`selected-text-format` = "count > 4"),
            choices = lesson_choices
          ),
          align = "center"
        )
      ),
      br(),
      
      # vocabulary in a table
      reactableOutput("vocab_table")
      
    ),
    
    # new page for key phrases
    tabPanel(
      "Key Phrases",
      value = "key_phrases"
    )
  
  )

)

# backend server logic
server <- function(input, output, session) {
  
  # initialise empty df to add previously-served questions to
  previous_question_entries <- vocab %>% slice(0)
  
  # initialise score counters
  current_score <- 0
  question_number <- 0
  
  # initialise empty dataframe for storing question-response info (for result charts)
  question_response_history <- tibble(
                                 date_time = Date(),
                                 entry_id = integer(),
                                 prompt = character(),
                                 correct_response = character(),
                                 actual_response = character(),
                                 was_correct = logical()
                               )
  
  # move navbar items to the right
  addClass(id = "page_nav_menu", class = "justify-content-end")
  
  # check that at least one lesson type selected by user; choose all if not
  observeEvent(
    input$settings_dropdown, {
      if (is.null(input$test_lessons_to_include)) {
        updateMultiInput(session = session, inputId = "test_lessons_to_include", selected = "all")
      } else if (length(input$test_lessons_to_include) > 1 &&
                 "all" %in% input$test_lessons_to_include) {
        updateMultiInput(
          session = session,
          inputId = "test_lessons_to_include",
          selected = input$test_lessons_to_include %>%
                       remove_catch_all_category(name = "all")
        )
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
                             lesson_selection = input$test_lessons_to_include,
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
  
  # update score counter
  observeEvent(
    input$submit_response, {
      req(input$vocab_test_input)
      question_number <<- question_number + 1
      if (isTRUE(check_answer_correct())) {
        current_score <<- current_score + 1
      }
    }
  )
  
  # generate score statement
  gen_score_statement <- reactive({
                           input$submit_response
                           paste0("Score: ", current_score, "/", question_number)
                         })
  
  # send score to ui
  output$score <- renderText(gen_score_statement())

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
          type = "error",
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
  
  # convert radio input to logical var for below
  show_score <- reactive(as.logical(input$show_score_choice))
  
  # hide or unhide score
  observeEvent({
      show_score() | input$alert_correct_answer | input$alert_incorrect_answer
    }, {
      if (input$submit_response >= 1 && show_score()) {
        shinyjs::show("vocab_test_score_box", anim = TRUE, animType = "fade")
      } else {
        shinyjs::hide("vocab_test_score_box", anim = TRUE, animType = "fade")
      }
    }
  )
  
  # capture record of question, response, correctness etc for vocab table
  observeEvent(
    input$submit_response, {
      question_response_history <<- bind_rows(
                                      question_response_history,
                                      tibble(
                                        date_time = now(),
                                        entry_id = gen_test_question()$entry_id,
                                        prompt = gen_test_question()$prompt,
                                        correct_response = gen_test_question()$correct_response,
                                        actual_response = input$vocab_test_input,
                                        was_correct = isTRUE(check_answer_correct())
                                      )
                                    )
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
      input$test_lessons_to_include
    }, {
      updateTextInput(inputId = "vocab_test_input", label = NULL, value = "")
    }
  )
  
  # as above, check that at least one lesson type selected by user for vocab table
  # only update when picker input closes: '_open' append let's you know if it's open
  # see ?pickerInput
  observeEvent(
    input$full_vocab_lessons_to_include_open, {
      if (isFALSE(input$full_vocab_lessons_to_include_open)) {
        if (is.null(input$full_vocab_lessons_to_include)) {
          updatePickerInput(
            session = session,
            inputId = "full_vocab_lessons_to_include",
            selected = "all"
          )
        } else if (length(input$full_vocab_lessons_to_include) > 1 && 
                   "all" %in% input$full_vocab_lessons_to_include) {
          updatePickerInput(
            session = session,
            inputId = "full_vocab_lessons_to_include",
            selected = input$full_vocab_lessons_to_include %>%
                         remove_catch_all_category(name = "all")
          )
        }
      }
    }
  )
  
  # get dimensions of window for below
  get_window_dimensions <- reactive({
                             tibble(
                               width = input$dimension[1],
                               height = input$dimension[2]
                             )
                           })
  
  # get lower margin height below table (min size, plus extra if on big screens)
  get_bottom_margin_size <- reactive({
                              min(0.15 * get_window_dimensions()$height, 150)
                            })
  
  # declare size of row (height)
  row_height <- 38
  
  # get table height (38 default height of row if not broken over several lines)
  get_vocab_table_height <- reactive({
                              (get_window_dimensions()$height - get_bottom_margin_size()) %>%
                                `-`(., 130) %>%                           # account for size of options
                                round_to(multiple_of = row_height) %>%    # find height in num of full rows
                                max(., row_height * 2)                    # ensure something is shown
                            })
  
  # get vocab table data
  get_vocab_table_data <- reactive({
                            input$nav_bar_menu
                            vocab %>%
                             filter(
                               lesson %in% process_lesson_selection(
                                             input$full_vocab_lessons_to_include
                                           )
                             ) %>%
                             left_join(
                               question_response_history %>%
                                 group_by(entry_id) %>%
                                 summarise(total = n(), correct = sum(was_correct)) %>%
                                 mutate(performance = paste0(correct, "/", total)) %>%
                                 select(-c(correct, total)),
                               by = "entry_id"
                             ) %>% 
                             select(-c(lesson, entry_id))
                          })
  
  # full vocabulary
  output$vocab_table <- renderReactable(
                          reactable(
                            get_vocab_table_data(),
                            borderless = TRUE,
                            searchable = TRUE,
                            pagination = FALSE,
                            highlight = TRUE,
                            height = get_vocab_table_height(),
                            defaultColDef = colDef(headerClass = "header", align = "left"),
                            defaultSorted = NULL,
                            columns = list(
                              english = colDef(name = "English", na = "-", defaultSortOrder = "asc"),
                              pinyin = colDef(name = "Pīnyīn", na = "-", sortable = FALSE),
                              mandarin = colDef(name = "Chinese", na = "-", sortable = FALSE),
                              performance = colDef(
                                name = "Recall Test Performance",
                                sortable = FALSE,
                                show = get_window_dimensions()$width > 780,
                                cell = function(value) {
                                  if (!is.na(value)) {
                                    evaluated_value <- eval(parse(text = value))
                                    chart_background_type <- "present"
                                  } else {
                                    evaluated_value <- 0
                                    chart_background_type <- "missing"
                                  }
                                  width <- paste0(evaluated_value * 100, "%")
                                  max_len <- max(nchar(get_vocab_table_data()$performance), 0, na.rm = TRUE)
                                  padding_str <- glue("% {max_len}s")
                                  value <- sprintf(padding_str, value) %>% format(., justify = "right")
                                  bar <- div(
                                    class = paste0("bar-chart bar-chart-background-", chart_background_type),
                                    style = list(marginRight = "6px"),
                                    div(class = "bar", style = list(width = width, backgroundColor = "#919191")) # "#dc7169"
                                  )
                                  div(class = "bar-cell", span(class = "number", value), bar)
                                }
                              )
                            )
                          )
                        )
  
}

# run it
shinyApp(ui, server)

