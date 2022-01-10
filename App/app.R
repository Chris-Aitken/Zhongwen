
# load necessary packages
library(shiny)
library(shinyalert)
library(shinyWidgets)
library(shinyjs)
library(reactable)
library(bslib)
library(readxl)
library(vroom)
library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(glue)
library(stringr)
library(lubridate)
library(htmltools)
library(waiter)
library(jsonlite)

# declare path to app files locally
path_to_project <- "~/Documents/Personal/Chinese/Zhongwen"

# load vocabulary with translations
vocab <- glue("{path_to_project}/Data/vocabulary.xlsx") %>%
         read_excel() %>%
         mutate(entry_id = row_number())

# load phrases as well
phrases <- glue("{path_to_project}/Data/phrases.xlsx") %>%
           read_excel()

# chapters of book vocabulary is from
lesson_nums <- unique(vocab$lesson)

# named lesson choices
lesson_choices <- c("all", lesson_nums) %>%
                  setNames(c("All", glue("Lesson {lesson_nums}")))

# different choices for languages to test
language_choices <- c("中文" = "mandarin",
                      "Pīnyīn" = "pinyin",
                      "English" = "english")

# content for steps for tracking progress long-term
progress_tracking_steps <- tribble(
  ~step_num, ~title, ~detail,
  1, "Upload Test Record", "If you have used the app previously, upload your test record",
  2, "Use App", "Proceed to use the app, and the test in particular",
  3, "Store Results", "When done, before closing the app, save your test record"
)

# function for removing explanatory notes from translations for evaluations
strip_notes <- function(text_in) {
  text_in %>%
    gsub("\\s*\\([^\\)]+\\)", "", ., ignore.case = TRUE) %>%
    gsub("\\s{2,}", " ", .) %>%
    trimws()
}

# function for stripping away punctuation
strip_punc <- function(text_in) {
  text_in %>%
    gsub("(?<=[A-z])\\p{Pd}(?=[A-z])", " ", ., perl = TRUE) %>%
    gsub("[[:punct:]]", "", .)
}

# function for processing lesson selections
process_lesson_selection <- function(lesson_selection) {
  int_val <- if (is.null(lesson_selection)) "all" else lesson_selection
  if (identical(int_val, "all")) lesson_nums else lesson_selection
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
  expected <- unlist(strsplit(expected, split = ",")) %>% strip_punc()
  val_to_check <- strip_punc(val_to_check)
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

# function for inserting a lesson selector into ui
insert_lesson_selector <- function(input_id) {
  fluidRow(
    column(
      12,
      pickerInput(
        inputId = input_id,
        div(icon("filter"), "Lessons to include"),
        multiple = TRUE,
        selected = "all",
        options = list(`selected-text-format` = "count > 3"),
        choices = lesson_choices
      ),
      align = "center"
    )
  )
}

# seq_along for df rows
rows_along <- function(df) seq(nrow(df))

# function for constructing cards
create_card <- function(mandarin, pinyin, english) {
  HTML(
    paste0('
      <div class="flip-card">
        <div class="flip-card-inner">
          <div class="flip-card-front">
            <h4>', mandarin, '</h4>
            <hr>
            ', pinyin, '
          </div>
          <div class="flip-card-back">
            ', english, '
          </div>
        </div>
      </div>
    ')
  )
}

# function for creating container for steps visual
enclose_in_steps_container <- function(full_content, id) {
  paste0('
    <ul id="', id,'" class="steps">
      ', full_content, '
    </ul>
  ')
}

# function for creating
create_step_item <- function(step_num, title, detail, marker_content = NULL) {
  if (is.null(marker_content)) marker_content <- step_num
  step_num <- step_num - 1
  paste0('
    <li class="step-item" step-id="', step_num,'">
      <div class="step-marker">
        ', marker_content,'
      </div>
      <div class="step-details">
        <p class="step-title">', title, '</p>
        <p class="step-content">', detail, '</p>
      </div>
    </li>
  ')
}

# function for turning step item strings into one cohesive piece of html
create_steps_html <- function(step_data, id) {
  pmap(step_data, create_step_item) %>%
    paste0(collapse = "") %>%
    enclose_in_steps_container(id = id) %>%
    HTML()
}

# function for adding html for font-awesome icon inside step marker
steps_insert_icon_html <- function(icon) {
  HTML(
    paste0('
      <i class="fa fa-', icon,'" role="presentation" aria-label="', icon,' icon">
      </i>
    ')
  )
}

# function for changing step default styling to 'complete' styling
make_step_style_complete <- function(step_id) {
  marker_selector <- glue('li.step-item[step-id="{step_id}"] div.step-marker')
  addClass(
    selector = marker_selector,
    class = "step-complete"
  )
  shinyjs::html(
    selector = marker_selector,
    html = steps_insert_icon_html("check")
  )
}

# version of fileInput from shiny without progress bar and name reported
simpleFileInput <- function (inputId, label,
                             multiple = FALSE,
                             accept = NULL,
                             width = NULL, 
                             buttonLabel = "Browse...",
                             placeholder = "No file selected") {
  
  restoredValue <- shiny::restoreInput(id = inputId, default = NULL)
  
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue <- NULL
  }
  
  if (!is.null(restoredValue)) {
    restoredValue <- jsonlite::toJSON(restoredValue, strict_atomic = FALSE)
  }
  
  inputTag <- tags$input(
    id = inputId,
    name = inputId,
    type = "file", 
    style = "position: absolute !important; top: -99999px !important; left: -99999px !important;", 
    `data-restore` = restoredValue
  )
  
  if (multiple) {
    inputTag$attribs$multiple <- "multiple"
  }
  
  if (length(accept) > 0) {
    inputTag$attribs$accept <- paste(accept, collapse = ",")
  }
  
  div(class = "form-group", style = css(width = validateCssUnit(width)), 
      shiny:::shinyInputLabel(inputId, label), div(class = "input-group", 
                                                   tags$label(class = "input-group-btn", 
                                                              span(class = "btn btn-default btn-file", buttonLabel, 
                                                                   inputTag)))
  )
  
}

# set theme for loading spinners
waiter_set_theme(html = spin_5(), color = transparent(0.4))

# user interface
ui <- fluidPage(
  
  # set theme
  theme = bs_theme(version = 4, bootswatch = "journal"),
  
  # some quick inline css
  tags$head(
    tags$style(
      HTML('
         div.container-fluid {
           max-width: 1400px;
         }
      
         .navbar {
           padding: 0.5rem 0rem;
           border-bottom: 1px solid rgba(0,0,0,0.3) !important;
         }
         
         .navbar-toggle {
           line-height: 0px;
           border-width: 0px;
         }
         
         .navbar.navbar-default {
           border: none;
         }
         
         .navbar-nav > li {
           padding-left: 10px;
           padding-right: 10px;
         }
         
         .navbar-light .navbar-toggler, .navbar-light .navbar-toggle, .navbar.navbar-default .navbar-toggler, .navbar.navbar-default .navbar-toggle {
           border-color: rgba(0,0,0,0.2);
         }
         
         #home_title_prefix {
           font-size: 1.05rem;
         }
         
         #home_title_english {
           margin-top: 0.1rem;
           margin-bottom: .5rem;
           font-family: "News Cycle","Arial Narrow Bold",sans-serif;
           font-weight: 700;
           line-height: 1.1;
         }
         
         .home-content {
           text-align: justify;
           margin-left: -10px;
           margin-right: -10px;
         }
         
         #record_upload {
           margin-bottom:4px;
         }
      
         .dropdown-toggle::after {
           display: none;
         }
         
         #settings_dropdown {
           line-height: 0px;
         }
         
         .dropdown-menu {
           margin: .75rem 0 0;
         }
         
         #dropdown-menu-settings_dropdown {
           box-shadow: 0 50px 100px rgba(50,50,93,.1),0 15px 35px rgba(50,50,93,.15),0 5px 15px rgba(0,0,0,.1);
         }
         
         .btn-group {
           margin-bottom: 4px;
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
         
         .bootstrap-select .dropdown-toggle .filter-option {
           text-align: center;
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
           background-color: #e1e1e1;
         }
         
         .bar-chart-background-missing {
           background-color: #f5f5f5;
         }
        
         .bar {
           height: 100%;
         }
         
         #score {
           font-size: 0.9rem;
         }
         
         .shiny-flow-layout > div {
           display: inline-block;
           vertical-align: top;
           padding-right: 20px;
           width: 300px;
         }
         
         .flip-card {
           width: auto;
           height: 150px;
           perspective: 1000px;
           margin-bottom: 20px;
           -webkit-backface-visibility: hidden;
           backface-visibility: hidden;
           -moz-backface-visibility: hidden;
         }
        
         .flip-card-inner {
           border: 1px solid #ced4da;
           border-radius: .25rem;
           position: relative;
           width: 100%;
           height: 100%;
           text-align: center;
           transition: transform 0.9s;
           transform-style: preserve-3d;
           box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
         }
         
         .flip-card:hover .flip-card-inner {
           transform: rotateY(180deg);
         }
        
         .flip-card-front, .flip-card-back {
           position: absolute;
           width: 100%;
           height: 100%;
           transform: rotateX(0deg);
           -webkit-backface-visibility: hidden;
           backface-visibility: hidden;
           -moz-backface-visibility: hidden;
         }
        
         .flip-card-front {
           padding-left: 16px;
           padding-right: 16px;
         }
         
         .flip-card-back {
           transform: rotateY(180deg);
           display: flex;
           align-items: center;
           justify-content: center;
         }
         
         .steps {
           display: -webkit-box;
           display: -ms-flexbox;
           display: flex;
           -ms-flex-wrap: wrap;
           flex-wrap: wrap;
           font-size: 1rem;
           min-height: 2rem;
         }
         
         .steps .step-item {
           margin-top: 0;
           position: relative;
           -webkit-box-flex: 1;
           -ms-flex-positive: 1;
           flex-grow: 1;
           -ms-flex-preferred-size: 0;
           flex-basis: 0;
         }
         
         .steps .step-marker {
           height: 2rem;
           width: 2rem;
           position: absolute;
           left: calc(50% - 1rem);
           color: #fff;
           background-color: #b8b8b8;
           border-radius: 50%;
           text-align: center;
           line-height: 2rem;
           z-index: 1;
         }
         
         .steps .step-item:not(:first-child)::before {
           height: .2em;
           width: 80%;
           bottom: 0;
           left: -40%;
           top: 1rem;
           content: " ";
           display: block;
           position: absolute;
         }
         
         .steps .step-item::before {
           background: -webkit-gradient(linear,right top,left top,color-stop(50%,#dbdbdb),color-stop(50%,#00d1b2));
           background: linear-gradient(to left,#dbdbdb 50%,#00d1b2 50%);
           background-position-x: 0%;
           background-position-y: 0%;
           background-size: auto;
           background-size: 200% 100%;
           background-position: right bottom;
         }
         
         .steps .step-details {
           margin-top: 2rem;
           margin-left: .5em;
           margin-right: .5em;
           padding-top: .2em;
           text-align: center;
         }
         
         .steps .step-title {
           font-size: 1.2rem;
           font-weight: 600;
           margin-top: 5px;
         }
         
         .steps .step-content {
           margin-top: -10px;
           margin-right: auto;
           margin-left: auto;
           max-width: 70%;
         }
         
         .step-complete {
           background-color: #919191 !important;
        	 transform: scale(1);
        	 animation: pulse 3s;
         }
         
         @keyframes pulse {
        	 0% {
        	 	 transform: scale(0.95);
        		 box-shadow: 0 0 0 0 rgba(0, 0, 0, 0.7);
        	 }
        
        	 70% {
         		 transform: scale(1);
         		 box-shadow: 0 0 0 10px rgba(0, 0, 0, 0);
        	 }
        
        	 100% {
        		 transform: scale(0.95);
        		 box-shadow: 0 0 0 0 rgba(0, 0, 0, 0);
        	 }
        }
         
         ul {
           list-style: none;
         }
         
         .boxxy .spinner--5 {
           border: 2px solid rgb(220,112,105);
           animation: spinner5 800ms linear infinite;
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
  
  # and another to allow for <return> to proxy for clicking submit in test
  tags$head(
    tags$script('
        $(function() {
          var $els = $("[data-proxy-click]");
          $.each(
            $els,
            function(idx, el) {
              var $el = $(el);
              var $proxy = $("#" + $el.data("proxyClick"));
              $el.keydown(function (e) {
                if (e.keyCode == 13) {
                  $proxy.click();
                }
              });
            }
          );
        });
    ')
  ),
  
  # set up necessary additional details
  useShinyjs(),
  useWaiter(),
  waiterPreloader(html = spin_5(), color = "#fff", fadeout = 3000),
  
  # create multi-page structure
  navbarPage(
    "当代中文 – Revision Tool",
    collapsible = TRUE,
    id = "page_nav_menu",
    
    # home page
    tabPanel(
      "Home",
      value = "home",
      
      # homepage top
      fluidRow(
        
        # homepage title
        column(
          5,
          offset = 1,
          br(),
          div(strong("A Revision Tool", id = "home_title_prefix"), "For"),
          div(h1("Contemporary Chinese", id = "home_title_english"), p("For Beginners")),
          h1("当代中文"),
          tags$hr(style = "height:20px; visibility:hidden;")
        ),
        
        # upload and download data buttons
        column(
          5,
          br(),
          div(
            style = "display:inline-block;vertical-align:top;",
            simpleFileInput(
              "record_upload",
              label = NULL,
              buttonLabel = div(icon("file-upload"), "Upload Record"),
              multiple = FALSE,
              placeholder = NULL,
              accept = ".tsv"
            )
          ),
          div(
            style = "display:inline-block;vertical-align:top;",
            disabled(
              downloadButton(
                "record_download",
                label = " Download Record",
                icon = icon("file-download")
              )
            )
          ),
          align = "right"
        )
        
      ),
      
      # homepage text
      column(
        10,
        offset = 1,
        div(
          p(
            paste0(
              "Learning Chinese can be challenging. This app provides a set of tools to help ",
              "those starting with the language. It is primarily geared towards students who are native English speakers, ",
              "and it is intended to serve as a complement to the"
            ),
            em("Contemporary Chinese"),
            "book series by Sinolingua."
          ),
          p(
            paste0(
              "The main feature of the app is a recall test, which allows students to evaluate and ",
              "improve their command of the vocabulary introduced by the textbook. The parameters ",
              "of the test can be adjusted freely, so that one can test their ability to recognise characters ",
              "and Pīnyīn, as well as respond appropriately with them."
            )
          ),
          p(
            paste0(
              "Alongside that, the full vocabulary is provided separately to reference. It is ",
              "searchable (in English, Pīnyīn and Chinese) and can be filtered by book chapter. Key phrases introduced by ",
              "the book are also available."
            )
          ),
          p(
            paste0(
              "Students' performance is tracked during a browsing session, but no data ",
              "is retained when the session ends. If you would like to track your performance over time, ",
              "please follow the process below:"
            )
          ),
          class = "home-content"
        ),
        align = "center"
      ),
      
      # create walkthrough for recording performance long-term
      tags$hr(style = "height:30px; visibility:hidden;"),
      column(
        10,
        offset = 1,
        create_steps_html(
          progress_tracking_steps,
          id = "progress_tracking_steps"
        ),
        align = "center"
      ),
      tags$hr(style = "height:15px; visibility:hidden;")
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
                  ) %>%
                    tagAppendAttributes(`data-proxy-click` = "submit_response")
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
              "Score: ",
              textOutput("score", inline = TRUE) %>% 
                tagAppendAttributes(class = "number")
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
      insert_lesson_selector(input_id = "full_vocab_lessons_to_include"),
      br(),
      
      # vocabulary in a table
      reactableOutput("vocab_table")
      
    ),
    
    # new page for key phrases
    tabPanel(
      "Key Phrases",
      value = "key_phrases",
      
      # selection of lessons to restrict vocab to
      br(),
      insert_lesson_selector(input_id = "phrases_lessons_to_include"),
      br(),
      
      # display cards containing key phrases
      fluidRow(
        column(
          12,
          uiOutput("phrase_cards"),
          align = "center"
        )
      ),
      br(),
      br()
      
    )
  
  )

)

# backend server logic
server <- function(input, output, session) {
  
  # move navbar items to the right
  addClass(id = "page_nav_menu", class = "justify-content-end")
  
  # function for ensuring user lesson choices are sensible
  adjust_lesson_selection <- function(selector_id, aggregate_cat = "all", update_func) {
    
    # if nothing selected
    if (is.null(input[[selector_id]])) {
      
      # change to "all" category
      update_func(
        session = session,
        inputId = selector_id,
        selected = aggregate_cat
      )
      
      # if "all" chosen alongside individual lessons
    } else if (length(input[[selector_id]]) > 1 &&
               aggregate_cat %in% input[[selector_id]]) {
      
      # remove "all" and retain individual lessons
      # (doesn't make sense to include everything and a subset - prioritise subset)
      update_func(
        session = session,
        inputId = selector_id,
        selected = input[[selector_id]] %>%
                     remove_catch_all_category(name = aggregate_cat)
      )
      
    }
  }
  
  # initialise reactive list to keep track of background variables that change
  tracked_obs <- reactiveValues()
  
  # initialise empty df to add previously-served questions to
  previous_question_entries <- vocab %>% slice(0)
  
  # initialise score counters
  current_score <- 0
  question_number <- 0
  
  # initialise empty dataframe for storing question-response info (for result charts)
  tracked_obs$question_response_history <- tibble(
                                             date_time = Date(),
                                             entry_id = integer(),
                                             prompt_type = character(),
                                             response_type = character(),
                                             sampling_type = character(),
                                             test_lesson_selection = character(),
                                             prompt = character(),
                                             correct_response = character(),
                                             actual_response = character(),
                                             was_correct = logical()
                                           )
  
  # track a download counter (basically, increments when downloadButton pressed)
  tracked_obs$download_counter <- 0
  
  # track number of times lesson selector for vocab table has closed
  tracked_obs$vocab_tbl_selector_closed <- 0
  tracked_obs$phrases_selector_closed <- 0
  
  # get dimensions of window for selectively hiding elements on small screens, etc
  get_window_dimensions <- reactive({
                             tibble(
                               width = input$dimension[1],
                               height = input$dimension[2]
                             )
                           })
  
  # hide entire steps visual if screen too small to accomodate it
  observeEvent(
    get_window_dimensions(), {
      if (get_window_dimensions()$width < 780) {
        shinyjs::hide("progress_tracking_steps", anim = FALSE)
      } else {
        shinyjs::show("progress_tracking_steps", anim = FALSE)
      }
    },
    ignoreInit = TRUE
  )
  
  # replace response history with uploaded record if provided
  observeEvent(
    input$record_upload, {
      tracked_obs$question_response_history <- vroom(input$record_upload$datapath, delim = "\t")
    }
  )
  
  # set up download handler
  output$record_download <- downloadHandler(
                              filename = function() {
                                "contemporary_chinese_record.tsv"
                              },
                              content = function(file) {
                                vroom_write(tracked_obs$question_response_history, file)
                                tracked_obs$download_counter <- tracked_obs$download_counter + 1
                              }
                            )
  
  # update first marker in steps visual when record uploaded (tick and colour)
  # note step id is 1 less than value shown inside marker (i.e., marker 1 has id 0)
  observeEvent(
    input$record_upload, {
      make_step_style_complete(step_id = "0")
    }
  )
  
  # update second marker in steps visual when at least one test question answered
  # also enable download button at the same time
  observeEvent(
    input$submit_response, {
      if (input$submit_response == 1) {
        make_step_style_complete(step_id = "1")
        enable("record_download")
      }
    }
  )
  
  # finally, update third marker when user saves record to local comp
  observeEvent(
    tracked_obs$download_counter, {
        make_step_style_complete(step_id = "2")
    },
    ignoreInit = TRUE
  )
  
  # check that at least one lesson type selected by user; choose all if not
  # remove all if some lessons selected as well
  observeEvent(
    input$settings_dropdown, {
      adjust_lesson_selection(
        "test_lessons_to_include",
        update_func = updateMultiInput
      )
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
                           paste0(current_score, "/", question_number)
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
        
        # show praise if answer is okay (approx)
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
          animation = FALSE,
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
          animation = FALSE,
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
      if (input$submit_response >= 1 & show_score()) {
        shinyjs::show("vocab_test_score_box", anim = TRUE, animType = "fade")
      } else {
        shinyjs::hide("vocab_test_score_box", anim = TRUE, animType = "fade")
      }
    }
  )
  
  # capture record of question, response, correctness etc for vocab table
  observeEvent(
    input$submit_response, {
      tracked_obs$question_response_history <- bind_rows(
                                                tracked_obs$question_response_history,
                                                tibble(
                                                  date_time = now(),
                                                  entry_id = gen_test_question()$entry_id,
                                                  prompt_type = input$prompt_type,
                                                  response_type = input$response_type,
                                                  sampling_type = input$sampling_type,
                                                  test_lesson_selection = paste0(input$test_lessons_to_include, collapse = ", "),
                                                  prompt = gen_test_question()$prompt,
                                                  correct_response = gen_test_question()$correct_response,
                                                  actual_response = input$vocab_test_input,
                                                  was_correct = isTRUE(check_answer_correct())
                                                )
                                              )
    }
  )
  
  # after feedback is displayed, move onto next question
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
        adjust_lesson_selection(
          "full_vocab_lessons_to_include",
          update_func = updatePickerInput
        )
      }
    }
  )
  
  # update counter for closing lesson selector upon interaction
  observeEvent(
    input$full_vocab_lessons_to_include_open, {
      tracked_obs$vocab_tbl_selector_closed <- tracked_obs$vocab_tbl_selector_closed +
                                                 as.numeric(
                                                   isFALSE(
                                                     input$full_vocab_lessons_to_include_open
                                                   )
                                                 )
    }
  )
  
  # get lower margin height below vocab table (min size, plus extra if on big screens)
  get_bottom_margin_size <- reactive({
                              min(0.15 * get_window_dimensions()$height, 150)
                            })
  
  # declare height of a single row in vocab_table
  row_height <- 38
  
  # get table height (38 default height of row if not broken over several lines)
  get_vocab_table_height <- reactive({
                              (get_window_dimensions()$height - get_bottom_margin_size()) %>%
                                `-`(., 130) %>%                           # account for size of options
                                round_to(multiple_of = row_height) %>%    # find height in num of full rows
                                max(., row_height * 2)                    # ensure something is shown
                            })
  
  
  
  # get vocab table data
  get_vocab_table_data <- eventReactive({
                              input$page_nav_menu == "vocab_table"
                              tracked_obs$vocab_tbl_selector_closed
                            }, {
                              Waiter$new(id = "vocab_table", fadeout = 3000)$show()
                              vocab %>%
                               filter(
                                 lesson %in% process_lesson_selection(
                                               input$full_vocab_lessons_to_include
                                             )
                               ) %>%
                               left_join(
                                 tracked_obs$question_response_history %>%
                                   group_by(entry_id) %>%
                                   summarise(total = n(), correct = sum(was_correct)) %>%
                                   mutate(performance = paste0(correct, "/", total)) %>%
                                   select(-c(correct, total)),
                                 by = "entry_id"
                               ) %>% 
                               select(-c(lesson, entry_id))
                            }
                          )
  
  # full vocabulary
  output$vocab_table <- renderReactable(
                          reactable(
                            get_vocab_table_data(),
                            borderless = TRUE,
                            searchable = TRUE,
                            pagination = FALSE,
                            highlight = TRUE,
                            height = get_vocab_table_height(),
                            defaultSorted = NULL,
                            defaultColDef = colDef(headerClass = "header", align = "left"),
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
                                                  div(class = "bar", style = list(width = width, backgroundColor = "#919191"))
                                                )
                                                div(class = "bar-cell", span(class = "number", value), bar)
                                              }
                                            )
                            )
                          )
                        )
  
  # and again, as above, check that at least one lesson type selected by user for phrase cards
  observeEvent(
    input$phrases_lessons_to_include_open, {
      if (isFALSE(input$phrases_lessons_to_include_open)) {
        adjust_lesson_selection(
          "phrases_lessons_to_include",
          update_func = updatePickerInput
        )
      }
    }
  )
  
  # again, update counter for closing lesson selector for phrases upon interaction
  observeEvent(
    input$phrases_lessons_to_include_open, {
      tracked_obs$phrases_selector_closed <- tracked_obs$phrases_selector_closed +
                                             as.numeric(
                                               isFALSE(
                                                 input$phrases_lessons_to_include_open
                                               )
                                             )
    }
  )
  
  # create reactive to pull relevant phrases
  get_phrases <- eventReactive(
                   tracked_obs$phrases_selector_closed, {
                     phrases %>%
                       filter(
                         lesson %in% process_lesson_selection(
                                       input$phrases_lessons_to_include
                                     )
                       ) %>%
                       select(-lesson)
                })
  
  # render cards showing key phrases
  output$phrase_cards <- renderUI({
                           
                           # make cards
                           html_cards <- pmap(get_phrases(), create_card)
                           
                           # now lay out cards left to right + top to bottom
                           do.call(shiny::flowLayout, html_cards)
    
                         })

}

# run it
shinyApp(ui, server)

