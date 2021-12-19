
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
get_random_entry <- function(df) {
  df %>%
    sample_n(1)
}

# user interface
ui <- fluidPage(
  tableOutput("vocab_to_test"),
  dataTableOutput("vocab_table")
)

# backend server logic
server <- function(input, output, session) {
  output$vocab_to_test <- renderTable(get_random_entry(vocab))
  output$vocab_table <- renderDataTable(
                          select(vocab, -lesson),
                          options = list(pageLength = 10)
                        )
}

# run it
shinyApp(ui, server)