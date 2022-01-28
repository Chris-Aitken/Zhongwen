
# load required packages
library(dplyr)
library(tidyr)
library(purrr)
library(forcats)
library(ggplot2)
library(ggradar)

# function for creating the data for the plot
calc_question_type_dist <- function(df) {
  
  # declare names of types to concentrate on
  types <- tibble(names = c("prompt", "response")) %>%
           mutate(col_names = paste0(names, "_type"))
  
  # get frequency of prompt-response pairs
  pair_frequency <- df %>%
                    group_by_at(types$col_names) %>%
                    summarise(n = n(), .groups = "drop") %>%
                    mutate(pair_num = row_number()) %>%
                    pivot_longer(types$col_names, names_to = "type") %>%
                    left_join(types, by = c("type" = "col_names")) %>%
                    select(-type) %>%
                    rename(type = names, language = value)
  
  # now aggregate it
  types_aggregated <- types$names %>%
                      map( ~ {
                        pair_frequency %>%
                          filter(type == .x) %>%
                          group_by(language) %>%
                          summarise(n = sum(n), .groups = "drop") %>%
                          mutate(total = sum(n), frac = (n / total), type = .x)
                      }) %>%
                      bind_rows() %>%
                      mutate(
                        across(
                          c("language", "type"),
                          as_factor
                        )
                      )
  
  # return all
  list(
    types = types,
    pair_frequency = pair_frequency,
    types_aggregated = types_aggregated
  )
  
}

# plot function
plot_question_type_dist <- function(df) {
  
  # get data into shape for plotting package
  plot_df <- df %>%
             mutate(
               type = case_when(
                 type == "prompt" ~ "Prompt Language",
                 type == "response" ~ "Response Language"
               )
             ) %>%
             pivot_wider(
               id_cols = type,
               names_from = language,
               values_from = frac
             )
  
  
  # plot as radar plot
  plot_df %>%
    rename(group = type) %>%
    ggradar(
      centre.y = 0,
      plot.legend = FALSE,
      values.radar = c("", "50%", ""),
      axis.labels = c("English", "Mandarin", "Pinyin"),
      grid.label.size = 5,
      background.circle.colour = "#ffffff",
      gridline.min.linetype = 1,
      gridline.mid.linetype = 1,
      gridline.max.linetype = 1,
      gridline.mid.colour = "#C5CBD3",
      axis.line.colour = "#C5CBD3",
      gridline.label.offset = 0.55,
      axis.label.size = 4.5,
      group.point.size = 3,
      group.line.width = 1,
      group.colours = "#c6392f"
    ) +
    facet_wrap(vars(group)) + 
    theme(
      strip.background = element_blank(),
      strip.text = element_text(size = 14)
    )
  
}

