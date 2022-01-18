

# load required packages
library(dplyr)
library(lubridate)
library(forcats)
library(ggplot2)

# function for storing quantile summary
quibble <- function(x, q = c(0.25, 0.5, 0.75)) {
  tibble("cuts" = quantile(x, q), "quantile" = q)
}

# function for creating calendar usage intensity graph (questions answered per day)
# df expected is the question_response_history from the app
calc_questions_per_day <- function(df, months_to_plot = 6) {

  # convert datetime to date-only format (remove time)
  df_response_history <- df %>%
                         mutate(date = date(date_time))
  
  # get dates spanning latest date's full month and n-months prior
  plot_date_span <- tibble(
                      date = seq(
                        floor_date(
                          today() %m-% months(months_to_plot),
                          unit = "month"
                        ),
                        ceiling_date(today(), unit = "month") - days(1),
                        by = "days"
                      )
                    )
  
  # link it back up to question data + get questions per day
  questions_per_day <- left_join(
                         plot_date_span,
                         df_response_history,
                         by = "date"
                       ) %>%
                       group_by(date) %>%
                       summarise(questions_answered = sum(!is.na(was_correct))) %>%
                       ungroup() %>%
                       mutate(week_start = floor_date(date, unit = "week")) %>%
                       group_by(week_start) %>%
                       mutate(week = cur_group_id(), days_in_week = n()) %>%
                       ungroup() %>% 
                       mutate(
                         weekday = wday(date, label = TRUE) %>% fct_rev(),
                         month = month(date, label = TRUE),
                         year = year(date),
                         month_year = paste0(as.character(month), ", ", year)
                       )
  
  # create labels for facet headings
  month_labels <- questions_per_day %>%
                  distinct(month, month_year) %>%
                  mutate(
                    facet_num = row_number(),
                    month = as.character(month),
                    label = ifelse(
                              facet_num == 1 | month == "Jan",
                              month_year,
                              gsub(",.*$", "", month_year)
                            ) %>%
                            factor() %>%
                            fct_inorder()
                  ) %>%
                  select(-c(month, facet_num))
  
  # link labels (with ordering from factor) back up
  questions_per_day <- questions_per_day %>%
                       left_join(month_labels, by = "month_year")
  
  # get colouring cutoffs
  colour_cuts <- questions_per_day %>%
                 filter(questions_answered > 0) %>%
                 summarise(quibble(questions_answered, c(0.25, 0.5, 0.75))) %>%
                 pull(cuts)
  
  # create new column in data binning counts according to these counts
  questions_per_day <- questions_per_day %>%
                       mutate(
                         colour_group = case_when(
                           questions_answered == 0 ~ 1,
                           questions_answered <= colour_cuts[names(colour_cuts) == "25%"] ~ 2,
                           questions_answered <= colour_cuts[names(colour_cuts) == "50%"] ~ 3,
                           questions_answered <= colour_cuts[names(colour_cuts) == "75%"] ~ 4,
                           TRUE ~ 5
                         ) %>% as_factor()
                       )
  
  # send this out
  questions_per_day
  
}

# function for drawing plot itself
plot_usage_calendar <- function(df) {
  
  # plot usage over time using colour of tiles to represent the number answered
  usage_plot <- df %>%
                ggplot(aes(x = week, y = weekday, fill = colour_group)) + 
                geom_tile(width = 0.85, height = 0.85) +
                scale_y_discrete(
                  labels = c("", "Fri", "", "Wed", "", "Mon", ""),
                  name = ""
                ) +
                scale_x_continuous(
                  labels = NULL,
                  name = ""
                ) +
                scale_fill_manual(
                  values = c("#ebedf0", "#ecb1ac", "#dc7069", "#c6392f", "#84261f"),
                  labels = c("", "", "", "", " More"),
                  name = NULL
                ) +
                guides(
                  fill = guide_legend(
                    title = "Less",           # hacky - use title on left of margin
                    title.position = "top",   # annotates the legend instead of title
                    title.vjust = -6.5,       # title.position = "left" doesn't work
                    title.hjust = -0.4,       # hjust & vjust control positioning, but
                    label.position = "right"  # only when position set to "top"
                  )
                ) +
                facet_grid(
                  cols = vars(label),
                  scales = "free_x",          # allows for months with differing nums of weeks
                  space = "free"              # allows aspect ratio to adjust - height & width (h&w) need to be set
                ) +                           # can't use aspect.ratio in junction with this, so h&w set 'squaredness'
                theme(
                  panel.background = element_blank(),
                  plot.margin = margin(4, 15, 25, 4, "pt"),
                  axis.ticks = element_blank(),
                  strip.background = element_blank(),
                  strip.text = element_text(hjust = 1.025),
                  text = element_text(size = 16),
                  legend.position = "bottom",
                  legend.direction = "horizontal",
                  legend.justification = c(0.9925,0),
                  legend.key.size = unit(14, "pt"),
                  legend.spacing.x = unit(1, "pt"),
                  legend.margin = margin(-25, 0, 0, 0),
                  legend.text = element_text(size = 13),
                  legend.title = element_text(size = 13)
                )
  
  # return plot
  usage_plot

}

