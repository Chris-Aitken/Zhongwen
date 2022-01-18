
library(dplyr)
library(lubridate)
library(forcats)
library(ggplot2)
library(cowplot)

# function for getting quantile summary
quibble <- function(x, q = c(0.25, 0.5, 0.75)) {
  tibble("cuts" = quantile(x, q), "quantile" = q)
}

# # declare path to app files locally
# path_to_project <- "~/Documents/Personal/Chinese/Zhongwen"
# 
# # load question history
# question_response_history <- glue("{path_to_project}/../contemporary_chinese_record.tsv") %>%
#                              vroom(delim = "\t")

# function for creating calendar usage intensity graph (questions answered per day)
calc_questions_per_day <- function(df, months_to_plot = 6) {

  # convert datetime to date-only format (remove time)
  df_response_history <- df %>%
                         mutate(date = date(date_time))
  
  # get dates spanning latest date and n-months prior
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
  
  # plot usage over time
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
                    title = "Less",
                    title.position = "left",
                    title.hjust = -10,
                    label.position = "right"
                  )
                ) +
                facet_grid(
                  cols = vars(label),
                  scales = "free_x",
                  space = "free"
                ) +
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
                  legend.margin = margin(-5, 0, 0, 0),
                  legend.text = element_text(size = 13)
                )
  
  # add label to left side of legend - plot width = 8.1 in & height = 2.35 in
  # ggdraw(usage_plot) + 
  #   draw_label("Fewer", 0.8, 0.1525, size = 13)
  usage_plot

}

