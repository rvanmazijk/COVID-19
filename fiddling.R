library(tidyverse)
library(magrittr)
library(lubridate)

theme_set(theme_minimal())

todays_date <- format(Sys.Date(), "%d %b %Y")

data_dir <- "csse_covid_19_data/csse_covid_19_time_series/"

confirmed <- read_csv(paste0(data_dir, "time_series_covid19_confirmed_global.csv"))
deaths    <- read_csv(paste0(data_dir, "time_series_covid19_deaths_global.csv"))
recovered <- read_csv(paste0(data_dir, "time_series_covid19_recovered_global.csv"))

tidy_data <- function(data, col_name) {
  colnames(data)[1:4] <- c("sub_region", "region", "lat", "lon")
  data %<>%
    gather(date, foo, -sub_region, -region, -lat, -lon) %>%
    mutate(date = date %>%
      str_remove("/20$") %>%
      {paste0("2020/", .)} %>%
      str_replace_all("/", "-") %>%
      as_date()
    )
  colnames(data)[colnames(data) == "foo"] <- col_name
  data
}
confirmed %<>% tidy_data("n_confirmed")
deaths    %<>% tidy_data("n_deaths")
recovered %<>% tidy_data("n_recovered")

data <-
  full_join(confirmed, deaths) %>%
  full_join(recovered) %>%
  mutate(
    case_mortality_rate = n_deaths/n_confirmed,
    n_active_cases      = n_confirmed - n_deaths - n_recovered
  ) %>%
  gather(state, n, n_confirmed, n_deaths, n_recovered, n_active_cases, case_mortality_rate)

data %>%
  filter(region == "Italy", state != "case_mortality_rate") %>%
  ggplot() +
    aes(date, n, colour = state) +
    geom_line() +
    labs(
      title    = "Italy | COVID-19 cases",
      subtitle = todays_date
    )

data %>%
  filter(region == "South Africa", state != "case_mortality_rate") %>%
  ggplot() +
    aes(date, n, colour = state) +
    geom_line() +
    labs(
      title    = "South Africa | COVID-19",
      subtitle = paste0(
        "Confirmed cases (", todays_date, "): ",
        data %$% max(n[region == "South Africa" & state == "n_confirmed"])
      )
    )

data %>%
  filter(
    region == "South Africa",
    !(state %in% c("case_mortality_rate", "n_confirmed"))
  ) %>%
  ggplot() +
    aes(date, n, fill = state) +
    geom_col()

data %>%
  filter(region == "South Africa", state != "case_mortality_rate") %>%
  spread(state, n) %>%
  mutate(
    prop_dead      = n_deaths       / n_confirmed,
    prop_active    = n_active_cases / n_confirmed,
    prop_recovered = n_recovered    / n_confirmed
  ) %>%
  filter(n_confirmed > 100) %>%
  ggplot() +
    aes(date, n_confirmed) +
    geom_line()

data %>%
  filter(region == "South Africa", state == "n_confirmed") %>%
  spread(state, n) %>%
  select(date, n_confirmed) %>%
  arrange(date) %>%
  (function(x) {
    x$p_value <- NULL
    x$p_value[2:nrow(x)] <- x$n_confirmed[2:nrow(x)] / x$n_confirmed[1:(nrow(x) - 1)]
    x
  })() %>%
  filter(n_confirmed > 0) %>%
  gather(state, n, n_confirmed, p_value) %>%
  ggplot() +
    aes(date, n) +
    geom_smooth(colour = "grey50", se = FALSE) +
    geom_line() +
    geom_point() +
    labs(
      title    = "South Africa | COVID-19",
      subtitle = paste0(
        "Confirmed cases (", todays_date, "): ",
        data %$% max(n[region == "South Africa" & state == "n_confirmed"])
      )
    ) +
    facet_grid(state ~ ., scales = "free_y")

data %>%
  filter(region == "South Africa") %>%
  spread(state, n) %>%
  group_by(date) %>%
  mutate(n_active_cases = n_confirmed - n_deaths - n_recovered) %>%
  mutate(prop_active_cases = n_active_cases/n_confirmed) %>%
  ggplot() +
    aes(date, prop_active_cases) +
    geom_smooth(colour = "grey50", se = FALSE) +
    geom_line() +
    geom_point() +
    labs(
      title    = "South Africa | COVID-19",
      subtitle = paste0(
        "Confirmed cases (", todays_date, "): ",
        data %$% max(n[region == "South Africa" & state == "n_confirmed"])
      )
    )

data %>%
  filter(region %in% c("US", "South Africa", "Italy", "China")) %>%
  group_by(region, date, state) %>%
  summarise(n = sum(n)) %>%
  spread(state, n) %>%
  ggplot() +
    aes(date, n_confirmed, colour = region) +
    geom_line() +
    geom_point()

data %>%
  select(region, sub_region, date, state, n) %>%
  mutate(n = ifelse(is.nan(n) | is.na(n), 0, n)) %>%
  filter(region %in% c("US", "South Africa", "Italy", "Spain", "UK")) %>%
  group_by(region, date, state) %>%
  summarise(n = sum(n)) %>%
  spread(state, n) %>%
  select(region, date, n_confirmed) %>%
  ungroup() %>%
  group_by(region) %>%
  arrange(date) %>%
  mutate(n_new = n_confirmed - lag(n_confirmed)) %>%
  mutate(n_new = ifelse(is.na(n_new), 0, n_new)) %>%
  ggplot() +
    aes(n_confirmed, n_new, colour = region) +
    geom_line() +
    #geom_smooth(size = 0.75, se = FALSE, alpha = 0.25) +
    scale_x_log10(lim = c(100, 1000000)) +
    scale_y_log10() +
    geom_abline(intercept = 0, slope = 1, colour = "grey25", linetype = "dashed")

data %>%
  select(region, sub_region, date, state, n) %>%
  mutate(n = ifelse(is.nan(n) | is.na(n), 0, n)) %>%
  filter(region == "South Africa") %>%
  group_by(region, date, state) %>%
  summarise(n = sum(n)) %>%
  spread(state, n) %>%
  select(region, date, n_confirmed) %>%
  ungroup() %>%
  group_by(region) %>%
  arrange(date) %>%
  mutate(n_new = n_confirmed - lag(n_confirmed)) %>%
  mutate(n_new = ifelse(is.na(n_new), 0, n_new)) %>%
  #mutate(n_new_3d_avg = pmap_dbl(list(lag(n_new), n_new, lead(n_new)),
  #  ~mean(c(..1, ..2, ..3))
  #)) %>%
  ggplot() +
    aes(n_confirmed, n_new) +
    #aes(n_confirmed, n_new_3d_avg) +
    geom_point(size = 1.5, alpha = 0.5) +
    geom_line(alpha = 0.5) +
    geom_smooth(size = 0.75, se = FALSE) +
    scale_x_log10(lim = c(1, 10000)) +
    scale_y_log10(lim = c(1, 10000)) +
    geom_abline(intercept = 0, slope = 1, colour = "grey25", linetype = "dashed")

data %>%
  filter(region == "Italy", state != "case_mortality_rate") %>%
  ggplot() +
    aes(date, n, colour = state) +
    geom_point() +
    geom_line()

data %>%
  filter(region == "Italy", state == "case_mortality_rate") %>%
  ggplot() +
    aes(date, n) +
    geom_point() +
    geom_line()
