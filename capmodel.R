# CAPM Analysis with AI Event Dummies
# Based on the Fama–French template, extended for CAPM + AI-event effects
# Author: Vu
# Date: 2025-04-21

# 1. Setup -------------------------------------------------------------------
rm(list = ls())
options(warn = -1)

library(tidyquant)
library(dplyr)
library(lubridate)

# 2. Helper: fetch and compute daily log-returns -----------------------------
get_returns <- function(ticker, start_date, end_date) {
  prices <- tq_get(ticker,
                   from = start_date,
                   to   = end_date,
                   get  = "stock.prices")
  returns <- prices %>%
    arrange(date) %>%
    mutate(logret = log(adjusted / lag(adjusted))) %>%
    select(date, logret) %>%
    filter(!is.na(logret))
  return(returns)
}

# 3. Fetch data -------------------------------------------------------------
start_date <- "2015-01-01"
end_date   <- "2024-12-31"
tickers    <- c("NVDA", "TSLA", "HSY", "COST", "MAR")
covid_date <- as.Date("2020-03-11")

stock_returns  <- lapply(tickers, get_returns, start_date, end_date)
names(stock_returns) <- tickers
market_returns <- get_returns("^GSPC", start_date, end_date) %>%
  rename(logret_mkt = logret)

# 4. Define AI event dates --------------------------------------------------
events <- list(
  Deepfake      = as.Date("2018-04-17"),
  ExecutiveOrder= as.Date("2019-02-11"),
  AlexaLaunch   = as.Date("2020-07-22"),
  ChatGPT       = as.Date("2022-11-30")
)

# 5. Merge and annotate -----------------------------------------------------
capm_data <- lapply(stock_returns, function(df) {
  df %>%
    left_join(market_returns, by = "date") %>%
    mutate(period_covid = ifelse(date < covid_date, "pre_COVID", "post_COVID"))
})
names(capm_data) <- tickers

capm_data <- lapply(capm_data, function(df) {
  for(evt in names(events)) {
    df <- df %>%
      mutate(!!paste0("Post_", evt) := as.integer(date >= events[[evt]]))
  }
  return(df)
})

# 6. Define windows for AI events (short vs long around event) ---------------
windows <- lapply(events, function(ed) {
  list(
    short_start = ed %m-% years(1),
    short_end   = ed %m+% years(1),
    long_start  = ed %m-% years(4),
    long_end    = ed %m+% years(4)
  )
})

# 7. Modeling function with checks -----------------------------------------
event_covid_window_capm <- function(data, stock, event_name, w, covid_period) {
  dummy <- paste0("Post_", event_name)
  df_cov <- filter(data, period_covid == covid_period)
  
  # Short-term subset
  df_short <- filter(df_cov, date >= w$short_start & date <= w$short_end)
  if (nrow(df_short) < 10) {
    fit_short <- NA
    warning(paste(stock, event_name, covid_period, "short window has too few observations"))
  } else {
    fit_short <- tryCatch(summary(lm(as.formula(paste("logret ~ logret_mkt +", dummy)), data = df_short)),
                          error = function(e) NA)
  }
  
  # Long-term subset
  df_long <- filter(df_cov, date >= w$long_start & date <= w$long_end)
  if (nrow(df_long) < 10) {
    fit_long <- NA
    warning(paste(stock, event_name, covid_period, "long window has too few observations"))
  } else {
    fit_long <- tryCatch(summary(lm(as.formula(paste("logret ~ logret_mkt +", dummy)), data = df_long)),
                         error = function(e) NA)
  }
  
  list(
    stock        = stock,
    event        = event_name,
    covid_period = covid_period,
    short_window = c(start = w$short_start, end = w$short_end),
    long_window  = c(start = w$long_start,  end = w$long_end),
    short_model  = fit_short,
    long_model   = fit_long
  )
}

# 8. Run models for all combinations ---------------------------------------
results <- setNames(vector("list", length(tickers)), tickers)
for (sym in tickers) {
  results[[sym]] <- setNames(vector("list", length(events)), names(events))
  for (evt in names(events)) {
    results[[sym]][[evt]] <- list(
      pre_COVID  = event_covid_window_capm(capm_data[[sym]], sym, evt, windows[[evt]], "pre_COVID"),
      post_COVID = event_covid_window_capm(capm_data[[sym]], sym, evt, windows[[evt]], "post_COVID")
    )
  }
}

# 9. Example: inspect NVDA, ChatGPT, post-COVID, long-term ----------------
results$MAR$Deepfake$pre_COVID$long_model

# 10. Summary Statistics for Each Ticker -------------------------------------
summary_stats <- lapply(capm_data, function(df) {
  df_numeric <- df %>% select(where(is.numeric))  # Exclude non-numeric (like period_covid)
  stats_summary <- summary(df_numeric)
  stats_sd <- sapply(df_numeric, sd, na.rm = TRUE)
  list(summary = stats_summary, sd = stats_sd)
})
