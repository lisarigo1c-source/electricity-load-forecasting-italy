# THESIS  - Lisa Rigo
# Day-Ahead Electricity Load Forecasting in Italy:
# A Comparison of Statistical Methods for Multiple Seasonality


# 1. LIBRARIES
{
  packages <- c(
    "readr",
    "dplyr",
    "tidyr",
    "tibble",
    "lubridate",
    "ggplot2",
    "scales",
    "forecast",
    "urca",
    "uroot",
    "stR"
  )
  
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
}
# 2. DATA IMPORT + MANAGE
{
  # Open Power System Data - Time Series, release 2020-10-06.
  data_file <- "time_series_60min_singleindex.csv"
  
  italy_load <- read_csv(
    data_file,
    col_select = c(
      "utc_timestamp",
      "IT_load_actual_entsoe_transparency"
    ),
    col_types = cols(
      utc_timestamp = col_datetime(),
      IT_load_actual_entsoe_transparency = col_double()
    ),
    show_col_types = FALSE
  ) |>
    rename(
      datetime_utc = utc_timestamp,
      load_mw = IT_load_actual_entsoe_transparency
    ) |>
    filter(
      datetime_utc >= as.POSIXct(
        "2015-01-01 01:00:00",
        tz = "UTC"
      ),
      datetime_utc < as.POSIXct(
        "2019-12-31 23:00:00",
        tz = "UTC"
      )
    ) |>
    arrange(datetime_utc)
}
# 3. RECOVER INITIAL ENTSO-E OBSERVATIONS
{
  # ENTSO-E Transparency Platform - Actual Total Load, Italy.
  recovered_hours <- tibble(
    datetime_utc = as.POSIXct(
      c(
        "2014-12-31 23:00:00",
        "2015-01-01 00:00:00"
      ),
      tz = "UTC"
    ),
    load_mw = c(
      24405,
      23126
    )
  )
  
  italy_load_complete <- bind_rows(
    recovered_hours,
    italy_load
  ) |>
    arrange(datetime_utc)
  
  # Check missing values
  colSums(is.na(italy_load_complete))
}
# 4. DEFINE THE 2015-2019 SAMPLE
{
  # Italian civil-time boundaries.
  start_local <- as.POSIXct(
    "2015-01-01 00:00:00",
    tz = "Europe/Rome"
  )
  
  end_local <- as.POSIXct(
    "2020-01-01 00:00:00",
    tz = "Europe/Rome"
  )
  
  start_utc <- with_tz(
    start_local,
    "UTC"
  )
  
  end_utc <- with_tz(
    end_local,
    "UTC"
  )
  
  load_main_complete <- italy_load_complete |>
    filter(
      datetime_utc >= start_utc,
      datetime_utc < end_utc
    ) |>
    arrange(datetime_utc)
}
# 5. CHECK DAYLIGHT-SAVING-TIME DAYS
{
  local_day_check <- load_main_complete |>
    mutate(
      local_date = as.Date(
        format(
          datetime_utc,
          "%Y-%m-%d",
          tz = "Europe/Rome"
        )
      )
    ) |>
    count(
      local_date,
      name = "n_hours"
    )
  
  dst_days <- local_day_check |>
    filter(
      n_hours != 24
    )
  
  print(dst_days)
}
# 6. DAYLIGHT-SAVING-TIME REGULARIZATION
{
  # Convert the original observations to Italian civil time.
  local_raw <- load_main_complete |>
    mutate(
      local_date = as.Date(
        format(
          datetime_utc,
          "%Y-%m-%d",
          tz = "Europe/Rome"
        )
      ),
      local_hour = as.integer(
        format(
          datetime_utc,
          "%H",
          tz = "Europe/Rome"
        )
      )
    )
  
  # Average duplicated autumn hours.
  hourly_grouped <- local_raw |>
    group_by(
      local_date,
      local_hour
    ) |>
    summarise(
      load_mw = mean(load_mw),
      n_original = n(),
      .groups = "drop"
    )
  
  # Create a regular 24-hour civil-time grid.
  civil_calendar <- expand_grid(
    local_date = seq.Date(
      from = as.Date("2015-01-01"),
      to = as.Date("2019-12-31"),
      by = "day"
    ),
    local_hour = 0:23
  )
  
  load_local_regularized <- civil_calendar |>
    left_join(
      hourly_grouped,
      by = c(
        "local_date",
        "local_hour"
      )
    ) |>
    mutate(
      source_type = case_when(
        is.na(n_original) ~ "DST_interpolated",
        n_original == 2 ~ "DST_averaged",
        TRUE ~ "observed"
      )
    )
  
  # Interpolate the missing spring 02:00 positions.
  missing_idx <- which(
    is.na(load_local_regularized$load_mw)
  )
  
  load_local_regularized$load_mw[missing_idx] <- (
    load_local_regularized$load_mw[missing_idx - 1] +
      load_local_regularized$load_mw[missing_idx + 1]
  ) / 2
  
  load_local_regularized$n_original[missing_idx] <- 0
  
  # Create the regular modeling index.
  load_local_regularized <- load_local_regularized |>
    mutate(
      model_index = row_number()
    )
  
  # Check missing values.
  colSums(is.na(load_local_regularized))
}
# 7. ITALIAN PUBLIC HOLIDAYS
{
  # National holidays, Easter Sunday and Easter Monday.
  fixed_holidays <- c(
    "01-01",
    "01-06",
    "04-25",
    "05-01",
    "06-02",
    "08-15",
    "11-01",
    "12-08",
    "12-25",
    "12-26"
  )
  
  fixed_dates <- as.Date(
    unlist(
      lapply(
        2015:2019,
        function(year) {
          paste(
            year,
            fixed_holidays,
            sep = "-"
          )
        }
      )
    )
  )
  
  easter_sunday <- as.Date(
    c(
      "2015-04-05",
      "2016-03-27",
      "2017-04-16",
      "2018-04-01",
      "2019-04-21"
    )
  )
  
  holiday_dates <- sort(
    unique(
      c(
        fixed_dates,
        easter_sunday,
        easter_sunday + 1
      )
    )
  )
}
# 8. CALENDAR VARIABLES
{
  load_full <- load_local_regularized |>
    mutate(
      Year = year(local_date),
      
      Month = month(local_date),
      
      Month_label = factor(
        Month,
        levels = 1:12,
        labels = month.abb
      ),
      
      Weekday = wday(
        local_date,
        week_start = 1
      ),
      
      Weekday_label = factor(
        Weekday,
        levels = 1:7,
        labels = c(
          "Mon", "Tue", "Wed", "Thu",
          "Fri", "Sat", "Sun"
        )
      ),
      
      Hour_num = local_hour,
      
      Hour = factor(
        local_hour,
        levels = 0:23
      ),
      
      isWeekend = factor(
        ifelse(
          Weekday >= 6,
          "Weekend",
          "Weekday"
        ),
        levels = c(
          "Weekday",
          "Weekend"
        )
      ),
      
      isHoliday = local_date %in% holiday_dates,
      
      Holiday = as.integer(isHoliday),
      
      DayType = case_when(
        isHoliday ~ "Holiday",
        Weekday == 6 ~ "Saturday",
        Weekday == 7 ~ "Sunday",
        TRUE ~ "Weekday"
      ),
      
      DayType = factor(
        DayType,
        levels = c(
          "Weekday",
          "Saturday",
          "Sunday",
          "Holiday"
        )
      )
    )
}
# 9. TRAIN-TEST SPLIT
{
  # Training sample: 2015-2018.
  load_train <- load_full |>
    filter(
      local_date < as.Date("2019-01-01")
    )
  
  # Test sample: 2019.
  load_test <- load_full |>
    filter(
      local_date >= as.Date("2019-01-01")
    )
  
  # Final sample sizes.
  nrow(load_train)
  nrow(load_test)
}


# 10. EXPLORATORY DATA ANALYSIS
{
{
  # Year labels for the full-series plot.
  year_breaks <- load_full |>
    filter(
      format(local_date, "%m-%d") == "01-01",
      local_hour == 0
    )
  
  # 10.1 Complete hourly load series
  plot(
    load_full$model_index,
    load_full$load_mw,
    type = "l",
    lwd = 0.5,
    xaxt = "n",
    main = "Hourly Electricity Load in Italy, 2015–2019",
    xlab = "Year",
    ylab = "Electricity load (MW)"
  )
  
  axis(
    side = 1,
    at = year_breaks$model_index,
    labels = year_breaks$Year
  )
}
# 10.2 Daily STL decomposition
{
  ts_hourly_daily <- ts(
    load_full$load_mw,
    frequency = 24
  )
  
  stl_daily <- stl(
    ts_hourly_daily,
    s.window = "periodic"
  )
  
  # Calendar-year positions.
  year_positions <- which(
    load_full$local_hour == 0 &
      format(load_full$local_date, "%m-%d") == "01-01"
  )
  
  axis_positions_daily <- as.numeric(
    time(stl_daily$time.series)
  )[year_positions]
  
  old_par <- par(no.readonly = TRUE)
  
  par(
    mfrow = c(4, 1),
    mar = c(0, 6, 0, 6),
    oma = c(6, 0, 4, 0),
    tck = -0.01,
    xaxt = "n"
  )
  
  plot(
    stl_daily,
    set.pars = NULL,
    range.bars = TRUE,
    main = "Daily STL Decomposition"
  )
  
  par(xaxt = "s")
  
  axis(
    side = 1,
    at = axis_positions_daily,
    labels = load_full$Year[year_positions]
  )
  
  par(old_par)
}
# 10.3 Daily ACF and PACF
{
  acf(
    ts_hourly_daily,
    lag.max = 72,
    main = "ACF — Daily Scale",
    xlab = "Lag (days)"
  )
  
  abline(
    v = c(1, 2, 3),
    col = "red",
    lty = 2
  )
  
  pacf(
    ts_hourly_daily,
    lag.max = 72,
    main = "PACF — Daily Scale",
    xlab = "Lag (days)"
  )
  
  abline(
    v = c(1, 2, 3),
    col = "red",
    lty = 2
  )
}
# 10.4 Weekly STL decomposition
{
  ts_hourly_weekly <- ts(
    load_full$load_mw,
    frequency = 168
  )
  
  stl_weekly <- stl(
    ts_hourly_weekly,
    s.window = "periodic"
  )
  
  axis_positions_weekly <- as.numeric(
    time(stl_weekly$time.series)
  )[year_positions]
  
  old_par <- par(no.readonly = TRUE)
  
  par(
    mfrow = c(4, 1),
    mar = c(0, 6, 0, 6),
    oma = c(6, 0, 4, 0),
    tck = -0.01,
    xaxt = "n"
  )
  
  plot(
    stl_weekly,
    set.pars = NULL,
    range.bars = TRUE,
    main = "Weekly STL Decomposition"
  )
  
  par(xaxt = "s")
  
  axis(
    side = 1,
    at = axis_positions_weekly,
    labels = load_full$Year[year_positions]
  )
  
  par(old_par)
}
# 10.5 Weekly ACF and PACF
{
  acf(
    ts_hourly_weekly,
    lag.max = 336,
    main = "ACF — Weekly Scale",
    xlab = "Lag (weeks)"
  )
  
  abline(
    v = c(
      1 / 7,
      1,
      2
    ),
    col = "red",
    lty = 2
  )
  
  pacf(
    ts_hourly_weekly,
    lag.max = 336,
    main = "PACF — Weekly Scale",
    xlab = "Lag (weeks)"
  )
  
  abline(
    v = c(
      1 / 7,
      1,
      2
    ),
    col = "red",
    lty = 2
  )
}
# 10.6 Annual pattern using monthly mean load
{
  monthly_load <- load_full |>
    group_by(
      Year,
      Month
    ) |>
    summarise(
      mean_load = mean(load_mw),
      .groups = "drop"
    ) |>
    arrange(
      Year,
      Month
    )
  
  ts_monthly <- ts(
    monthly_load$mean_load,
    start = c(2015, 1),
    frequency = 12
  )
  
  stl_annual <- stl(
    ts_monthly,
    s.window = "periodic"
  )
  
  plot(
    stl_annual,
    main = "Annual STL Decomposition of Monthly Mean Load"
  )
  
  acf(
    ts_monthly,
    lag.max = 36,
    main = "ACF of Monthly Mean Electricity Load",
    xlab = "Lag (years)"
  )
  
  abline(
    v = c(1, 2, 3),
    col = "red",
    lty = 2
  )
}
# 10.7 Mean load by month and year
{
  print(
    ggplot(
      monthly_load,
      aes(
        x = Month,
        y = mean_load,
        color = factor(Year),
        group = Year
      )
    ) +
      geom_line(
        linewidth = 1
      ) +
      geom_point(
        size = 2
      ) +
      scale_x_continuous(
        breaks = 1:12,
        labels = month.abb
      ) +
      labs(
        title = "Mean Electricity Load by Month and Year",
        x = "Month",
        y = "Mean electricity load (MW)",
        color = "Year"
      ) +
      theme_minimal()
  )
}
# 10.8 Mean hourly profile by day of the week
{
  hourly_by_weekday <- load_full |>
    group_by(
      Weekday_label,
      Hour_num
    ) |>
    summarise(
      mean_load = mean(load_mw),
      .groups = "drop"
    )
  
  print(
    ggplot(
      hourly_by_weekday,
      aes(
        x = Hour_num,
        y = mean_load,
        color = Weekday_label,
        group = Weekday_label
      )
    ) +
      geom_line(
        linewidth = 1
      ) +
      geom_point(
        size = 1.5
      ) +
      scale_x_continuous(
        breaks = seq(
          0,
          23,
          by = 2
        )
      ) +
      labs(
        title = "Mean Electricity Load by Hour and Day of the Week",
        x = "Hour of day",
        y = "Mean electricity load (MW)",
        color = "Day"
      ) +
      theme_minimal()
  )
  
  # Load distribution by day type
  {
    print(
      ggplot(
        load_full,
        aes(
          x = DayType,
          y = load_mw
        )
      ) +
        geom_boxplot(
          outlier.alpha = 0.15,
          outlier.size = 0.5
        ) +
        labs(
          title = "Electricity Load Distribution by Day Type",
          x = "Day type",
          y = "Electricity load (MW)"
        ) +
        theme_minimal()
    )
  }
  
  # Mean load by hour and day of the week
  {
    weekday_hour_heatmap <- load_full |>
      group_by(
        Weekday_label,
        Hour_num
      ) |>
      summarise(
        mean_load = mean(load_mw),
        .groups = "drop"
      )
    
    print(
      ggplot(
        weekday_hour_heatmap,
        aes(
          x = Hour_num,
          y = Weekday_label,
          fill = mean_load
        )
      ) +
        geom_tile() +
        scale_x_continuous(
          breaks = seq(0, 23, by = 2)
        ) +
        labs(
          title = "Mean Electricity Load by Hour and Day of the Week",
          x = "Hour of day",
          y = "Day of the week",
          fill = "Mean load (MW)"
        ) +
        theme_minimal()
    )
  }
}
# 10.9 Mean hourly profile by year
{
  hourly_by_year <- load_full |>
    group_by(
      Year,
      Hour_num
    ) |>
    summarise(
      mean_load = mean(load_mw),
      .groups = "drop"
    )
  
  print(
    ggplot(
      hourly_by_year,
      aes(
        x = Hour_num,
        y = mean_load,
        color = factor(Year),
        group = Year
      )
    ) +
      geom_line(
        linewidth = 1
      ) +
      geom_point(
        size = 1.5
      ) +
      scale_x_continuous(
        breaks = seq(
          0,
          23,
          by = 2
        )
      ) +
      labs(
        title = "Mean Electricity Load by Hour and Year",
        x = "Hour of day",
        y = "Mean electricity load (MW)",
        color = "Year"
      ) +
      theme_minimal()
  )
}
# 10.10 Mean hourly profile by month
{
  hourly_by_month <- load_full |>
    group_by(
      Month_label,
      Hour_num
    ) |>
    summarise(
      mean_load = mean(load_mw),
      .groups = "drop"
    )
  
  print(
    ggplot(
      hourly_by_month,
      aes(
        x = Hour_num,
        y = mean_load
      )
    ) +
      geom_line(
        linewidth = 1
      ) +
      facet_wrap(
        ~ Month_label,
        ncol = 3
      ) +
      scale_x_continuous(
        breaks = c(
          0,
          6,
          12,
          18,
          23
        )
      ) +
      labs(
        title = "Mean Daily Electricity Load Profile by Month",
        x = "Hour of day",
        y = "Mean electricity load (MW)"
      ) +
      theme_minimal()
  )
}
# 10.11 Mean load by weekday and year
{
  weekday_by_year <- load_full |>
    group_by(
      Year,
      Weekday_label
    ) |>
    summarise(
      mean_load = mean(load_mw),
      .groups = "drop"
    )
  
  print(
    ggplot(
      weekday_by_year,
      aes(
        x = Weekday_label,
        y = mean_load,
        color = factor(Year),
        group = Year
      )
    ) +
      geom_line(
        linewidth = 1
      ) +
      geom_point(
        size = 2
      ) +
      labs(
        title = "Mean Electricity Load by Day of the Week and Year",
        x = "Day of the week",
        y = "Mean electricity load (MW)",
        color = "Year"
      ) +
      theme_minimal()
  )
}
# 10.12 Load distributions
{
  # Distribution by hour of day.
  print(
    ggplot(
      load_full,
      aes(
        x = Hour,
        y = load_mw
      )
    ) +
      geom_boxplot(
        outlier.alpha = 0.15,
        outlier.size = 0.5
      ) +
      labs(
        title = "Electricity Load Distribution by Hour",
        x = "Hour of day",
        y = "Electricity load (MW)"
      ) +
      theme_minimal()
  )
  
  # Distribution by day of the week.
  print(
    ggplot(
      load_full,
      aes(
        x = Weekday_label,
        y = load_mw
      )
    ) +
      geom_boxplot(
        outlier.alpha = 0.15,
        outlier.size = 0.5
      ) +
      labs(
        title = "Electricity Load Distribution by Day of the Week",
        x = "Day of the week",
        y = "Electricity load (MW)"
      ) +
      theme_minimal()
  )
  
  # Distribution by month.
  print(
    ggplot(
      load_full,
      aes(
        x = Month_label,
        y = load_mw
      )
    ) +
      geom_boxplot(
        outlier.alpha = 0.15,
        outlier.size = 0.5
      ) +
      labs(
        title = "Electricity Load Distribution by Month",
        x = "Month",
        y = "Electricity load (MW)"
      ) +
      theme_minimal()
  )
}
# 10.13 Mean hourly profile by day type
{
  hourly_by_daytype <- load_full |>
    group_by(
      DayType,
      Hour_num
    ) |>
    summarise(
      mean_load = mean(load_mw),
      .groups = "drop"
    )
  
  print(
    ggplot(
      hourly_by_daytype,
      aes(
        x = Hour_num,
        y = mean_load,
        color = DayType,
        group = DayType
      )
    ) +
      geom_line(
        linewidth = 1
      ) +
      geom_point(
        size = 1.5
      ) +
      scale_x_continuous(
        breaks = c(
          0, 4, 8, 12, 16, 20, 23
        )
      ) +
      labs(
        title = "Mean Electricity Load by Hour and Day Type",
        x = "Hour of day",
        y = "Mean electricity load (MW)",
        color = "Day type"
      ) +
      theme_minimal()
  )
}
# 10.14 Daily mean load around August
{
  summer_daily <- load_full |>
    filter(
      (Month == 7 & day(local_date) >= 15) |
        Month == 8 |
        (Month == 9 & day(local_date) <= 15)
    ) |>
    group_by(
      Year,
      local_date
    ) |>
    summarise(
      mean_load = mean(load_mw),
      .groups = "drop"
    ) |>
    mutate(
      # Common year used only to align calendar days across years.
      PlotDate = as.Date(
        paste0(
          "2000-",
          format(local_date, "%m-%d")
        )
      )
    )
  
  print(
    ggplot(
      summer_daily,
      aes(
        x = PlotDate,
        y = mean_load
      )
    ) +
      geom_vline(
        xintercept = as.numeric(
          as.Date(
            c(
              "2000-08-01",
              "2000-09-01"
            )
          )
        ),
        linetype = "dashed"
      ) +
      geom_vline(
        xintercept = as.numeric(
          as.Date("2000-08-15")
        ),
        linetype = "dashed"
      ) +
      geom_line(
        linewidth = 0.7
      ) +
      facet_wrap(
        ~ Year,
        ncol = 2
      ) +
      scale_x_date(
        breaks = as.Date(
          c(
            "2000-07-15",
            "2000-08-01",
            "2000-08-15",
            "2000-09-01",
            "2000-09-15"
          )
        ),
        date_labels = "%d %b"
      ) +
      labs(
        title = "Daily Mean Electricity Load Around August",
        x = "Calendar day",
        y = "Mean electricity load (MW)"
      ) +
      theme_minimal()
  )
}
# 10.15 Extreme observations
{
  # Report civil date and hour rather than artificial timestamps.
  lowest_load <- load_full |>
    slice_min(
      load_mw,
      n = 10,
      with_ties = FALSE
    ) |>
    select(
      local_date,
      local_hour,
      load_mw,
      Weekday_label,
      isHoliday,
      source_type
    )
  
  highest_load <- load_full |>
    slice_max(
      load_mw,
      n = 10,
      with_ties = FALSE
    ) |>
    select(
      local_date,
      local_hour,
      load_mw,
      Weekday_label,
      isHoliday,
      source_type
    )
  
  print(lowest_load)
  print(highest_load)
}
# 11. STATIONARITY AND SEASONAL STABILITY
# 11.1 Augmented Dickey-Fuller test
{
  y_train_mw <- as.numeric(
    load_train$load_mw
  )
  
  n_train <- length(y_train_mw)
  
  # Schwert-type maximum lag order.
  adf_max_lags <- floor(
    12 * (n_train / 100)^(1 / 4)
  )
  
  # ADF with intercept.
  adf_drift <- ur.df(
    y_train_mw,
    type = "drift",
    lags = adf_max_lags,
    selectlags = "BIC"
  )
  
  summary(adf_drift)
  
  # ADF with intercept and linear trend.
  adf_trend <- ur.df(
    y_train_mw,
    type = "trend",
    lags = adf_max_lags,
    selectlags = "BIC"
  )
  
  summary(adf_trend)
}
# 11.2 Annual seasonal stability
{
  # Monthly series for a fixed weekday-hour profile.
  build_monthly_profile <- function(data, weekday_id, hour_id) {
    
    data |>
      filter(
        Weekday == weekday_id,
        local_hour == hour_id,
        !isHoliday
      ) |>
      group_by(
        Year,
        Month
      ) |>
      summarise(
        mean_load = mean(load_mw),
        .groups = "drop"
      ) |>
      arrange(
        Year,
        Month
      )
  }
  
  # Monday at 08:00.
  annual_mon8 <- build_monthly_profile(
    data = load_train,
    weekday_id = 1,
    hour_id = 8
  )
  
  ts_annual_mon8 <- ts(
    annual_mon8$mean_load,
    start = c(2015, 1),
    frequency = 12
  )
  
  # Saturday at 08:00.
  annual_sat8 <- build_monthly_profile(
    data = load_train,
    weekday_id = 6,
    hour_id = 8
  )
  
  ts_annual_sat8 <- ts(
    annual_sat8$mean_load,
    start = c(2015, 1),
    frequency = 12
  )
  
  ch_annual_mon8 <- ch.test(
    ts_annual_mon8
  )
  
  ch_annual_sat8 <- ch.test(
    ts_annual_sat8
  )
  
  print(ch_annual_mon8)
  print(ch_annual_sat8)
}
}
# Lag dependence at relevant hourly lags
{
  load_train_ts <- ts(
    load_train$load_mw,
    frequency = 168
  )
  
gglagplot(
  load_train_ts,
  set.lags = c(1, 24, 168),
  do.lines = FALSE,
  colour = FALSE,
  seasonal = FALSE
) +
  scale_x_continuous(
    labels = scales::label_number(
      scale_cut = scales::cut_short_scale()
    )
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      scale_cut = scales::cut_short_scale()
    )
  ) +
  labs(
    title = "Lagged Dependence in Hourly Electricity Load",
    x = "Lagged electricity load (MW)",
    y = "Electricity load (MW)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold")
  )
#the lag plots reveal strong short-term persistence and substantial dependence at the daily and weekly seasonal lags.
}

# MODELS
{
# 12. WEEKLY SEASONAL NAIVE BENCHMARK
{
{
  H_naive <- 24
  M_naive <- 168
  
  train_naive <- as.numeric(
    load_train$load_mw
  )
  
  # Weekly seasonal-naive point forecast.
  snaive_fit <- snaive(
    ts(
      train_naive,
      frequency = M_naive
    ),
    h = H_naive
  )
  
  point_naive <- as.numeric(
    snaive_fit$mean
  )
  
  snaive_forecast <- data.frame(
    model_index = load_test$model_index[1:H_naive],
    local_date = load_test$local_date[1:H_naive],
    local_hour = load_test$local_hour[1:H_naive],
    observed_mw = load_test$load_mw[1:H_naive],
    point_forecast_mw = point_naive
  )
  
  print(snaive_forecast)
}
# 12.1 Bootstrap prediction intervals
{
  B_naive <- 1000
  alpha_naive <- 0.05
  
  # Historical weekly seasonal-naive residuals.
  residuals_naive <-
    train_naive[(M_naive + 1):length(train_naive)] -
    train_naive[1:(length(train_naive) - M_naive)]
  
  # Nonparametric residual bootstrap.
  set.seed(123)
  
  bootstrap_residuals <- matrix(
    sample(
      residuals_naive,
      size = B_naive * H_naive,
      replace = TRUE
    ),
    nrow = B_naive,
    ncol = H_naive
  )
  
  bootstrap_paths <- sweep(
    bootstrap_residuals,
    MARGIN = 2,
    STATS = point_naive,
    FUN = "+"
  )
  
  lower95_naive <- apply(
    bootstrap_paths,
    2,
    quantile,
    probs = alpha_naive / 2
  )
  
  upper95_naive <- apply(
    bootstrap_paths,
    2,
    quantile,
    probs = 1 - alpha_naive / 2
  )
  
  snaive_forecast$lower95_mw <- lower95_naive
  snaive_forecast$upper95_mw <- upper95_naive
  
  print(snaive_forecast)
}
# 12.2 Day-ahead forecast plot
{
  observed_short <- bind_rows(
    tail(load_train, 48),
    load_test[1:H_naive, ]
  )
  
  print(
    ggplot() +
      geom_ribbon(
        data = snaive_forecast,
        aes(
          x = model_index,
          ymin = lower95_mw,
          ymax = upper95_mw
        ),
        alpha = 0.20
      ) +
      geom_line(
        data = observed_short,
        aes(
          x = model_index,
          y = load_mw
        ),
        linewidth = 0.8
      ) +
      geom_line(
        data = snaive_forecast,
        aes(
          x = model_index,
          y = point_forecast_mw
        ),
        linewidth = 0.9,
        linetype = "dashed"
      ) +
      geom_vline(
        xintercept = tail(load_train$model_index, 1) + 0.5,
        linetype = "dashed"
      ) +
      labs(
        title = "Weekly Seasonal Naive — Day-Ahead Forecast",
        subtitle = "95% bootstrap prediction intervals",
        x = "Modeling index",
        y = "Electricity load (MW)"
      ) +
      theme_minimal()
  )
}
}
# 13. MSTL + ARIMA
  {
{
# 13.1 Log transformation
{
    daily_variation <- load_train |>
      group_by(local_date) |>
      summarise(
        mean_mw = mean(load_mw),
        sd_mw = sd(load_mw),
        mean_log = mean(log(load_mw)),
        sd_log = sd(log(load_mw)),
        .groups = "drop"
      )
    
    variance_diagnostic <- data.frame(
      Scale = c(
        "Original MW",
        "Natural logarithm"
      ),
      Pearson = c(
        cor(
          daily_variation$mean_mw,
          daily_variation$sd_mw,
          method = "pearson"
        ),
        cor(
          daily_variation$mean_log,
          daily_variation$sd_log,
          method = "pearson"
        )
      ),
      Spearman = c(
        cor(
          daily_variation$mean_mw,
          daily_variation$sd_mw,
          method = "spearman"
        ),
        cor(
          daily_variation$mean_log,
          daily_variation$sd_log,
          method = "spearman"
        )
      )
    )
    
    print(variance_diagnostic)
    
    # Mean-variance relationship before and after log transformation.
    old_par <- par(no.readonly = TRUE)
    
    par(mfrow = c(1, 2))
    
    plot(
      daily_variation$mean_mw,
      daily_variation$sd_mw,
      xlab = "Daily mean load (MW)",
      ylab = "Daily standard deviation (MW)",
      main = "Original Scale"
    )
    
    plot(
      daily_variation$mean_log,
      daily_variation$sd_log,
      xlab = "Daily mean log-load",
      ylab = "Daily standard deviation of log-load",
      main = "Log Scale"
    )
    
    par(old_par)
    
    # Log transformation for variance stabilization.
    mstl_lambda <- 0
  }
# 13.2 MSTL decomposition
{
  # Daily, weekly and approximate annual seasonal periods.
  mstl_periods <- c(
    24,
    168,
    8766
  )
  
  # Standard MSTL seasonal smoothing windows.
  mstl_windows <- c(
    11,
    15,
    19
  )
  
  # Log transformation.
  mstl_lambda <- 0
  
  mstl_train <- msts(
    load_train$load_mw,
    seasonal.periods = mstl_periods
  )
  
  mstl_fit <- mstl(
    mstl_train,
    lambda = mstl_lambda,
    s.window = mstl_windows,
    iterate = 2,
    robust = FALSE
  )
  
  seasonal_cols <- c(
    "Seasonal24",
    "Seasonal168",
    "Seasonal8766"
  )
  
  # Seasonally adjusted series on the log scale.
  mstl_adjusted_log <- ts(
    as.numeric(
      mstl_fit[, "Trend"] +
        mstl_fit[, "Remainder"]
    ),
    frequency = 1
  )
}
# 13.3 MSTL decomposition diagnostics
{
    print(
      autoplot(mstl_fit) +
        labs(
          title = "MSTL Decomposition of Italian Electricity Load",
          subtitle = "Seasonal periods: 24, 168 and 8766 hours",
          x = NULL,
          y = NULL
        )
    )
}
# 13.4 MSTL component strength
{
    component_strength <- function(component, remainder) {
      max(
        0,
        1 -
          var(remainder) /
          var(component + remainder)
      )
    }
    
    mstl_strength <- data.frame(
      Component = c(
        "Daily",
        "Weekly",
        "Annual",
        "Total seasonality",
        "Trend"
      ),
      
      Strength = c(
        component_strength(
          mstl_fit[, "Seasonal24"],
          mstl_fit[, "Remainder"]
        ),
        
        component_strength(
          mstl_fit[, "Seasonal168"],
          mstl_fit[, "Remainder"]
        ),
        
        component_strength(
          mstl_fit[, "Seasonal8766"],
          mstl_fit[, "Remainder"]
        ),
        
        component_strength(
          rowSums(
            mstl_fit[, seasonal_cols, drop = FALSE]
          ),
          mstl_fit[, "Remainder"]
        ),
        
        component_strength(
          mstl_fit[, "Trend"],
          mstl_fit[, "Remainder"]
        )
      )
    )
    
    print(mstl_strength)
}  
# 13.5 ARIMA differencing and identification
{
    # Maximum ADF lag order.
    adf_max_lag <- floor(
      12 * (length(mstl_adjusted_log) / 100)^(1 / 4)
    )
    
    # ADF with intercept.
    adf_mstl <- ur.df(
      mstl_adjusted_log,
      type = "drift",
      lags = adf_max_lag,
      selectlags = "BIC"
    )
    
    # ADF with intercept and linear trend.
    adf_mstl_trend <- ur.df(
      mstl_adjusted_log,
      type = "trend",
      lags = adf_max_lag,
      selectlags = "BIC"
    )
    
    summary(adf_mstl)
    summary(adf_mstl_trend)
    
    # ADF evidence supports d = 0.
    mstl_adjusted_ts <- ts(
      as.numeric(mstl_adjusted_log),
      frequency = 1
    )
    
    # ACF and PACF for ARMA identification.
    old_par <- par(no.readonly = TRUE)
    
    par(
      mfrow = c(2, 1),
      mar = c(4, 4, 3, 1)
    )
    
    acf(
      mstl_adjusted_ts,
      lag.max = 336,
      main = "ACF — MSTL Adjusted Log-Load",
      xlab = "Lag (hours)"
    )
    
    abline(
      v = c(24, 48, 72, 168, 336),
      col = "red",
      lty = 2
    )
    
    pacf(
      mstl_adjusted_ts,
      lag.max = 336,
      main = "PACF — MSTL Adjusted Log-Load",
      xlab = "Lag (hours)"
    )
    
    abline(
      v = c(24, 48, 72, 168, 336),
      col = "red",
      lty = 2
    )
    
    par(old_par)
} 
  # 13.6 ARIMA model selection
  {
    candidate_orders <- list(
      ARIMA_100 = c(1, 0, 0),
      ARIMA_200 = c(2, 0, 0),
      ARIMA_300 = c(3, 0, 0),
      ARIMA_101 = c(1, 0, 1),
      ARIMA_201 = c(2, 0, 1),
      ARIMA_202 = c(2, 0, 2),
      ARIMA_301 = c(3, 0, 1),
      ARIMA_302 = c(3, 0, 2)
    )
    
    manual_fits <- lapply(
      candidate_orders,
      function(order) {
        Arima(
          mstl_adjusted_ts,
          order = order,
          include.mean = TRUE,
          method = "ML"
        )
      }
    )
    
    manual_comparison <- do.call(
      rbind,
      lapply(
        names(manual_fits),
        function(model) {
          fit <- manual_fits[[model]]
          
          data.frame(
            Model = model,
            AIC = AIC(fit),
            AICc = fit$aicc,
            BIC = BIC(fit)
          )
        }
      )
    ) |>
      arrange(AICc)
    
    print(manual_comparison)
    
    # Final specification selected by auto.arima with d = 0.
    mstl_arima_final <- auto.arima(
      mstl_adjusted_ts,
      d = 0,
      seasonal = FALSE,
      ic = "aicc",
      stepwise = FALSE,
      approximation = FALSE
    )
    
    summary(mstl_arima_final)
    arimaorder(mstl_arima_final)
  }
}
# 13.7 Residual diagnostics
{
    # Ljung-Box test and residual diagnostics.
    checkresiduals(
      mstl_arima_final,
      lag = 168
    )
    
    # Residual ACF and PACF.
    old_par <- par(no.readonly = TRUE)
    
    par(mfrow = c(2, 1))
    
    acf(
      residuals(mstl_arima_final),
      lag.max = 336,
      main = "ACF — ARIMA(3,0,2) Residuals",
      xlab = "Lag (hours)"
    )
    
    abline(
      v = c(24, 48, 72, 168, 336),
      col = "red",
      lty = 2
    )
    
    pacf(
      residuals(mstl_arima_final),
      lag.max = 336,
      main = "PACF — ARIMA(3,0,2) Residuals",
      xlab = "Lag (hours)"
    )
    
    abline(
      v = c(24, 48, 72, 168, 336),
      col = "red",
      lty = 2
    )
    
    par(old_par)
  }  
# 13.8 First day-ahead forecast
{
    H_mstl <- 24
    B_mstl <- 1000
    alpha_mstl <- 0.05
    
    forecast_date_mstl <- as.Date(
      "2019-01-01"
    )
    
    future_mstl <- load_test |>
      filter(
        local_date == forecast_date_mstl
      ) |>
      arrange(local_hour)
    
    # Forecast each MSTL seasonal component by repeating
    # its final estimated seasonal cycle.
    future_seasonal_components <- sapply(
      seq_along(mstl_periods),
      function(j) {
        
        last_cycle <- tail(
          mstl_fit[, seasonal_cols[j]],
          mstl_periods[j]
        )
        
        rep(
          last_cycle,
          length.out = H_mstl
        )
      }
    )
    
    colnames(future_seasonal_components) <-
      seasonal_cols
    
    future_seasonal_sum <- rowSums(
      future_seasonal_components
    )
    
    # Forecast Trend + Remainder with ARIMA(3,0,2).
    adjusted_forecast <- forecast(
      mstl_arima_final,
      h = H_mstl
    )
    
    adjusted_mean_log <- as.numeric(
      adjusted_forecast$mean
    )
    
    # Complete forecast on the log scale.
    forecast_log <- adjusted_mean_log +
      future_seasonal_sum
    
    # Direct exponential back-transformation, without bias adjustment.
    forecast_direct_exp_mw <- exp(
      forecast_log
    )
  } 
# 13.9  Bootstrap and prediction intervals
{
    set.seed(20190101)
    
    #  bootstrap paths for Trend + Remainder.
    adjusted_boot_paths <- replicate(
      B_mstl,
      as.numeric(
        simulate(
          mstl_arima_final,
          nsim = H_mstl,
          future = TRUE,
          bootstrap = TRUE
        )
      )
    )
    
    # Add the fixed MSTL seasonal components.
    log_boot_paths <- sweep(
      adjusted_boot_paths,
      MARGIN = 1,
      STATS = future_seasonal_sum,
      FUN = "+"
    )
    
    # Back-transform complete simulated paths to MW.
    mw_boot_paths <- exp(
      log_boot_paths
    )
    
    # Direct exponential back-transformation, without bias adjustment.
    point_forecast_mw <- exp(forecast_log)
    
    # 95% bootstrap prediction intervals.
    lower95_mstl <- apply(
      mw_boot_paths,
      1,
      quantile,
      probs = alpha_mstl / 2
    )
    
    upper95_mstl <- apply(
      mw_boot_paths,
      1,
      quantile,
      probs = 1 - alpha_mstl / 2
    )
    
    mstl_forecast <- data.frame(
      local_date = future_mstl$local_date,
      local_hour = future_mstl$local_hour,
      observed_mw = future_mstl$load_mw,
      point_forecast_mw = point_forecast_mw,
      lower95_mw = lower95_mstl,
      upper95_mw = upper95_mstl
    )
    
    print(mstl_forecast)
  }
# 13.10 First day-ahead forecast plot
{
    make_local_datetime <- function(date, hour) {
      as.POSIXct(
        paste(
          date,
          sprintf("%02d:00:00", hour)
        ),
        tz = "Europe/Rome"
      )
    }
    
    # Last 72 training hours.
    plot_train_mstl <- tail(
      load_train,
      72
    ) |>
      mutate(
        datetime = make_local_datetime(
          local_date,
          local_hour
        )
      )
    
    # Forecast day.
    plot_forecast_mstl <- mstl_forecast |>
      mutate(
        datetime = make_local_datetime(
          local_date,
          local_hour
        )
      )
    
    split_time <- min(
      plot_forecast_mstl$datetime
    )
    
    print(
      ggplot() +
        geom_ribbon(
          data = plot_forecast_mstl,
          aes(
            x = datetime,
            ymin = lower95_mw,
            ymax = upper95_mw,
            fill = "95% PI"
          ),
          alpha = 0.25
        ) +
        geom_line(
          data = plot_train_mstl,
          aes(
            x = datetime,
            y = load_mw,
            colour = "Observed train"
          ),
          linewidth = 0.7
        ) +
        geom_line(
          data = plot_forecast_mstl,
          aes(
            x = datetime,
            y = observed_mw,
            colour = "Observed test"
          ),
          linewidth = 0.8
        ) +
        geom_line(
          data = plot_forecast_mstl,
          aes(
            x = datetime,
            y = point_forecast_mw,
            colour = "Forecast"
          ),
          linewidth = 0.9
        ) +
        geom_vline(
          xintercept = split_time,
          linetype = "dashed"
        ) +
        scale_x_datetime(
          date_breaks = "12 hours",
          date_labels = "%d %b\n%H:%M",
          timezone = "Europe/Rome"
        ) +
        labs(
          title = "MSTL + ARIMA(3,0,2) — Day-Ahead Forecast",
          subtitle = "Forecast for 1 January 2019",
          x = "Local time (Europe/Rome)",
          y = "Electricity load (MW)",
          colour = NULL,
          fill = NULL
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom"
        )
    )
}
}
# 14. STR + ARMA
{
# 14.1 STR model setup
{
  # Daily, weekly and approximate annual seasonal periods.
  str_periods <- c(
    24,
    168,
    8766
  )
  
  # Log-transformed training series.
  str_train_msts <- msts(
    log(load_train$load_mw),
    seasonal.periods = str_periods
  )
  
  # Working days: Monday-Friday excluding public holidays.
  str_calendar <- load_full |>
    mutate(
      is_working_day =
        Weekday <= 5 & !isHoliday,
      
      # Separate daily positions for working and non-working days.
      str_day_position =
        local_hour +
        if_else(
          is_working_day,
          0L,
          100L
        )
    )
  
  # Hourly working/non-working daily topology.
  str_daily_structure <- list(
    segments = list(
      c(0, 24),
      c(100, 124)
    ),
    
    sKnots = c(
      as.list(
        c(
          1:23,
          101:123
        )
      ),
      
      list(
        c(
          0, 24,
          100, 124
        )
      )
    )
  )
}
# 14.2 STR predictors
{
    str_periods <- attr(
      str_train_msts,
      "msts"
    )
    
    str_data <- as.vector(
      str_train_msts
    )
    
    str_times <- as.vector(
      time(str_train_msts)
    )
    
    # Trend predictor.
    trend_structure <- list(
      segments = list(
        c(0, 1)
      ),
      sKnots = list(
        c(1, 0)
      )
    )
    
    trend_time_knots <- seq(
      from = first(str_times),
      to = last(str_times),
      length.out = max(
        16,
        length(str_data) /
          max(str_periods) * 2 + 1
      )
    )
    
    str_trend <- list(
      name = "Trend",
      data = rep(1, length(str_data)),
      times = str_times,
      seasons = rep(1, length(str_data)),
      timeKnots = trend_time_knots,
      seasonalStructure = trend_structure,
      lambdas = c(1, 0, 0)
    )
    
    # Construct seasonal predictors following the AutoSTR structure.
    make_str_seasonal_predictor <- function(
    period,
    periods,
    data,
    times
    ) {
      
      previous_period <- max(
        1,
        periods[periods < period]
      )
      
      n_season_knots <- floor(
        period / sqrt(previous_period)
      ) + 1
      
      seasonal_structure <- list(
        segments = list(
          c(0, period)
        ),
        sKnots = c(
          as.list(
            tail(
              head(
                seq(
                  0,
                  period,
                  length.out = n_season_knots
                ),
                -1
              ),
              -1
            )
          ),
          list(
            c(period, 0)
          )
        )
      )
      
      seasonal_time_knots <- seq(
        from = first(times),
        to = last(times),
        length.out =
          length(data) /
          max(periods) + 1
      )
      
      list(
        name = paste(
          "Seasonality",
          period
        ),
        data = rep(1, length(data)),
        times = times,
        seasons = seq_along(data) %% period,
        timeKnots = seasonal_time_knots,
        seasonalStructure = seasonal_structure,
        lambdas = c(1, 1, 1)
      )
    }
    
    # Standard seasonal predictors.
    str_daily <- make_str_seasonal_predictor(
      period = 24,
      periods = str_periods,
      data = str_data,
      times = str_times
    )
    
    str_weekly <- make_str_seasonal_predictor(
      period = 168,
      periods = str_periods,
      data = str_data,
      times = str_times
    )
    
    str_annual <- make_str_seasonal_predictor(
      period = 8766,
      periods = str_periods,
      data = str_data,
      times = str_times
    )
    
    # Replace the standard daily component with
    # the working/non-working-day topology.
    str_daily$name <-
      "Daily working/non-working topology"
    
    str_daily$seasons <- str_calendar |>
      filter(
        local_date < as.Date("2019-01-01")
      ) |>
      pull(
        str_day_position
      )
    
    str_daily$seasonalStructure <-
      str_daily_structure
    
    # Final STR specification.
    str_predictors <- list(
      str_trend,
      str_weekly,
      str_daily,
      str_annual
    )
  }  
# 14.3 STR estimation
{
    str_fit <- STR(
      data = as.numeric(str_train_msts),
      predictors = str_predictors,
      robust = FALSE,
      gapCV = 24,
      reltol = 0.005,
      confidence = NULL,
      trace = FALSE
    )
    
    # Optimised smoothing parameters.
    str_lambdas <- str_fit$input$lambdas
    
    print(str_lambdas)
    print(str_fit$optim.CV.MSE)
    
    # Save the computationally expensive STR fit.
    dir.create(
      "models",
      showWarnings = FALSE
    )
    
    saveRDS(
      str_fit,
      file = "models/STR_fit_2015_2018.rds"
    )
  }  
# 14.4 STR component and remainder diagnostics
{
    # Estimated STR components.
    plotBeta(
      str_fit,
      predictorN = 1
    )  # Trend
    
    plotBeta(
      str_fit,
      predictorN = 2
    )  # Weekly seasonality
    
    plotBeta(
      str_fit,
      predictorN = 3
    )  # Working/non-working daily seasonality
    
    plotBeta(
      str_fit,
      predictorN = 4
    )  # Annual seasonality
    
    # STR remainder.
    str_remainder <- as.numeric(
      str_fit$output$random$data
    )
    
    # ACF and PACF of the remainder.
    old_par <- par(no.readonly = TRUE)
    
    par(mfrow = c(2, 1))
    
    acf(
      str_remainder,
      lag.max = 336,
      main = "ACF — STR Remainder",
      xlab = "Lag (hours)"
    )
    
    abline(
      v = c(24, 168, 336),
      col = "red",
      lty = 2
    )
    
    pacf(
      str_remainder,
      lag.max = 336,
      main = "PACF — STR Remainder",
      xlab = "Lag (hours)"
    )
    
    abline(
      v = c(24, 168, 336),
      col = "red",
      lty = 2
    )
    
    par(old_par)
    
    # Ljung-Box test for residual dependence.
    str_ljung_box <- Box.test(
      str_remainder,
      lag = 336,
      type = "Ljung-Box",
      fitdf = 0
    )
    
    print(str_ljung_box)
  }
# 14.5 ARMA model for the STR remainder
{
    # Differencing diagnostics.
    str_adf <- ur.df(
      str_remainder,
      type = "none",
      selectlags = "AIC"
    )
    
    summary(str_adf)
    
    # Non-seasonal ARMA selection with d = 0.
    str_arma_final <- auto.arima(
      str_remainder,
      d = 0,
      seasonal = FALSE,
      allowmean = FALSE,
      allowdrift = FALSE,
      ic = "aicc",
      stepwise = FALSE,
      approximation = FALSE,
      trace = FALSE
    )
    
    summary(str_arma_final)
    arimaorder(str_arma_final)
    
    # Save the selected remainder model.
    saveRDS(
      str_arma_final,
      file = "models/STR_ARMA_remainder_2015_2018.rds"
    )
  }
# 14.6 ARMA residual diagnostics
{
    checkresiduals(
      str_arma_final,
      lag = 336
    )
    
    old_par <- par(no.readonly = TRUE)
    
    par(mfrow = c(2, 1))
    
    acf(
      residuals(str_arma_final),
      lag.max = 336,
      main = "ACF — STR + ARMA Residuals",
      xlab = "Lag (hours)"
    )
    
    abline(
      v = c(24, 168, 336),
      col = "red",
      lty = 2
    )
    
    pacf(
      residuals(str_arma_final),
      lag.max = 336,
      main = "PACF — STR + ARMA Residuals",
      xlab = "Lag (hours)"
    )
    
    abline(
      v = c(24, 168, 336),
      col = "red",
      lty = 2
    )
    
    par(old_par)
  }  
# 14.7 First day-ahead forecast
{
    H_str <- 24
    B_str <- 1000
    alpha_str <- 0.05
    
    forecast_date_str <- as.Date(
      "2019-01-01"
    )
    
    # Calendar information for the forecast day.
    future_str <- str_calendar |>
      filter(
        local_date == forecast_date_str
      ) |>
      arrange(local_hour)
    
    # STR model specification selected on the training sample.
    str_template <- str_fit$input$predictors
    
    time_start <- str_template[[1]]$times[1]
    
    time_step <- median(
      diff(str_template[[1]]$times)
    )
    
    n_train_str <- nrow(load_train)
    
    # Observed training data followed by 24 unknown future values.
    str_response_extended <- c(
      log(load_train$load_mw),
      rep(NA_real_, H_str)
    )
    
    global_index <- seq_len(
      n_train_str + H_str
    )
    
    str_times_extended <-
      time_start +
      (global_index - 1) * time_step
    
    # Seasonal coordinates.
    trend_seasons <- rep(
      1,
      length(global_index)
    )
    
    weekly_seasons <-
      global_index %% 168
    
    annual_seasons <-
      global_index %% 8766
    
    daily_seasons <- c(
      str_calendar$str_day_position[
        str_calendar$local_date <
          as.Date("2019-01-01")
      ],
      future_str$str_day_position
    )
    
    # Extend each STR predictor to the forecast horizon.
    extend_str_predictor <- function(
    template,
    seasons,
    times
    ) {
      
      predictor <- template
      
      predictor$data <- rep(
        1,
        length(times)
      )
      
      predictor$times <- times
      predictor$seasons <- seasons
      
      predictor$timeKnots <- seq(
        from = min(times),
        to = max(times),
        length.out = length(
          template$timeKnots
        )
      )
      
      predictor
    }
    
    str_predictors_forecast <- list(
      extend_str_predictor(
        str_template[[1]],
        trend_seasons,
        str_times_extended
      ),
      extend_str_predictor(
        str_template[[2]],
        weekly_seasons,
        str_times_extended
      ),
      extend_str_predictor(
        str_template[[3]],
        daily_seasons,
        str_times_extended
      ),
      extend_str_predictor(
        str_template[[4]],
        annual_seasons,
        str_times_extended
      )
    )
    
    # STR forecast with fixed smoothing parameters.
    str_forecast_fit <- STRmodel(
      data = str_response_extended,
      predictors = str_predictors_forecast,
      lambdas = str_lambdas,
      confidence = NULL,
      trace = FALSE
    )
    
    str_log_forecast <- tail(
      as.numeric(
        str_forecast_fit$output$forecast$data
      ),
      H_str
    )
    
    # Remainder based only on observed training positions.
    str_remainder_forecast <- as.numeric(
      str_forecast_fit$output$random$data
    )[1:n_train_str]
    
    # Keep the ARMA order selected on the training sample.
    str_arma_order <- unname(
      arimaorder(str_arma_final)
    )
    
    # Re-estimate ARMA coefficients on the current STR remainder.
    str_arma_forecast_fit <- Arima(
      str_remainder_forecast,
      order = str_arma_order,
      include.mean = FALSE,
      method = "ML"
    )
    
    arma_correction <- forecast(
      str_arma_forecast_fit,
      h = H_str
    )
    
    arma_log_forecast <- as.numeric(
      arma_correction$mean
    )
    
    # Final STR + ARMA point forecast on the log scale.
    str_final_log_forecast <-
      str_log_forecast +
      arma_log_forecast
  }
# 14.8  bootstrap and prediction intervals
{
    set.seed(20190101)
    
    # Conditional bootstrap paths for the ARMA remainder.
    str_arma_boot_paths <- replicate(
      B_str,
      as.numeric(
        simulate(
          str_arma_forecast_fit,
          nsim = H_str,
          future = TRUE,
          bootstrap = TRUE
        )
      )
    )
    
    # Add the STR forecast to each simulated ARMA path.
    str_log_boot_paths <- sweep(
      str_arma_boot_paths,
      MARGIN = 1,
      STATS = str_log_forecast,
      FUN = "+"
    )
    
    # Back-transform complete simulated paths to MW.
    str_mw_boot_paths <- exp(
      str_log_boot_paths
    )
    
    # Direct exponential back-transformation, without bias adjustment.
    str_point_forecast_mw <- exp(str_final_log_forecast)
    
    # 95% bootstrap prediction intervals.
    str_lower95 <- apply(
      str_mw_boot_paths,
      1,
      quantile,
      probs = alpha_str / 2
    )
    
    str_upper95 <- apply(
      str_mw_boot_paths,
      1,
      quantile,
      probs = 1 - alpha_str / 2
    )
    
    str_forecast <- data.frame(
      local_date = future_str$local_date,
      local_hour = future_str$local_hour,
      observed_mw = future_str$load_mw,
      point_forecast_mw = str_point_forecast_mw,
      lower95_mw = str_lower95,
      upper95_mw = str_upper95,
      is_working_day = future_str$is_working_day
    )
    
    print(str_forecast)
}
# 14.9 First day-ahead forecast plot
{
    # Last 72 training hours.
    plot_train_str <- tail(
      load_train,
      72
    ) |>
      mutate(
        datetime = make_local_datetime(
          local_date,
          local_hour
        )
      )
    
    # Forecast day.
    plot_forecast_str <- str_forecast |>
      mutate(
        datetime = make_local_datetime(
          local_date,
          local_hour
        )
      )
    
    split_time <- min(
      plot_forecast_str$datetime
    )
    
    print(
      ggplot() +
        geom_ribbon(
          data = plot_forecast_str,
          aes(
            x = datetime,
            ymin = lower95_mw,
            ymax = upper95_mw,
            fill = "95% PI"
          ),
          alpha = 0.25
        ) +
        geom_line(
          data = plot_train_str,
          aes(
            x = datetime,
            y = load_mw,
            colour = "Observed train"
          ),
          linewidth = 0.7
        ) +
        geom_line(
          data = plot_forecast_str,
          aes(
            x = datetime,
            y = observed_mw,
            colour = "Observed test"
          ),
          linewidth = 0.8
        ) +
        geom_line(
          data = plot_forecast_str,
          aes(
            x = datetime,
            y = point_forecast_mw,
            colour = "Forecast"
          ),
          linewidth = 0.9
        ) +
        geom_vline(
          xintercept = split_time,
          linetype = "dashed"
        ) +
        scale_x_datetime(
          date_breaks = "12 hours",
          date_labels = "%d %b\n%H:%M",
          timezone = "Europe/Rome"
        ) +
        labs(
          title = "STR + ARMA(5,0,0) — Day-Ahead Forecast",
          subtitle = "Forecast for 1 January 2019",
          x = "Local time (Europe/Rome)",
          y = "Electricity load (MW)",
          colour = NULL,
          fill = NULL
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom"
        )
    )
  } 
}
}


# 15. OUT-OF-SAMPLE EVALUATION
{
# 15.1 Rolling evaluation design
{
  # Common rolling design.
  H_oos <- 24
  B_oos <- 1000
  alpha_oos <- 0.05
  
  # Fixed estimation window equal to the original 2015-2018 training sample.
  n_window_oos <- nrow(load_train)
  
  # Complete regularized 2015-2019 series.
  oos_data <- bind_rows(
    load_train,
    load_test
  ) |>
    arrange(model_index)
  
  # One day-ahead forecast origin for each day of 2019.
  rolling_dates <- seq.Date(
    from = as.Date("2019-01-01"),
    to = as.Date("2019-12-31"),
    by = "day"
  )
  
  n_origins_oos <- length(
    rolling_dates
  )
  
  print(
    data.frame(
      Estimation_window = n_window_oos,
      Forecast_horizon = H_oos,
      Forecast_origins = n_origins_oos,
      Bootstrap_replications = B_oos
    )
  )
}
# 15.2 Weekly Seasonal Naive rolling forecasts
{
  snaive_period <- 168
  
  run_snaive_origin <- function(
    target_date,
    seed
  ) {
    
    # Target 24-hour forecast day.
    target_idx <- which(
      oos_data$local_date == target_date
    )
    
    train_end <- min(target_idx) - 1
    train_start <- train_end - n_window_oos + 1
    
    window_data <- oos_data[
      train_start:train_end,
    ]
    
    future_data <- oos_data[
      target_idx,
    ]
    
    y_window <- as.numeric(
      window_data$load_mw
    )
    
    n_window <- length(y_window)
    
    # Weekly seasonal-naive point forecast.
    point_forecast <- y_window[
      n_window -
        snaive_period +
        seq_len(H_oos)
    ]
    
    # Historical seasonal-naive residuals
    # within the current estimation window.
    residuals_window <-
      y_window[
        (snaive_period + 1):n_window
      ] -
      y_window[
        1:(n_window - snaive_period)
      ]
    
    # Nonparametric residual bootstrap.
    set.seed(seed)
    
    residual_draws <- matrix(
      sample(
        residuals_window,
        size = B_oos * H_oos,
        replace = TRUE
      ),
      nrow = B_oos,
      ncol = H_oos
    )
    
    bootstrap_paths <- sweep(
      residual_draws,
      MARGIN = 2,
      STATS = point_forecast,
      FUN = "+"
    )
    
    lower95 <- apply(
      bootstrap_paths,
      2,
      quantile,
      probs = alpha_oos / 2
    )
    
    upper95 <- apply(
      bootstrap_paths,
      2,
      quantile,
      probs = 1 - alpha_oos / 2
    )
    
    data.frame(
      origin_date = target_date,
      local_date = future_data$local_date,
      local_hour = future_data$local_hour,
      horizon = seq_len(H_oos),
      actual_mw = future_data$load_mw,
      point_forecast_mw = point_forecast,
      lower95_mw = lower95,
      upper95_mw = upper95
    )
  }
  
  # Run all 365 forecast origins.
  snaive_rolling <- bind_rows(
    lapply(
      rolling_dates,
      function(date) {
        run_snaive_origin(
          target_date = date,
          seed = as.integer(
            format(date, "%Y%m%d")
          )
        )
      }
    )
  )
  
  print(
    head(
      snaive_rolling,
      24
    )
  )
}
# 15.3 MSTL + ARIMA rolling forecasts
{
  mstl_arima_order <- unname(
    arimaorder(mstl_arima_final)
  )
  
  mstl_rolling_dir <- file.path(
    "rolling_results",
    "MSTL_ARIMA302_direct_exp"
  )
  
  dir.create(
    mstl_rolling_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  run_mstl_origin <- function(
    target_date,
    seed
  ) {
    
    # Current forecast origin.
    target_idx <- which(
      oos_data$local_date == target_date
    )
    
    train_end <- min(target_idx) - 1
    train_start <- train_end - n_window_oos + 1
    
    window_data <- oos_data[
      train_start:train_end,
    ]
    
    future_data <- oos_data[
      target_idx,
    ]
    
    # Re-estimate MSTL on the current rolling window.
    window_msts <- msts(
      window_data$load_mw,
      seasonal.periods = mstl_periods
    )
    
    mstl_rolling_fit <- mstl(
      window_msts,
      lambda = mstl_lambda,
      s.window = mstl_windows,
      iterate = 2,
      robust = FALSE
    )
    
    # Seasonally adjusted log-load.
    adjusted_log <- ts(
      as.numeric(
        mstl_rolling_fit[, "Trend"] +
          mstl_rolling_fit[, "Remainder"]
      ),
      frequency = 1
    )
    
    # Re-estimate ARIMA coefficients with fixed order.
    arima_rolling_fit <- Arima(
      adjusted_log,
      order = mstl_arima_order,
      include.mean = TRUE,
      method = "ML"
    )
    
    # Forecast Trend + Remainder.
    adjusted_forecast <- forecast(
      arima_rolling_fit,
      h = H_oos
    )
    
    # Extrapolate MSTL seasonal components.
    mstl_seasonal_cols <- c(
      "Seasonal24",
      "Seasonal168",
      "Seasonal8766"
    )
    
    future_seasonal_components <- sapply(
      seq_along(mstl_periods),
      function(j) {
        
        last_cycle <- tail(
          mstl_rolling_fit[
            , mstl_seasonal_cols[j]
          ],
          mstl_periods[j]
        )
        
        rep(
          last_cycle,
          length.out = H_oos
        )
      }
    )
    
    future_seasonal_sum <- rowSums(
      future_seasonal_components
    )
    
    # Conditional residual bootstrap.
    set.seed(seed)
    
    adjusted_boot_paths <- replicate(
      B_oos,
      as.numeric(
        simulate(
          arima_rolling_fit,
          nsim = H_oos,
          future = TRUE,
          bootstrap = TRUE
        )
      )
    )
    
    # Reconstruct and back-transform complete paths.
    log_boot_paths <- sweep(
      adjusted_boot_paths,
      MARGIN = 1,
      STATS = future_seasonal_sum,
      FUN = "+"
    )
    
    mw_boot_paths <- exp(
      log_boot_paths
    )
    
    # Complete log-scale point forecast.
    forecast_log <- as.numeric(adjusted_forecast$mean) +
      future_seasonal_sum
    
    # Direct exponential back-transformation, without bias adjustment.
    point_forecast_mw <- exp(forecast_log)
    
    # Bootstrap 95% prediction intervals.
    
    lower95 <- apply(
      mw_boot_paths,
      1,
      quantile,
      probs = alpha_oos / 2
    )
    
    upper95 <- apply(
      mw_boot_paths,
      1,
      quantile,
      probs = 1 - alpha_oos / 2
    )
    
    data.frame(
      origin_date = target_date,
      local_date = future_data$local_date,
      local_hour = future_data$local_hour,
      horizon = seq_len(H_oos),
      actual_mw = future_data$load_mw,
      point_forecast_mw = point_forecast_mw,
      lower95_mw = lower95,
      upper95_mw = upper95
    )
  }
  
  # Run the 365 rolling origins.
  for (target_date in rolling_dates) {
    
    target_date <- as.Date(
      target_date,
      origin = "1970-01-01"
    )
    
    output_file <- file.path(
      mstl_rolling_dir,
      paste0(
        "MSTL_",
        format(target_date, "%Y-%m-%d"),
        ".rds"
      )
    )
    
    # Skip forecasts already completed.
    if (file.exists(output_file)) {
      next
    }
    
    forecast_origin <- run_mstl_origin(
      target_date = target_date,
      seed = as.integer(
        format(target_date, "%Y%m%d")
      )
    )
    
    saveRDS(
      forecast_origin,
      output_file
    )
  }
  
  # Combine all rolling forecasts.
  mstl_files <- file.path(
    mstl_rolling_dir,
    paste0(
      "MSTL_",
      format(rolling_dates, "%Y-%m-%d"),
      ".rds"
    )
  )
  
  mstl_rolling <- bind_rows(
    lapply(
      mstl_files,
      readRDS
    )
  ) |>
    arrange(
      local_date,
      local_hour
    )
}
# 15.4 STR + ARMA rolling forecasts
{
  str_arma_order <- unname(
    arimaorder(str_arma_final)
  )
  
  str_template <- str_fit$input$predictors
  
  str_time_start <- str_template[[1]]$times[1]
  
  str_time_step <- median(
    diff(str_template[[1]]$times)
  )
  
  str_rolling_dir <- file.path(
    "rolling_results",
    "STR_ARMA500_direct_exp"
  )
  
  dir.create(
    str_rolling_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  run_str_origin <- function(
    target_date,
    seed
  ) {
    
    # Target forecast day.
    target_idx <- which(
      str_calendar$local_date == target_date
    )
    
    train_end <- min(target_idx) - 1
    train_start <- train_end - n_window_oos + 1
    
    window_data <- str_calendar[
      train_start:train_end,
    ]
    
    future_data <- str_calendar[
      target_idx,
    ]
    
    # Observed rolling window + unknown future load.
    str_response <- c(
      log(window_data$load_mw),
      rep(NA_real_, H_oos)
    )
    
    # Absolute positions in the complete modelling grid.
    global_index <- train_start:(
      train_end + H_oos
    )
    
    str_times <- str_time_start +
      (global_index - 1) * str_time_step
    
    # Seasonal coordinates.
    trend_seasons <- rep(
      1,
      length(global_index)
    )
    
    weekly_seasons <-
      global_index %% 168
    
    annual_seasons <-
      global_index %% 8766
    
    daily_seasons <- c(
      window_data$str_day_position,
      future_data$str_day_position
    )
    
    # Extend the fixed STR predictor structure.
    str_predictors_rolling <- list(
      extend_str_predictor(
        str_template[[1]],
        trend_seasons,
        str_times
      ),
      extend_str_predictor(
        str_template[[2]],
        weekly_seasons,
        str_times
      ),
      extend_str_predictor(
        str_template[[3]],
        daily_seasons,
        str_times
      ),
      extend_str_predictor(
        str_template[[4]],
        annual_seasons,
        str_times
      )
    )
    
    # Re-estimate STR components with fixed smoothing parameters.
    str_rolling_fit <- STRmodel(
      data = str_response,
      predictors = str_predictors_rolling,
      lambdas = str_lambdas,
      confidence = NULL,
      trace = FALSE
    )
    
    # 24-hour STR forecast on the log scale.
    str_log_forecast <- tail(
      as.numeric(
        str_rolling_fit$output$forecast$data
      ),
      H_oos
    )
    
    # STR remainder from observed positions only.
    str_remainder_rolling <- as.numeric(
      str_rolling_fit$output$random$data
    )[seq_len(n_window_oos)]
    
    # Re-estimate ARMA coefficients with fixed order.
    str_arma_rolling_fit <- Arima(
      str_remainder_rolling,
      order = str_arma_order,
      include.mean = FALSE,
      method = "ML"
    )
    
    # Conditional residual bootstrap.
    set.seed(seed)
    
    arma_boot_paths <- replicate(
      B_oos,
      as.numeric(
        simulate(
          str_arma_rolling_fit,
          nsim = H_oos,
          future = TRUE,
          bootstrap = TRUE
        )
      )
    )
    
    # Complete simulated log-scale paths.
    log_boot_paths <- sweep(
      arma_boot_paths,
      MARGIN = 1,
      STATS = str_log_forecast,
      FUN = "+"
    )
    
    # Back-transform to MW.
    mw_boot_paths <- exp(
      log_boot_paths
    )
    
    # Forecast the ARMA remainder on the log scale.
    arma_point_forecast <- forecast(
      str_arma_rolling_fit,
      h = H_oos
    )
    
    # Complete log-scale point forecast.
    forecast_log <- str_log_forecast +
      as.numeric(arma_point_forecast$mean)
    
    # Direct exponential back-transformation, without bias adjustment.
    point_forecast_mw <- exp(forecast_log)
    
    # Bootstrap 95% prediction intervals.
    
    lower95 <- apply(
      mw_boot_paths,
      1,
      quantile,
      probs = alpha_oos / 2
    )
    
    upper95 <- apply(
      mw_boot_paths,
      1,
      quantile,
      probs = 1 - alpha_oos / 2
    )
    
    data.frame(
      origin_date = target_date,
      local_date = future_data$local_date,
      local_hour = future_data$local_hour,
      horizon = seq_len(H_oos),
      actual_mw = future_data$load_mw,
      point_forecast_mw = point_forecast_mw,
      lower95_mw = lower95,
      upper95_mw = upper95
    )
  }
  
  # Run the 365 rolling origins.
  for (target_date in rolling_dates) {
    
    target_date <- as.Date(
      target_date,
      origin = "1970-01-01"
    )
    
    output_file <- file.path(
      str_rolling_dir,
      paste0(
        "STR_",
        format(target_date, "%Y-%m-%d"),
        ".rds"
      )
    )
    
    # Skip forecasts already completed.
    if (file.exists(output_file)) {
      next
    }
    
    forecast_origin <- run_str_origin(
      target_date = target_date,
      seed = as.integer(
        format(target_date, "%Y%m%d")
      )
    )
    
    saveRDS(
      forecast_origin,
      output_file
    )
  }
  
  # Combine all rolling forecasts.
  str_files <- file.path(
    str_rolling_dir,
    paste0(
      "STR_",
      format(rolling_dates, "%Y-%m-%d"),
      ".rds"
    )
  )
  
  str_rolling <- bind_rows(
    lapply(
      str_files,
      readRDS
    )
  ) |>
    arrange(
      local_date,
      local_hour
    )
}
# 15.5 Standardise OOS results
{
  evaluate_forecasts <- function(data, model_name) {
    
    data |>
      transmute(
        model = model_name,
        origin_date = as.Date(origin_date),
        local_date = as.Date(local_date),
        local_hour = as.integer(local_hour),
        horizon = as.integer(horizon),
        
        actual_mw = as.numeric(actual_mw),
        point_forecast_mw = as.numeric(point_forecast_mw),
        lower95_mw = as.numeric(lower95_mw),
        upper95_mw = as.numeric(upper95_mw),
        
        # Point-forecast evaluation in MW.
        error = actual_mw - point_forecast_mw,
        squared_error = error^2,
        
        # Prediction-interval evaluation.
        covered95 =
          actual_mw >= lower95_mw &
          actual_mw <= upper95_mw,
        
        interval_width = upper95_mw - lower95_mw,
        
        winkler95 = case_when(
          actual_mw < lower95_mw ~
            interval_width +
            (2 / alpha_oos) * (lower95_mw - actual_mw),
          
          actual_mw > upper95_mw ~
            interval_width +
            (2 / alpha_oos) * (actual_mw - upper95_mw),
          
          TRUE ~ interval_width
        )
      )
  }
  
  eval_snaive <- evaluate_forecasts(
    snaive_rolling,
    "Weekly Seasonal Naive"
  )
  
  eval_mstl <- evaluate_forecasts(
    mstl_rolling,
    "MSTL + ARIMA(3,0,2)"
  )
  
  eval_str <- evaluate_forecasts(
    str_rolling,
    "STR + ARMA(5,0,0)"
  )
  
  # Common long-format evaluation dataset.
  model_comparison_2019 <- bind_rows(
    eval_mstl,
    eval_str,
    eval_snaive
  )
}
# 15.6 Overall and horizon-specific OOS performance
{
  # Overall performance across all 8,760 forecasts.
  overall_metrics <- model_comparison_2019 |>
    group_by(model) |>
    summarise(
      N = n(),
      MSE = mean(squared_error),
      RMSE = sqrt(mean(squared_error)),
      Bias = mean(error),
      Coverage_95 = mean(covered95),
      Mean_PI_width = mean(interval_width),
      Mean_Winkler_95 = mean(winkler95),
      .groups = "drop"
    )
  
  print(
    overall_metrics
  )
  
  # Performance separately for each forecast horizon.
  horizon_metrics <- model_comparison_2019 |>
    group_by(
      model,
      horizon
    ) |>
    summarise(
      N = n(),
      MSE = mean(squared_error),
      RMSE = sqrt(mean(squared_error)),
      Bias = mean(error),
      Coverage_95 = mean(covered95),
      Mean_PI_width = mean(interval_width),
      Mean_Winkler_95 = mean(winkler95),
      .groups = "drop"
    )
  
  print(
    horizon_metrics,
    n = 72
  )
}
# 15.7 Diebold-Mariano tests
{
  dm_pairs <- list(
    c(
      "MSTL + ARIMA(3,0,2)",
      "STR + ARMA(5,0,0)"
    ),
    c(
      "MSTL + ARIMA(3,0,2)",
      "Weekly Seasonal Naive"
    ),
    c(
      "STR + ARMA(5,0,0)",
      "Weekly Seasonal Naive"
    )
  )
  
  run_dm_pair <- function(
    model_1,
    model_2
  ) {
    
    results <- lapply(
      seq_len(H_oos),
      function(h) {
        
        data_1 <- model_comparison_2019 |>
          filter(
            model == model_1,
            horizon == h
          ) |>
          arrange(local_date)
        
        data_2 <- model_comparison_2019 |>
          filter(
            model == model_2,
            horizon == h
          ) |>
          arrange(local_date)
        
        error_1 <- data_1$error
        error_2 <- data_2$error
        
        loss_1 <- error_1^2
        loss_2 <- error_2^2
        
        dm <- dm.test(
          error_1,
          error_2,
          alternative = "two.sided",
          h = 1,
          power = 2
        )
        
        data.frame(
          model_1 = model_1,
          model_2 = model_2,
          horizon = h,
          N = length(error_1),
          MSE_model_1 = mean(loss_1),
          MSE_model_2 = mean(loss_2),
          Mean_loss_diff = mean(
            loss_1 - loss_2
          ),
          DM_statistic = as.numeric(
            dm$statistic
          ),
          p_value = as.numeric(
            dm$p.value
          )
        )
      }
    )
    
    bind_rows(results)
  }
  
  # Run all three pairwise comparisons.
  dm_results <- bind_rows(
    lapply(
      dm_pairs,
      function(pair) {
        run_dm_pair(
          pair[1],
          pair[2]
        )
      }
    )
  ) |>
    group_by(
      model_1,
      model_2
    ) |>
    mutate(
      p_value_Holm = p.adjust(
        p_value,
        method = "holm"
      ),
      Significant_raw =
        p_value < 0.05,
      Significant_Holm =
        p_value_Holm < 0.05,
      
      Better_model_Holm = case_when(
        !Significant_Holm ~
          "No significant difference",
        
        Mean_loss_diff < 0 ~
          model_1,
        
        Mean_loss_diff > 0 ~
          model_2,
        
        TRUE ~
          "Tie"
      )
    ) |>
    ungroup()
  
  print(
    dm_results,
    n = 72
  )
  
  # Summary by model pair.
  dm_summary <- dm_results |>
    group_by(
      model_1,
      model_2
    ) |>
    summarise(
      Significant_raw =
        sum(Significant_raw),
      
      Significant_Holm =
        sum(Significant_Holm),
      
      Model_1_better_Holm =
        sum(
          Significant_Holm &
            Mean_loss_diff < 0
        ),
      
      Model_2_better_Holm =
        sum(
          Significant_Holm &
            Mean_loss_diff > 0
        ),
      
      .groups = "drop"
    )
  
  print(dm_summary)
}
# 15.8 Pairwise Winkler-score comparisons
{
  winkler_pairs <- dm_pairs
  
  run_winkler_pair <- function(
    model_1,
    model_2
  ) {
    
    results <- lapply(
      seq_len(H_oos),
      function(h) {
        
        data_1 <- model_comparison_2019 |>
          filter(
            model == model_1,
            horizon == h
          ) |>
          arrange(local_date)
        
        data_2 <- model_comparison_2019 |>
          filter(
            model == model_2,
            horizon == h
          ) |>
          arrange(local_date)
        
        winkler_1 <- data_1$winkler95
        winkler_2 <- data_2$winkler95
        
        # Winkler loss differential:
        # negative -> model 1 has lower Winkler loss.
        loss_diff <- winkler_1 - winkler_2
        
        n <- length(loss_diff)
        
        winkler_statistic <-
          mean(loss_diff) /
          (sd(loss_diff) / sqrt(n))
        
        p_value <- 2 * pt(
          -abs(winkler_statistic),
          df = n - 1
        )
        
        data.frame(
          model_1 = model_1,
          model_2 = model_2,
          horizon = h,
          N = n,
          Mean_Winkler_model_1 =
            mean(winkler_1),
          Mean_Winkler_model_2 =
            mean(winkler_2),
          Mean_loss_diff =
            mean(loss_diff),
          Winkler_statistic =
            winkler_statistic,
          p_value =
            p_value
        )
      }
    )
    
    bind_rows(results)
  }
  
  # Run all three pairwise comparisons.
  winkler_results <- bind_rows(
    lapply(
      winkler_pairs,
      function(pair) {
        run_winkler_pair(
          pair[1],
          pair[2]
        )
      }
    )
  ) |>
    group_by(
      model_1,
      model_2
    ) |>
    mutate(
      p_value_Holm = p.adjust(
        p_value,
        method = "holm"
      ),
      
      Significant_raw =
        p_value < 0.05,
      
      Significant_Holm =
        p_value_Holm < 0.05,
      
      Better_model_Holm = case_when(
        !Significant_Holm ~
          "No significant difference",
        
        Mean_loss_diff < 0 ~
          model_1,
        
        Mean_loss_diff > 0 ~
          model_2,
        
        TRUE ~
          "Tie"
      )
    ) |>
    ungroup()
  
  print(
    winkler_results,
    n = 72
  )
  
  # Summary by model pair.
  winkler_summary <- winkler_results |>
    group_by(
      model_1,
      model_2
    ) |>
    summarise(
      Significant_raw =
        sum(Significant_raw),
      
      Significant_Holm =
        sum(Significant_Holm),
      
      Model_1_better_Holm =
        sum(
          Significant_Holm &
            Mean_loss_diff < 0
        ),
      
      Model_2_better_Holm =
        sum(
          Significant_Holm &
            Mean_loss_diff > 0
        ),
      
      .groups = "drop"
    )
  
  print(winkler_summary)
}
# 15.9 Exact binomial coverage tests
{
  nominal_coverage <- 1 - alpha_oos
  
  run_coverage_tests <- function(
    model_name
  ) {
    
    model_data <- model_comparison_2019 |>
      filter(
        model == model_name
      )
    
    # Coverage tests by forecast horizon.
    by_horizon <- bind_rows(
      lapply(
        seq_len(H_oos),
        function(h) {
          
          horizon_data <- model_data |>
            filter(
              horizon == h
            )
          
          n <- nrow(horizon_data)
          covered <- sum(
            horizon_data$covered95
          )
          
          test <- binom.test(
            x = covered,
            n = n,
            p = nominal_coverage,
            alternative = "two.sided",
            conf.level = 0.95
          )
          
          data.frame(
            model = model_name,
            horizon = h,
            N = n,
            Covered_n = covered,
            Coverage_95 = covered / n,
            CI_lower = test$conf.int[1],
            CI_upper = test$conf.int[2],
            p_value = test$p.value
          )
        }
      )
    ) |>
      mutate(
        p_value_Holm = p.adjust(
          p_value,
          method = "holm"
        ),
        Significant_raw =
          p_value < 0.05,
        Significant_Holm =
          p_value_Holm < 0.05
      )
    
    # Overall coverage test.
    n_overall <- nrow(model_data)
    
    covered_overall <- sum(
      model_data$covered95
    )
    
    overall_test <- binom.test(
      x = covered_overall,
      n = n_overall,
      p = nominal_coverage,
      alternative = "two.sided",
      conf.level = 0.95
    )
    
    overall <- data.frame(
      model = model_name,
      N = n_overall,
      Covered_n = covered_overall,
      Coverage_95 =
        covered_overall / n_overall,
      CI_lower =
        overall_test$conf.int[1],
      CI_upper =
        overall_test$conf.int[2],
      p_value =
        overall_test$p.value
    )
    
    list(
      by_horizon = by_horizon,
      overall = overall
    )
  }
  
  model_names <- c(
    "MSTL + ARIMA(3,0,2)",
    "STR + ARMA(5,0,0)",
    "Weekly Seasonal Naive"
  )
  
  coverage_tests <- lapply(
    model_names,
    run_coverage_tests
  )
  
  coverage_by_horizon <- bind_rows(
    lapply(
      coverage_tests,
      function(x) x$by_horizon
    )
  )
  
  coverage_overall <- bind_rows(
    lapply(
      coverage_tests,
      function(x) x$overall
    )
  )
  
  print(
    coverage_by_horizon,
    n = 72
  )
  
  print(
    coverage_overall
  )
}
# 15.10 Final thesis-ready tables
{
  model_order <- c(
    "MSTL + ARIMA(3,0,2)",
    "STR + ARMA(5,0,0)",
    "Weekly Seasonal Naive"
  )
  
  # Overall model comparison.
  final_overall_table <- overall_metrics |>
    mutate(
      model = factor(
        model,
        levels = model_order
      ),
      Coverage_gap_pp =
        100 * abs(Coverage_95 - 0.95)
    ) |>
    arrange(model)
  
  print(
    final_overall_table
  )
  
  # Performance by forecast horizon.
  final_horizon_table <- horizon_metrics |>
    mutate(
      model = factor(
        model,
        levels = model_order
      ),
      Coverage_gap_pp =
        100 * abs(Coverage_95 - 0.95)
    ) |>
    arrange(
      horizon,
      model
    )
  
  # Point-forecast metrics by horizon.
  final_point_table <- final_horizon_table |>
    select(
      horizon,
      model,
      MSE,
      RMSE,
      Bias
    ) |>
    pivot_wider(
      names_from = model,
      values_from = c(MSE, RMSE, Bias),
      names_glue = "{.value}__{model}"
    )
  
  # Prediction-interval metrics by horizon.
  final_interval_table <- final_horizon_table |>
    select(
      horizon,
      model,
      Coverage_95,
      Coverage_gap_pp,
      Mean_PI_width,
      Mean_Winkler_95
    ) |>
    pivot_wider(
      names_from = model,
      values_from = c(
        Coverage_95,
        Coverage_gap_pp,
        Mean_PI_width,
        Mean_Winkler_95
      ),
      names_glue = "{.value}__{model}"
    )
  
  print(
    final_point_table,
    n = 24
  )
  
  print(
    final_interval_table,
    n = 24
  )
}
# 15.11 Final comparative figures
{
  plot_data <- final_horizon_table |>
    mutate(
      model = factor(
        model,
        levels = model_order
      )
    )
  
  # RMSE by forecast horizon.
  plot_rmse <- ggplot(
    plot_data,
    aes(
      x = horizon,
      y = RMSE,
      group = model,
      linetype = model,
      shape = model
    )
  ) +
    geom_line(
      linewidth = 0.8
    ) +
    geom_point(
      size = 1.8
    ) +
    scale_x_continuous(
      breaks = 1:24
    ) +
    labs(
      title = "Point Forecast Accuracy by Forecast Horizon",
      x = "Forecast horizon",
      y = "RMSE (MW)",
      linetype = "Model",
      shape = "Model"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom"
    )
  
  print(plot_rmse)
  
  # MSE by forecast horizon.
  plot_mse <- ggplot(
    plot_data,
    aes(
      x = horizon,
      y = MSE,
      group = model,
      linetype = model,
      shape = model
    )
  ) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.8) +
    scale_x_continuous(breaks = 1:24) +
    labs(
      title = "Point Forecast MSE by Forecast Horizon",
      x = "Forecast horizon",
      y = "MSE (MW squared)",
      linetype = "Model",
      shape = "Model"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom"
    )
  
  print(plot_mse)
  
  # Bias by forecast horizon.
  plot_bias <- ggplot(
    plot_data,
    aes(
      x = horizon,
      y = Bias,
      group = model,
      linetype = model,
      shape = model
    )
  ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    geom_line(
      linewidth = 0.8
    ) +
    geom_point(
      size = 1.8
    ) +
    scale_x_continuous(
      breaks = 1:24
    ) +
    labs(
      title = "Forecast Bias by Forecast Horizon",
      x = "Forecast horizon",
      y = "Bias (MW)",
      linetype = "Model",
      shape = "Model"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom"
    )
  
  print(plot_bias)
  
  
  # Empirical 95% coverage by forecast horizon.
  plot_coverage <- ggplot(
    plot_data,
    aes(
      x = horizon,
      y = Coverage_95,
      group = model,
      linetype = model,
      shape = model
    )
  ) +
    geom_hline(
      yintercept = 0.95,
      linetype = "dashed"
    ) +
    geom_line(
      linewidth = 0.8
    ) +
    geom_point(
      size = 1.8
    ) +
    scale_x_continuous(
      breaks = 1:24
    ) +
    scale_y_continuous(
      labels = percent_format(
        accuracy = 1
      )
    ) +
    labs(
      title = "Prediction-Interval Coverage by Forecast Horizon",
      x = "Forecast horizon",
      y = "Empirical coverage",
      linetype = "Model",
      shape = "Model"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom"
    )
  
  print(plot_coverage)
  
  
  # Winkler score by forecast horizon.
  plot_winkler <- ggplot(
    plot_data,
    aes(
      x = horizon,
      y = Mean_Winkler_95,
      group = model,
      linetype = model,
      shape = model
    )
  ) +
    geom_line(
      linewidth = 0.8
    ) +
    geom_point(
      size = 1.8
    ) +
    scale_x_continuous(
      breaks = 1:24
    ) +
    labs(
      title = "Prediction-Interval Performance by Forecast Horizon",
      x = "Forecast horizon",
      y = "Mean Winkler score",
      linetype = "Model",
      shape = "Model"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom"
    )
  
  print(plot_winkler)
  # Mean prediction-interval width by forecast horizon
  {
    plot_interval_width <- ggplot(
      final_horizon_table,
      aes(
        x = horizon,
        y = Mean_PI_width,
        group = model,
        linetype = model,
        shape = model
      )
    ) +
      geom_line(
        linewidth = 0.8
      ) +
      geom_point(
        size = 1.8
      ) +
      scale_x_continuous(
        breaks = 1:24
      ) +
      labs(
        title = "Prediction-Interval Width by Forecast Horizon",
        x = "Forecast horizon",
        y = "Mean 95% prediction-interval width (MW)",
        linetype = "Model",
        shape = "Model"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom"
      )
    
    print(plot_interval_width)
  }
}
# 15.12 Save final evaluation results
{
  final_results_dir <- file.path(
    "results",
    "final_evaluation_2019_direct_exp_MSE_RMSE"
  )
  
  dir.create(
    final_results_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  # Thesis-ready tables.
  write.csv(
    final_overall_table,
    file.path(
      final_results_dir,
      "overall_model_comparison.csv"
    ),
    row.names = FALSE
  )
  
  write.csv(
    final_point_table,
    file.path(
      final_results_dir,
      "point_metrics_by_horizon.csv"
    ),
    row.names = FALSE
  )
  
  write.csv(
    final_interval_table,
    file.path(
      final_results_dir,
      "interval_metrics_by_horizon.csv"
    ),
    row.names = FALSE
  )
  
  # Detailed statistical comparisons.
  saveRDS(
    dm_results,
    file.path(
      final_results_dir,
      "DM_results.rds"
    )
  )
  
  saveRDS(
    winkler_results,
    file.path(
      final_results_dir,
      "Winkler_results.rds"
    )
  )
  
  saveRDS(
    coverage_by_horizon,
    file.path(
      final_results_dir,
      "coverage_tests_by_horizon.rds"
    )
  )
  
  saveRDS(
    coverage_overall,
    file.path(
      final_results_dir,
      "coverage_tests_overall.rds"
    )
  )
  
  # Complete standardised OOS dataset.
  saveRDS(
    model_comparison_2019,
    file.path(
      final_results_dir,
      "model_comparison_2019.rds"
    )
  )
  
  # Final figures.
  ggsave(
    file.path(
      final_results_dir,
      "RMSE_by_horizon.png"
    ),
    plot = plot_rmse,
    width = 9,
    height = 5.5,
    dpi = 300
  )
  
  ggsave(
    file.path(
      final_results_dir,
      "MSE_by_horizon.png"
    ),
    plot = plot_mse,
    width = 9,
    height = 5.5,
    dpi = 300
  )
  
  ggsave(
    file.path(
      final_results_dir,
      "Bias_by_horizon.png"
    ),
    plot = plot_bias,
    width = 9,
    height = 5.5,
    dpi = 300
  )
  
  ggsave(
    file.path(
      final_results_dir,
      "Coverage_by_horizon.png"
    ),
    plot = plot_coverage,
    width = 9,
    height = 5.5,
    dpi = 300
  )
  
  ggsave(
    file.path(
      final_results_dir,
      "Winkler_by_horizon.png"
    ),
    plot = plot_winkler,
    width = 9,
    height = 5.5,
    dpi = 300
  )
  
  # Reproducibility information.
  capture.output(
    sessionInfo(),
    file = file.path(
      final_results_dir,
      "sessionInfo.txt"
    )
  )
}
}



