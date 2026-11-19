# Electricity-load-forecasting-italy
Day-ahead electricity load forecasting in Italy using R, MSTL, STR and ARMA models, with rolling evaluation and bootstrap prediction intervals.
# Day-Ahead Electricity Load Forecasting in Italy

**Master's thesis project | Business Analytics · Time Series Forecasting · Energy Analytics**

**Tools:** R  
**Methods:** Multiple seasonality, MSTL, STR, ARMA, rolling-origin evaluation, bootstrap prediction intervals, forecast accuracy testing

## Project Overview

This project investigates **day-ahead hourly electricity load forecasting in Italy**, comparing statistical approaches designed to model multiple seasonal patterns. The objective is to produce forecasts for the next 24 hours and assess both point forecast accuracy and uncertainty.

Italian electricity demand displays pronounced **daily, weekly and annual seasonality**, alongside differences between working and non-working days. The analysis compares a simple weekly benchmark with two decomposition-based forecasting models.

## Data

- **Source:** [Open Power System Data — Time Series (2020-10-06)](https://doi.org/10.25832/time_series/2020-10-06), using Italian actual electricity load originating from the ENTSO-E Transparency Platform.
- **Period analysed:** January 2015–December 2019.
- **Model development:** 2015–2018.
- **Out-of-sample evaluation:** 2019.
- **Frequency:** Hourly, using a regular 24-hour-per-day Italian civil-time representation with explicit treatment of daylight saving time transitions.

The original source data are not redistributed in this repository.

## Forecasting Models

1. **Weekly Seasonal Naive:** Forecasts repeat the corresponding hour of the preceding week (`m = 168`).
2. **MSTL + ARMA(3,0,2):** Multiple seasonal decomposition, followed by an ARMA model for the seasonally adjusted series.
3. **STR + ARMA(5,0,0):** Seasonal-Trend decomposition using Regression, with an intraday seasonal pattern that can differ between working and non-working days.

The decomposition-based models use a logarithmic transformation of electricity load.

## Evaluation Design

- **365 rolling daily forecast origins** during 2019, each producing **24 hourly forecasts** (8,760 evaluated forecasts per model).
- **Point forecasts:** Mean squared error (MSE) and root mean squared error (RMSE).
- **Prediction intervals:** 95% bootstrap intervals, examined using empirical coverage, average width and Winkler score.
- **Model comparisons:** Horizon-specific Diebold–Mariano tests, including Holm adjustments for multiple comparisons.

## Selected Findings

| Model | Out-of-sample RMSE (MW) |
|---|---:|
| MSTL + ARMA(3,0,2) | 1,759.9 |
| STR + ARMA(5,0,0) | 1,923.9 |
| Weekly Seasonal Naive | 3,475.5 |

- **MSTL achieved the best overall point forecast accuracy**, although the relative performance of MSTL and STR varied by forecast horizon.
- **STR achieved the lowest aggregate Winkler score** for the 95% prediction intervals, reflecting a different trade-off between interval width and coverage.
- Neither decomposition-based model attained nominal 95% overall interval coverage in the test period, an important limitation of the interval forecasts.


## Repository Contents

- `analysis/` — R code and notes documenting the modelling workflow.
- `presentation/` — Project presentation materials.
- `data/` — Data source and preparation notes.


## Academic Context

Master's thesis in Business Analytics, University of Bologna.  
**Author:** Lisa Rigo  

## Data Reference

Open Power System Data (2020). *Data Package Time Series*. https://doi.org/10.25832/time_series/2020-10-06
