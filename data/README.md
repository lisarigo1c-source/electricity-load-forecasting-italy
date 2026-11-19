# Data

**Source:** Open Power System Data (2020), *Time Series*, release 2020-10-06: https://doi.org/10.25832/time_series/2020-10-06

The analysis uses `IT_load_actual_entsoe_transparency` and `utc_timestamp` from `time_series_60min_singleindex.csv`. It retains the 2015–2019 Italian electricity load sample, supplements two initial timestamps with explicitly documented ENTSO-E values, and constructs a regular 24-hour-per-day civil-time series in `Europe/Rome` by handling daylight-saving-time transitions.

The original dataset is not redistributed. To run the R script, obtain the source CSV from the cited release and place `time_series_60min_singleindex.csv` in the repository root, not in this `data/` folder, because that is the relative location referenced by the code.
