# main

## `data_ep25_import`

## `data_ep25_tidy_combine`

- columns:
    - Sample
    - Day
    - Replicate
    - y

## `data_ep25_tidy_split`

# lib

## `data_ep25_analysis`

### `[["regression"]]`

- linear regression

### `[["summary"]]`

- summary of linear regression

### `[["coef"]]`

- from `[["summary"]]`
- coefficients of regression

### `[["p_value"]]`

- from `[["coef"]]`

### `[["is_significant"]]`

- from `[["p_value"]]`

### `[["prediction"]]`

- from `[["regression"]]`

### `[["maxday"]]`

- find maximal days by within acceptable drifting 95% CI

## `data_ep25_report_fig`

### `[["raw"]]`

- 原始數據，作為QC用途(確認有無outlier)
- from `data_ep25_tidy_combine`
- aes(color = replicate) + facet_wrap(~ Sample)

### `[["regression"]]`

- from `data_ep25_tidy_combine`
- group_by(Day) %>% summarize(y_mean = mean(y))
- geom_smooth()

## `data_ep25_report_critical`

- from `data_ep25_analysis`
    - final stability claim as the minimum of sample stability claims
