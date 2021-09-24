source("ep7_main.r")

# ! tmp
# * Load PKG
pkg_lst <- c(
    "dplyr",
    "DT",
    "flextable",
    "ggplot2"
)

lapply(
    pkg_lst,
    library,
    character.only = TRUE
)

# * analysis
## 依照CLSI EP7Ed3E Table 3
## 計算y_control & y_test: mean, sd
## 對比y_control VS y_test:
## diff, diff_lwr, diff_upr,
## pct_diff, pct_diff_lwr, pct_diff_upr
data_ep7_analysis_diff <- data_ep7_tidy_combine %>%
    group_by(
        condition,
        interferent,
        interferent_level,
        analyte_level
    ) %>%
    summarise(
        test_mean = mean(y_test),
        test_sd = sd(y_test),
        control_mean = mean(y_control),
        control_sd = sd(y_control),
        # 計算diff_mean (and 95% CI), diff_sd
        diff_mean = mean(y_test - y_control),
        diff_sd = sd(y_test - y_control),
        ## 依照CLSI EP7Ed3E eq7-8計算
        df = length(y_test) + length(y_control) - 2,
        margin = qt(
            p = 1 - 0.05 / 2,
            df = df
        ) *
            sqrt(
                control_sd^2 / length(y_control) +
                    test_sd^2 / length(y_test)
            ),
        diff_mean_upr = diff_mean + margin,
        diff_mean_lwr = diff_mean - margin,
        pctdiff_mean = 100 * diff_mean / mean(y_control),
        pctdiff_mean_upr = 100 * diff_mean_upr / mean(y_control),
        pctdiff_mean_lwr = 100 * diff_mean_lwr / mean(y_control)
    ) %>%
    ungroup()

# * report_tab
## Difference表格
data_ep7_report_tab_diff <- data_ep7_analysis_diff %>%
    transmute(
        Interferent = interferent,
        "Interferent Level" = interferent_level,
        "Analyte Level" = analyte_level,
        "Test Mean" = round(
            test_mean,
            digits = 3
        ),
        "Test SD" = round(
            test_sd,
            digits = 3
        ),
        "Control Mean" = round(
            control_mean,
            digits = 3
        ),
        "Control SD" = round(
            control_sd,
            digits = 3
        ),
        "Difference" = round(
            diff_mean,
            digits = 3
        ),
        "95% CI Lwr." = round(
            diff_mean_upr,
            digits = 3
        ),
        "95% CI Upr." = round(
            diff_mean_lwr,
            digits = 3
        )
    )

data_ep7_report_tab_diff_dt <- datatable(
    data_ep7_report_tab_diff,
    rownames = FALSE
)

## Percentage Difference表格
## Difference表格
data_ep7_report_tab_pctdiff <- data_ep7_analysis_diff %>%
    transmute(
        Interferent = interferent,
        "Interferent Level" = interferent_level,
        "Analyte Level" = analyte_level,
        "Test Mean" = round(
            test_mean,
            digits = 3
        ),
        "Test SD" = round(
            test_sd,
            digits = 3
        ),
        "Control Mean" = round(
            control_mean,
            digits = 3
        ),
        "Control SD" = round(
            control_sd,
            digits = 3
        ),
        "%Difference" = round(
            pctdiff_mean,
            digits = 3
        ),
        "95% CI Lwr." = round(
            pctdiff_mean_upr,
            digits = 3
        ),
        "95% CI Upr." = round(
            pctdiff_mean_lwr,
            digits = 3
        )
    )

data_ep7_report_tab_pctdiff_dt <- datatable(
    data_ep7_report_tab_pctdiff,
    rownames = FALSE
)