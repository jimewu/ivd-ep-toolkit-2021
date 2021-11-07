# * analysis

## paired difference分析

### 依照CLSI EP7Ed3E Table 3
### 對比y_control VS y_test

ep7_analysis[["paired difference"]] <-
    ep7_tidy[["combine"]] %>%
    group_by(
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

ep7_report_tab[["paired difference"]] <-
    ep7_analysis[["paired difference"]] %>%
    transmute(
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

ep7_report_tab[["paired difference FT"]] <-
    ep7_report_tab[["paired difference"]] %>%
    flextable() %>%
    align(
        align = "center",
        part = "header"
    ) %>%
    align(
        align = "center",
        part = "body"
    )


ep7_report_tab[["paired difference DT"]] <-
    datatable(
        ep7_report_tab[["paired difference"]],
        rownames = FALSE
    )

## Percentage Difference表格

## Difference表格

ep7_report_tab[["paired %difference"]] <-
    ep7_analysis[["paired difference"]] %>%
    transmute(
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

ep7_report_tab[["paired %difference FT"]] <-
    ep7_report_tab[["paired %difference"]] %>%
    flextable() %>%
    align(
        align = "center",
        part = "header"
    ) %>%
    align(
        align = "center",
        part = "body"
    )


ep7_report_tab[["paired %difference DT"]] <- datatable(
    ep7_report_tab[["paired %difference"]],
    rownames = FALSE
)