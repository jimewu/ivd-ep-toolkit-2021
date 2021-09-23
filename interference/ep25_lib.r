# ! tmp
# * Load PKG
pkg_lst <- c(
    "readODS",
    "dplyr",
    "flextable",
    "ggplot2",
    "here"
)

lapply(
    pkg_lst,
    library,
    character.only = TRUE
)

# * analysis

## 1st order regression
data_ep25_analysis_rgs <- lapply(
    data_ep25_tidy_split,
    function(x) {
        lm(
            formula = y ~ day,
            data = x
        )
    }
)

## regression summary
data_ep25_analysis_sum <- lapply(
    data_ep25_analysis_rgs,
    summary
)

## 方便:從summary()結果拿出coefficients
data_ep25_analysis_sum_coef <- lapply(
    data_ep25_analysis_sum,
    function(x) {
        result <- x$coefficients
        result <- as.data.frame(result)

        return(result)
    }
)

## 方便:從summary()結果拿出p-value
data_ep25_analysis_sum_p <- lapply(
    data_ep25_analysis_sum_coef,
    function(x) {
        p_value <- x[nrow(x), ncol(x)]
        return(p_value)
    }
)

## 方便:從summary()結果拿出slope
data_ep25_analysis_sum_slope <- lapply(
    data_ep25_analysis_sum_coef,
    function(x) {
        p_value <- x[nrow(x), "Estimate"]
        return(p_value)
    }
)

## 產生僅包含regression顯著的資料集
data_ep25_analysis_sig_split <- data_ep25_tidy_split

for (x in names(data_ep25_analysis_sig_split)) {
    if (data_ep25_analysis_sum_p[[x]] >= 0.05) {
        data_ep25_analysis_sig_split[[x]] <- NULL
    }
}

## 將regression顯著的資料集合併
if (length(data_ep25_analysis_sig_split) == 1) {
    data_ep25_analysis_sig_combine <- data_ep25_analysis_sig_split[[1]]
} else if (length(data_ep25_analysis_sig_split) > 1) {
    data_ep25_analysis_sig_combine <- data_ep25_analysis_sig_split[[1]]

    for (
        x in seq(
            2, length(data_ep25_analysis_sig_split)
        )
    ) {
        data_ep25_analysis_sig_combine <- rbind(
            data_ep25_analysis_sig_combine,
            data_ep25_analysis_sig_split[[x]]
        )
    }
}

## 用regression model去逐日推算每天的值以及95% CI
data_ep25_analysis_predict <-
    lapply(
        names(data_ep25_analysis_rgs),
        function(x) {
            predict <- predict.lm(
                object = data_ep25_analysis_rgs[[x]],
                newdata = data.frame(
                    # 預測範圍是從day 0到最後一天，逐日
                    day = seq(
                        0,
                        max(data_ep25_tidy_split[[x]]$day),
                        1
                    )
                ),
                # 取95% CI
                interval = "confidence",
                level = 0.95
            ) %>%
                data.frame() %>%
                # 整理結果
                cbind(
                    sample = x,
                    day = seq(
                        0,
                        max(data_ep25_tidy_split[[x]]$day),
                        1
                    ),
                    .
                )

            return(predict)
        }
    )

names(data_ep25_analysis_predict) <- names(data_ep25_analysis_rgs)

data_ep25_analysis_maxday <- sapply(
    names(data_ep25_analysis_predict),
    function(x) {
        # 取出day0時的y值
        day0 <- data_ep25_analysis_predict[[x]] %>%
            filter(day == 0)

        y0 <- day0$fit

        # 計算允許的limit
        limit <- ifelse(
            data_ep25_analysis_sum_slope[[x]] < 0,
            y0 * (100 - setting_ep25$allowable_drift) / 100,
            y0 * (100 + setting_ep25$allowable_drift) / 100
        )

        # 在以下情況分別算出maxday: p>= 0.05, 顯著負漂移, 顯著正漂移
        if (data_ep25_analysis_sum_p[[x]] >= 0.05) {
            day_pass <- max(data_ep25_analysis_predict[[x]]$day)
        } else if (data_ep25_analysis_sum_slope[[x]] < 0) {
            tb_pass <- data_ep25_analysis_predict[[x]] %>%
                filter(lwr > limit)

            day_pass <- max(tb_pass$day)
        } else {
            tb_pass <- data_ep25_analysis_predict[[x]] %>%
                filter(upr < limit)

            day_pass <- max(tb_pass$day)
        }

        return(day_pass)
    }
)

# * report_fig
## qc圖 for 找outlier
data_ep25_report_fig_qc <- ggplot(
    data_ep25_tidy_combine,
    aes(
        x = day,
        y = y
    )
) +
    geom_boxplot() +
    geom_line(
        aes(
            y = limit_lwr
        ),
        linetype = 2,
        color = "#00AFBB"
    ) +
    geom_line(
        aes(
            y = limit_upr
        ),
        linetype = 2,
        color = "#00AFBB"
    ) +
    facet_wrap(
        ~sample,
        scales = "free_y"
    ) +
    ylab(setting_ep25$unit_of_device)

## 結果圖帶regression curve
data_ep25_report_fig_sig <- ggplot(
    data_ep25_analysis_sig_combine,
    aes(
        x = day,
        y = y
    )
) +
    geom_point() +
    geom_smooth(
        method = "lm",
        formula = y ~ x,
        se = TRUE,
        level = 0.95
    ) +
    geom_line(
        aes(
            y = limit_lwr
        ),
        linetype = 2,
        color = "#00AFBB"
    ) +
    geom_line(
        aes(
            y = limit_upr
        ),
        linetype = 2,
        color = "#00AFBB"
    ) +
    ylab(setting_ep25$unit_of_device) +
    facet_wrap(
        ~sample,
        scales = "free_y"
    )

# * report_tab
## 取出所有的原始y值，合併為單一data.frame並轉置
data_ep25_report_tab_raw <- data.frame(
    y = data_ep25_tidy_split[[1]]$y
)

for (
    x in seq(
        2, length(data_ep25_tidy_split)
    )
) {
    data_ep25_report_tab_raw <- cbind(
        data_ep25_report_tab_raw,
        data_ep25_tidy_split[[x]]$y
    )
}

data_ep25_report_tab_raw <- data_ep25_report_tab_raw %>%
    round(digits = 3) %>%
    t() %>%
    data.frame(
        sample = names(data_ep25_tidy_split),
        .
    )

colnames(data_ep25_report_tab_raw) <- c(
    "sample",
    data_ep25_tidy_split[[1]]$day
)

## 產生flextable
data_ep25_report_tab_raw <- data_ep25_report_tab_raw %>%
    flextable() %>%
    set_header_labels(
        sample = "Sample"
    ) %>%
    add_header_row(
        values = c(
            "Sample",
            "Day"
        ),
        colwidths = c(
            1, length(data_ep25_tidy_split[[1]]$day)
        )
    ) %>%
    # 兩層header合併
    merge_v(
        part = "header"
    ) %>%
    align(
        align = "center",
        part = "all"
    )

## regression結果表格
data_ep25_report_tab_regression_split <- lapply(
    names(data_ep25_analysis_sum_coef),
    function(x) {
        # 取值並設定digits = 3
        result <- data.frame(
            intercept = format(
                data_ep25_analysis_sum_coef[[x]][1, "Estimate"],
                digits = 3
            ),
            slope = format(
                data_ep25_analysis_sum_coef[[x]][2, "Estimate"],
                digits = 3
            ),
            slope_se = format(
                data_ep25_analysis_sum_coef[[x]][2, "Std. Error"],
                digits = 3
            ),
            slope_t = format(
                data_ep25_analysis_sum_coef[[x]][2, "t value"],
                digits = 3
            ),
            # p-value改用scientific表示
            p = format(
                data_ep25_analysis_sum_coef[[x]][2, "Pr(>|t|)"],
                scientific = TRUE,
                digits = 3
            ),
            maxday = data_ep25_analysis_maxday[[x]]
        )

        return(result)
    }
)

names(data_ep25_report_tab_regression_split) <- names(data_ep25_analysis_sum_coef)

if (
    length(data_ep25_report_tab_regression_split) == 1
) {
    data_ep25_report_tab_regression_combine <- data_ep25_report_tab_regression_split[[1]]
} else if (
    length(data_ep25_report_tab_regression_split) > 1
) {
    data_ep25_report_tab_regression_combine <- data_ep25_report_tab_regression_split[[1]]

    for (
        x in seq(
            2, length(data_ep25_report_tab_regression_split)
        )
    ) {
        data_ep25_report_tab_regression_combine <- rbind(
            data_ep25_report_tab_regression_combine,
            data_ep25_report_tab_regression_split[[x]]
        )
    }
}

data_ep25_report_tab_regression_combine <- data_ep25_report_tab_regression_combine %>%
    data.frame(
        sample = names(data_ep25_report_tab_regression_split),
        .
    ) %>%
    flextable() %>%
    set_header_labels(
        sample = "Sample",
        intercept = "Intercept",
        slope = "Slope",
        slope_se = "Slope SE",
        slope_t = "Slope t",
        p = "p-value",
        maxday = "Stability"
    ) %>%
    add_header_row(
        values = c(
            sample = "Sample",
            regression_results = "Regression Results"
        ),
        colwidths = c(
            1, ncol(data_ep25_report_tab_regression_combine)
        )
    ) %>%
    merge_v(
        part = "header"
    ) %>%
    align(
        align = "center",
        part = "header"
    ) %>%
    align(
        align = "center",
        part = "body"
    )