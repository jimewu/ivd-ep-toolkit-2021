# * analysis

## 1st order regression

ep25_analysis[["regression"]] <- lapply(
    ep25_tidy[["split"]],
    function(x) {
        lm(
            formula = y ~ day,
            data = x
        )
    }
)

## regression summary

ep25_analysis[["summary"]] <- lapply(
    ep25_analysis[["regression"]],
    summary
)

## 方便:從summary()結果拿出coefficients

ep25_analysis[["summary coef"]] <- lapply(
    ep25_analysis[["summary"]],
    function(x) {
        result <- x$coefficients
        result <- as.data.frame(result)

        return(result)
    }
)

## 方便:從summary()結果拿出p-value

ep25_analysis[["summary p"]] <- lapply(
    ep25_analysis[["summary coef"]],
    function(x) {
        p_value <- x[nrow(x), ncol(x)]
        return(p_value)
    }
)

## 方便:從summary()結果拿出slope

ep25_analysis[["summary slope"]] <- lapply(
    ep25_analysis[["summary coef"]],
    function(x) {
        result <- x[nrow(x), "Estimate"]
        return(result)
    }
)

## 產生僅包含regression顯著的資料集

ep25_analysis[["sample regressed"]] <- ep25_tidy[["combine"]] %>%
    filter(
        sample %in% names(ep25_analysis[["summary p"]])[
            ep25_analysis[["summary p"]] < 0.05
        ]
    )

## 用regression model去逐日推算每天的值以及95% CI

ep25_analysis[["predict"]] <-
    lapply(
        names(ep25_analysis[["regression"]]),
        function(x) {
            predict <- predict.lm(
                object = ep25_analysis[["regression"]][[x]],
                newdata = data.frame(
                    # 預測範圍是從day 0到最後一天，逐日
                    day = seq(
                        0,
                        max(ep25_tidy[["split"]][[x]]$day),
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
                        max(ep25_tidy[["split"]][[x]]$day),
                        1
                    ),
                    .
                )

            return(predict)
        }
    )

names(ep25_analysis[["predict"]]) <-
    names(ep25_analysis[["regression"]])

ep25_analysis[["maxday"]] <- sapply(
    names(ep25_analysis[["predict"]]),
    function(x) {
        # 取出day0時的y值
        day0 <- ep25_analysis[["predict"]][[x]] %>%
            filter(day == 0)

        y0 <- day0$fit

        # 計算允許的limit
        limit <- ifelse(
            ep25_analysis[["summary slope"]][[x]] < 0,
            y0 * (
                100 - ep25_import[["setting"]]$allowable_drift
            ) / 100,
            y0 * (
                100 + ep25_import[["setting"]]$allowable_drift
            ) / 100
        )

        # 在以下情況分別算出maxday: p>= 0.05, 顯著負漂移, 顯著正漂移
        if (
            ep25_analysis[["summary p"]][[x]] >= 0.05
        ) {
            day_pass <- max(ep25_analysis[["predict"]][[x]]$day)
        } else if (
            ep25_analysis[["summary slope"]][[x]] < 0
        ) {
            tb_pass <- ep25_analysis[["predict"]][[x]] %>%
                filter(lwr > limit)

            day_pass <- max(tb_pass$day)
        } else {
            tb_pass <- ep25_analysis[["predict"]][[x]] %>%
                filter(upr < limit)

            day_pass <- max(tb_pass$day)
        }

        return(day_pass)
    }
)

# * report_fig

## raw data圖 (找outlier)

ep25_report_fig[["raw"]] <- ggplot(
    ep25_tidy[["combine"]],
    aes(
        x = day,
        y = y
    )
) +
    geom_point() +
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
    ylab(ep25_import[["setting"]]$unit_of_device)

## 結果圖帶regression curve

ep25_report_fig[["regression"]] <- ggplot(
    ep25_analysis[["sample regressed"]],
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
    ylab(ep25_import[["setting"]]$unit_of_device) +
    facet_wrap(
        ~sample,
        scales = "free_y"
    )

# * report_tab

## raw data

### 取出所有的day以及原始y值(重新命名為sample)

ep25_report_tab[["raw split"]] <- lapply(
    names(ep25_tidy[["split"]]),
    function(x) {
        result <- ep25_tidy[["split"]][[x]] %>%
            select(day, y)

        colnames(result) <- c("day", x)

        return(result)
    }
)

### 合併為單一data.frame

ep25_report_tab[["raw"]] <- ep25_report_tab[["raw split"]][[1]]

for (
    x in seq(
        2, length(ep25_report_tab[["raw split"]])
    )
) {
    ep25_report_tab[["raw"]] <- merge(
        ep25_report_tab[["raw"]],
        ep25_report_tab[["raw split"]][[x]]
    )
}

### 取小數到第三位 + 轉置

ep25_report_tab[["raw"]] <- ep25_report_tab[["raw"]] %>%
    round(digits = 3) %>%
    t() %>%
    as.data.frame()

### 以day命名欄位

colnames(ep25_report_tab[["raw"]]) <- as.vector(
    ep25_report_tab[["raw"]]["day", ]
)

### 加上sample欄位，轉為flextable

ep25_report_tab[["raw"]] <- ep25_report_tab[["raw"]] %>%
    cbind(
        sample = rownames(ep25_report_tab[["raw"]]),
        .
    ) %>%
    # 拿掉day這一列
    .[-1, ] %>%
    # 產生flextable
    flextable() %>%
    set_header_labels(
        sample = "Sample"
    ) %>%
    add_header_row(
        values = c(
            sample = "Sample",
            day = "Day"
        ),
        colwidths = c(
            1, ncol(ep25_report_tab[["raw"]])
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

ep25_report_tab[["regression split"]] <- lapply(
    names(ep25_analysis[["summary coef"]]),
    function(x) {
        # 取值並設定digits = 3
        result <- data.frame(
            intercept = format(
                ep25_analysis[["summary coef"]][[x]][1, "Estimate"],
                digits = 3
            ),
            slope = format(
                ep25_analysis[["summary coef"]][[x]][2, "Estimate"],
                digits = 3
            ),
            slope_se = format(
                ep25_analysis[["summary coef"]][[x]][2, "Std. Error"],
                digits = 3
            ),
            slope_t = format(
                ep25_analysis[["summary coef"]][[x]][2, "t value"],
                digits = 3
            ),
            # p-value改用scientific表示
            p = format(
                ep25_analysis[["summary coef"]][[x]][2, "Pr(>|t|)"],
                scientific = TRUE,
                digits = 3
            ),
            maxday = ep25_analysis[["maxday"]][[x]]
        )

        return(result)
    }
)

names(ep25_report_tab[["regression split"]]) <- names(
    ep25_analysis[["summary coef"]]
)

### 表格合併

if (
    length(ep25_report_tab[["regression split"]]) == 1
) {
    ep25_report_tab[["regression"]] <-
        ep25_report_tab[["regression split"]][[1]]
} else if (
    length(ep25_report_tab[["regression split"]]) > 1
) {
    ep25_report_tab[["regression"]] <-
        ep25_report_tab[["regression split"]][[1]]

    for (
        x in seq(
            2, length(ep25_report_tab[["regression split"]])
        )
    ) {
        ep25_report_tab[["regression"]] <- rbind(
            ep25_report_tab[["regression"]],
            ep25_report_tab[["regression split"]][[x]]
        )
    }
}

ep25_report_tab[["regression"]] <-
    ep25_report_tab[["regression"]] %>%
    data.frame(
        sample = names(ep25_report_tab[["regression split"]]),
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
            1, ncol(ep25_report_tab[["regression"]])
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

ep25_report_crit[["maxday"]] <- min(ep25_analysis[["maxday"]])