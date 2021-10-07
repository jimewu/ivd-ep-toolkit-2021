source("main_ep17lod.r")

# * Load PKG
pkg_lst <- c(
    "dplyr",
    "flextable",
    "DT",
    "ggplot2"
)

lapply(
    pkg_lst,
    library,
    character.only = TRUE
)

# * analysis
### 計算各樣品的Mean, SD, Ratio > LOB, Median
data_ep17lod_analysis[["basic"]] <- data_ep17lod_tidy[["combine"]] %>%
    group_by(
        sample,
        reagent_lot
    ) %>%
    summarize(
        n = length(y),
        # parametric statistics
        mean = mean(y),
        sd = sd(y),
        # non-parametric statistics
        positivity = length(y[y > setting_ep17$lob]) / n,
        median = median(y)
    ) %>%
    ungroup() %>%
    arrange(reagent_lot)

# data_ep17lod_analysis_basic <- data_ep17lod_tidy_combine %>%
#     group_by(
#         sample,
#         reagent_lot
#     ) %>%
#     summarize(
#         n = length(y),
#         mean = mean(y),
#         sd = sd(y),
#         positivity = length(y[y > setting_ep17$lob]) / n,
#         median = median(y)
#     ) %>%
#     ungroup() %>%
#     arrange(reagent_lot)

### 以Shapiro-Wilk Test檢定SD是否為常態分佈
data_ep17lod_analysis[["spw"]] <- shapiro.test(
    data_ep17lod_analysis[["basic"]]$sd
)$p.value


# data_ep17lod_analysis_spw <- shapiro.test(
#     data_ep17lod_analysis_basic$sd
# )

### 計算cp
data_ep17lod_analysis[["cp"]] <- data.frame(
    L = sum(data_ep17lod_analysis[["basic"]]$n),
    J = length(
        levels(
            factor(data_ep17lod_analysis[["basic"]]$sample)
        )
    )
) %>%
    mutate(
        upr = qnorm(0.95),
        lwr = 1 - (4 * (L - J))^(-1),
        cp = upr / lwr
    )

data_ep17lod_analysis[["cp"]] <- data_ep17lod_analysis[["cp"]]$cp


# imd <- vector(mode = "list")

# imd[["cp"]] <- data.frame(
#     L = sum(data_ep17lod_analysis_basic$n),
#     J = length(
#         levels(
#             factor(data_ep17lod_analysis_basic$sample)
#         )
#     )
# ) %>%
#     mutate(
#         upr = qnorm(0.95),
#         lwr = 1 - (4 * (L - J))^(-1),
#         cp = upr / lwr
#     )

### 取出reagent lot數量
data_ep17lod_analysis[["reagent_lot number"]] <- length(
    levels(
        factor(data_ep17lod_analysis[["basic"]]$reagent_lot)
    )
)

# imd[["rlot_num"]] <- length(
#     levels(
#         factor(data_ep17lod_analysis_basic$reagent_lot)
#     )
# )

### 定義計算SDL公式
get_sdl <- function(data) {
    result <- data %>%
        group_by(
            sample
        ) %>%
        summarize(
            n = length(y),
            sd = sd(y)
        ) %>%
        mutate(
            upr = (n - 1) * sd^2,
            lwr = n - 1
        ) %>%
        summarize(
            sdl = sqrt(
                sum(upr) / sum(lwr)
            )
        )

    return(result)
}

### if-loop: 若reagent_lot ≤3則分批計算，若≥4則合併計算
if (
    imd[["rlot_num"]] >= 4
) {
    data_ep17lod_analysis_lod <- data.frame(
        cp = imd[["cp"]]$cp,
        sdl = get_sdl(data_ep17lod_tidy_combine),
        lod = setting_ep17$lob +
            sdl * imd[["cp"]]$cp
    )
} else if (
    imd[["rlot_num"]] <= 3
) {
    data_ep17lod_analysis_lod_lot <- lapply(
        data_ep17lod_tidy_split,
        function(x) {
            result <- data.frame(
                sdl = get_sdl(x)
            ) %>%
                transmute(
                    sdl_lot = sdl,
                    lod_lot = setting_ep17$lob +
                        sdl_lot * imd[["cp"]]$cp
                ) %>%
                cbind(
                    reagent_lot = x$reagent_lot[1],
                    cp = imd[["cp"]]$cp,
                    .
                )

            return(result)
        }
    )
}

### if-loop: 若有data_ep17lod_analysis_lod_lot, 則合併並計算final LoD

if (
    is.null(data_ep17lod_analysis_lod_lot) == FALSE
) {
    data_ep17lod_analysis_lod <- data_ep17lod_analysis_lod_lot[[1]]

    for (
        x in seq(2, length(data_ep17lod_analysis_lod_lot))
    ) {
        data_ep17lod_analysis_lod <- rbind(
            data_ep17lod_analysis_lod,
            data_ep17lod_analysis_lod_lot[[x]]
        )
    }

    data_ep17lod_analysis_lod <- data_ep17lod_analysis_lod %>%
        cbind(
            .,
            final_lod = max(.$lod_lot)
        )
}

# * report_fig

### raw data (color = reagent lot)
data_ep17lod_fig_raw <- ggplot(
    data_ep17lod_tidy_combine,
    aes(
        x = sample,
        y = y,
        color = sample
    )
) +
    geom_jitter() +
    xlab("Sample") +
    ylab("Measurement Value")


### 濃度 VS SD (color = reagent lot)
data_ep17lod_fig_sd <- ggplot(
    data_ep17lod_analysis_basic,
    aes(
        x = mean,
        y = sd,
        color = factor(reagent_lot)
    )
) +
    geom_point() +
    xlab("Measurement Values") +
    ylab("SD")

### qqplot of SD
data_ep17lod_fig_qq <- ggplot(
    data_ep17lod_analysis_basic,
    aes(
        sample = sd
    )
) +
    stat_qq() +
    stat_qq_line(
        linetype = 2,
        color = "red",
        alpha = 0.5
    ) +
    xlab("Normal Distribution") +
    ylab("SD")

# * report_tab

### 各樣品 x 各濃度的SD表
data_ep17lod_report_tab_raw <- data_ep17lod_analysis_basic %>%
    mutate(
        mean = round(mean, digits = 3),
        sd = round(sd, digits = 3)
    ) %>%
    flextable() %>%
    align(
        align = "center",
        part = "header"
    ) %>%
    align(
        align = "center",
        part = "body"
    ) %>%
    set_header_labels(
        sample = "Sample",
        reagent_lot = "Reagent Lot",
        n = "N",
        mean = "Mean",
        sd = "SD",
        positivity = "Ratio >Lob"
    ) %>%
    add_footer_lines(
        values = paste(
            "unit of mean and SD:",
            setting_ep17$unit_of_device
        )
    ) %>%
    align(
        align = "right",
        part = "footer"
    )

### LoD分析結果

data_ep17lod_report_tab_lod <- data_ep17lod_analysis_lod %>%
    round(digits = 3) %>%
    flextable() %>%
    align(
        align = "center",
        part = "header"
    ) %>%
    align(
        align = "center",
        part = "body"
    ) %>%
    merge_v(
        part = "body"
    ) %>%
    set_header_labels(
        lod_lot = "Lot LoD",
        final_lod = "Final LoD"
    ) %>%
    compose(
        part = "header",
        j = "sdl_lot",
        value = as_paragraph(
            "Lot SD",
            as_sub("L")
        )
    ) %>%
    compose(
        part = "header",
        j = "cp",
        value = as_paragraph(
            "C",
            as_sub("P")
        )
    ) %>%
    add_footer_lines(
        values = paste(
            "unit of SD and LoD:",
            setting_ep17$unit_of_device
        )
    ) %>%
    align(
        align = "right",
        part = "footer"
    )

# * report_crit: 結論數字

### Shapiro-Wilk test p-value
data_ep17lod_report_crit_spw <- data_ep17lod_analysis_spw$p.value

### Final LoD
data_ep17lod_report_crit_lod <- data_ep17lod_analysis_lod$final_lod[1]