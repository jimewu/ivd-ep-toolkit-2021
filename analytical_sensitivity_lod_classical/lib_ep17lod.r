# * analysis
## Descriptive Statistics: 計算各樣品的Mean, SD, Ratio > LOB, Median
ep17lod_analysis[["desc"]] <- ep17lod_tidy[["combine"]] %>%
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
        positivity = length(y[y > ep17lod_import[["setting"]]$lob]) / n,
        median = median(y)
    ) %>%
    ungroup() %>%
    arrange(reagent_lot)

## 以Shapiro-Wilk Test檢定SD是否為常態分佈
ep17lod_analysis[["spw.p"]] <- shapiro.test(
    ep17lod_analysis[["desc"]]$sd
)$p.value


## 計算cp
ep17lod_analysis[["cp"]] <- data.frame(
    L = sum(ep17lod_analysis[["desc"]]$n),
    J = length(
        levels(
            factor(ep17lod_analysis[["desc"]]$sample)
        )
    )
) %>%
    mutate(
        upr = qnorm(0.95),
        lwr = 1 - (4 * (L - J))^(-1),
        cp = upr / lwr
    )

ep17lod_analysis[["cp"]] <- ep17lod_analysis[["cp"]]$cp

## 取出reagent lot數量
ep17lod_analysis[["reagent_lot number"]] <- length(
    levels(
        factor(ep17lod_analysis[["desc"]]$reagent_lot)
    )
)

## 定義計算SDL公式
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

## if-loop:
### reagent_lot≥4: 合併計算
### reagent_lot≤3: 分批計算
if (
    ep17lod_analysis[["reagent_lot number"]] >= 4
) {
    ep17lod_analysis[["lod"]] <- data.frame(
        cp = ep17lod_analysis[["cp"]],
        sdl = get_sdl(ep17lod_tidy[["combine"]])$sdl
    ) %>%
        mutate(
            lod = ep17lod_import[["setting"]]$lob +
                sdl * cp
        )
} else if (
    ep17lod_analysis[["reagent_lot number"]] <= 3
) {
    ep17lod_analysis[["lot lod"]] <- lapply(
        ep17lod_tidy[["split"]],
        function(x) {
            result <- data.frame(
                cp = ep17lod_analysis[["cp"]],
                sdl_lot = get_sdl(x)$sdl
            ) %>%
                mutate(
                    lod_lot = ep17lod_import[["setting"]]$lob +
                        sdl_lot * cp
                ) %>%
                cbind(
                    reagent_lot = x$reagent_lot[1],
                    .
                )

            return(result)
        }
    )
}

## if-loop: 若有ep17lod_analysis[["lot lod"]], 則合併並計算final LoD

if (
    is.null(ep17lod_analysis[["lot lod"]]) == FALSE
) {
    ep17lod_analysis[["lod"]] <- ep17lod_analysis[["lot lod"]][[1]]

    for (
        x in seq(2, length(ep17lod_analysis[["lot lod"]]))
    ) {
        ep17lod_analysis[["lod"]] <- rbind(
            ep17lod_analysis[["lod"]],
            ep17lod_analysis[["lot lod"]][[x]]
        )
    }

    ep17lod_analysis[["lod"]] <- ep17lod_analysis[["lod"]] %>%
        cbind(
            .,
            final_lod = max(.$lod_lot)
        )
}

## 計算non-parametric option
ep17lod_analysis[["non-parametric lod"]] <- ep17lod_analysis[["desc"]] %>%
    select(
        sample,
        reagent_lot,
        n,
        positivity,
        median
    ) %>%
    group_by(sample) %>%
    summarize(
        lod = ifelse(
            all(
                positivity > (1 - ep17lod_import[["setting"]]$beta)
            ),
            mean(median),
            NA
        )
    ) %>%
    arrange(lod)

# * report_fig

## raw data (color = reagent lot)
ep17lod_report_fig[["raw"]] <- ggplot(
    ep17lod_tidy[["combine"]],
    aes(
        x = sample,
        y = y,
        color = sample
    )
) +
    geom_jitter() +
    xlab("Sample") +
    ylab("Measurement Value")


## 濃度 VS SD (color = reagent lot)
ep17lod_report_fig[["sd"]] <- ggplot(
    ep17lod_analysis[["desc"]],
    aes(
        x = mean,
        y = sd,
        color = factor(reagent_lot)
    )
) +
    geom_point() +
    xlab("Measurement Values") +
    ylab("SD")

## qqplot of SD
ep17lod_report_fig[["qq"]] <- ggplot(
    ep17lod_analysis[["desc"]],
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

## 各樣品 x 各濃度的parametric descriptive statistics
ep17lod_report_tab[["raw parametric"]] <- ep17lod_analysis[["desc"]] %>%
    transmute(
        sample = sample,
        reagent_lot = reagent_lot,
        n = n,
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
        sd = "SD"
    ) %>%
    add_footer_lines(
        values = paste(
            "unit of mean and SD:",
            ep17lod_import[["setting"]]$unit_of_device,
            "\nShapiro-Wilk Test of SD: p =",
            round(
                ep17lod_analysis[["spw.p"]],
                digits = 3
            )
        )
    ) %>%
    align(
        align = "right",
        part = "footer"
    )

## 各樣品 x 各濃度的non-parametric descriptive statistics
ep17lod_report_tab[["raw non-parametric"]] <- ep17lod_analysis[["desc"]] %>%
    arrange(sample) %>%
    transmute(
        sample = sample,
        reagent_lot = reagent_lot,
        n = n,
        positivity = round(positivity, digits = 3),
        median = round(median, digits = 3)
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
        positivity = "Ratio > LoB",
        median = "Median"
    ) %>%
    add_footer_lines(
        values = paste(
            "unit of median:",
            ep17lod_import[["setting"]]$unit_of_device,
            "\nShapiro-Wilk Test of SD: p =",
            round(
                ep17lod_analysis[["spw.p"]],
                digits = 3
            )
        )
    ) %>%
    align(
        align = "right",
        part = "footer"
    )


## LoD分析結果

### Parametric

ep17lod_report_tab[["lod parametric"]] <- ep17lod_analysis[["lod"]] %>%
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
            ep17lod_import[["setting"]]$unit_of_device
        )
    ) %>%
    align(
        align = "right",
        part = "footer"
    )

### Non-parametric

ep17lod_report_tab[["lod non-parametric"]] <-
    ep17lod_analysis[["non-parametric lod"]] %>%
    mutate(
        lod = round(lod, digits = 3)
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
    merge_v(
        part = "body"
    ) %>%
    set_header_labels(
        sample = "Sample",
        lod = "LoD"
    ) %>%
    add_footer_lines(
        values = paste(
            "unit of LoD:",
            ep17lod_import[["setting"]]$unit_of_device
        )
    ) %>%
    align(
        align = "right",
        part = "footer"
    )


# * report_crit: 結論數字

## Shapiro-Wilk test p-value

ep17lod_report_crit[["spw p-value"]] <- ep17lod_analysis[["spw.p"]]

## Final LoD

### Parametric

ep17lod_report_crit[["parametric final lod"]] <-
    ep17lod_analysis[["lod"]]$final_lod[1]

### Non-parametric

ep17lod_report_crit[["non-parametric final lod"]] <-
    ep17lod_analysis[["non-parametric lod"]]$lod[1]