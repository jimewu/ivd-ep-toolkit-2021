# * analysis
ep17loq_analysis[["te"]] <- ep17loq_tidy[["combine"]] %>%
    group_by(
        reagent_lot,
        sample
    ) %>%
    summarize(
        ref = mean(y_ref),
        mean = mean(y),
        sd = sd(y)
    ) %>%
    mutate(
        bias = mean - ref,
        te_wgs = abs(bias) + 2 * sd,
        pct_te_wgs = 100 * te_wgs / ref,
        te_rms = (sd^2 + bias^2)^0.5,
        pct_te_rms = 100 * te_rms / ref
    ) %>%
    ungroup() %>%
    arrange(sample)

# ep17loq_analysis[["regression wgs"]] <- vector(mode = "list")

# ep17loq_analysis[["regression wgs"]][["regression"]] <- lm(
#     formula = pct_te_wgs ~ ref,
#     data = ep17loq_analysis[["te"]]
# )

# ep17loq_analysis[["regression wgs"]][["reverse regression"]] <- lm(
#     formula = ref ~ pct_te_wgs,
#     data = ep17loq_analysis[["te"]]
# )

# ep17loq_analysis[["regression wgs"]][["summary"]] <- summary(
#     ep17loq_analysis[["regression wgs"]][["regression"]]
# )

# * report_fig

## raw data (color = reagent lot)

ep17loq_report_fig[["raw"]] <- ggplot(
    ep17loq_tidy[["combine"]],
    aes(
        x = y_ref,
        y = y,
        color = factor(reagent_lot),
        shape = factor(y_ref)
    )
) +
    geom_jitter() +
    xlab("Reference Value") +
    ylab("Measured Value") +
    labs(
        color = "Reagent Lot",
        shape = "Reference Value"
    )

ep17loq_report_fig[["te wgs"]] <- ggplot(
    ep17loq_analysis[["te"]],
    aes(
        x = ref,
        y = pct_te_wgs
    )
) +
    geom_point(
        aes(color = factor(reagent_lot))
    ) +
    geom_hline(
        yintercept = 100 * ep17loq_import[["setting"]]$acceptance_criteria,
        color = "blue"
    ) +
    geom_smooth(
        method = "lm",
        formula = y ~ x,
        se = FALSE,
        linetype = 2,
        color = "black",
        alpha = 0.5
    ) +
    xlab(
        "Reference Value"
    ) +
    ylab(
        "%TE (Westgard Method)"
    ) +
    labs(
        color = "Reagent Lot"
    )

ep17loq_report_fig[["te rms"]] <- ggplot(
    ep17loq_analysis[["te"]],
    aes(
        x = ref,
        y = pct_te_rms
    )
) +
    geom_point(
        aes(color = factor(reagent_lot))
    ) +
    geom_hline(
        yintercept = 100 * ep17loq_import[["setting"]]$acceptance_criteria,
        color = "blue"
    ) +
    geom_smooth(
        method = "lm",
        formula = y ~ x,
        se = FALSE,
        linetype = 2,
        color = "black",
        alpha = 0.5
    ) +
    xlab(
        "Reference Value"
    ) +
    ylab(
        "%TE (RMS Method)"
    ) +
    labs(
        color = "Reagent Lot"
    )


# * report_tab
## Raw Data
ep17loq_report_tab[["raw"]] <- ep17loq_tidy[["combine"]] %>%
    datatable(
        options = list(scrollY = "450px"),
        colnames = c(
            "Sample",
            "Reagent Lot",
            "測量值",
            "真值(以參考方法得到)"
        )
    )


## 各樣品 x 各濃度的parametric descriptive statistics

ep17loq_report_tab[["te"]] <- cbind(
    ep17loq_analysis[["te"]][, c(1:2)],
    round(
        ep17loq_analysis[["te"]][, c(3:10)],
        digits = 3
    )
) %>%
    arrange(
        reagent_lot
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
        reagent_lot = "Reagent Lot",
        sample = "Sample",
        ref = "Reference Value",
        mean = "Measured Value: Mean",
        sd = "Measured Value: SD",
        bias = "Bias",
        te_wgs = "TE (Westgard)",
        pct_te_wgs = "%TE (Westgard)",
        te_rms = "TE (RMS)",
        pct_te_rms = "%TE (RMS)"
    )