# * analysis

ep6_analysis[["desc"]] <- ep6_tidy[["combine"]] %>%
    group_by(
        sample,
        rc
    ) %>%
    summarize(
        mean = mean(y),
        sd = sd(y),
        replicate = length(y)
    ) %>%
    mutate(
        cv = 100 * sd / mean
    ) %>%
    ungroup() %>%
    mutate(
        expected = max(.$mean),
        expected = expected * rc,
        var = sd^2,
        weight = replicate / var
    )

ep6_analysis[["model ideal"]] <- lm(
    formula = mean ~ expected + 0,
    data = ep6_analysis[["desc"]],
    weight = ep6_analysis[["desc"]]$weight
)

ep6_analysis[["model nonideal"]] <- lm(
    formula = mean ~ expected,
    data = ep6_analysis[["desc"]],
    weight = ep6_analysis[["desc"]]$weight
)

ep6_analysis[["desc ideal"]] <- cbind(
    ep6_analysis[["desc"]],
    predicted = predict(ep6_analysis[["model ideal"]])
) %>%
    mutate(
        deviation = mean - predicted,
        pct_deviation = 100 * deviation / mean
    )

ep6_analysis[["desc nonideal"]] <- cbind(
    ep6_analysis[["desc"]],
    predicted = predict(ep6_analysis[["model nonideal"]])
) %>%
    mutate(
        deviation = mean - predicted,
        pct_deviation = 100 * deviation / mean
    )

# * report_fig

## raw data

ep6_report_fig[["raw all"]] <- ggplot(
    ep6_tidy[["combine"]],
    aes(
        x = rc,
        y = y,
        color = factor(replicate)
    )
) +
    geom_point() +
    xlab("RC") +
    ylab(
        paste(
            "Measured Value (",
            ep6_import[["setting"]]$unit_of_device[1],
            ")",
            sep = ""
        )
    ) +
    labs(
        color = "Replicate"
    )

ep6_report_fig[["raw facet"]] <- ep6_report_fig[["raw all"]] +
    facet_wrap(
        facets = ~sample,
        scales = "free"
    ) +
    scale_x_continuous(
        breaks = seq(0, 1, by = 0.05)
    )

ep6_report_fig[["sd"]] <- ggplot(
    ep6_analysis[["desc"]],
    aes(
        x = mean,
        y = sd
    )
) +
    geom_point(
        color = "blue"
    ) +
    xlab(
        paste(
            "Measured Value (",
            ep6_import[["setting"]]$unit_of_device[1],
            ")",
            sep = ""
        )
    ) +
    ylab("SD")


ep6_report_fig[["cv"]] <- ggplot(
    ep6_analysis[["desc"]],
    aes(
        x = mean,
        y = cv
    )
) +
    geom_point(
        color = "blue"
    ) +
    xlab(
        paste(
            "Measured Value (",
            ep6_import[["setting"]]$unit_of_device[1],
            ")",
            sep = ""
        )
    ) +
    ylab("%CV")

ep6_report_fig[["regression ideal"]] <-
    rbind(
        data.frame(
            expected = ep6_analysis[["desc ideal"]]$expected,
            y = ep6_analysis[["desc ideal"]]$mean,
            type = "Measured Value"
        ),
        data.frame(
            expected = ep6_analysis[["desc ideal"]]$expected,
            y = ep6_analysis[["desc ideal"]]$predicted,
            type = "Predicted Value"
        )
    )

ep6_report_fig[["regression ideal"]] <-
    ggplot(
        data = ep6_report_fig[["regression ideal"]],
        aes(
            x = expected,
            y = y,
            color = type
        )
    ) +
    geom_point() +
    geom_line(
        data = ep6_report_fig[["regression ideal"]][
            ep6_report_fig[["regression ideal"]]$type == "Predicted Value",
        ]
    ) +
    xlab("Expected Value") +
    ylab(
        paste(
            "Measured Value (",
            ep6_import[["setting"]]$unit_of_device[1],
            ")",
            sep = ""
        )
    ) +
    labs(
        color = "Type"
    )

ep6_report_fig[["deviation ideal"]] <-
    ggplot(
        data = ep6_analysis[["desc ideal"]],
        aes(
            x = predicted,
            y = deviation
        )
    ) +
    geom_point(
        color = "blue"
    ) +
    xlab(
        "Expected Value"
    ) +
    ylab(
        "Deviation"
    )

ep6_report_fig[["%deviation ideal"]] <-
    ggplot(
        data = ep6_analysis[["desc ideal"]],
        aes(
            x = predicted,
            y = pct_deviation
        )
    ) +
    geom_point(
        color = "blue"
    ) +
    geom_hline(
        yintercept = ep6_import[["setting"]]$acceptance_criteria[1] * c(1, -1),
        color = "red",
        linetype = 2
    ) +
    xlab(
        "Expected Value"
    ) +
    ylab(
        "%Deviation"
    )

ep6_report_fig[["deviation nonideal"]] <-
    ggplot(
        data = ep6_analysis[["desc nonideal"]],
        aes(
            x = predicted,
            y = deviation
        )
    ) +
    geom_point(
        color = "blue"
    ) +
    xlab(
        "Expected Value"
    ) +
    ylab(
        "Deviation"
    )

ep6_report_fig[["%deviation nonideal"]] <-
    ggplot(
        data = ep6_analysis[["desc nonideal"]],
        aes(
            x = predicted,
            y = pct_deviation
        )
    ) +
    geom_point(
        color = "blue"
    ) +
    geom_hline(
        yintercept = ep6_import[["setting"]]$acceptance_criteria[1] * c(1, -1),
        color = "red",
        linetype = 2
    ) +
    xlab(
        "Expected Value"
    ) +
    ylab(
        "%Deviation"
    )

ep6_report_fig[["regression nonideal"]] <-
    rbind(
        data.frame(
            expected = ep6_analysis[["desc nonideal"]]$expected,
            y = ep6_analysis[["desc nonideal"]]$mean,
            type = "Measured Value"
        ),
        data.frame(
            expected = ep6_analysis[["desc nonideal"]]$expected,
            y = ep6_analysis[["desc nonideal"]]$predicted,
            type = "Predicted Value"
        )
    )

ep6_report_fig[["regression nonideal"]] <-
    ggplot(
        data = ep6_report_fig[["regression nonideal"]],
        aes(
            x = expected,
            y = y,
            color = type
        )
    ) +
    geom_point() +
    geom_line(
        data = ep6_report_fig[["regression nonideal"]][
            ep6_report_fig[["regression nonideal"]]$type == "Predicted Value",
        ]
    ) +
    xlab("Expected Value") +
    ylab(
        paste(
            "Measured Value (",
            ep6_import[["setting"]]$unit_of_device[1],
            ")",
            sep = ""
        )
    ) +
    labs(
        color = "Type"
    )

# * report_tab

ep6_report_tab[["raw"]] <- ep6_tidy[["combine"]] %>%
    datatable(
        options = list(scrollY = "375px"),
        colnames = c(
            "Sample",
            "RC",
            "Replicate",
            "Measured Value"
        )
    )


ep6_report_tab[["desc ideal"]] <-
    ep6_analysis[["desc ideal"]][
        , c(
            "sample",
            "rc",
            "replicate",
            "mean",
            "sd",
            "cv",
            "var",
            "expected",
            "weight",
            "predicted",
            "deviation",
            "pct_deviation"
        )
    ] %>%
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
    set_header_labels(
        sample = "Sample",
        rc = "RC",
        mean = "Measured Value (Mean)",
        sd = "SD",
        replicate = "Replicate",
        cv = "%CV",
        expected = "Expected Value",
        var = "Var",
        weight = "Weight",
        predicted = "Predicted Value",
        deviation = "Deviation",
        pct_deviation = "%Deviation"
    )

ep6_report_tab[["desc nonideal"]] <-
    ep6_analysis[["desc nonideal"]][
        , c(
            "sample",
            "rc",
            "replicate",
            "mean",
            "sd",
            "cv",
            "var",
            "expected",
            "weight",
            "predicted",
            "deviation",
            "pct_deviation"
        )
    ] %>%
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
    set_header_labels(
        sample = "Sample",
        rc = "RC",
        mean = "Measured Value (Mean)",
        sd = "SD",
        replicate = "Replicate",
        cv = "%CV",
        expected = "Expected Value",
        var = "Var",
        weight = "Weight",
        predicted = "Predicted Value",
        deviation = "Deviation",
        pct_deviation = "%Deviation"
    )