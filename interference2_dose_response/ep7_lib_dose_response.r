# * analysis

## paired difference分析

### 依照CLSI EP7Ed3E Table 3
### 對比y_control VS y_test

ep7_analysis[["dose response"]] <-
    ep7_tidy[["combine"]] %>%
    arrange(interferent_level) %>%
    group_by(
        interferent_level,
    ) %>%
    summarise(
        mean = mean(y),
        sd = sd(y),
        cv = 100 * sd / mean
    ) %>%
    mutate(
        diff = .$mean[1],
        diff = mean - diff,
        pct_diff = 100 * diff / .$mean[1]
    ) %>%
    ungroup()

ep7_analysis[["below"]] <- ep7_analysis[["dose response"]] %>%
    filter(
        pct_diff <= 100 * ep7_import[["setting"]]$allowable_interference[1]
    )


ep7_analysis[["above"]] <- ep7_analysis[["dose response"]] %>%
    filter(
        pct_diff >= 100 * ep7_import[["setting"]]$allowable_interference[1]
    )

ep7_analysis[["neighbor"]] <- rbind(
    ep7_analysis[["below"]][
        nrow(ep7_analysis[["below"]]),
    ],
    ep7_analysis[["above"]][1, ]
)

ep7_analysis[["regression"]] <- lm(
    interferent_level ~ pct_diff,
    data = ep7_analysis[["neighbor"]]
)

ep7_analysis[["pass"]] <- predict(
    ep7_analysis[["regression"]],
    newdata = data.frame(
        pct_diff = 100 * ep7_import[["setting"]]$allowable_interference[1]
    )
)

# * report_tab

## raw

ep7_report_tab[["raw"]] <- ep7_tidy[["combine"]] %>%
    datatable(
        options = list(scrollY = "375px"),
        colnames = c(
            "Interferent Added",
            "Measured Values"
        )
    )

## Difference表格

ep7_report_tab[["dose response"]] <-
    ep7_analysis[["dose response"]] %>%
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
        interferent_level = "Interferent Added",
        mean = "Mean",
        sd = "SD",
        cv = "%CV",
        diff = "Difference",
        pct_diff = "%Difference"
    ) %>%
    add_footer_lines(
        values = paste(
            "unit of interferent: ",
            ep7_import[["setting"]]$unit_of_interferent[1],
            sep = ""
        )
    ) %>%
    add_footer_lines(
        values = paste(
            "unit of measured values: ",
            ep7_import[["setting"]]$unit_of_device[1],
            sep = ""
        )
    ) %>%
    align(
        align = "right",
        part = "footer"
    )

# * report_fig

ep7_report_fig[["raw"]] <-
    ggplot(
        ep7_tidy[["combine"]],
        aes(
            x = interferent_level,
            y = y
        )
    ) +
    geom_point(
        color = "blue"
    ) +
    xlab(
        paste(
            "Interferent Added, ",
            ep7_import[["setting"]]$unit_of_interferent[1],
            sep = ""
        )
    ) +
    ylab(
        paste(
            "Measured values, ",
            ep7_import[["setting"]]$unit_of_device[1],
            sep = ""
        )
    )


ep7_report_fig[["difference"]] <- ggplot(
    ep7_analysis[["dose response"]],
    aes(
        x = interferent_level,
        y = diff
    )
) +
    geom_point(
        color = "blue"
    ) +
    geom_line(
        alpha = 0.5
    ) +
    xlab(
        paste(
            "Interferent Added, ",
            ep7_import[["setting"]]$unit_of_interferent[1],
            sep = ""
        )
    ) +
    ylab(
        paste(
            "Difference, ",
            ep7_import[["setting"]]$unit_of_device[1],
            sep = ""
        )
    )

ep7_report_fig[["%difference"]] <- ggplot(
    ep7_analysis[["dose response"]],
    aes(
        x = interferent_level,
        y = pct_diff
    )
) +
    geom_point(
        color = "blue"
    ) +
    geom_line(
        alpha = 0.5
    ) +
    geom_hline(
        yintercept = 100 * ep7_import[["setting"]]$allowable_interference,
        color = "red",
        linetype = 2
    ) +
    xlab(
        paste(
            "Interferent Added, ",
            ep7_import[["setting"]]$unit_of_interferent[1],
            sep = ""
        )
    ) +
    ylab("% Difference")