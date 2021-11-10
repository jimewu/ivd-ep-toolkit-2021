# * analysis
ep17lod_analysis[["regression"]] <- lapply(
    ep17lod_tidy[["split"]],
    function(x) {
        rgs1 <- lm(
            y ~ poly(log_conc, 1),
            x
        )

        rgs2 <- lm(
            y ~ poly(log_conc, 2),
            x
        )

        rgs3 <- lm(
            y ~ poly(log_conc, 3),
            x
        )

        sum1 <- summary(rgs1)
        sum2 <- summary(rgs2)
        sum3 <- summary(rgs3)

        coef1 <- sum1$coefficients
        coef2 <- sum2$coefficients
        coef3 <- sum3$coefficients

        if (
            coef3[nrow(coef3), ncol(coef3)] < 0.05
        ) {
            rgs_best <- rgs3
            sum_best <- sum3
            formula_best <- as.formula(
                y ~ poly(x, 3)
            )
        } else if (
            coef2[nrow(coef2), ncol(coef2)] < 0.05
        ) {
            rgs_best <- rgs2
            sum_best <- sum2
            formula_best <- as.formula(
                y ~ poly(x, 2)
            )
        } else {
            rgs_best <- rgs1
            sum_best <- sum1
            formula_best <- as.formula(
                y ~ poly(x, 1)
            )
        }

        predict_y <- predict(
            rgs_best,
            newdata = data.frame(
                log_conc = seq(
                    min(x$log_conc),
                    max(x$log_conc),
                    by = 0.01
                )
            )
        )

        predict <- data.frame(
            log_conc = seq(
                min(x$log_conc),
                max(x$log_conc),
                by = 0.01
            ),
            y = predict_y
        ) %>%
            mutate(
                concentration = 10^log_conc,
                reagent_lot = x$reagent_lot[1]
            )

        lod <- predict %>%
            filter(
                y >= 1 - ep17lod_import[["setting"]]$beta
            )

        result <- list(
            rgs = rgs_best,
            sum = sum_best,
            formula = formula_best,
            predict = predict,
            lod = lod[1, ]
        )

        return(result)
    }
)

ep17lod_analysis[["predict combine"]] <-
    ep17lod_analysis[["regression"]][[1]][["predict"]]

for (
    x in 2:length(ep17lod_analysis[["regression"]])
) {
    ep17lod_analysis[["predict combine"]] <- rbind(
        ep17lod_analysis[["predict combine"]],
        ep17lod_analysis[["regression"]][[x]][["predict"]]
    )
}


# * report_fig

## raw data (color = reagent lot)

ep17lod_report_fig[["raw"]] <- ggplot(
    ep17lod_tidy[["combine"]],
    aes(
        x = concentration,
        y = y,
        color = factor(reagent_lot)
    )
) +
    geom_point() +
    geom_line(
        data = ep17lod_analysis[["predict combine"]],
        aes(group = factor(reagent_lot))
    ) +
    scale_x_continuous(trans = "log10") +
    xlab("log10 [Concentration]") +
    ylab("Probit") +
    labs(color = "Reagent Lot")


# * report_tab
## Raw Data
ep17lod_report_tab[["raw"]] <- datatable(
    round(
        ep17lod_tidy[["combine"]][
            , c(
                "reagent_lot",
                "concentration",
                "log_conc",
                "y"
            )
        ],
        digits = 3
    ),
    options = list(scrollY = "375px"),
    colnames = c(
        "Reagent Lot",
        "Concentration",
        "Log10 [Concentration]",
        "Measured Value"
    )
)


## 各樣品 x 各濃度的parametric descriptive statistics

ep17lod_report_tab[["lod"]] <- ep17lod_analysis[["regression"]][[1]][["lod"]]

for (
    x in 2:length(ep17lod_analysis[["regression"]])
) {
    ep17lod_report_tab[["lod"]] <- rbind(
        ep17lod_report_tab[["lod"]],
        ep17lod_analysis[["regression"]][[x]][["lod"]]
    )
}

ep17lod_report_tab[["lod"]] <- ep17lod_report_tab[["lod"]] %>%
    select(
        reagent_lot,
        concentration
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
        concentration = "Concentration"
    )