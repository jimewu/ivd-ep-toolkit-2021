# * analysis

## Rosner's Test for Outliers

ep9_analysis[["rosner"]] <- lapply(
    list(
        diff = "diff",
        pctdiff = "pctdiff"
    ),
    function(x) {
        result <- rosnerTest(
            ep9_tidy[["combine"]][, x],
            k = floor(
                0.05 * length(ep9_tidy[["combine"]]$y_test)
            )
        )$all.stats

        result <- result[
            , colnames(result) != "Obs.Num"
        ]

        return(result)
    }
)

## regression analysis for method comparison

ep9_analysis[["regression"]] <- lapply(
    list(
        LinReg = data.frame(
            method.reg = "LinReg",
            method.ci = "bootstrap"
        ),
        Deming = data.frame(
            method.reg = "Deming",
            method.ci = "bootstrap"
        ),
        WLinReg = data.frame(
            method.reg = "WLinReg",
            method.ci = "bootstrap"
        ),
        WDeming = data.frame(
            method.reg = "WDeming",
            method.ci = "bootstrap"
        ),
        PaBa = data.frame(
            method.reg = "PaBa",
            method.ci = "bootstrap"
        )
    ),
    function(x) {
        result <- vector(mode = "list")

        result[["mcreg"]] <- mcreg(
            x = ep9_tidy[["combine"]]$y_ref,
            y = ep9_tidy[["combine"]]$y_test,
            mref.name = ep9_import[["setting"]]$name_of_ref,
            mtest.name = ep9_import[["setting"]]$name_of_test,
            error.ratio = ep9_import[["setting"]]$repeatability /
                ep9_import[["setting"]]$repeatability_ref,
            alpha = 0.05,
            method.reg = x$method.reg,
            method.ci = x$method.ci
        )

        result[["coef"]] <- result[["mcreg"]] %>%
            MCResult.getCoefficients() %>%
            as.data.frame() %>%
            cbind(
                .,
                ideal = c(0, 1)
            ) %>%
            mutate(
                is.ideal = ifelse(
                    (LCI - ideal) * (UCI - ideal) < 0,
                    TRUE,
                    FALSE
                ),
                ci.range = UCI - LCI
            )

        return(result)
    }
)

# * report_critical

## Candidate Outliers based on Diff

ep9_report_crit[["outlier"]] <- lapply(
    ep9_analysis[["rosner"]],
    function(x) {
        result <- x[
            x$Outlier == TRUE,
        ]$Value

        # 如果沒有outlier則回傳NA
        if (
            length(result) == 0
        ) {
            return(NA)
        } else {
            return(result)
        }
    }
)

# * report_tab

ep9_report_tab[["rosner"]] <- lapply(
    ep9_analysis[["rosner"]],
    function(x) {
        # 對數值欄取到小數第三位
        for (
            i in seq(
                1, ncol(x)
            )
        ) {
            if (class(x[, i]) == "numeric") {
                x[, i] <- round(
                    x[, i],
                    digits = 3
                )
            }
        }

        return(x)
    }
)

## DT版本

ep9_report_tab[["rosner DT"]] <- lapply(
    ep9_report_tab[["rosner"]],
    function(x) {
        result <- datatable(
            x,
            rownames = FALSE
        )

        return(result)
    }
)

# * report_fig

## 原始結果圖

ep9_report_fig[["raw"]] <- ggplot(
    ep9_tidy[["combine"]],
    aes(
        x = y_ref,
        y = y_test
    )
) +
    geom_point() +
    xlab(ep9_import[["setting"]]$name_of_ref) +
    ylab(ep9_import[["setting"]]$name_of_test)

## difference plot

ep9_report_fig[["difference"]] <- ggplot(
    rbind(
        diff = data.frame(
            type_x = "Measurement Value",
            x = ep9_tidy[["combine"]]$y_ref,
            y = ep9_tidy[["combine"]]$diff
        ),
        odr_diff = data.frame(
            type_x = "Ordered Measurement",
            x = ep9_tidy[["combine"]]$order,
            y = ep9_tidy[["combine"]]$diff
        )
    ),
    aes(
        x = x,
        y = y
    )
) +
    geom_point() +
    geom_hline(
        yintercept = 0,
        alpha = 0.5
    ) +
    geom_hline(
        yintercept = sd(ep9_tidy[["combine"]]$diff) * c(
            -3, -2, -1, 1, 2, 3
        ),
        alpha = 0.5,
        linetype = 2
    ) +
    xlab(ep9_import[["setting"]]$name_of_ref) +
    ylab(paste(
        "Difference (",
        ep9_import[["setting"]]$unit_of_device,
        ")",
        sep = ""
    )) +
    facet_wrap(
        ~type_x,
        scales = "free"
    )

## %difference plot

ep9_report_fig[["%difference"]] <- ggplot(
    rbind(
        pctdiff = data.frame(
            type_x = "Measurement Value",
            x = ep9_tidy[["combine"]]$y_ref,
            y = ep9_tidy[["combine"]]$pctdiff
        ),
        odr_pctdiff = data.frame(
            type_x = "Ordered Measurement",
            x = ep9_tidy[["combine"]]$order,
            y = ep9_tidy[["combine"]]$pctdiff
        )
    ),
    aes(
        x = x,
        y = y
    )
) +
    geom_point() +
    geom_hline(
        yintercept = 0,
        alpha = 0.5
    ) +
    geom_hline(
        yintercept = sd(ep9_tidy[["combine"]]$pctdiff) * c(
            -3, -2, -1, 1, 2, 3
        ),
        alpha = 0.5,
        linetype = 2
    ) +
    xlab(ep9_import[["setting"]]$name_of_ref) +
    ylab("Difference (%)") +
    geom_hline(
        yintercept = 0,
        alpha = 0.5
    ) +
    facet_wrap(
        ~type_x,
        scales = "free"
    )

# compareFit

ep9_report_fig[["comparefit fun"]] <-
    function(x = ep9_analysis[["regression"]]) {
        compareFit(
            x[["PaBa"]][["mcreg"]],
            x[["WDeming"]][["mcreg"]],
            x[["WLinReg"]][["mcreg"]],
            x[["Deming"]][["mcreg"]],
            x[["LinReg"]][["mcreg"]]
        )
    }