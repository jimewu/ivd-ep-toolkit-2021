# * analysis

## 計算在不假設candidate干擾物方向(不知為正干擾或負干擾)時所需的replicate

ep7_analysis[["replicate 2side"]] <- 2 * (
    (
        qnorm(1 - ep7_import[["setting"]]$alpha / 2) +
            qnorm(1 - ep7_import[["setting"]]$beta)
    ) *
        ep7_import[["setting"]]$repeatability /
        ep7_import[["setting"]]$allowable_interference
)^2

## 計算在candidate干擾物方向已知時所需的replicate
ep7_analysis[["replicate 1side"]] <- 2 * (
    (
        qnorm(1 - ep7_import[["setting"]]$alpha) +
            qnorm(1 - ep7_import[["setting"]]$beta)
    ) *
        ep7_import[["setting"]]$repeatability /
        ep7_import[["setting"]]$allowable_interference
)^2

# * report_critical

## replicate數值無條件進位為整數，且若計算值低於5則以5取代(CLSI EP07Ed3E要求)

ep7_report_crit[["replicate 2side"]] <- ifelse(
    ep7_analysis[["replicate 2side"]] >= 5,
    ceiling(ep7_analysis[["replicate 2side"]]),
    5
)

## replicate數值無條件進位為整數，且若計算值低於5則以5取代(CLSI EP07Ed3E要求)

ep7_report_crit[["replicate 1side"]] <- ifelse(
    ep7_analysis[["replicate 1side"]] >= 5,
    ceiling(ep7_analysis[["replicate 1side"]]),
    5
)

ep7_report_tab[["replicate"]] <- data.frame(
    variable = c(
        "Alpha",
        "Beta",
        "Repeatability (EP5) (%CV)",
        "Allowable Interference (%)",
        "Replicate (One-sided)",
        "Replicate (Two-sided)"
    ),
    number = c(
        ep7_import[["setting"]]$alpha,
        ep7_import[["setting"]]$beta,
        ep7_import[["setting"]]$repeatability * 100,
        ep7_import[["setting"]]$allowable_interference * 100,
        ep7_report_crit[["replicate 1side"]],
        ep7_report_crit[["replicate 2side"]]
    )
) %>%
    flextable() %>%
    set_header_labels(
        variable = "變數",
        number = "數值"
    ) %>%
    align(
        align = "center",
        part = "header"
    ) %>%
    align(
        align = "center",
        part = "body"
    )