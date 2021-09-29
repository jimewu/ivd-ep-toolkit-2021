
# Load PKG
pkg_lst <- c(
    "knitr",
    "readODS",
    "dplyr",
    "flextable",
    "here"
)

lapply(
    pkg_lst,
    library,
    character.only = TRUE
)

source("lib_ft.R")

# # ! TMP
# setwd(
#     paste(
#         git = here(),
#         category = "A_醫療器材/醫材案件-縱向產品/Z-跨科/Z.IVD",
#         case = "CS1_Leadgene_Indoxyl_Sulfate_ELISA",
#         task = "OUT3.0_Preclinical_Study_Report/RMarkdown",
#         sep = "/"
#     )
# )

# # ! Import Data
# data.import.lot1 <- read_ods(
#     "data.ods",
#     sheet = "ep17.2021-0518.lot1"
# )

# data.import.lot2 <- read_ods(
#     "data.ods",
#     sheet = "ep17.2021-0518.lot2.0819"
# )

# data.import <- list(
#     Lot1 = data.import.lot1,
#     Lot2 = data.import.lot2
# )

# * Data Tidy
## data已經分組完成且無須產生欄位
data.tidy <- data.import

data.tidy.combine <- data.tidy[[1]]

for (
    x in seq(2, length(data.tidy))
) {
    data.tidy.combine <-
        rbind(
            data.tidy.combine,
            data.tidy[[x]]
        )
}

# * Data Analysis: Shapiro-Wilk Analysis
data.analyze <- lapply(
    data.tidy,
    function(x) {
        SPW <- shapiro.test(x$y)

        # 取出p-value
        p.value <- SPW[["p.value"]]
        return(p.value)
    }
)

## 以Shapiro-Wilk Test結果的p-value判斷是否為parametric
### 依照https://www.statology.org/shapiro-wilk-test-r/
### 當Shapiro-Wilk Test p-value < 0.05表示non-parametric
data.analyze.if.parametric <- lapply(
    data.analyze,
    function(x) {
        if.parametric <- ifelse(
            x < 0.05,
            FALSE,
            TRUE
        )
    }
)

### 全部都是parametric才當成parametric來處理，否則都以non-parametric處理後續
data.analyze.if.parametric.all <- data.analyze.if.parametric %>%
    unlist() %>%
    all()

data.analyze.lob <- lapply(
    seq(1, length(data.analyze.if.parametric)),
    function(x) {
        if (data.analyze.if.parametric.all) {
            # Parametric下LoB計算
            MB <- mean(data.tidy[[x]]$y)
            SDB <- sd(data.tidy[[x]]$y)
            CP <- 1.645 / (1 - (1 / (4 * (nrow(data.tidy.combine) - 5))))

            LOB <- MB + CP * SDB

            result <- data.frame(
                Lot = names(data.analyze.if.parametric)[x],
                MB, SDB, CP, LOB
            )

            return(result)
        } else {
            # Non-parametric下LoB計算
            Rank.Position <- 0.5 + 0.95 * length(data.tidy[[x]]$y)

            Rank.Position.upr <- ceiling(Rank.Position)
            Rank.Position.lwr <- floor(Rank.Position)

            y_arrange <- data.tidy[[x]]$y %>%
                sort()

            LOB <- mean(
                c(
                    y_arrange[Rank.Position.upr],
                    y_arrange[Rank.Position.lwr]
                )
            )

            result <- data.frame(
                Lot = names(data.analyze.if.parametric)[x],
                Rank.Position.Lwr = y_arrange[Rank.Position.lwr],
                Rank.Position.Upr = y_arrange[Rank.Position.upr],
                LOB
            )

            return(result)
        }
    }
)

# Report Compilation
## Reference: CLSI EP17 Table A5-6
data.report <- data.analyze.lob

data.report.combine <- data.report[[1]]

for (x in 2:length(data.report)) {
    data.report.combine <- rbind(
        data.report.combine,
        data.report[[x]]
    )
}

if (data.analyze.if.parametric.all) {
    data.report.combine$MB <-
        data.report.combine$MB %>%
        round(digits = 3)

    data.report.combine$SDB <-
        data.report.combine$SDB %>%
        round(digits = 3)

    data.report.combine$CP <-
        data.report.combine$CP %>%
        round(digits = 3)

    data.report.combine$LOB <-
        data.report.combine$LOB %>%
        round(digits = 3)
} else {
    data.report.combine$Rank.Position.Lwr <-
        data.report.combine$Rank.Position.Lwr %>%
        round(digits = 3)

    data.report.combine$Rank.Position.Upr <-
        data.report.combine$Rank.Position.Upr %>%
        round(digits = 3)

    data.report.combine$LOB <-
        data.report.combine$LOB %>%
        round(digits = 3)
}

### 取較大者為最終LOB
data.report.lob.final <- max(data.report.combine$LOB)

### 欄位名稱調整
if (data.analyze.if.parametric.all) {
    data.report.ft <- data.report.combine %>%
        flextable() %>%
        set_header_labels(
            LOB = "LoB"
        ) %>%
        ### 向中間對齊
        align(
            align = "center",
            part = "all"
        ) %>%
        # parametric相關欄位
        ## MB欄位將B下標
        compose(
            j = 2, part = "header",
            value = as_paragraph("M", as_sub("B"))
        ) %>%
        ## SDB欄位將B下標
        compose(
            j = 3, part = "header",
            value = as_paragraph("SD", as_sub("B"))
        ) %>%
        ## CP欄位將P下標
        compose(
            j = 4, part = "header",
            value = as_paragraph("C", as_sub("P"))
        ) %>%
        add_footer_lines(
            values = paste(
                "unit:",
                params$dev_unit3,
                sep = " "
            )
        )
} else {
    data.report.ft <- data.report.combine %>%
        flextable() %>%
        set_header_labels(
            Rank.Position.Lwr = "Lwr.",
            Rank.Position.Upr = "Upr.",
            LOB = "LoB"
        ) %>%
        add_header_row(
            values = c(
                "Lot",
                "Rank Position",
                "LoB"
            ),
            colwidths = c(
                1, 2, 1
            )
        ) %>%
        merge_v(
            part = "header"
        ) %>%
        theme_ms() %>%
        ### 向中間對齊
        align(
            align = "center",
            part = "header"
        ) %>%
        align(
            align = "center",
            part = "body"
        ) %>%
        add_footer_lines(
            values = paste(
                "unit of values:",
                params$dev_unit3,
                sep = " "
            )
        ) %>%
        align(
            align = "right",
            part = "footer"
        )
}