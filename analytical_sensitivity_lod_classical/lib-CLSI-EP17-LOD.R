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

# # ! TMP
# data.report.lob.final <- 0.34

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
# data.import <- read_ods(
#     "data.ods",
#     sheet = "ep17.2021-0518.lod"
# )

# Data Tidy
## 第一層: 先依照Lot分組，第二層" 再依照Sample分組
data.tidy <- data.import %>%
    mutate(
        tmp.split1 = paste(
            "Lot", Lot,
            sep = "."
        )
    ) %>%
    mutate(
        tmp.split2 = paste(
            "Sample", Sample,
            sep = "."
        )
    )

data.tidy.split <- data.tidy %>%
    split(data.tidy$tmp.split1)

data.tidy.split <- lapply(
    data.tidy.split,
    function(x) {
        result <- split(x, x$tmp.split2)
    }
)

data.tidy.split1 <- data.tidy.split[["Lot.1"]]
data.tidy.split2 <- data.tidy.split[["Lot.2"]]

### 拿掉用來分組的暫存欄位tmp.split
data.tidy$tmp.split1 <- NULL
data.tidy$tmp.split2 <- NULL

data.tidy.split1 <- lapply(
    data.tidy.split1,
    function(x) {
        x$tmp.split1 <- NULL
        x$tmp.split2 <- NULL

        return(x)
    }
)

data.tidy.split2 <- lapply(
    data.tidy.split2,
    function(x) {
        x$tmp.split1 <- NULL
        x$tmp.split2 <- NULL

        return(x)
    }
)

data.tidy.split <- NULL

# Data Analysis: LOD
## Lot1
### 取出樣品名稱
data.analyze1.Sample <- sapply(
    data.tidy.split1,
    function(x) {
        Sample <- x$Sample[1]
    }
)

### 取出各濃度結果筆數
data.analyze1.NUM <- sapply(
    data.tidy.split1,
    function(x) {
        NUM <- nrow(x)
    }
)

### 取出各樣品SD
data.analyze1.SD <- sapply(
    data.tidy.split1,
    function(x) {
        SD <- sd(x$y)
    }
)

### 組成報告
data.analyze1 <- data.frame(
    Sample = data.analyze1.Sample,
    Lot1.NUM = data.analyze1.NUM,
    Lot1.SD = data.analyze1.SD
)

### 計算SDL

data.analyze1 <- data.analyze1 %>%
    mutate(
        tmp.down = Lot1.NUM - 1,
        tmp.up = tmp.down * Lot1.SD^2
    )

data.analyze1.sdl <-
    (sum(data.analyze1$tmp.up) / sum(data.analyze1$tmp.down))^0.5

#### 計算SDL完成，拿掉不需要的暫存欄位
data.analyze1$tmp.down <- NULL
data.analyze1$tmp.up <- NULL


### 計算L, J, CP
#### L: 總資料筆數
Lot1.L <- nrow(data.import)
#### J: Lot內樣本數量
Lot1.J <- nrow(data.analyze1)

data.analyze1.cp <-
    1.645 / (1 - (1 / (4 * (Lot1.L - Lot1.J))))

data.analyze1.lod <-
    data.report.lob.final + data.analyze1.cp * data.analyze1.sdl

## Lot2
### 取出樣品名稱
data.analyze2.Sample <- sapply(data.tidy.split2, function(x) {
    Sample <- x$Sample[1]
})

### 取出各濃度結果筆數
data.analyze2.NUM <- sapply(data.tidy.split2, function(x) {
    NUM <- nrow(x)
})

### 取出各樣品SD
data.analyze2.SD <- sapply(data.tidy.split2, function(x) {
    SD <- sd(x$y)
})

### 組成報告
data.analyze2 <- data.frame(
    Sample = data.analyze2.Sample,
    Lot2.NUM = data.analyze2.NUM,
    Lot2.SD = data.analyze2.SD
)

### 計算SDL
data.analyze2 <- data.analyze2 %>%
    mutate(
        tmp.down = Lot2.NUM - 1,
        tmp.up = tmp.down * Lot2.SD^2
    )

data.analyze2.sdl <-
    (sum(data.analyze2$tmp.up) / sum(data.analyze2$tmp.down))^0.5

#### 計算SDL完成，拿掉不需要的暫存欄位
data.analyze2$tmp.down <- NULL
data.analyze2$tmp.up <- NULL


### 計算L, J, CP
#### L: 總資料筆數
Lot2.L <- nrow(data.import)
Lot2.J <- nrow(data.analyze2)

data.analyze2.cp <-
    1.645 / (1 - (1 / (4 * (Lot2.L - Lot2.J))))

data.analyze2.lod <-
    data.report.lob.final + data.analyze2.cp * data.analyze2.sdl

# Report Compilation
## Reference: CLSI EP17 page 54
### data.report1: 依照CLSI EP17 page 54的上半部(各樣品)
data.report1 <- merge(
    data.analyze1,
    data.analyze2
) %>%
    round(digits = 3)

data.report1.ft <-
    flextable(data.report1) %>%
    set_header_labels(
        Sample = "Conc. Level",
        Lot1.NUM = "n",
        Lot1.SD = "SD",
        Lot2.NUM = "n",
        Lot2.SD = "SD"
    ) %>%
    add_header_row(
        values = c(
            "Sample",
            "Device Lot 1",
            "Device Lot 2"
        ),
        colwidths = c(1, 2, 2)
    ) %>%
    theme_ms() %>%
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
            "unit of concentration levels and SD:",
            params$dev_unit3,
            sep = " "
        )
    ) %>%
    align(
        align = "right",
        part = "footer"
    )

### data.report2: 依照CLSI EP17 page 54的下半部
data.report2.lot1 <- data.frame(
    Lot = 1,
    SDL = data.analyze1.sdl,
    CP = data.analyze1.cp,
    LOD = data.analyze1.lod
)

data.report2.lot2 <- data.frame(
    Lot = 2,
    SDL = data.analyze2.sdl,
    CP = data.analyze2.cp,
    LOD = data.analyze2.lod
)

data.report2 <- rbind(
    data.report2.lot1,
    data.report2.lot2
) %>%
    round(digits = 3)

data.report2.ft <-
    flextable(data.report2) %>%
    set_header_labels(
        Lot = "Device Lot",
        LOD = "LoD"
    ) %>%
    compose(
        part = "header",
        j = "SDL",
        value = as_paragraph(
            "SD",
            as_sub("L")
        )
    ) %>%
    compose(
        part = "header",
        j = "CP",
        value = as_paragraph(
            "C",
            as_sub("P")
        )
    ) %>%
    theme_ms() %>%
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
            "unit of LoD:",
            params$dev_unit3,
            sep = " "
        )
    ) %>%
    align(
        align = "right",
        part = "footer"
    )

data.report3.lod <- max(
    data.analyze1.lod,
    data.analyze2.lod
)