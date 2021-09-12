
# Load PKG
pkg_lst <- c(
    "knitr",
    "readODS",
    "dplyr",
    "flextable",
    "ggplot2",
    "here"
)

lapply(
    pkg_lst,
    library,
    character.only = TRUE
)

# ! TMP
setwd(
    paste(
        git = here(),
        category = "stability",
        sep = "/"
    )
)

# ! Import Data
data_ep25_import <- read_ods(
    "data.ods",
    sheet = "ep25.2021-0621.shelf"
)

# * Data Tidy
## 選取需要的欄位
data.tidy <- data.import %>%
    select(
        Day,
        Conc.Level,
        Dev.Lot,
        y
    )

## 依照樣品Conc.Level & 器材Dev.Lot分組
data.tidy <- data.tidy %>%
    mutate(
        tmp.split = paste(
            Conc.Level,
            Dev.Lot
        )
    )

data.tidy <- data.tidy %>%
    split(data.tidy$tmp.split)

### 移除tmp.split欄位
data.tidy <- lapply(
    data.tidy,
    function(x) {
        x$tmp.split <- NULL
        return(x)
    }
)

## 移除outlier (依照前一輪QC結果)
### 先保留一份沒有移除outlier的data.tidy，作為後續產生report時使用
data.tidy.pre.out <- data.tidy

## 移除outlier: Sample 3 (Lot 1)-Device Lot 2中Day 450
data.tidy[["H (Lot 1) 2"]] <-
    data.tidy[["H (Lot 1) 2"]] %>%
    filter(
        !(
            Conc.Level == "H (Lot 1)" &
                Dev.Lot == 2 &
                Day == 450
        )
    )

## 以圖進行QC
### 先把資料全部合併方便之後畫出QC圖
data.tidy.qc <- lapply(
    data.tidy,
    function(x) {
        limit.upr <- x$y[1] * (1 + params$pass_ep25)
        limit.lwr <- x$y[1] * (1 - params$pass_ep25)

        result <- data.frame(
            x,
            limit.upr,
            limit.lwr
        )

        return(result)
    }
)

data.tidy.qc.combine <- data.tidy.qc[[1]]

for (x in 2:length(data.tidy.qc)) {
    data.tidy.qc.combine <- rbind(
        data.tidy.qc.combine,
        data.tidy.qc[[x]]
    )
}

### 增加可讀label for 畫圖
data.tidy.qc.combine <- data.tidy.qc.combine %>%
    mutate(
        Label.Conc.Level = paste("Sample", Conc.Level),
        Label.Dev.Lot = paste("Device Lot", Dev.Lot)
    )

### 畫出QC圖
data.tidy.qc.plot <- ggplot(
    data.tidy.qc.combine,
    aes(x = Day, y = y)
) +
    # 資料以點呈現
    geom_point() +
    # 允收上下限
    geom_line(aes(y = limit.upr),
        linetype = 2,
        color = "#00AFBB"
    ) +
    geom_line(aes(y = limit.lwr),
        linetype = 2,
        color = "#00AFBB"
    ) +
    # 以樣品分列，以器材Lot分欄
    facet_grid(Label.Conc.Level ~ Label.Dev.Lot,
        scales = "free"
    )

# Data Analysis: 1st Regression
data.analyze.rgs <- lapply(data.tidy, function(x) {
    lm(y ~ Day, x)
})

## 對Regression做Summary
data.analyze.sum <- lapply(data.analyze.rgs, summary)

### 從Summary取出coefficients
data.analyze.sum.coef <- lapply(data.analyze.sum, function(x) {
    result <- x$coefficients
    result <- as.data.frame(result)
    return(result)
})

#### 從coefficients取出p-value
data.analyze.sum.p.value <- lapply(data.analyze.sum.coef, function(x) {
    p.value <- x[nrow(x), ncol(x)]
    return(p.value)
})

##### 把p-value以是否significant (p<0.05)表示(TRUE/FALSE)
data.analyze.sum.if.sig <- lapply(data.analyze.sum.p.value, function(x) {
    if.sig <- ifelse(x < 0.05, TRUE, FALSE)
    return(if.sig)
})

## 用1st regression結果，對data.tidy中的天數範圍進行逐日推算
data.analyze.predict <- lapply(names(data.tidy), function(x) {
    day.max <- data.tidy[[x]]$Day %>% max()
    day.range <- data.frame(Day = c(0:day.max))

    PRDT <- predict.lm(data.analyze.rgs[[x]],
        newdata = day.range,
        interval = "confidence", level = 0.95
    )
    PDRT <- data.frame(PRDT)

    result <- data.frame(
        Conc.Level = data.tidy[[x]]$Conc.Level[1],
        Dev.Lot = data.tidy[[x]]$Dev.Lot[1],
        Day = c(0:day.max),
        PRDT
    )
    return(result)
})

### 把data.analyze.predict重新命名成和data.tidy一致for後續使用
data.analyze.predict.name <-
    sapply(data.analyze.predict, function(x) {
        name <- paste(x$Conc.Level[1], x$Dev.Lot[1], sep = " ")

        return(name)
    })

names(data.analyze.predict) <- data.analyze.predict.name

## 依照params$pass_ep25，再從data.analyze.sum.coef下面的斜率estimate區分方向，
## 從data.analyze.predict中找出允許的最大天數
data.analyze.maxday <- lapply(names(data.analyze.predict), function(x) {
    # 如果regression斜率<0則perc.limit為負
    perc.limit <- ifelse(data.analyze.sum.coef[[x]]$Estimate[2] < 0,
        params$pass_ep25 * -1,
        params$pass_ep25
    )


    y0 <- data.tidy[[x]] %>% filter(Day == 0)
    y0 <- y0$y

    limit <- y0 * (1 + perc.limit)

    if (perc.limit < 0) {
        tb.pass <- data.analyze.predict[[x]] %>% filter(lwr > limit)
    } else {
        tb.pass <- data.analyze.predict[[x]] %>% filter(upr < limit)
    }

    day.pass <- tb.pass$Day %>% max()

    result <- data.frame(
        Conc.Level = data.tidy[[x]]$Conc.Level[1],
        Dev.Lot = data.tidy[[x]]$Dev.Lot[1],
        p.value = data.analyze.sum.p.value[[x]],
        if.sig = data.analyze.sum.if.sig[[x]],
        day.pass
    )

    return(result)
})

### 把data.analyze.maxday重新命名成和data.tidy一致for後續使用
data.analyze.maxday.name <-
    sapply(data.analyze.maxday, function(x) {
        name <- paste(x$Conc.Level[1], x$Dev.Lot[1], sep = " ")

        return(name)
    })

names(data.analyze.maxday) <- data.analyze.maxday.name


# Data: Report
## 依照CLSI EP25 Appendix A表格進行整理
data.report1 <- data.frame(y1 = data.tidy.pre.out[[1]]$y)

for (x in 2:length(data.tidy)) {
    data.report1 <- cbind(
        data.report1,
        data.tidy.pre.out[[x]]$y
    )
}

## for report，取3位小數呈現
data.report1 <- data.report1 %>% round(digits = 3)

data.report1 <- t(data.report1)

### 從data.tidy.pre.out取出各樣品Conc.Level
Conc.Level <- sapply(data.tidy.pre.out, function(x) {
    x$Conc.Level[1]
})

### 從data.tidy.pre.out取出各樣品Dev.Lot
Dev.Lot <- sapply(data.tidy.pre.out, function(x) {
    x$Dev.Lot[1]
})

data.report1 <- data.frame(
    Conc.Level,
    Dev.Lot,
    data.report1
)

## 產生flextable
### 標頭與欄位名稱
HEADER <- c("Sample", "Device", "Day")
CNAME <- colnames(data.report1)
CNAME <- c(CNAME[1], "Lot", data.tidy[[1]]$Day)
colnames(data.report1) <- CNAME

data.report1.ft <-
    flextable(data.report1)

data.report1.ft <-
    data.report1.ft %>%
    merge_v(
        part = "body",
        j = "Conc.Level"
    ) %>%
    add_header_row(
        values = HEADER,
        colwidths = c(1, 1, length(data.tidy[[1]]$Day))
    ) %>%
    align(align = "center", part = "all")



### 基於Conc.Level與Dev.Lot，產生data.report2以及tmp.name (用於後續取出數據)
data.report2 <- data.frame(Conc.Level, Dev.Lot)
data.report2 <- data.report2 %>% mutate(tmp.name = paste(Conc.Level, Dev.Lot))

#### 從data.analyze.sum.coef取出Slope
Slope <- sapply(data.report2$tmp.name, function(x) {
    result <- data.analyze.sum.coef[[x]]$Estimate[2] %>%
        round(digits = 3)
})

#### 從data.analyze.sum.coef取出Intercept
Intercept <- sapply(data.report2$tmp.name, function(x) {
    result <- data.analyze.sum.coef[[x]]$Estimate[1] %>%
        round(digits = 3)
})

#### 從data.analyze.sum.coef取出Slope SE
Slope.SE <- sapply(data.report2$tmp.name, function(x) {
    result <- data.analyze.sum.coef[[x]]$"Std. Error"[2] %>%
        round(digits = 3)
})

#### 從data.analyze.sum.coef取出Slope t
Slope.t <- sapply(data.report2$tmp.name, function(x) {
    result <- data.analyze.sum.coef[[x]]$"t value"[2] %>%
        round(digits = 3)
})

#### 從data.analyze.sum.coef取出p-value
p.value <- sapply(data.report2$tmp.name, function(x) {
    result <- data.analyze.sum.coef[[x]][2, 4] %>%
        round(digits = 3)
})

#### 從data.analyze.maxday取出day.pass
day.pass <- sapply(data.report2$tmp.name, function(x) {
    if (data.analyze.maxday[[x]]$p.value < 0.05) {
        result <- data.analyze.maxday[[x]]$day.pass
    } else {
        result <- "n.s."
    }

    return(result)
})

#### 拿掉tmp.name
data.report2$tmp.name <- NULL

data.report2 <- cbind(
    data.report2,
    Slope,
    Intercept,
    Slope.SE,
    Slope.t,
    p.value,
    day.pass
)

data.report2.stability <- min(data.report2$day.pass)

## 產生flextable
### 標頭與欄位名稱
HEADER <- c("Sample", "Device", "Regression Results")

data.report2.ft <- flextable(data.report2)
data.report2.ft <- data.report2.ft %>%
    merge_v(j = 1) %>%
    set_header_labels(
        Conc.Level = "Conc. Level",
        Dev.Lot = "Lot",
        Slope.SE = "Slope SE",
        Slope.t = "Slope t",
        p.value = "p-value",
        day.pass = "Stability"
    ) %>%
    add_header_row(
        values = HEADER,
        colwidths = c(1, 1, length(colnames(data.report2)) - 2)
    ) %>%
    align(align = "center", part = "all")

## 從data.analyze.sum.if.sig決定是否拿掉(若為FALSE)
## data.tidy中的同名項目到data.report3

data.report3 <- data.tidy

### 拿掉regression不顯著的資料
for (x in names(data.analyze.sum.if.sig)) {
    if (data.analyze.sum.if.sig[[x]] == FALSE) {
        data.report3[[x]] <- NULL
    }
}

### 畫出允收上下限
data.report3 <- lapply(data.report3, function(x) {
    limit.upr <- x$y[1] * (1 + params$pass_ep25)
    limit.lwr <- x$y[1] * (1 - params$pass_ep25)

    result <- data.frame(
        x,
        limit.upr,
        limit.lwr
    )

    return(result)
})

### 表格合併
data.report3.combine <- data.report3[[1]]

for (x in 2:length(data.report3)) {
    data.report3.combine <- rbind(
        data.report3.combine,
        data.report3[[x]]
    )
}

### 增加可讀label for 畫圖
data.report3.combine <- data.report3.combine %>%
    mutate(
        Label.Conc.Level = paste("Sample", Conc.Level),
        Label.Dev.Lot = paste("Device Lot", Dev.Lot)
    )

### 畫圖
data.report3.plot <- ggplot(
    data.report3.combine,
    aes(x = Day, y = y)
) +
    # 資料以點呈現
    geom_point() +
    # 加上lm & 95%CI
    geom_smooth(
        method = "lm",
        formula = y ~ x,
        se = TRUE,
        level = 0.95
    ) +
    # 允收上下限
    geom_line(aes(y = limit.upr),
        linetype = 2,
        color = "#00AFBB"
    ) +
    geom_line(aes(y = limit.lwr),
        linetype = 2,
        color = "#00AFBB"
    ) +
    # 以樣品分列，以器材Lot分欄
    facet_grid(Label.Conc.Level ~ Label.Dev.Lot,
        scales = "free"
    )