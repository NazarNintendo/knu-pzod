library(xts)

setwd("D:/Projects/r/knu-pzod/Personal Task")
data <- read.csv("all_stocks_5yr.csv")


ibm_data <- data[data$Name == "IBM",]
rownames(ibm_data) <- as.Date(ibm_data$date)
ibm_data <- subset(ibm_data, select = -c(date, Name) )
head(ibm_data)
ibm_data_weekly <- apply.weekly(ibm_data, FUN = mean)
head(ibm_data_weekly)
ibm_data_monthly <- apply.monthly(ibm_data_weekly, FUN = mean)
head(ibm_data_monthly)

amd_data <- data[data$Name == "AMD",]
rownames(amd_data) <- as.Date(amd_data$date)
amd_data <- subset(amd_data, select = -c(date, Name) )
head(amd_data)
amd_data_weekly <- apply.weekly(amd_data, FUN = mean)
head(amd_data_weekly)
amd_data_monthly <- apply.monthly(amd_data_weekly, FUN = mean)
head(amd_data_monthly)



mean(ibm_data_monthly$close)
sd(ibm_data_monthly$close)
var(ibm_data_monthly$close)

mean(amd_data_monthly$close)
sd(amd_data_monthly$close)
var(amd_data_monthly$close)


plot.ts(ibm_data_monthly)
plot.ts(amd_data_monthly)

ibm_pct <- -diff(ibm_data_monthly$close)/ibm_data_monthly$close[-1]
amd_pct <- -diff(amd_data_monthly$close)/amd_data_monthly$close[-1]

plot.ts(ibm_pct * 100)
plot.ts(amd_pct * 100)

ibm_cumprod <- cumprod(ibm_pct + 1)
amd_cumprod <- cumprod(amd_pct + 1)

plot.ts(ibm_cumprod)
plot.ts(amd_cumprod)
