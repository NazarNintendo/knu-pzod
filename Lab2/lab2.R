setwd("C:/Users/User/Desktop/FIT/PZOD/Lab2")

load_filtered_files_to_df <- function(directory, ids) {
  wd <- getwd()
  files <- dir(directory, pattern = "\\.csv", full.names = FALSE)
  filtered_files <- files[as.numeric(gsub(".csv", "", files)) %in% ids]
  setwd(directory)
  data <- do.call(rbind, lapply(filtered_files, read.csv))
  data <- na.omit(data)
  setwd(wd)
  return (data)
}

pollutantmean <- function(directory, pollutant, ids) {
  data <- load_filtered_files_to_df(directory, ids)
  return (mean(data[,pollutant]))
}

pollutantmean("specdata", "nitrate", 70:72)

complete <- function(directory, ids) {
  data <- load_filtered_files_to_df(directory, ids)
  return (aggregate(data$ID, list(data$ID), length))
}

complete("specdata", c(2, 4, 8, 10, 12))

corr <- function(directory, threshold) {
  complete_data <- complete(directory, 1:1e6)
  complete_above_threshold <- subset(complete_data, complete_data[2] >= threshold)
  data <- load_filtered_files_to_df(directory, complete_above_threshold$Group.1)
  if (is.null(data)) return (vector(mode="numeric", length=0))
  data <- do.call(rbind, lapply(
      split(data, data$ID), 
      function(x) cor(x$nitrate, x$sulfate)
  ))
  return (data)
}

cr <- corr("specdata", 400)
head(cr)
## [1] -0.01895754 -0.04389737 -0.06815956 -0.07588814  0.76312884 -0.15782860
summary(cr)
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -0.17623 -0.03109  0.10021  0.13969  0.26849  0.76313