setwd("C:/Users/User/Desktop/FIT/PZOD/Lab3")
data <- readRDS("hike_data.rds")

data[1, "features"]

get_type <- function (x) strsplit(x, split = ", ")[[1]][2]

get_length <- function (x) {
  miles <- strsplit(x, split = " ")[[1]][1]
  miles <- as.numeric(miles)
  type <- strsplit(x, split = " ")[[1]][3]
  if (type == "one-way") {
    miles <- miles * 2
  } 
  return(miles)
}

get_location_general <- function(x) strsplit(x, split = " -- ")[[1]][1]

clean_hike_trails <- data.frame(
  gain = as.numeric(data$gain),
  highpoint = as.numeric(data$highpoint),
  rating = as.numeric(data$rating),
  trip = sapply(data$length, function(x) strsplit(x, split = ", ")[[1]][2]),
  length_total = sapply(data$length, get_length),
  location_general = sapply(data$location, get_location_general),
  id = seq.int(nrow(data))
)

head(clean_hike_trails)

# Question 1. How many routes have rating more than 4.9
new_data <- subset(clean_hike_trails, rating > 4.9)
nrow(new_data)

# Question 2. How many routes are “Good for kids” (hint: you can use (unnest function)?
new_data <- subset(unnest(data, cols = c("features")), features == "Good for kids")
nrow(new_data)

# Question 3. Which unique features can routes have?
features <- unique(unnest(data, cols = c("features"))$features)
features

# Question 4. What is the most common rating of a route?
tail(names(sort(table(clean_hike_trails$rating))), 1)

# Question 5. Your own question and answer.
# How many routes have highpoint more than 10000?
new_data <- subset(clean_hike_trails, highpoint > 10000)
nrow(new_data)
