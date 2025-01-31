library(data.table)

locations <- data.frame(
  name = c("Bukhara", "Fergana", "Samarkand", "Tashkent"),
  latitude = c(39.7741, 40.3777, 39.6542, 41.2995),
  longitude = c(64.4248, 71.7913, 66.9597, 69.2401)
)

data = NULL
for (loc in 1:nrow(locations)) {
  location_data = read.csv(paste0("data/", locations[loc, ]$name, ".csv"))
  location_data$time = as.Date(location_data$time)
  location_data$location = as.factor(locations[loc, ]$name)
  
  if (is.null(data)) {
    data = location_data
  } else {
    data = rbind(data, location_data)
  }
}

data = as.data.table(data)
data = melt(data, id.vars = c("time", "location"))
data$year = as.integer(format(data$time, "%Y"))
data$month = as.integer(format(data$time, "%m"))
