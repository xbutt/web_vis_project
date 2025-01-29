library(terra)

locations <- data.frame(
  name = c("Bukhara", "Fergana", "Samarkand", "Tashkent"),
  latitude = c(39.7741, 40.3777, 39.6542, 41.2995),
  longitude = c(64.4248, 71.7913, 66.9597, 69.2401)
)
variables = c("t2m", "sf", "tp")

for (loc in 1:nrow(locations)) {
  location_data = NULL
  for (variable in variables) {
    raster_data = rast("data/data_era5.nc", subds = variable)
    location_coordinates = matrix(c(locations[loc, ]$longitude, locations[loc, ]$latitude), ncol = 2)
    variable_data = extract(raster_data, location_coordinates, method = "simple")
    variable_data = unlist(variable_data)
    if (variable == "t2m") {
      variable_data = variable_data - 273.15
    } else if (variable == "sf") {
      variable_data = variable_data * 1000
    } else if (variable == "tp") {
      variable_data = variable_data * 1000
    }
    
    if (is.null(location_data)) {
      location_data = data.frame(
        time = seq(from = as.Date("2000-01-01"), by = "month", length.out = length(variable_data)))
    }
    location_data[[variable]] = variable_data
  }
  write.csv(location_data, paste0("data/", locations[loc, ]$name, ".csv"), row.names = FALSE)
}