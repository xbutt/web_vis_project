library(ggplot2)
library(dplyr)

# Function to create boxplots
create_boxplot <- function(data, variable_name, title, y_label, color_palette) {
  ggplot(
    data %>% filter(.data$variable == variable_name),  
    aes(x = factor(year), y = value, fill = location)
  ) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7) +
    facet_wrap(~ location) +
    scale_fill_manual(values = color_palette) +
    scale_x_discrete(guide = guide_axis(angle = 45)) +
    labs(
      title = title,
      x = "Year",
      y = y_label
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = "#2C3E50"),
      axis.title.x = element_text(face = "bold", size = 14),
      axis.title.y = element_text(face = "bold", size = 14),
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 14)
    )
}

#function to create barplots
create_barplot <- function(data, variable_name, title, y_label, color_palette) {
  ggplot(
    data %>%
      filter(.data$variable == variable_name) %>% 
      group_by(year, location) %>% 
      summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop"),  # Aggregate total values
    aes(x = factor(year), y = total_value, fill = location)
  ) +
    geom_bar(stat = "identity", position = "dodge", color = "#023047") +
    scale_fill_manual(values = color_palette, name = "Location") +
    scale_x_discrete(guide = guide_axis(angle = 45)) +
    labs(
      title = title,
      x = "Year",
      y = y_label
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = "#2C3E50"),
      axis.title.x = element_text(face = "bold", size = 14),
      axis.title.y = element_text(face = "bold", size = 14),
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 14, color = "#2C3E50"),
      legend.text = element_text(size = 12),
      strip.text = element_text(face = "bold", size = 14)
    )
}