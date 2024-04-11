# Calculate the average for each unique combination
average_data <- final_estimated_table %>%
  group_by(points_level, item, district, residency, sim) %>%
  summarize(
    average_apps = mean(apps)
  ) %>%
  group_by(points_level, item, district, residency) %>%
  summarize(
    average_apps = mean(average_apps)
  )

# Write the average data to a CSV file
write.csv(average_data, file = "average_data.csv", row.names = FALSE)



# Load necessary libraries
library(dplyr)

# Aggregate total apps for each combination of item and district for the year 2023 in 'd'
total_apps_2023 <- d %>%
  filter(year == 2022) %>%
  group_by(item, district) %>%
  summarise(total_apps = sum(apps, na.rm = TRUE))

# Aggregate average apps for each combination of item and district in 'average_data'
average_apps_data <- average_data %>%
  group_by(item, district) %>%
  summarise(average_apps_total = sum(average_apps, na.rm = TRUE))

# Merge the aggregated data based on item and district
merged_data <- merge(total_apps_2023, average_apps_data, by = c("item", "district"))

# Calculate absolute percentage error for each row after adding a small value to avoid division by zero
small_value <- 1e-10
merged_data$absolute_percentage_error <- abs(merged_data$average_apps_total - merged_data$total_apps) / (merged_data$total_apps + small_value) * 100

# Calculate MAPE
mape <- mean(merged_data$absolute_percentage_error, na.rm = TRUE)

# Print MAPE
print(paste("Mean Absolute Percentage Error (MAPE):", mape))


