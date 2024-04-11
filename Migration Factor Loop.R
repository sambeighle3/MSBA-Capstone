library(tidyverse)
library(readxl)
library(here)
library(forecast)
library(scales) # just needed for plotting in testing

source(here("functions.r"))

d <- read_excel(here("filled_data.xlsx"),sheet=1) %>% 
  janitor::clean_names() %>% 
  # Some of these names are annoying
  rename(
    item = item_description,
    points_level = number_of_points,
    apps = number_of_applications,
    successes = number_of_successes,
    adjusted_points = real_entries,
    total_points = real_total_entries
  )

d <- d %>% 
  filter(residency %in% c("RESIDENT","NONRESIDENT"))

# Building 2024 estimated table for 270-50
est_2024 <- simulate_apps_tables(d %>% 
                                   filter(item =="DEERPERMIT",
                                          district=="210-50",
                                          residency=="RESIDENT"),
                                 this_year = 2022,
                                 num_tables = 1) %>% 
  mutate(
    item = "DEERPERMIT",
    district="210-50",
    residency="RESIDENT"
  )

# Now non-resident
est_2024 <- est_2024 %>% 
  bind_rows(
    simulate_apps_tables(d %>% 
                           filter(item=="DEERPERMIT",
                             district=="210-50",
                                  residency=="NONRESIDENT"),
                         this_year = 2022,
                         num_tables = 1) %>% 
      mutate(
        item="DEERPERMIT",
        district="210-50",
        residency="NONRESIDENT"
      )
    
  )

run_lottery(est_2024,num_permits = 43) %>% 
  data.frame()

# Get unique combinations of item and district

unique_combinations <- d %>%
  distinct(item, district)

unique_combinations

# Initialize an empty list to store the results
estimated_tables <- list()



# Iterate through each unique combination of item and district
for (i in 1:nrow(unique_combinations)) {
  this_item <- unique_combinations$item[i]
  this_district <- unique_combinations$district[i]

  cat("Processing item:", this_item, "and district:", this_district, "\n")
  
  # Generate estimated table for residents
  est_resident <- simulate_apps_tables(d %>% 
                                         filter(item == this_item,
                                                district == this_district,
                                                residency == "RESIDENT"),
                                       this_year = 2022,
                                       num_tables = 100) %>% 
    mutate(
      item = this_item,
      district = this_district,
      residency = "RESIDENT"
    )
  
  # Generate estimated table for non-residents
  est_nonresident <- simulate_apps_tables(d %>% 
                                            filter(item == this_item,
                                                   district == this_district,
                                                   residency == "NONRESIDENT"),
                                          this_year = 2022,
                                          num_tables = 100) %>% 
    mutate(
      item = this_item,
      district = this_district,
      residency = "NONRESIDENT"
    )
  
  # Bind the estimated tables together
  if(nrow(est_resident) > 0){
    holder <- est_resident
  } 
  if(nrow(est_nonresident) > 0){
    holder <- bind_rows(holder,est_nonresident)
  } 
  
  if(exists("holder")){
    estimated_tables[[i]] <- holder
    rm(holder)
  }
}

# Combine all estimated tables into a single data frame
final_estimated_table <- do.call(bind_rows, estimated_tables)


# Replace negative values with 0 in the final_estimated_table
final_estimated_table <- final_estimated_table %>%
  mutate_at(vars(-item, -district, -residency), ~replace(., . < 0, 0))
final_estimated_table <- final_estimated_table %>%
  mutate_at(vars(-item, -district, -residency), ~ifelse(is.nan(.), 0, .))

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



# Now we are simulating wins from lottery - this one only does it 1 time

# Run the lottery function on the final_estimated_table
library(dplyr)

# Filter the original dataframe for the year 2022
d_2022 <- d %>%
  filter(year == 2022)

# Group the filtered dataframe by item, district, and sim, and calculate the sum of successes
successes_sum <- d_2022 %>%
  group_by(item, district) %>%
  summarize(total_successes = sum(successes))

# Save the result as a new dataframe
Num_Permits_2024 <- successes_sum

# Initialize an empty list to store results
all_results <- list()

# Iterate through each unique combination of item, district, and sim
unique_combinations <- unique(final_estimated_table[, c("item", "district", "sim")])
print(unique_combinations)


unique_sims <- unique(final_estimated_table$sim)







## This is the good one

# Create an empty data frame to store all results
final_results <- data.frame(
  sim = character(),
  item = character(),
  district = character(),
  point_level = numeric(),
  residency = character(),
  successes = numeric(),
  stringsAsFactors = FALSE
)

# Define the number of times to run the lottery
num_runs <- 100

# Iterate over unique combinations
for (i in 1:nrow(unique_combinations)) {
  this_item <- unique_combinations$item[i]
  this_district <- unique_combinations$district[i]
  this_sim <- unique_combinations$sim[i]  # Add this line to retrieve the sim value
  
  cat("Processing sim:", this_sim, ", item:", this_item, ", and district:", this_district, "\n")
  
  # Find the corresponding num_permits for this combination
  num_permits <- Num_Permits_2024$total_successes[
    Num_Permits_2024$item == this_item &
      Num_Permits_2024$district == this_district
  ]
  
  # Filter the final estimated table for this combination and sim
  current_data <- final_estimated_table %>%
    filter(sim == this_sim, item == this_item, district == this_district)
  
  # Run the lottery function for each run
  for (run in 1:num_runs) {
    if (nrow(current_data) > 0) {
      result <- run_lottery(current_data, num_permits = num_permits) %>%
        mutate(sim = this_sim,
               item = this_item,
               district = this_district)
      
      # Store the result in final_results
      final_results <- rbind(final_results, result)
    } else {
      cat("No data found for sim:", this_sim, ", item:", this_item, ", and district:", this_district, "\n")
    }
  }
}

completed_table <- final_results
# Now final_results contains the results for every run
# You can work with this data frame as needed

