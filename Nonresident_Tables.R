library(dplyr)

# Assuming 'd' is your DataFrame

# Group by 'item' and 'district', then calculate the sum of 'successes' and the count of nonresident applicants
nonresident_table <- d %>%
  group_by(item, district) %>%
  summarize(
    total_successes = sum(successes),
    total_nonresident = sum(ifelse(residency == "NONRESIDENT", apps, 0))
  ) %>%
  mutate(
    nonresident_percentage = total_nonresident / total_successes
  )

# Print the resulting table
print(nonresident_table)


getwd()

d_missing <- read_excel(here("Full_Data_Set_ForTest.xlsx"),sheet=1) %>% 
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

