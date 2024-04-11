require(tidyverse)
require(here)
require(forecast)
require(assertthat)

refresh <- function() {
  # a convenience function. Call this to re-read the 
  # functions if you make a change. 
  source(here("functions.r"))
}

# Some functions for our sampling
get_row_migration <- function(target_level,data,target_year=2023) {
  # For a given points_level, this returns the previous
  # migration fractions. We assume points_level != 0 or Blank. 
  # We'll return a data.frame with the following columns: 
  # year: the year that had this migration "into it". If 2006 is 
  #       the first year in our data set, then our first year
  #       will be 2007. 
  # apps: the number of apps in that year
  # lag_apps: the number of apps in year N-1
  # carry_forward: the number of failures at one lower point
  #                value. So if points_level is 3, then this
  #                will be the number of people who drew unsuccessfully
  #                in the previous year.
  # migration_factor: apps/carry_forward
  # migration_amount: apps - carry_forward. I think *not* using percents
  #                   will make sense at the higher levels of points
  #                   where apps is very low.
  # we'll cut down the data to only years up through this year
  # as a convenience for later functions.
  
  # from testing
  # data <- d %>% filter(district=="270-50",residency=="RESIDENT")
  
  if(target_level %in% c(0,"Blank")){
    stop("This function only works for points_level > 0.")
  }
  
  prev_points <- as.numeric(target_level) - 1
  before_df <- data %>% 
    filter(points_level==prev_points) %>% 
    mutate(carry_forward = apps - successes,
           join_year = year+1,       # most of these lines are to get a 
           original_year = year) %>% # column to join onto after_df
    mutate(year=NULL) %>%            # we want it to be called "year"
    rename(lag_apps = apps,
           year = join_year) %>% 
    mutate(join_year=NULL)
  
  
  after_df <- data %>% 
    filter(points_level==target_level) %>% 
    select(year,apps) %>% 
    left_join(before_df %>% 
                select(year, carry_forward, lag_apps),
              by="year")
  
  
  after_df <- after_df %>% 
    mutate(migration_factor = 
             if_else(carry_forward==0 | is.na(carry_forward),
                     NA_real_,
                     apps/carry_forward),
           migration_amount = apps - carry_forward)
  
  # cut down to just through this_year
  after_df <- after_df %>% 
    filter(year <= target_year)
  
  return(after_df)  
  
}

#get_row_migration(5,d %>% 
#                    filter(district=="270-50",residency=="RESIDENT"))
#get_row_migration(18,d %>% 
#                     filter(district=="270-50",residency=="RESIDENT"))
# It looks weird to me that so many of these are negative 
# Sam has confirmed it's right. I guess I'm not understanding
# all the forces behind people putting in at a particular level

get_zero_blank_estimate <- function(data,
                                    get_zero=T,
                                    this_year=2023,
                                    level=c(0.9)) {
  # gets the estimate of the number of people who will 
  # put in with a zero or blank in `this_year + 1`
  # if `get_zero` is true then we're doing zeros, otherwise
  # it's blanks. 
  
  if(get_zero){
    # TODO: If there's an NA in apps between good years, 
    # we might want to put in the zero rather than 
    # drop it like I'm doing.
    data <- data %>% 
      filter(year <= this_year,
             points_level==0,
             !is.na(apps)) %>% 
      select(year,apps) %>% 
      arrange(year) %>% 
      mutate(diff_apps = apps - lag(apps))
    
  } else {
    data <- data %>% 
      filter(year <= this_year,
             points_level=="Blank",
             !is.na(apps)) %>% 
      select(year,apps) %>% 
      arrange(year) %>% 
      mutate(diff_apps = apps - lag(apps))
  }
  # Check for ses
  if (nrow(data) < 2) {
    print(paste("Skipping item:", unique(data$item), "and district:", unique(data$district), "due to insufficient data"))
    return(NULL)
  }
  

  # Chat and I worked out the below.
  # Applying Simple Exponential Smoothing on the differenced data
  fit <- ses(data$diff_apps[!is.na(data$diff_apps)], h = 1,level = level)
  
  forecast_result <- forecast(fit)
  
  # Reintegrating the forecast
  last_app_value <- tail(data$apps, 1)
  point_forecast <- last_app_value + as.numeric(forecast_result$mean)
  lower_bounds <- last_app_value + as.numeric(forecast_result$lower)
  upper_bounds <- last_app_value + as.numeric(forecast_result$upper)
  
  # Returning the forecasted value for apps in this_year + 1 with uncertainty intervals
  return(list(point_forecast = max(point_forecast,0), 
              lower_bounds = max(lower_bounds,0), 
              upper_bounds = upper_bounds))  
}

#debug(get_zero_blank_estimate)
# get_zero_blank_estimate(d %>% filter(district=="270-50"))
# get_zero_blank_estimate(d %>% filter(district=="270-50"),get_zero = F)
# get_zero_blank_estimate(d %>% filter(district=="270-50"),get_zero = F,level=0.5)
# get_zero_blank_estimate(d %>% filter(district=="270-50"),get_zero = T,level=0.5,this_year = 2019)

# Now we need a function that returns n_sim estimates of a 
# particular points_level for a particular year. We handle
# 0 and "Blank" in other ways
simulate_points_levels <- function(data,
                                   target_level,
                                   this_year,
                                   n_sim=1000) {
  
  # for testing
  #data <- d %>% filter(district=="270-50",residency=="RESIDENT")
  #n_sim <- 10
  #this_year <- 2023
  #target_level <- 6
  
  
  if(target_level %in% c(0,"Blank")){
    stop("This appraoch doesn't work for 0 or Blank points levels.")
  }
  
  if(!(target_level %in% unique(data$points_level))){
    stop("Something is wrong. this_points_level is not in data")
  }
  
  # start with the migration table
  mtable <- get_row_migration(data=data,target_level=target_level,target_year=this_year)
  
  starting_value <- mtable %>% 
    filter(year==this_year) %>% 
    pull(apps)
  
  if(is.na(starting_value)){
    warning("our starting value is NA. Using 0.")
    starting_value <- 0
  }
  
  if(starting_value < 50) { # 50 is kind of arbitrary
    # we use migration amounts
    amounts <- mtable %>% 
      filter(!is.na(migration_amount)) %>% 
      pull(migration_amount)
    
    weights <- mtable %>% 
      filter(!is.na(migration_amount)) %>% 
      mutate(migration_weight = 1/(this_year - year + 1)) %>% 
      pull(migration_weight)
    
    if(length(amounts) < 4){
      warning("For this year, we have fewer than 4 migration amounts")
    }
    
    sim_results <- sample(amounts,size=n_sim,replace=T,prob=weights)
    
    return(sim_results+starting_value)    
    
  } else {
    # we use migration factors
    
    factors <- mtable %>% 
      filter(!is.na(migration_factor)) %>% 
      pull(migration_factor)
    
    weights <- mtable %>% 
      filter(!is.na(migration_factor)) %>% 
      mutate(migration_weight = 1/(this_year - year + 1)) %>% 
      pull(migration_weight)
    
    if (length(factors) < 4) {
      warning(paste("For item", this_item, "and district", this_district, ", we have fewer than 4 migration factors"))
    }
    
    
    if(all(is.na(factors))){
      print("Hit a points_level with no factors. Returning starting value")
      sim_results <- rep(starting_value,n_sim)
    } else {
      sim_results <- sample(factors,size=n_sim,replace=T,prob=weights) * starting_value
    }
    
    
    return(sim_results)    
  }
  
}

#debug(simulate_points_levels)
# (x <- simulate_points_levels(
#   data = d %>% filter(district=="270-50",residency=="RESIDENT"),
#   target_level = 5,
#   this_year = 2023)
#   )


# I think we're ready to write a function that makes forecast
# apps tables for a given year. 

simulate_apps_tables <- function(data,this_year=2024,num_tables=1000){
  # given a data frame that's just the data for one district-item-
  # residency combo, this function will simulate and return num_tables
  # sets of simulations, in one big data.frame. We're forecasting
  # for "this_year". The columns of that 
  # data frame are
  # 1. sim: a counter of the simulation
  # 2. points_level: Blank, 0, 1, ..., max(points_level)
  # 3. apps: a simulated number of apps
  # 
  # There should be length(unique(points_level))*num_tables in 
  # the return. 
  
  # from testing
  # data <- d %>% filter(district=="270-50",residency=="RESIDENT")
  # num_tables <- 10
  # this_year <- 2023
  

  
  
  
  unique_years <- data %>% pull(year) %>% unique()
  this_district <- unique(data$district)
  this_item <- unique(data$item)
  this_residency <- unique(data$item)
  
  assert_that(all(length(this_district)==1,
                  length(this_item)==1,
                  length(this_residency==1)),
              msg = "simulate_apps_tables is being called with muliple item-district-residency!")
  if(!(this_year %in% unique_years)){
    warning(paste("You called simulate_apps_tables with a year that isn't",
                  "in the data for item", unique(data$item), "and district", unique(data$district), ". Returning empty DF", sep="\n"))
    return(data.frame())
  }
  
  
  
  results <- expand_grid(
    sim = 1:num_tables,
    points_level = unique(data$points_level), 
    apps = NA_integer_
  )
  
  holder <- get_zero_blank_estimate(data,
                                    get_zero = T,
                                    this_year = this_year,
                                    level = 0.95)
  
  results$apps[results$points_level==0] <- 
    pmax(rnorm(n=num_tables,
               mean=holder$point_forecast,
               sd=(holder$upper_bounds-holder$point_forecast)/2),0)
  # will round at the end. I feel like this parametric "replication"
  # is probably the best we can do. 
  
  holder <- get_zero_blank_estimate(data,
                                    get_zero = F,
                                    this_year = this_year,
                                    level = 0.95)
  
  results$apps[results$points_level=="Blank"] <- 
    pmax(rnorm(n=num_tables,
               mean=holder$point_forecast,
               sd=(holder$upper_bounds-holder$point_forecast)/2),0)
  
  for(this_level in unique(data$points_level)){
    if(this_level==0 | this_level=="Blank"){
      next
    } else {
      holder <- simulate_points_levels(data,this_level,this_year,num_tables)
      results$apps[results$points_level==this_level] <- holder
    }
  }
  results$apps <- pmax(round(results$apps),0)
  
  return(results)
  
}


# Lottery Code starts here 

run_lottery <- function(points_table, num_permits, nonresident_cap_pct=0.1){
  

  # add adjusted points
  points_table$adjusted_points <- 1
  
  # Doing it this old-school way to avoid the warning that comes
  # from attempting to call as.numeric on "Blank"
  points_table$adjusted_points[
    !(points_table$points_level %in% c(0,"Blank"))
  ] <- as.numeric(points_table$points_level[!(points_table$points_level %in% c(0,"Blank"))])^2 + 1
  
  resident_tally <- 0
  nonresident_tally <- 0
  points_table$successes <- 0
  nonresident_cap <- floor(nonresident_cap_pct*num_permits)
  indices <- 1:nrow(points_table)
  
  for(i in 1:num_permits){
    
    # We need to calculate total points each time through
    points_table <- points_table %>% 
      mutate(total_points = (apps-successes)*adjusted_points)

    # Check if there are positive probabilities
    if (sum(points_table$total_points > 0) < 1) {
      cat("Too few positive probabilities for lottery selection. Skipping iteration.\n")
      cat("Item:", points_table$item[1], ", District:", points_table$district[1], "\n")
      next  # Skip to next iteration
    }
    # make our lottery selection
    row_idx <- sample(indices,size=1,prob = points_table$total_points)
    points_table$successes[row_idx] <- points_table$successes[row_idx] + 1
    
    if(points_table$residency[row_idx]=="RESIDENT"){
      resident_tally <- resident_tally + 1
    } else {
      nonresident_tally <- nonresident_tally + 1
      if(nonresident_tally==nonresident_cap){
        # no more chances to win
        points_table <- points_table %>% 
          mutate(adjusted_points = if_else(residency=="NONRESIDENT",
                                           0,adjusted_points))
      }
    }
    
  }
  
  # Getting rid of adjusted_points and total_points
  # since we're using the the former for bookkeeping
  return(points_table %>% select(-adjusted_points,-total_points))
  
}

#for debugging
#points_table <- simulate_apps_tables(d %>% 
 #                                 filter(item =="DEERPERMIT",
  #                                       district=="210-50",
  #                                       residency=="RESIDENT"),
 #                                this_year = 2023,
 #                               num_tables = 1) %>% 
  #mutate(
 #   item = "DEERPERMIT",
  #  district="210-50",
   # residency="RESIDENT"
 # ) %>% # Now non-resident
#  bind_rows(
 #   simulate_apps_tables(d %>% 
   #                        filter(item=="DEERPERMIT",
   #                               district=="210-50",
   #                               residency=="NONRESIDENT"),
   #                      this_year = 2023,
   #                     num_tables = 1) %>% 
    #  mutate(
    #    item="DEERPERMIT",
    #    district="210-50",
    #    residency="NONRESIDENT"       )
 # )
#
 #num_permits <- d %>% 
  # filter(item=="DEERPERMIT",district=="210-50",year==2023) %>% 
  # pull(successes) %>% sum()
