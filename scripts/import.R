# making bins easier to handle
# 0 = Start-0:01:00, 1 = 0:01:00-0:02:00, etc.
time_bins <- tibble(
  bin_name = c("Start-0:01:00", "0:01:00-0:02:00", "0:02:00-0:03:00", "0:03:00-0:04:00", "0:04:00-0:05:00", "0:05:00-0:06:00", "0:06:00-0:07:00", "0:07:00-0:08:00", "0:08:00-0:09:00", "0:09:00-0:10:00", "0:10:00-0:11:00", "0:11:00-0:12:00", "0:12:00-0:13:00", "0:13:00-0:14:00", "0:14:00-0:15:00"),
  Bin = c(1:15) %>% as.factor()
)

trial_info <- function(fam) {
  info <- read_excel(sprintf("./data/%s/%s", fam, grep("trial info", dir(paste0("./data/", fam)), value = T))) %>% 
    mutate(Trial = as.factor(Trial),
           Tray = as.factor(Tray)#,
           # Time = format(Time, "%H:%M:%S")
           )
  return(info)
}

read_meta <- function(fam) {
  meta <- read_excel(sprintf("./data/%s/%s", fam, grep("genotypes", dir(paste0("./data/", fam)), value = T))) %>% 
    mutate(Genotype = factor(Genotype, levels = c("wt", "het", "hom")),
           Tray = as.factor(Tray))
  return(meta)
}

read_data <- function(fam) {
  data <- read_excel(sprintf("./data/%s/%s", fam, grep("(time bins)", dir(paste0("./data/", fam)), value = T))) %>% # read the overall data
    # renaming columns
    dplyr::select(
      Trial = `...2`,
      Position = `...3`,
      bin_name = `...4`,
      Dist_travelled_total = "Distance moved center-point Total mm",
      Dist_travelled_mean = "Distance moved center-point Mean mm",
      Dist_travelled_var = matches("Distance moved center-point Variance",.),
      Velocity_mean = "Velocity center-point Mean mm/s",
      Velocity_var = matches("Velocity center-point Variance",.),
      Time_moving_total = "Movement Moving / center-point Cumulative Duration s",
      # Time_moving_mean = "Movement Moving / center-point Mean s",
      # Time_moving_var = matches("Movement Moving / center-point Variance",.),
      Freq_moving = "Movement Moving / center-point Frequency",
      Acceleration_max = matches("Acceleration center-point Maximum",.),
      Acceleration_min = matches("Acceleration center-point Minimum",.),
      Acceleration_var = matches("Acceleration center-point Variance",.),
      Freq_in_middle = "In zone middle / center-point Frequency",
      Time_in_middle_total = "In zone middle / center-point Cumulative Duration s",
      Time_in_middle_mean = "In zone middle / center-point Mean s",
      Time_in_middle_var = matches("In zone middle / center-point Variance",.),
      Dist_to_middle_total = "Distance to zone middle / center-point Total mm",
      Dist_to_middle_mean = "Distance to zone middle / center-point Mean mm",
      Dist_to_middle_var = matches("Distance to zone middle / center-point Variance",.),
      Mobility_total = "Mobility Body fill Total %",
      Mobility_mean = "Mobility Body fill Mean %",
      Mobility_var = matches("Mobility Body fill Variance",.),
      Meander_total = "Meander center-point / absolute Total deg/mm",
      Meander_mean = "Meander center-point / absolute Mean deg/mm",
      Meander_var = matches("Meander center-point / absolute Variance",.),
      Heading_mean = "Heading center-point Mean deg",
      Heading_var = matches("Heading center-point Variance",.)
      # Angular_vel_min = "Angular velocity center-point / relative Minimum deg/s",
      # Angular_vel_max = "Angular velocity center-point / relative Maximum deg/s",
      # Angular_vel_mean = contains(c("Angular velocity", "Mean deg/s"),.),
      # Angular_vel_total = "Angular velocity center-point / relative Total deg/s",
      # Angular_vel_var = contains(c("Angular velocity", "Variance"),.)
      ) %>%
    # Making the bins numbered rather than chr
    left_join(time_bins) %>% select(-bin_name) %>%
    relocate(Bin, .after = Position) %>%
    # emoving "Trial" from trial number and converting to factor
    mutate(Trial = str_remove(Trial, pattern = "Trial") %>% str_trim("left") %>% as.factor()) %>% 
    na.omit() %>% droplevels()
  # read in metadata and merge the dataframes
  data <- trial_info(fam) %>% right_join(data, by = c("Trial")) # shows which tray is in each trial + trial times
  data <- read_meta(fam) %>% right_join(data, by = c("Tray", "Position")) %>% na.omit() # joining the trial number with metadata
  # make the variances type dbl and other variables factors
  data <- data %>% mutate(
    Position = as.factor(Position),
    fish_id = paste0(fish_id, ".", Parents) %>% as.factor()
    ) %>% select(-Parents) %>%
    # fixing the null values represented by a "-" chr
    mutate_at(c(20, 21, 28:32), ~str_replace(., "-", "0")) %>%
    mutate_at(c(20, 21, 28:32), as.numeric)
  return(data)
}
