# Clearing environment 

rm(list = ls())

# Loading the data 

#install.packages("readxl")  
library(readxl)

farms <- read_excel("C:/Users/scana/OneDrive/Documents/research/organic_order/usda_csa_data.xlsx")

# Cleaning up the dataset

#install.packages(c("ggplot2", "maps", "dplyr")) 
#install.packages("sf")
library(ggplot2)
library(maps) 
library(dplyr)
library(sf)
library(stringr)

farms_clean <- farms %>%
  select(-starts_with("orgnization"),
         -(continueoperate_year:listing_desc),
         -location_desc, 
         -(season_weeks:products_otherdesc), 
         -(share_size:share_size_otherdesc),
         -(share_itemsinsharebox_otherdesc:share_availabledate),
         -share_paymentoption_otherdesc, 
         -deliverpickup_method_otherdesc,
         -(specialproductionmethods_other_desc:bulktable_row_id)
         )



############################################################################

# Create map

farms_clean <- farms_clean %>%
  filter(!is.na(latitude) & !is.na(longitude)) # drops 25 farms 

farms_clean <- farms_clean %>%
  mutate(
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude)
  )

mainland_bbox <- list(
  lon_min = -125,  # western edge (includes CA)
  lon_max = -66,   # eastern edge (includes ME)
  lat_min = 24,    # southern edge (includes FL Keys)
  lat_max = 50     # northern edge (roughly southern Canada border)
)

farms_mainland <- farms_clean %>%
  filter(longitude >= mainland_bbox$lon_min,
         longitude <= mainland_bbox$lon_max,
         latitude >= mainland_bbox$lat_min,
         latitude <= mainland_bbox$lat_max) # drops 22 more farms

farms_non_mainland <- farms_clean %>%
  filter(longitude < mainland_bbox$lon_min |
         longitude > mainland_bbox$lon_max |
         latitude < mainland_bbox$lat_min | 
         latitude > mainland_bbox$lat_max)

us_map <- map_data("state")

ggplot() + 
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group),
               fill = "gray90", color = "black") +
  geom_point(data = farms_mainland, aes(x = longitude, y = latitude),
             color = "blue", alpha = 0.6, size = 1) + 
  coord_fixed(1.3) + # could have just edited this to exclude HI and AK
  theme_minimal() + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

ggsave(
  "csa_location_map.png",  # output filename
  plot = last_plot(), # or your plot object, e.g., plot = my_plot
  width = 10,         # width in inches
  height = 6,         # height in inches
  dpi = 300           # higher dpi = higher quality; 300 is standard for print
)

rm(farms_mainland, farms_non_mainland) # remove unneeded datasets 


#########################################################################

# New data analysis 

# Create indicators for single- vs. multi-farm CSA 

farms_clean <- farms_clean %>%
  mutate(
    single = case_when(
      is.na(num_supplyfarms) | num_supplyfarms == 1 ~ 1,
      TRUE ~ 0
    )
  )

farms_clean <- farms_clean %>% relocate(single, .after = num_supplyfarms)


# Create indicator for Organically Certified 

farms_clean <- farms_clean %>%
  mutate(
    organic = case_when(
      specialproductionmethods_1 == 1 ~ 1,
      TRUE ~ 0
    )
  )


# Create indicator for no pesticides 

farms_clean <- farms_clean %>%
  mutate(
    no_pesticides = case_when(
      specialproductionmethods_8 == 1 ~ 1,
      TRUE ~ 0
    )
  )

farms_clean <- farms_clean %>% relocate(c(organic, no_pesticides), .before = specialproductionmethods)  


# Create indicators for pick-up or delivery 

farms_clean <- farms_clean %>%
  mutate(
    delivery_to_cust = case_when(
      deliverpickup_method_1 == 1 | deliverpickup_method_2 == 1 ~ 1,
      TRUE ~ 0
    )
  )
  # The above includes the options delivery to customer's homes and workplaces 

farms_clean <- farms_clean %>%
  mutate(
    pickup_general = case_when(
      deliverpickup_method_3 == 1 | deliverpickup_method_4 == 1 ~ 1,
      TRUE ~ 0 
    )
  )

farms_clean <- farms_clean %>%
  mutate(
    pickup_required = case_when(
      (deliverpickup_method_3 == 1 | deliverpickup_method_4 == 1) & 
        deliverpickup_method_1 == 0 & deliverpickup_method_2 == 0 ~ 1,
      TRUE ~ 0
    )
  )

farms_clean <- farms_clean %>% relocate(c(delivery_to_cust, pickup_general, pickup_required), .before = deliverpickup_method)


# Create indicator for whether the consumer picks what's in their box

farms_clean <- farms_clean %>%
  mutate(
    farmer_pick = case_when(
      share_itemsinsharebox_1 == 1 ~ 1, 
      TRUE ~ 0
    )
  ) %>%
  relocate(farmer_pick, .before = share_itemsinsharebox)


# Create indicator for whether it is a lump-sum payment

farms_clean <- farms_clean %>%
  mutate(
    lump = case_when(
      share_paymentoption_1 == 1 & share_paymentoption_2 == 0 ~ 1,
      TRUE ~ 0
    )
  )

farms_clean <- farms_clean %>%
  mutate(
    lump_flex = case_when(
      share_paymentoption_1 == 1 ~ 1,
      TRUE ~ 0
    )
  )

farms_clean <- farms_clean %>%
  mutate(
    labor_for_fee = case_when(
      share_paymentoption_3 == 1 ~ 1,
      TRUE ~ 0
    )
  )

farms_clean <- farms_clean %>% relocate(c(lump, lump_flex, labor_for_fee), .before = share_paymentoption)


# Extract month start and month end (numerical) and create length of season
  # variable. 

farms_clean <- farms_clean %>%
  mutate(
    # First, remove any year prefix
    month_start_raw = sub("^\\d{4}-", "", season_month_start),
    month_end_raw   = sub("^\\d{4}-", "", season_month_end),
    
    # Then, convert month names or numeric strings to numeric month numbers
    month_start = case_when(
      grepl("^[0-9]+$", month_start_raw) ~ as.numeric(month_start_raw),
      TRUE ~ match(tolower(month_start_raw), tolower(month.name))
    ),
    
    month_end = case_when(
      grepl("^[0-9]+$", month_end_raw) ~ as.numeric(month_end_raw),
      TRUE ~ match(tolower(month_end_raw), tolower(month.name))
    )
  ) %>%
  select(-month_start_raw, -month_end_raw)

  # Check for NAs from bad output 

invalid_start <- farms_clean %>% filter(is.na(month_start) & !is.na(season_month_start))
invalid_end   <- farms_clean %>% filter(is.na(month_end) & !is.na(season_month_end))

if (nrow(invalid_start) > 0 || nrow(invalid_end) > 0) {
  warning("There are invalid month_start or month_end entries. Review them with `invalid_start` and `invalid_end`.")
} # all good it appears (7/5/25)

  # Check that month numbers fall in valid range 1-12.

out_of_range <- farms_clean %>% filter(
  !is.na(month_start) & (month_start < 1 | month_start > 12) |
    !is.na(month_end) & (month_end < 1 | month_end > 12)
)

if (nrow(out_of_range) > 0) {
  warning("Some month_start or month_end values are outside 1-12. Review them with `out_of_range`.")
} # all good it appears (7/5/25)

rm(invalid_start, invalid_end, out_of_range) # dropping datasets from sanity check

farms_clean <- farms_clean %>% 
  relocate(month_start, .after = season_month_end) %>%
  relocate(month_end, .after = month_start)

farms_clean <- farms_clean %>%
  mutate(
    season_length_months = case_when(
      # If end >= start, simple difference + 1 (inclusive)
      !is.na(month_start) & !is.na(month_end) & month_end >= month_start ~
        month_end - month_start + 1,
      
      # If end < start, season wraps to next year: (months left in year + months in new year) + 1
      !is.na(month_start) & !is.na(month_end) & month_end < month_start ~
        (12 - month_start + 1) + month_end,
      
      # Otherwise (missing data), NA
      TRUE ~ NA_real_
    )
  )

farms_clean <- farms_clean %>%
  relocate(season_length_months, .after = month_end)


# Dropping variables I no longer need for the analysis

#usda_csa_data_clean <- farms_clean %>%
#  select(-update_time, 
#         -(season_month_start:season_month_end),
#         -(share_itemsinsharebox:share_itemsinsharebox_888),
#         -(share_paymentoption:share_paymentoption_888),
#         -(deliverpickup_method:deliverpickup_method_888),
#         -(specialproductionmethods:specialproductionmethods_888)
#         )

write.csv(farms_clean, "usda_csa_data_clean.csv", row.names = FALSE)


# The actual analysis 

farms_clean <- farms_clean %>%
  mutate(share_number = as.numeric(share_number))

get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

mean_single <- mean(farms_clean$single, na.rm = TRUE)
mode_month_start <- get_mode(farms_clean$month_start)
mode_month_end <- get_mode(farms_clean$month_end)
mean_season_length <- mean(farms_clean$season_length_months, na.rm = TRUE)
mode_season_length <- get_mode(farms_clean$season_length_months)
median_season_length <- median(farms_clean$season_length_months, na.rm = TRUE)
mean_share_number <- mean(farms_clean$share_number, na.rm = TRUE)
mode_share_number <- get_mode(farms_clean$share_number)
median_share_number <- median(farms_clean$share_number, na.rm = TRUE)

mean_farmer_pick <- farms_clean %>%
  filter(share_itemsinsharebox_1 == 1 | share_itemsinsharebox_2 == 1 | share_itemsinsharebox_3 == 1) %>%
  summarise(mean_farmer_pick = mean(farmer_pick, na.rm = TRUE)) %>%
  pull(mean_farmer_pick)

mean_lump <- farms_clean %>%
  filter(share_paymentoption_1 == 1| share_paymentoption_2 == 1 | share_paymentoption_3 == 1 | share_paymentoption_4 == 1) %>%
  summarise(mean_lump = mean(lump, na.rm = TRUE)) %>%
  pull(mean_lump)

mean_lump_flex <- farms_clean %>%
  filter(share_paymentoption_1 == 1| share_paymentoption_2 == 1 | share_paymentoption_3 == 1 | share_paymentoption_4 == 1) %>%
  summarise(mean_lump_flex = mean(lump_flex, na.rm = TRUE)) %>%
  pull(mean_lump_flex)

mean_labor_for_fee <- farms_clean %>%
  filter(share_paymentoption_1 == 1| share_paymentoption_2 == 1 | share_paymentoption_3 == 1 | share_paymentoption_4 == 1) %>%
  summarise(mean_labor_for_fee = mean(labor_for_fee, na.rm = TRUE)) %>%
  pull(mean_labor_for_fee)

mean_pickup_req <- farms_clean %>%
  filter(deliverpickup_method_1 == 1 | deliverpickup_method_2 == 1 | deliverpickup_method_3 == 1 | deliverpickup_method_4 == 1) %>%
  summarise(mean_pickup_req = mean(pickup_required, na.rm = TRUE)) %>%
  pull(mean_pickup_req)


mean_organic <- farms_clean %>%
  filter(if_any(starts_with("specialproductionmethods"), ~ .x == 1)) %>%
  summarise(mean_organic = mean(organic, na.rm = TRUE)) %>%
  pull(mean_organic)

mean_no_pest <- farms_clean %>%
  filter(if_any(starts_with("specialproductionmethods"), ~ .x == 1)) %>%
  summarise(mean_no_pest = mean(no_pesticides, na.rm = TRUE)) %>%
  pull(mean_no_pest)
  


library(tibble)

summary_stats <- tibble(
  mean_single = mean_single,
  mode_month_start = mode_month_start,
  mode_month_end = mode_month_end,
  mean_season_length = mean_season_length,
  median_season_length = median_season_length,
  mode_season_length = mode_season_length,
  mean_share_number = mean_share_number,
  median_share_number = median_share_number,
  mode_share_number = mode_share_number,
  mean_farmer_pick = mean_farmer_pick,
  mean_lump = mean_lump,
  mean_lump_flex = mean_lump_flex,
  mean_labor_for_fee = mean_labor_for_fee,
  mean_pickup_required = mean_pickup_req,
  mean_organic = mean_organic,
  mean_no_pesticides = mean_no_pest
)

library(tidyr)

summary_stats_long <- summary_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = "statistic",
    values_to = "value"
  )

print(summary_stats_long)

write.csv(summary_stats, "summary_stats.csv", row.names = FALSE)





