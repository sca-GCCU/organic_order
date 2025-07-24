# Clear environment 

rm(list = ls())

# Packages  

library(usmap)
library(sf)
library(readxl)
library(tidyverse)

# CSA Directory Data 

  # Load 

farms <- read_excel("C:/Users/scana/OneDrive/Documents/research/organic_order/usda_csa_data.xlsx")

  # Clean 

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


#########################################################################

# Prepping for Map

farms_clean <- farms_clean %>% 
  filter(!is.na(latitude) & !is.na(longitude)) %>% 
  mutate(
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude)
  ) # drops 25 farms

  # Filter out "bad" entries (identified below)

farms_clean <- farms_clean  %>% 
  filter(!listing_id %in% c(201229, 201595)) # drops 2; 27 excluded total  

farms_transformed <- usmap_transform(farms_clean, input_names = c("longitude", "latitude"))


  # Checking for "bad" entries (filtered out above in future)

coords <- st_coordinates(farms_transformed) # extract X and Y coordinates from the POINT geometry column

farms_with_coords <- cbind(farms_transformed, coords) # combine the coordinates with the rest of the data

top_farms <- farms_with_coords[order(-farms_with_coords$Y), ] # recorder 

head(top_farms, 2) # identify top two: 201229, 201595 

rm(list = c("coords", "farms_with_coords", "top_farms", "top_farm"))


#########################################################################

# Map

plot_usmap() +
  geom_sf(data = farms_transformed, color = "blue", size = 1) +
  theme_minimal() + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank())

ggsave(
  "csa_location_map.png",  # output filename
  plot = last_plot(), # or your plot object, e.g., plot = my_plot
  width = 10,         # width in inches
  height = 6,         # height in inches
  dpi = 300           # higher dpi = higher quality; 300 is standard for print
)


#########################################################################

# Prepping for Data Analysis  

# Create indicators for single- vs. multi-farm CSA 

farms_clean <- farms_clean %>%
  mutate(
    single = case_when(
      is.na(num_supplyfarms) | num_supplyfarms == 1 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  relocate(single, .after = num_supplyfarms)

  # Count farms that do not list a number of supply farms as single farm
  # establishments.


# Create indicator for Organically Certified and no pesticides 

farms_clean <- farms_clean %>%
  mutate(
    any_special_prod = if_else(rowSums(select(., starts_with("specialproductionmethods_")) == 1, na.rm = TRUE) > 0, TRUE, FALSE),
    
    organic = case_when(
      specialproductionmethods_1 == 1 ~ 1,
      any_special_prod ~ 0,
      TRUE ~ NA_real_
    ),
    
    no_pesticides = case_when(
      specialproductionmethods_8 == 1 ~ 1,
      any_special_prod ~ 0, 
      TRUE ~ NA_real_
    )
  ) %>% relocate(c(organic, no_pesticides), .before = specialproductionmethods)


# Create indicators for pick-up or delivery 

farms_clean <- farms_clean %>%
  mutate(
    any_delivery = if_else(rowSums(select(., starts_with("deliverpickup_method_")) == 1, na.rm = TRUE) > 0, TRUE, FALSE),
    
    delivery_to_cust = case_when(
      deliverpickup_method_1 == 1 | deliverpickup_method_2 == 1 ~ 1,
      any_delivery ~ 0, 
      TRUE ~ NA_real_
    ),
    
    pickup_general = case_when(
      deliverpickup_method_3 == 1 | deliverpickup_method_4 == 1 ~ 1,
      any_delivery ~ 0, 
      TRUE ~ NA_real_ 
    )
  ) %>% relocate(c(delivery_to_cust, pickup_general), .before = deliverpickup_method)

# delivery_to_cust includes the options delivery to customer's homes or workplaces.
# pickup_general = 1 if the farm has a pickup option (either neighborhood or farm)

#farms_clean <- farms_clean %>%
#  mutate(
#    pickup_required = case_when(
#      (deliverpickup_method_3 == 1 | deliverpickup_method_4 == 1) & 
#        deliverpickup_method_1 == 0 & deliverpickup_method_2 == 0 ~ 1,
#      TRUE ~ 0
#    )
#  )


# Create indicator for whether the consumer picks what's in their box

farms_clean <- farms_clean %>%
  mutate(
    any_share_items = if_else(rowSums(select(., starts_with("share_itemsinsharebox_")) == 1, na.rm = TRUE) > 0, TRUE, FALSE),
    
    farmer_pick = case_when(
      share_itemsinsharebox_1 == 1 ~ 1, 
      any_share_items ~ 0, 
      TRUE ~ NA_real_
    )
  ) %>%
  relocate(farmer_pick, .before = share_itemsinsharebox)


# Create indicator for whether it is a lump-sum payment

farms_clean <- farms_clean %>%
  mutate(
    any_paymentopt = if_else(rowSums(select(., starts_with("share_paymentoption_")) == 1, na.rm = TRUE) > 0, TRUE, FALSE), 
    
    lump = case_when(
      share_paymentoption_1 == 1 & share_paymentoption_2 == 0 ~ 1,
      any_paymentopt ~ 0, 
      TRUE ~ NA_real_
    )
  )

farms_clean <- farms_clean %>%
  mutate(
    lump_flex = case_when(
      share_paymentoption_1 == 1 ~ 1,
      any_paymentopt ~ 0,
      TRUE ~ NA_real_
    )
  )

farms_clean <- farms_clean %>%
  mutate(
    labor_for_fee = case_when(
      share_paymentoption_3 == 1 ~ 1,
      any_paymentopt ~ 0,
      TRUE ~ NA_real_
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

farms_clean <- farms_clean %>%
  mutate(share_number = as.numeric(share_number))

farms_clean <- farms_clean %>% 
  mutate(num_supplyfarms = as.numeric(num_supplyfarms))

# Dropping variables I no longer need for the analysis

usda_csa_data_clean <- farms_clean %>%
  select(-update_time, 
         -(season_month_start:season_month_end),
         -(share_itemsinsharebox:share_itemsinsharebox_888),
         -(share_paymentoption:share_paymentoption_888),
         -(deliverpickup_method:deliverpickup_method_888),
         -(specialproductionmethods:specialproductionmethods_888)
         )

write.csv(farms_clean, "usda_csa_data_clean.csv", row.names = FALSE)


#########################################################################

# Data Analysis 

usda_csa_data_clean %>%
  summarise(across(
    c(month_start, month_end, season_length_months, share_number, 
      single, farmer_pick, lump, labor_for_fee, pickup_general, delivery_to_cust, organic),
    list(
      mean   = ~mean(.x, na.rm = TRUE),
      median = ~median(.x, na.rm = TRUE),
      sd     = ~sd(.x, na.rm = TRUE),
      n      = ~sum(!is.na(.x))
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variable", "stat"),
    names_pattern = "^(.*)_(mean|median|sd|n)$"
  ) %>%
  pivot_wider(names_from = stat, values_from = value)


# Histograms and Box Plots 

  # Number of supply farms 

ggplot(usda_csa_data_clean, aes(x = num_supplyfarms)) + 
  geom_histogram() + 
  labs(y = "Count", x = "Number of Supply Farms")

ggsave(
  "hist_supplyfarms.png",  # output filename
  plot = last_plot(), # or your plot object, e.g., plot = my_plot
  dpi = 300           # higher dpi = higher quality; 300 is standard for print
)

ggplot(usda_csa_data_clean, aes(x = num_supplyfarms)) + 
  geom_boxplot(fill = "blue", outlier.color = "red", outlier.size = 1) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y  = element_blank()) +
  labs(x = "Number of Supply Farms")

ggsave(
  "boxplot_supplyfarms.png",  # output filename
  plot = last_plot(), # or your plot object, e.g., plot = my_plot
  dpi = 300           # higher dpi = higher quality; 300 is standard for print
)

sum(!is.na(usda_csa_data_clean$num_supplyfarms))

  # Number of shares 

ggplot(usda_csa_data_clean, aes(x = share_number)) + 
  geom_histogram() + 
  labs(y = "Count", x = "Number of Shares")

ggsave(
  "hist_shares.png",  # output filename
  plot = last_plot(), # or your plot object, e.g., plot = my_plot
  dpi = 300           # higher dpi = higher quality; 300 is standard for print
)

ggplot(usda_csa_data_clean, aes(x = share_number)) + 
  geom_boxplot(fill = "blue", outlier.color = "red", outlier.size = 1) + 
  theme(axis.ticks.y = element_blank(),
        axis.text.y  = element_blank()) +
  labs(x = "Number of Shares")

ggsave(
  "boxplot_shares.png",  # output filename
  plot = last_plot(), # or your plot object, e.g., plot = my_plot
  dpi = 300           # higher dpi = higher quality; 300 is standard for print
)

sum(!is.na(usda_csa_data_clean$share_number))




