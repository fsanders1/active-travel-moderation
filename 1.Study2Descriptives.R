# Load in libraries
library(dplyr)
library(tidyr)
library(knitr)
library(psych)

# Load in dataframes

################################## Imputed data #################################################################
mod_imp <- readRDS("...rds")

## Convert to long format
impdat <- mice::complete(mod_imp, action="long", include = F)

################################### Study 2 ###################################################################
############################# Imputed ########################################################################
study2vars <- c('building_density_age28mum',
                'connectivity_density_age28mum',
                'greenspace_age28mum',
                'facility_density_age28mum',
                'facility_richness_age28mum',
                'pop_density_age28mum',
                'bus_lines_age28mum',
                'walkability_age28mum',
                'nearest_road_age28mum',
                'age_at_birth_mums','familial_depressive_history', 
                'contextual_age28mum', 'age_at_depression_age32mum', 'depression_at_age28mum','age_at_travel_age30mum', 
                'physical_activity_age28mum','smoking_age28mum','active_travel_age30mum', 'depression_at_age32mum', 'ethnicity_mum')

# Subset just those variables plus the .imp and .id cols
impdat2 <- impdat[, c(".imp", ".id", study2vars)]

# Convert all vars to numeric (describe requires numerics)
impdat2[, study2vars] <- lapply(impdat2[, study2vars], function(x) as.numeric(as.character(x)))

# Split by imputation
imps_split2 <- split(impdat2, impdat2$.imp)

# Apply psych::describe() to each imputed dataset
descriptives_list2 <- lapply(imps_split2, function(df) {
  desc <- describe(df[, study2vars])
  as.matrix(desc[, sapply(desc, is.numeric)])
})

# Turn into 3D array and take mean across imputations
desc_array2 <- simplify2array(descriptives_list2)
pooled_desc2 <- apply(desc_array2, c(1, 2), mean, na.rm = TRUE)

# Convert to dataframe
pooled_desc_df2 <- as.data.frame(pooled_desc2)

# Only keep descriptive info that we want
pooled_desc_df2 <- pooled_desc_df2[, c("n", "mean", "sd", "min", "max", "se")]

pooled_desc_df_long2 <- pooled_desc_df2 %>%
  mutate(Method = "Pooled_Imputed")

pooled_desc_df_long2 <- as.data.frame(pooled_desc_df_long2)

################################## Original ##########################################################
original_df2 <- original_df[, c(study2vars)]

desc_original2 <- describe(original_df2)

# Only keep descriptive info that we want
desc_original2 <- desc_original2[, c("n", "mean", "sd", "min", "max", "se")]

desc_original2 <- desc_original2 %>%
  mutate(Method = "Original")

desc_original2 <- as.data.frame(desc_original2)
################################# Combine ############################################################

desc_original2 <- desc_original2 %>%
  rename_with(~paste0("Original_", .), everything())  

pooled_desc_df_long2 <- pooled_desc_df_long2 %>%
  rename_with(~paste0("Imputed_", .), everything())  

# Merge the original and imputed descriptives by variable name
joint_desc2 <- merge(desc_original2, pooled_desc_df_long2, by = "row.names", all = TRUE)

joint_desc2 <- rename(joint_desc2, Variable = Row.names)

# Reorder by statistic rather than dataset
joint_desc2 <- joint_desc2 %>%
  select(Variable, Original_n, Imputed_n, 
         Original_mean, Imputed_mean, 
         Original_sd, Imputed_sd,
         Original_min, Imputed_min,
         Original_max, Imputed_max,
         Original_se, Imputed_se)


# Round all numeric columns to 2 decimal places
joint_desc2[] <- lapply(joint_desc2, function(x) {
  if (is.numeric(x)) {
    round(x, 2)  # Round to 2 decimal places
  } else {
    x
  }
})

# Apply format to all numeric columns to avoid scientific notation
joint_desc2[] <- lapply(joint_desc2, function(x) {
  if (is.numeric(x)) {
    format(x, scientific = FALSE, trim = TRUE)
  } else {
    x
  }
})

# Save table
write.csv(joint_desc2, file = "....csv", row.names = FALSE)

