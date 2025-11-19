# ---
# Week 11 Exercise: Part 1 - CWD Data Simulation 
# Set seed for reproducibility
set.seed(456)

# ---
# 1. Simulation Parameters 
# ---
n_total <- 100 # 100 observations total
n_per_group <- n_total / 2 # 50 observations per status

# Define the "true" parameters for the simulation
# Model: odba = b0 + b1*age + b2*cwd_status + b3*(age * cwd_status)

# Parameters for "negative" (reference) group:
true_intercept_neg <- 10.0 # Baseline activity for a CWD-negative deer
true_slope_neg     <- -0.5 # Activity declines by 0.5 units per year

# Parameters for "positive" (treatment) group:
true_intercept_shift <- -2.0  # CWD-positive deer are less active at any age
true_slope_interaction <- -0.3 # Activity declines *faster* in CWD-positive deer

# Error term
true_error_sd <- 1.5

# ---
# 2. Generate Predictor Variables (X1 and X2)
# ---

# X1 (Continuous): Age in Years
x_continuous <- sample(2:15, n_total, replace = TRUE)
names(x_continuous) <- "age_yrs"

# X2 (Categorical): CWD Status
# Generate 50 "negative" and 50 "positive"
x_categorical <- rep(c("negative", "positive"), each = n_per_group)
names(x_categorical) <- "cwd_status"

# Create a numeric dummy variable for the model (0 = negative, 1 = positive)
is_cwd_positive <- ifelse(x_categorical == "positive", 1, 0)


# ---
# 3. Generate Response Variable (Y)
# ---

# Calculate the "true" Y value for each observation
# based on our parameters and the interaction term
true_y <- (true_intercept_neg + (is_cwd_positive * true_intercept_shift)) +
  (true_slope_neg     + (is_cwd_positive * true_slope_interaction)) * x_continuous

# Add random normal error
errors <- rnorm(n_total, mean = 0, sd = true_error_sd)

# Calculate final Y (Response): ODBA (activity)
y_response <- true_y + errors


# ---
# 4. Create and Save Final Dataset
# ---
# Combine *only* the observable variables into a data frame
sim_data_for_partner <- data.frame(
  odba = y_response,
  age_yrs = x_continuous,
  cwd_status = x_categorical
)

# Check the head of the data (ages should be integers now)
print("--- Head of data to be shared: ---")
print(head(sim_data_for_partner))

# Save to a .csv file
file_to_share <- "cwd_ancova_data_to_share.csv"
write.csv(sim_data_for_partner, file_to_share, row.names = FALSE)

print(paste("Simulation complete. Data saved to:", file_to_share))

# ---
# Week 11 Exercise: Part 2 - Analyzing Partner's Data (Tickus bittus)
# ---
#
# Our goal is to answer the question:
# "How does sensitivity of questing duration to a humidity index
#  differ between tick nymphs and adults?"
#
# This question is *specifically* asking about an INTERACTION.
# "Sensitivity" = slope
# "differ between nymphs and adults" = are the slopes different?
#
# This is a test of the interaction term: humidity_index * life_stage

# ---
# 1. Load Partner's Data
# ---
partner_file <- "tick_data.csv"
partner_data <- read.csv(partner_file)

# ---
# 2. Inspect the Data
# ---

# Look at the first few rows to confirm column names
print("--- Head of partner's data: ---")
print(head(partner_data))

# Look at the data types
# We expect questing_duration & humidity_index to be 'num' (numeric)
# and life_stage to be 'chr' (character) or 'Factor'.
print("--- Structure of partner's data: ---")
str(partner_data)

# Optional but good practice: Convert character to factor
# (lm() does this automatically, but it's good to be explicit)
partner_data$life_stage <- as.factor(partner_data$life_stage)
print("--- 'life_stage' levels: ---")
print(levels(partner_data$life_stage))


# ---
# 3. Fit the FULL Model (with interaction)
# ---

# This is the main step.
# The `*` symbol fits:
#   1. humidity_index (main effect)
#   2. life_stage (main effect)
#   3. humidity_index:life_stage (the interaction term)
#
full_model <- lm(questing_duration ~ humidity_index * life_stage,
                 data = partner_data)

# Look at the summary of the full model
print("--- Summary of FULL Model (with interaction): ---")
summary(full_model)

# ---
# 4. Fit the REDUCED model (since interaction was NOT significant)
# ---

# We use '+' instead of '*' to fit only the main effects.
# This model assumes the slopes are parallel.
reduced_model <- lm(questing_duration ~ humidity_index + life_stage,
                    data = partner_data)

# Look at the summary of this new reduced model
print("--- Summary of REDUCED Model (no interaction): ---")
summary(reduced_model)
