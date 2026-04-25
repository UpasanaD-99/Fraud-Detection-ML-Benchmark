
# PHASE 1: SETUP, LOADING, AND DATA QUALITY ASSURANCE



library(tidyverse)
library(scales)
library(lubridate)
library(caret)


# Load Dataset
file_path <- "../Dataset & Feature Engineered data/transactions.csv"
if (file.exists(file_path)) {
  df <- read.csv(file_path)
  print("SUCCESS: Dataset loaded successfully.")
} else {
  print("WARNING: File not found at path. Opening manual selector...")
  df <- read.csv(file.choose())
  print("SUCCESS: Dataset loaded manually.")
}

# Data Type Conversion
# Converting categorical variables to Factors
df$is_fraud <- as.factor(df$is_fraud)           # Target Variable
df$merchant_category <- as.factor(df$merchant_category)
df$channel <- as.factor(df$channel)
df$avs_match <- as.factor(df$avs_match)

# Dimensionality Reduction
# Removing high-cardinality identifiers (IDs)
df_clean <- df %>% select(-transaction_id, -user_id)

# DATA QUALITY ASSESSMENT
print("--- DATA QUALITY ASSESSMENT REPORT ---")
print(paste("1. Total Records:", nrow(df_clean)))
print(paste("2. Missing Values:", sum(is.na(df_clean))))
print(paste("3. Duplicate Rows:", sum(duplicated(df_clean))))
print(paste("4. Negative Amounts:", sum(df_clean$amount < 0)))
print("------------------------------------------")



# PHASE 2: EDA
custom_colors <- c("0" = "#5D6D7E", "1" = "#C0392B")





# Plot 1: Class Imbalance Analysis
p1 <- ggplot(df_clean, aes(x = is_fraud, fill = is_fraud)) +
  geom_bar(width = 0.6) +
  geom_text(stat='count', aes(label=scales::comma(..count..)), vjust=-0.5, fontface="bold", size=4) + 
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Dataset Imbalance",
       subtitle = "Fraud represents a minority class (~2 %). Accuracy metrics may be misleading.",
       x = "Transaction Class", y = "Count") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", panel.grid.major.x = element_blank())

ggsave("Figures/plot_1_class_imbalance.png", p1, width = 8, height = 6, dpi = 300)
print(p1)

# Plot 2: Spending Behavior (Log Scale)
p2 <- ggplot(df_clean, aes(x = amount, fill = is_fraud)) +
  geom_histogram(bins = 50, color = "white", alpha = 0.85) +
  scale_x_log10(labels = dollar) + 
  scale_fill_manual(values = custom_colors) +
  facet_wrap(~is_fraud, scales = "free_y", labeller = as_labeller(c("0" = "Legitimate", "1" = "Fraud"))) +
  labs(title = "Spending Behavior Analysis",
       subtitle = "Fraud shows Multimodal distribution: spikes at very low and very high amounts.",
       x = "Transaction Amount (Log Scale)", y = "Frequency") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

ggsave("Figures/plot_2_spending_behavior.png", p2, width = 10, height = 6, dpi = 300)
print(p2)

# Plot 3: Geolocation Analysis
p3 <- ggplot(df_clean, aes(x = is_fraud, y = shipping_distance_km, fill = is_fraud)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 1, outlier.alpha = 0.3) +
  scale_y_log10() + 
  scale_fill_manual(values = custom_colors) +
  scale_x_discrete(labels = c("Legit", "Fraud")) +
  labs(title = "Geolocation Risk Analysis",
       subtitle = "Fraudulent transactions exhibit significantly higher median shipping distances.",
       x = NULL, y = "Distance (km) [Log Scale]") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", panel.grid.major.x = element_blank())

ggsave("Figures/plot_3_geolocation.png", p3, width = 8, height = 6, dpi = 300)
print(p3)


# Plot 4: High-Risk Sectors in Merchant Categories
p4 <- df_clean %>%
  group_by(merchant_category) %>%
  summarise(fraud_rate = mean(as.numeric(is_fraud)-1)) %>% 
  ggplot(aes(x = reorder(merchant_category, fraud_rate), y = fraud_rate)) +
  geom_col(fill = "#C0392B", width = 0.7) +
  geom_text(aes(label = percent(fraud_rate, accuracy = 0.1)), hjust = -0.1, size = 4) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.2))) +
  coord_flip() + 
  labs(title = "High-Risk Merchant Sectors",
       subtitle = "Electronics and Travel sectors show disproportionately high attack rates.",
       x = NULL, y = "Fraud Rate") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.y = element_blank())

ggsave("Figures/plot_4_high_risk_sectors.png", p4, width = 9, height = 6, dpi = 300)
print(p4)

# Plot 5: Temporal Patterns (Time of Day)
p5 <- df_clean %>%
  mutate(clean_time = as.POSIXct(transaction_time, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC"),
         Hour = as.numeric(format(clean_time, "%H"))) %>%
  group_by(Hour, is_fraud) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  ggplot(aes(x = Hour, y = Count, color = is_fraud)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = custom_colors) +
  facet_wrap(~is_fraud, scales = "free_y", labeller = as_labeller(c("0" = "Legit Volume", "1" = "Fraud Volume"))) + 
  labs(title = "Temporal Attack Vectors",
       subtitle = "Fraudulent activity remains active during standard sleeping hours.",
       x = "Hour of Day (UTC)", y = "Transaction Volume") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

ggsave("Figures/plot_5_temporal_patterns.png", p5, width = 10, height = 6, dpi = 300)
print(p5)

# Plot 6: Security Checks (AVS)
p6 <- ggplot(df_clean, aes(x = avs_match, fill = is_fraud)) +
  geom_bar(position = "fill", width = 0.6) + 
  facet_grid(~channel) + 
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = custom_colors, labels = c("Legit", "Fraud")) +
  labs(title = "Security Vulnerability Analysis",
       subtitle = "AVS Mismatch creates a significant increase in fraud probability.",
       x = "AVS Match (0 = Fail, 1 = Pass)", y = "Probability", fill = "Class") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.x = element_blank())

ggsave("Figures/plot_6_avs_security.png", p6, width = 10, height = 6, dpi = 300)
print(p6)

# PLOT 7: FRAUD PER COUNTRY
df_clean <- df_clean %>%
  mutate(is_fraud = as.numeric(as.character(is_fraud)))
fpc <- df_clean %>%
  group_by(country) %>%
  summarise(
    total_transactions = n(),
    fraud_transactions = sum(is_fraud, na.rm = TRUE),
    fraud_rate = fraud_transactions / total_transactions,
    .groups = "drop"
  )

p7_filtered <- fpc %>%
  filter(total_transactions >= 500)

p7 <- ggplot(p7_filtered,
                  aes(x = reorder(country, fraud_rate),
                      y = fraud_rate)) +
  geom_col(fill = "#C0392B") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title = "Fraud Rate per Country",
    subtitle = "Countries with at least 500 transactions",
    x = "Country",
    y = "Fraud Rate (%)"
  ) +
  theme_minimal(base_size = 12)


print(p7)
ggsave(filename = "Figures/plot_7_fraud_rate_per_country.png", plot = p7, width = 10, height = 6, dpi = 300)

# PLOT 8: SKEWNESS IN THE DATA
skew_data <- df_clean %>%
  select(amount, shipping_distance_km, account_age_days) %>%
  pivot_longer(
    cols = everything(),
    names_to = "feature",
    values_to = "value"
  )
p8 <- ggplot(skew_data, aes(x = value)) +
  geom_histogram(aes(y = ..density..),
                 bins = 50,
                 fill = "#C0392B",
                 alpha = 0.6) +
  geom_density(color = "#1B2631", linewidth = 0.8) +
  facet_wrap(~ feature, scales = "free") +
  labs(
    title = "Distribution and Skewness of Key Transaction Variables",
    subtitle = "Histograms with density overlays (free scales)",
    x = "Value",
    y = "Density"
  ) +
  theme_minimal(base_size = 12)
print(p8)

ggsave(
  filename = "Figures/plot_8_skewness_continuous_features.png",
  plot = p8,
  width = 12,
  height = 6,
  dpi = 300
)


# PHASE 3: FEATURE ENGINEERING & PRE-PROCESSING

# Plot heatmap
numeric_df <- df_clean %>%
  mutate(
    is_fraud = as.numeric(is_fraud) - 1,
    avs_match = as.numeric(as.character(avs_match)),
    cvv_result = as.numeric(as.character(cvv_result)),
    three_ds_flag = as.numeric(as.character(three_ds_flag))
  ) %>%
  select(where(is.numeric))

# Compute correlation matrix in Heatmap style
cor_mat <- cor(numeric_df, use = "complete.obs")

cor_long <- as.data.frame(as.table(cor_mat))
colnames(cor_long) <- c("Var1", "Var2", "Correlation")

p_corr <- ggplot(cor_long, aes(
  x = Var1,
  y = Var2,
  fill = Correlation
)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(
    aes(label = sprintf("%.2f", Correlation)),
    size = 3
  ) +
  scale_fill_gradient2(
    low = "#4575B4",
    mid = "#F7F7F7",
    high = "#D73027",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  coord_fixed() +
  labs(
    title = "Correlation Heatmap (Numeric Features)",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

print(p_corr)
ggsave("Figures/correlation_heatmap.png", p_corr, width = 10, height = 8, dpi = 300)

df_engineered <- df_clean %>%
  # Remove non-predictive features according to correlation analysis
  select(-avg_amount_user, -promo_used) %>%
  
  # Parse Timestamps & Filter Corrupt Data (6 records were found)
  mutate(transaction_time = ymd_hms(transaction_time, tz = "UTC")) %>%
  filter(!is.na(transaction_time)) %>% 
  
  # Extract Raw Time Components
  mutate(
    hour = hour(transaction_time),
    day = day(transaction_time),
    month = month(transaction_time),
    # Adjusting weekday index to match standard 0-6 format (Mon=0)
    weekday = wday(transaction_time, week_start = 1) - 1
  ) %>%
  
  # 4. Binary Encoding (0/1)
  mutate(
    # Flag transactions outside standard business hours (18:00 - 05:00)
    evening = ifelse(hour >= 6 & hour <= 17, 0, 1),
    # Flag weekends (Sat=5, Sun=6)
    weekend = ifelse(weekday %in% c(5, 6), 1, 0),
    # Encode Channel: Web=1, App=0
    channel_encoded = ifelse(channel == "web", 1, 0)
  ) %>%
  
  # Cyclical Time Transformation (Sine/Cosine)
  # Transforms linear time into circular features to preserve temporal proximity (e.g., 23h vs 00h).
  mutate(
    days_in_month_val = days_in_month(transaction_time),
    
    time_day_sin = sin(day * (2 * pi / days_in_month_val)),
    time_day_cos = cos(day * (2 * pi / days_in_month_val)),
    
    time_month_sin = sin(month * (2 * pi / 12)),
    time_month_cos = cos(month * (2 * pi / 12)),
    
    time_wd_sin = sin(weekday * (2 * pi / 7)),
    time_wd_cos = cos(weekday * (2 * pi / 7)),
    
    time_hour_sin = sin(hour * (2 * pi / 24)),
    time_hour_cos = cos(hour * (2 * pi / 24))
  ) %>%
  
  # 6. Final Cleanup
  select(-channel) %>%
  rename(channel = channel_encoded) %>%
  select(-transaction_time, -days_in_month_val, -hour, -day, -month, -weekday)

# PHASE 4: DATA EXPORT

save_path <- "Dataset & Feature Engineered data/transactions_pre_processed.csv"

write.csv(df_engineered, save_path, row.names = FALSE)

print(paste("File saved at:", save_path))

## Train/Test Split (80/20)
df <- read.csv("Dataset & Feature Engineered data/transactions_pre_processed.csv")

split_and_save_data <- function(
  df,
  target_col = "is_fraud",
  test_size = 0.2,
  random_state = 42,
  train_path = "Dataset & Feature Engineered data/train.csv",
  test_path = "Dataset & Feature Engineered data/test.csv"
) {

  set.seed(random_state)

  train_index <- createDataPartition(
    df[[target_col]],
    p = 1 - test_size,
    list = FALSE
  )

  train_df <- df[train_index, ]
  test_df  <- df[-train_index, ]
  write.csv(train_df, train_path, row.names = FALSE)
  write.csv(test_df, test_path, row.names = FALSE)

  return(list(train_df = train_df, test_df = test_df))
}

splits <- split_and_save_data(
  df,
  target_col = "is_fraud",
  test_size = 0.2,
  random_state = 42,
  train_path = "train.csv",
  test_path = "test.csv"
)

train_df <- splits$train_df
test_df  <- splits$test_df
print("Train/Test split completed and files saved.")