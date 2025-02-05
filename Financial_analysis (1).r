#set the working directory
setwd("C:\\Users\\shivb\\Desktop\\R Programming\\R Project")
# instakk the necessary libraries if not installed
install.packages("dplyr") 
install.packages("ggplot2") 
install.packages("moments") 
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(moments) 
# Load datasets
nifty_500 <- read.csv("NIFTY 500.csv")
nifty_auto <- read.csv("NIFTY AUTO.csv")
nifty_bank <- read.csv("NIFTY BANK.csv")
nifty_it <- read.csv("NIFTY IT.csv")
nifty_healthcare <- read.csv("NIFTY HEALTHCARE.csv")
nifty_infra <- read.csv("NIFTY INFRASTRUCTURE.csv")
# Inspect the structure of the datasets
str(nifty_500)
str(nifty_auto)
str(nifty_bank)
str(nifty_it)
str(nifty_healthcare)
str(nifty_infra)
# View the first few rows of each dataset
head(nifty_500)
head(nifty_auto)
head(nifty_bank)
head(nifty_it)
head(nifty_healthcare)
head(nifty_infra)
# Check for missing values
sum(is.na((nifty_500)))
sum(is.na(nifty_auto))
sum(is.na((nifty_bank)))
sum(is.na((nifty_it)))
sum(is.na((nifty_healthcare)))
sum(is.na((nifty_infra)))
# Remove rows with significant missing data if necessary
nifty_500 <- na.omit(nifty_500)
nifty_auto <- na.omit(nifty_auto)
nifty_bank <- na.omit(nifty_bank)
nifty_it <- na.omit(nifty_it)
nifty_healthcare <- na.omit(nifty_healthcare)
nifty_infra <- na.omit(nifty_infra)
# Standardize column names for easier manipulation
colnames(nifty_500) <- tolower(colnames(nifty_500))
colnames(nifty_auto) <- tolower(colnames(nifty_auto))
colnames(nifty_bank) <- tolower(colnames(nifty_bank))
colnames(nifty_it) <- tolower(colnames(nifty_it))
colnames(nifty_healthcare) <- tolower(colnames(nifty_healthcare))
colnames(nifty_infra) <- tolower(colnames(nifty_infra))
# Summary statistics for NIFTY 500
summary(nifty_500)
summary(nifty_bank)
summary(nifty_auto)
summary(nifty_healthcare)
summary(nifty_it)
summary(nifty_infra)
# Calculate additional statistics
overallMarket_stats <- nifty_500 %>%
    summarise(
        mean_price = mean(close, na.rm = TRUE),
        median_price = median(close, na.rm = TRUE),
        sd_price = sd(close, na.rm = TRUE),
        range_price = range(close, na.rm = TRUE)
    )
overallMarket_stats
# Repeat for NIFTY AUTO
auto_stats <- nifty_auto %>%
    summarise(
        mean_price = mean(close, na.rm = TRUE),
        median_price = median(close, na.rm = TRUE),
        sd_price = sd(close, na.rm = TRUE),
        range_price = range(close, na.rm = TRUE)
    )
auto_stats
# discriptive statistics for NIFTY Health
healthcare_stats <- nifty_healthcare %>%
    summarise(
        mean_price = mean(close, na.rm = TRUE),
        median_price = median(close, na.rm = TRUE),
        sd_price = sd(close, na.rm = TRUE),
        range_price = range(close, na.rm = TRUE)
    )
healthcare_stats
# discriptive statistics for NIFTY IT
it_stats <- nifty_healthcare %>%
    summarise(
        mean_price = mean(close, na.rm = TRUE),
        median_price = median(close, na.rm = TRUE),
        sd_price = sd(close, na.rm = TRUE),
        range_price = range(close, na.rm = TRUE)
    )
it_stats
# discriptive statistics for NIFTY INFRA
infra_stats <- nifty_infra %>%
    summarise(
        mean_price = mean(close, na.rm = TRUE),
        median_price = median(close, na.rm = TRUE),
        sd_price = sd(close, na.rm = TRUE),
        range_price = range(close, na.rm = TRUE)
    )
infra_stats
# discriptive statistics for NIFTY BANK
bank_stats <- nifty_bank %>%
    summarise(
        mean_price = mean(close, na.rm = TRUE),
        median_price = median(close, na.rm = TRUE),
        sd_price = sd(close, na.rm = TRUE),
        range_price = range(close, na.rm = TRUE)
    )
bank_stats
# creating Histogram of banking sector
ggplot(nifty_bank, aes(x = close)) +
    geom_histogram(binwidth = 50, fill = "blue", alpha = 0.7) +
    labs(
        title = "Distribution of Closing Prices: Banking Sector", x =
            "Closing Price", y = "Frequency"
    ) +
    theme_minimal()
# Histogram of the automotive sector:
ggplot(nifty_auto, aes(x = close)) +
    geom_histogram(binwidth = 50, fill = "blue", alpha = 0.7) +
    labs(
        title = "Distribution of Closing Prices: Automotive sector", x =
            "Closing Price", y = "Frequency"
    ) +
    theme_minimal()
# Histogram of Healthcare Sector:
ggplot(nifty_healthcare, aes(x = close)) +
    geom_histogram(binwidth = 50, fill = "blue", alpha = 0.7) +
    labs(
        title = "Distribution of Closing Prices: HealthCare Sector", x =
            "Closing Price", y = "Frequency"
    ) +
    theme_minimal()
# Histogram of IT Sector:
ggplot(nifty_it, aes(x = close)) +
    geom_histogram(binwidth = 50, fill = "blue", alpha = 0.7) +
    labs(title = "Distribution of Closing Prices: IT Sector", x = "Closing
Price", y = "Frequency") +
    theme_minimal()
# Histogram of Infrastructure Sector:
ggplot(nifty_infra, aes(x = close)) +
    geom_histogram(binwidth = 50, fill = "blue", alpha = 0.7) +
    labs(
        title = "Distribution of Closing Prices: Infrastructure Sector",
        x = "Closing Price", y = "Frequency"
    ) +
    theme_minimal()
# Line Plot for Banking Sector:
nifty_bank$date <- as.Date(nifty_bank$date, format = "%d %b %Y")
ggplot(nifty_bank, aes(x = date, y = close)) +
    geom_line(color = "blue") +
    labs(
        title = "Banking Sector Closing Prices Over Time", x = "Date",
        y = "Closing Price"
    ) +
    theme_minimal()
# Line Plot for Automotive Sector:
nifty_auto$date <- as.Date(nifty_auto$date, format = "%d %b %Y")
ggplot(nifty_auto, aes(x = date, y = close)) +
    geom_line(color = "blue") +
    labs(
        title = "Automotive Sector Closing Prices Over Time", x = "Date",
        y = "Closing Price"
    ) +
    theme_minimal()
# Line Plot for Healthcare Sector:
nifty_healthcare$date <- as.Date(nifty_healthcare$date, format = "%d %b
%Y")
ggplot(nifty_healthcare, aes(x = date, y = close)) +
    geom_line(color = "blue") +
    labs(
        title = "HealthCare Sector Closing Prices Over Time", x = "Date",
        y = "Closing Price"
    ) +
    theme_minimal()
# Line Plot for IT Sector:
nifty_it$date <- as.Date(nifty_it$date, format = "%d %b %Y")
ggplot(nifty_it, aes(x = date, y = close)) +
    geom_line(color = "blue") +
    labs(
        title = "Information Technology Sector Closing Prices Over Time",
        x = "Date", y = "Closing Price"
    ) +
    theme_minimal()
# Line Plot for Infrastructure
nifty_infra$date <- as.Date(nifty_infra$date, format = "%d %b %Y")
ggplot(nifty_infra, aes(x = date, y = close)) +
    geom_line(color = "blue") +
    labs(
        title = "Infrastructure Sector Closing Prices Over Time", x =
            "Date", y = "Closing Price"
    ) +
    theme_minimal()
# Box Plot Comparision of different Sectors:
# Convert `open`, `high`, `low`, and `close` to numeric type in all
datasets
nifty_bank <- nifty_bank %>%
    mutate(
        open = as.numeric(open),
        high = as.numeric(high),
        low = as.numeric(low),
        close = as.numeric(close)
    )
nifty_auto <- nifty_auto %>%
    mutate(
        open = as.numeric(open),
        high = as.numeric(high),
        low = as.numeric(low),
        close = as.numeric(close)
    )
nifty_healthcare <- nifty_healthcare %>%
    mutate(
        open = as.numeric(open),
        high = as.numeric(high),
        low = as.numeric(low),
        close = as.numeric(close)
    )
nifty_it <- nifty_it %>%
    mutate(
        open = as.numeric(open),
        high = as.numeric(high),
        low = as.numeric(low),
        close = as.numeric(close)
    )
nifty_infra <- nifty_infra %>%
    mutate(
        open = as.numeric(open),
        high = as.numeric(high),
        low = as.numeric(low),
        close = as.numeric(close)
    )

# Combine datasets for a comparative boxplot
nifty_bank$sector <- "Banking"
nifty_auto$sector <- "Auto"
nifty_healthcare$sector <- "Health"
nifty_it$sector <- "IT"
nifty_infra$sector <- "Infra"
# Combine datasets
combined_data <- bind_rows(
    nifty_bank, nifty_auto, nifty_healthcare,
    nifty_it, nifty_infra
)
# Create the boxplot
ggplot(combined_data, aes(x = sector, y = close, fill = sector)) +
    geom_boxplot() +
    labs(
        title = "Closing Price Distribution Across Sectors",
        x = "Sector",
        y = "Closing Price"
    ) +
    theme_minimal()
# Linear regression model for NIFTY BANK
commodities_model <- lm(close ~ open + high + low, data = nifty_bank)
summary(commodities_model)
# Visualization of regression line for NIFTY BANK
ggplot(nifty_bank, aes(x = open, y = close)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", color = "red") +
    labs(
        title = "Regression: Open vs Closing Price (Banking Sector)", x =
            "Open", y = "Close"
    ) +
    theme_minimal()
# Linear regression model for NIFTY AUTO
commodities_model <- lm(close ~ open + high + low, data = nifty_auto)
summary(commodities_model)
# Visualization of regression line for NIFTY Automotive
ggplot(nifty_auto, aes(x = open, y = close)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", color = "red") +
    labs(
        title = "Regression: Open vs Closing Price (Automotive Sector)",
        x = "Open", y = "Close"
    ) +
    theme_minimal()
# Linear regression model for NIFTY HEALTHCARE
commodities_model <- lm(close ~ open + high + low,
    data =
        nifty_healthcare
)
summary(commodities_model)
# Visualization of regression line for NIFTY Healthcare
ggplot(nifty_healthcare, aes(x = open, y = close)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", color = "red") +
    labs(
        title = "Regression: Open vs Closing Price (Healthcare Sector)",
        x = "Open", y = "Close"
    ) +
    theme_minimal()
# Linear regression model for NIFTY IT
commodities_model <- lm(close ~ open + high + low, data = nifty_it)
summary(commodities_model)
# Visualization of regression line for NIFTY IT
ggplot(nifty_it, aes(x = open, y = close)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", color = "red") +
    labs(
        title = "Regression: Open vs Closing Price (IT Sector)", x =
            "Open", y = "Close"
    ) +
    theme_minimal()
# Linear regression model for NIFTY Infra
commodities_model <- lm(close ~ open + high + low, data = nifty_infra)
summary(commodities_model)
# Visualization of regression line for NIFTY Infrastructure
ggplot(nifty_infra, aes(x = open, y = close)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", color = "red") +
    labs(title = "Regression: Open vs Closing Price (Infrastructure
Sector)", x = "Open", y = "Close") +
    theme_minimal()

# Corelation analysis Between NIFTY 500 and and different sectors:
# Define lockdown periods
lockdown_1_start <- as.Date("2020-03-25")
lockdown_1_end <- as.Date("2020-05-31")
lockdown_2_start <- as.Date("2021-04-14")
lockdown_2_end <- as.Date("2021-05-31")
# Add a lockdown indicator column for each dataset
add_lockdown_indicator <- function(data) {
    data <- data %>%
        mutate(
            lockdown = ifelse(
                (date >= lockdown_1_start & date <= lockdown_1_end) |
                    (date >= lockdown_2_start & date <= lockdown_2_end), 1, 0
            )
        )
    return(data)
}
# Convert the `date` column to Date type for all datasets
nifty_auto$date <- as.Date(nifty_auto$date, format = "%d %b %Y")
nifty_bank$date <- as.Date(nifty_bank$date, format = "%d %b %Y")
nifty_healthcare$date <- as.Date(nifty_healthcare$date, format = "%d %b %Y")
nifty_it$date <- as.Date(nifty_it$date, format = "%d %b %Y")
nifty_infra$date <- as.Date(nifty_infra$date, format = "%d %b %Y")
nifty_500$date <- as.Date(nifty_500$date, format = "%d %b %Y")

nifty_auto <- add_lockdown_indicator(nifty_auto)
nifty_bank <- add_lockdown_indicator(nifty_bank)
nifty_healthcare <- add_lockdown_indicator(nifty_healthcare)
nifty_it <- add_lockdown_indicator(nifty_it)
nifty_infra <- add_lockdown_indicator(nifty_infra)
nifty_500 <- add_lockdown_indicator(nifty_500)

# Function to calculate daily volatility
calculate_volatility <- function(data) {
    data <- data %>%
        arrange(date) %>%
        mutate(volatility = (close - lag(close)) / lag(close) * 100)
    return(data)
}
nifty_auto <- calculate_volatility(nifty_auto)
nifty_bank <- calculate_volatility(nifty_bank)
nifty_healthcare <- calculate_volatility(nifty_healthcare)
nifty_it <- calculate_volatility(nifty_it)
nifty_infra <- calculate_volatility(nifty_infra)
nifty_500 <- calculate_volatility(nifty_500)

# Correlation between lockdown and volatility for NIFTY AUTO
cor_auto <- cor(nifty_auto$lockdown, nifty_auto$volatility,
    use =
        "complete.obs"
)

# Correlation for NIFTY BANK
cor_bank <- cor(nifty_bank$lockdown, nifty_bank$volatility,
    use = "complete.obs"
)

# Correlation for NIFTY HEALTHCARE
cor_healthcare <- cor(nifty_healthcare$lockdown,
    nifty_healthcare$volatility,
    use = "complete.obs"
)

# Correlation for NIFTY IT
cor_it <- cor(nifty_it$lockdown, nifty_it$volatility, use = "complete.obs") 
 
# Correlation for NIFTY INFRA 
cor_infra <- cor(nifty_infra$lockdown, nifty_infra$volatility, use = "complete.obs") 
 
# Correlation for NIFTY 500 
cor_all <- cor(nifty_500$lockdown, nifty_500$volatility, use = "complete.obs") 
 
# Print results 
cat("Correlation between lockdowns and volatility:\n") 
cat("NIFTY AUTO:", cor_auto, "\n") 
cat("NIFTY BANK:", cor_bank, "\n") 
cat("NIFTY HEALTHCARE:", cor_healthcare, "\n") 
cat("NIFTY IT:", cor_it, "\n") 
cat("NIFTY INFRA:", cor_infra, "\n") 
cat("NIFTY 500:", cor_all, "\n")
# Boxplot  of volatility during lockdown across sectors: 
# Add sector labels to each dataset 
nifty_auto$sector <- "AUTO" 
nifty_bank$sector <- "BANK" 
nifty_it$sector <- "IT" 
nifty_healthcare$sector <- "HEALTHCARE" 
nifty_infra$sector <- "INFRA" 
 
# Combine all datasets 
combined_data <- bind_rows(nifty_auto, nifty_bank, nifty_it, 
nifty_healthcare, nifty_infra) 
 
# Plot volatility during lockdown across sectors 
ggplot(combined_data, aes(x = factor(lockdown), y = volatility, fill = 
factor(lockdown))) + 
  geom_boxplot() +
  facet_wrap(~ sector, scales = "free_y") + 
labs( 
title = "Volatility During and Outside Lockdown Across Sectors", 
x = "Lockdown (0 = No, 1 = Yes)", 
y = "Volatility (%)" 
) + 
scale_fill_manual(values = c("0" = "blue", "1" = "red"), labels = 
c("Outside Lockdown", "During Lockdown")) + 
theme_minimal() + 
theme( 
strip.text = element_text(size = 12, face = "bold"), 
legend.title = element_blank() 
) 
# Function to calculate skewness and kurtosis 
calculate_distribution_metrics <- function(data, column) { 
  skew <- skewness(data[[column]], na.rm = TRUE) 
  kurt <- kurtosis(data[[column]], na.rm = TRUE) 
  return(data.frame(Skewness = skew, Kurtosis = kurt)) 
} 
 
# Skewness and kurtosis for closing prices 
price_metrics_auto <- calculate_distribution_metrics(nifty_auto, 
"close") 
price_metrics_bank <- calculate_distribution_metrics(nifty_bank, 
"close") 
price_metrics_it <- calculate_distribution_metrics(nifty_it, "close") 
price_metrics_healthcare <- 
calculate_distribution_metrics(nifty_healthcare, "close") 
price_metrics_infra <- calculate_distribution_metrics(nifty_infra, 
"close")
 
# Print the results for each sector 
print("Closing Price Metrics:") 
print(paste("NIFTY AUTO: ", price_metrics_auto)) 
print(paste("NIFTY BANK: ", price_metrics_bank))   
print(paste("NIFTY IT: ", price_metrics_it)) 
print(paste("NIFTY HEALTHCARE: ", price_metrics_healthcare)) 
print(paste("NIFTY INFRA: ", price_metrics_infra)) 
# Convert data frame to a string and use `cat()` 
cat("Closing Price Metrics:\n") 
Closing Price Metrics: 
cat("NIFTY AUTO: ", paste("Skewness:", price_metrics_auto$Skewness, 
"Kurtosis:", price_metrics_auto$Kurtosis), "\n") 
cat("NIFTY BANK: ", paste("Skewness:", price_metrics_bank$Skewness, 
"Kurtosis:", price_metrics_bank$Kurtosis), "\n") 
cat("NIFTY IT: ", paste("Skewness:", price_metrics_it$Skewness, 
"Kurtosis:", price_metrics_it$Kurtosis), "\n") 
cat("NIFTY HEALTHCARE: ", paste("Skewness:", 
price_metrics_healthcare$Skewness, "Kurtosis:", 
price_metrics_healthcare$Kurtosis), "\n") 
cat("NIFTY INFRA: ", paste("Skewness:", price_metrics_infra$Skewness, 
"Kurtosis:", price_metrics_infra$Kurtosis), "\n") 
# Check for missing or non-finite values 
sum(is.na(nifty_auto$volatility))       # Missing values in volatility 
sum(is.infinite(nifty_auto$volatility)) # Infinite values in volatility 
sum(is.na(nifty_auto$close))            # Missing values in closing 
prices 
# Remove rows with non-finite or missing values 
nifty_auto <- nifty_auto %>% 
  filter(is.finite(volatility) & !is.na(volatility) & is.finite(close) 
& !is.na(close)) 
nifty_bank <- nifty_bank %>% 
  filter(is.finite(volatility) & !is.na(volatility) & is.finite(close) 
& !is.na(close)) 
nifty_it <- nifty_it %>% 
  filter(is.finite(volatility) & !is.na(volatility) & is.finite(close) 
& !is.na(close)) 
nifty_healthcare <- nifty_healthcare %>% 
  filter(is.finite(volatility) & !is.na(volatility) & is.finite(close) 
& !is.na(close)) 
nifty_infra <- nifty_infra %>% 
  filter(is.finite(volatility) & !is.na(volatility) & is.finite(close) 
& !is.na(close))
# Plot histogram for volatility 
ggplot(combined_data, aes(x = volatility, fill = sector)) + 
geom_histogram(binwidth = 1, alpha = 0.7, show.legend = FALSE) + 
facet_wrap(~ sector, scales = "free_y") + 
labs( 
title = "Distribution of Volatility Across Sectors", 
x = "Volatility (%)", 
y = "Frequency" 
) + 
theme_minimal() + 
theme(strip.text = element_text(size = 12, face = "bold")) 
# Plot density for closing prices 
ggplot(combined_data, aes(x = close, fill = sector)) + 
geom_density(alpha = 0.7, show.legend = FALSE) + 
facet_wrap(~ sector, scales = "free_y") + 
labs( 
title = "Density of Closing Prices Across Sectors", 
x = "Closing Price", 
y = "Density" 
) + 
theme_minimal() + 
theme(strip.text = element_text(size = 12, face = "bold")) 