# Lim Eazen, TP078935
# Brayden Yoong Policarpio , TP079094
# Manreen Kaur A/P Jagjit Singh , TP071290
# Ong Kai Ying, TP086065
# Manreen Kaur A/P Jagjit Singh (TP071290):
# Objective: To analyze trends in web hacking activities by examining web server distributions, ransom attack occurrences, and downtime across different countries. The goal is to identify which web servers and countries experience the most attacks and prolonged downtime, providing insights for cybersecurity improvements.
# Analysis:
# Analysis 1: (2D & 3D Lollipop Charts: Web Server): Identifies the most commonly used web servers.
# Analysis 2: (Barchart: Web Servers and Ransom Attacks): Analyzes which web servers are most vulnerable to attacks.
# Analysis 3: (Histogram: Country and Downtime): Shows which countries experience the highest downtime due to cyberattacks.
# Analysis 4: (Pie Chart: Country and Ransom Attacks): Top 5 Countries with highest Ransom Attacks
# Analysis 5: (Bar Chart & Line Graph: Web Server, Ransom Attacks and Downtime): Examines the correlation between ransom attacks and downtime for different web servers.
# Analysis 6: (Clustered Bar Chart & Line Graph: Web Server, Ransom Attacks, Downtime and Country): Extends Analysis 4 by adding the country factor, providing regional insights into cyber threats.

# ==============================================
# DATA CLEANING AND EXPLORATORY ANALYSIS SCRIPT
# ==============================================

# 1. Install & Load Required libraries
# Install the libraries
install.packages("stringr")     # Install stringr for string processing
install.packages("ggplot2")     # Install ggplot2 for visualization
install.packages("ggpubr", dependencies = TRUE)      
install.packages("corrplot", dependencies = TRUE)    
install.packages("reshape2", dependencies = TRUE) 
install.packages("ggridges")
install.packages("dplyr")      # Install dplyr if not already installed
install.packages("tidyr")      # Install tidyr for data manipulation
install.packages("stringdist")
install.packages("ggrepel")
install.packages("plotly")
if (!requireNamespace("countrycode", quietly = TRUE)) install.packages("countrycode")
install.packages("knitr")  # For kable to display table
install.packages("DT")     # For interactive tables
install.packages("tidyverse")
install.packages("reader")
nstall.packages("ggforce")
install.packages("viridis")
install.packages("lubridate")
install.packages("broom")

# Load the Libraries
library(stringr)
library(ggplot2)
library(ggpubr)
library(corrplot)
library(reshape2)
library(ggridges)
library(dplyr)
library(tidyr)
library(stringdist)
library(ggrepel)
library(plotly)
library(countrycode)
library(knitr)
library(DT)
library(tidyverse)
library(readr)

# 2. Import original data
setwd("C:/Users/Manreen/OneDrive/Documents/R PFDA ASSIGNMENT")
data <- read.csv("4.hackingdata.csv")

# 3. Inspect data structure
str(data)   # Check the structure of the dataset
dim(data)   # Check the number of rows and columns
head(data)  # View the first few rows
summary(data) 

# 4. Data cleaning
# Convert relevant columns to numeric, removing special characters
colnames(data)
names(data) <- trimws(names(data))  # Remove extra spaces
names(data) <- tolower(names(data)) # Convert to lowercase for consistency
data <- read.csv("4.hackingdata.csv")

data <- data %>%
  mutate(
    Ransom = as.numeric(gsub("[^0-9.]", "", as.character(Ransom))),
    Loss = as.numeric(gsub("[^0-9.]", "", as.character(Loss)))
  )

# Replace only NA and empty values in Ransom with the mean, keeping existing values unchanged
if ("Ransom" %in% colnames(data)) {
  ransom_mean <- mean(data$Ransom, na.rm = TRUE)  # Calculate mean excluding NAs
  data$Ransom[is.na(data$Ransom) | data$Ransom == ""] <- ransom_mean  # Replace only NA and empty values
}

# Handle missing values by replacing them with the mean
num_cols <- sapply(data, is.numeric)
data[, num_cols] <- lapply(data[, num_cols], function(col) {
  ifelse(is.na(col), mean(col, na.rm = TRUE), col)
})

# Fill empty categorical values with the most frequent value
fill_most_frequent <- function(column) {
  column[column == ""] <- NA
  column[is.na(column)] <- names(which.max(table(column)))
  return(column)
}

data$Lang <- fill_most_frequent(data$Lang)
data$WebServer <- fill_most_frequent(data$WebServer)
data$Country <- fill_most_frequent(data$Country)

# Convert Date column to proper format
data$Date <- as.Date(data$Date, format = "%d/%m/%Y")

# Get a list of valid country names
Country <- unique(countrycode::codelist$country.name.en)

# Sample data with incorrect country names and missing spaces
df <- data.frame(
  Country = c("Untied States", "UNITED STATE", "Germnay", "Barzil", "RUSSIAN FEDE",   
              "UNITED KINGD", "NETHERLANDS\"", "NEWZEALAND", "PUERTORICO", "CZECH REPUBL",
              "SOUTHAMERICA", "VIET NAM"),  
  count = c(1555, 257, 6908, 6625, 68, 257, 75, 77, 10, 23, 100, 200)  
)

# Function to find the closest correct country name
find_closest_match <- function(country) {  
  distances <- stringdist::stringdist(country, Country, method = "jw")  # Jaro-Winkler similarity  
  return(Country[which.min(distances)])  # Return the closest match  
}  

# Apply function to correct country names
df$Country <- sapply(df$Country, find_closest_match)

# Define a dictionary mapping countries to their primary languages
Country_to_lang <- c(
  "UNITED STATES" = "English",
  "GERMANY" = "German",
  "BRAZIL" = "Portuguese",
  "RUSSIA" = "Russian",
  "UNITED KINGDOM" = "English",
  "NETHERLANDS" = "Dutch",
  "NEW ZEALAND" = "English",
  "PUERTO RICO" = "Spanish",
  "CZECH REPUBLIC" = "Czech",
  "SOUTH AMERICA" = "Spanish",
  "VIETNAM" = "Vietnamese"
)

# Ensure country names are in uppercase to match the dictionary keys
data$Country <- toupper(data$Country)

# Fix spacing issues (e.g., SOUTHAMERICA → SOUTH AMERICA)
df$Country <- gsub("([A-Z])([A-Z]+)", "\\1 \\2", df$Country)

# Map corrected country names to languages
df$lang <- Country_to_lang[df$Country]

# Handle missing language values by assigning the most frequent language
if (sum(!is.na(df$lang)) > 0) {  # Check if there are non-NA values
  most_frequent_lang <- names(which.max(table(na.omit(df$lang))))  # Get most frequent language
  df$lang[is.na(df$lang)] <- most_frequent_lang  # Assign to missing values
} else {
  df$lang[is.na(df$lang)] <- "UNKNOWN"  # Fallback if all values are NA
}

# Convert all text to uppercase
df$Country <- toupper(df$Country)
df$lang <- toupper(df$lang)

# Remove duplicate entries
data <- unique(data)

# 5. Outlier detection and handling

# Define outlier detection function first
outlier_detection <- function(column) {
  if (sum(!is.na(column)) == 0) {  # If all values are NA, return column unchanged
    return(column)
  }
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  mean <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * mean
  upper_bound <- Q3 + 1.5 * mean
  column[column < lower_bound | column > upper_bound] <- NA
  
# Replace outliers with mean to maintain the same length
  return(ifelse(is.na(column), mean(column, na.rm = TRUE), column))
}

# Ensure column names exist
colnames(data)

# Check missing values and total rows
sum(is.na(data$Downtime))  # Count NA values
length(data$Downtime)
sum(is.na(data$Ransom))    # Count NA values
length(data$Downtime)

# Apply outlier detection only if the column exists
if ("Downtime" %in% colnames(data)) {
  data$Downtime <- outlier_detection(data$Downtime)
}

# Replace missing values with mean
if ("Downtime" %in% colnames(data)) {
  data$Downtime[is.na(data$Downtime)] <- mean(data$Downtime, na.rm = TRUE)
}

if ("Ransom" %in% colnames(data)) {
  data$Ransom[is.na(data$Ransom)] <- mean(data$Ransom, na.rm = TRUE)
}

# Ensure no negative or zero values
if ("Ransom" %in% colnames(data)) {
  data$Ransom[data$Ransom <= 0 | is.na(data$Ransom)] <- NA
}

if ("Downtime" %in% colnames(data)) {
  data$Downtime[data$Downtime <= 0 | is.na(data$Downtime)] <- NA
}

# Ensure num_cols exists before applying the function
if (exists("num_cols")) {
  data[, num_cols] <- lapply(data[, num_cols], function(col) {
    ifelse(is.na(col), mean(col, na.rm = TRUE), col)
  })
}

# Check summary of Downtime to confirm no errors
summary(data$Downtime)

# 6. LOG TRANSFORMATION FOR LARGE VALUES
# Rename column if necessary
colnames(data)[colnames(data) == "DownTime"] <- "Downtime"

# Convert Downtime and Ransom to numeric, removing non-numeric characters
data$Downtime <- as.numeric(gsub("[^0-9.]", "", as.character(data$Downtime)))
data$Ransom <- as.numeric(gsub("[^0-9.]", "", as.character(data$Ransom)))

# Print summary to verify transformations
summary(data$Downtime)
summary(data$Ransom)

# Ensure corrected_country exists in data
if (!"Country" %in% colnames(data)) {
  data$Country <- toupper(data$Country)  # Use existing 'Country' column
}

# Now count occurrences and sort
data %>%
  count(Country) %>%
  arrange(desc(n))

# Step 7: Save cleaned data
# Saved cleaned data in the same csv file
write.csv(data, "4.hackingdata.csv", row.names = FALSE)

# Display summary statistics
summary(data)

# After saving data.csv, verify it by running
new_data <- read.csv("4.hackingdata.csv")
str(new_data)
head(new_data)


# OBJECTIVE: To analyze trends in web hacking activities by examining web server distributions, ransom attack occurrences, and downtime across different countries. The goal is to identify which web servers and countries experience the most attacks and prolonged downtime. 
# Manreen Kaur A/P Jagjit Singh (TP071290) Analysis & Conclusion

# Analysis
# 3.1.1 Analysis 1-1.1: Lollipop Chart: The distribution of Top 10 Webserver based on Count and Percentage (2D Graph)
# Load required libraries
library(ggplot2)
library(dplyr)

# Ensure dataset exists
if (!exists("data")) {
  stop("Error: 'data' does not exist. Please load or create the dataset.")
}

# Ensure 'WebServer' column exists
if (!"WebServer" %in% colnames(data)) {
  stop("Error: 'WebServer' column does not exist in 'data'. Please check the dataset.")
}

# Replace empty strings and missing values with "Unknown"
data$WebServer[is.na(data$WebServer) | data$WebServer == ""] <- "Unknown"

# Create a mapping of web servers to their short forms
short_names <- list(
  "Apache" = "Apa", "nginx" = "Ngx", "LiteSpeed" = "LSpd", 
  "IIS/4.0" = "IIS4", "IIS/5.0" = "IIS5", "IIS/5.1" = "IIS5_1", "IIS/6.0" = "IIS6", "IIS/7.0" = "IIS7",
  "IIS/7.5" = "IIS7_5", "IIS/8.0" = "IIS8", "IIS/8.5" = "IIS8_5", "IIS/9.0" = "IIS9", "IIS" = "IIS",
  "GWS" = "GWS", "Zeus" = "Zeu", "Tomcat" = "Tmc", "SunONE WebServer" = "SunONE",
  "IBM_HTTP_Server" = "IBM", "Oracle AS" = "OraAS", "Oracle-iPlanet-Web-Server/7.0" = "OraIPWS",
  "Microsoft-IIS/9.0 mod_ruid2/0.9" = "IIS9_mod", "CoffeeMaker" = "CofMk",
  "WebServer" = "WebS", "IdeaWebServer" = "IdWS", "BigIP" = "BIGIP",
  "Power MOD by web4host.net" = "PModW", "cloudflare-nginx" = "CFNgx",
  "SamBar" = "SamB", "YTS" = "YTS", "lighttpd" = "Ltpd", "ATS" = "ATS",
  "Varnish" = "Vrn", "RedHatVN config by info@redhatvn.net" = "RHVN",
  "Rackcorpcdn/2.1" = "RackCDN", "Tengine" = "Tng", "CherryPy/2.3.0" = "ChPy",
  "Squid" = "Sq", "Resin" = "Res", "Caddy" = "Cdy", "Zope" = "Zop",
  "Lotus-Domino" = "LotD", "MiniGun" = "MnGn", "WP Engine/4.0" = "WPE",
  "Oversee Turing v1.0.0" = "OvTur", "X-Artvisual-serverlinux15.artvisual.net" = "XArt",
  "Sputnik2.aus" = "Sp2", "Sputnik6.aus" = "Sp6", "sputnik3.aus" = "Sp3",
  "sputnik4.aus" = "Sp4", "SCICUBE_LIMITED" = "SCI", "Safe3 Web Firewall" = "S3WF",
  "TB-Server" = "TBs", "X-KRYPTON" = "X-K", "fs5" = "FS5", "mBog" = "mBog",
  "PNS Box HTTP Proxy Service" = "PNSBox", "Depdiknas WebServer/1.0" = "DepWS",
  "Secured By FS4HOST.COM" = "FS4H", "Dimofinf Hosting" = "DimH",
  "Hosting" = "Host", "Dungeon9" = "Dgn9", "ExaServer" = "ExaS",
  "NOYB" = "NOYB", "Unknown" = "N/A"
)

# Convert web server names to short forms
data$WebServer <- sapply(data$WebServer, function(x) {
  if (!is.null(short_names[[x]])) { 
    return(short_names[[x]])
  } else {
    return(substr(x, 1, 5))  
  }
})

# Count occurrences of each web server
webserver_counts <- data %>%
  group_by(WebServer) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Identify top web servers based on occurrence count
threshold <- 10  # Number of web servers to keep
top_webservers <- webserver_counts %>%
  top_n(threshold, Count) %>%
  pull(WebServer)

# Group all other web servers into "OTHERS"
data$WebServer <- ifelse(data$WebServer %in% top_webservers, data$WebServer, "OTHERS")

# Merge "N/A" into "OTHERS"
data$WebServer[data$WebServer == "N/A"] <- "OTHERS"

# Recount after grouping
webserver_counts <- data %>%
  group_by(WebServer) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Convert WebServer to a factor with proper ordering
data$WebServer <- factor(data$WebServer, levels = c(top_webservers, "OTHERS"))

# Compute percentages
webserver_counts <- webserver_counts %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the lollipop chart
p <- ggplot(webserver_counts, aes(x = reorder(WebServer, Count), y = Count)) +
  geom_segment(aes(xend = WebServer, yend = 0), color = "black") +
  geom_point(color = "darkmagenta", size = 4) +
  geom_text(aes(label = paste0(Count, " (", round(Percentage, 1), "%)")), 
            hjust = -0.2, size = 2.5, check_overlap = TRUE) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 125000, by = 25000), expand = expansion(mult = c(0, 0.1))) +  
  labs(title = "Web Server Distribution - Lollipop Chart", x = "Web Server", y = "Count") +
  theme_minimal() +
  theme(plot.margin = margin(10, 10, 10, 10),
        axis.line = element_line(color = "black"))  # Make x and y axis visible

# Print the plot
print(p)

# Analysis 1-1.2: Lollipop Chart: The distribution of Top 5 Webserver based on Count and Percentage (3D Graph) 
# Load necessary libraries
library(plotly)
library(dplyr)

# Ensure dataset exists
if (!exists("data")) {
  stop("Error: 'data' does not exist. Please load or create the dataset.")
}

# Ensure 'WebServer' column exists
if (!"WebServer" %in% colnames(data)) {
  stop("Error: 'WebServer' column does not exist in 'data'. Please check the dataset.")
}

# Replace empty strings and missing values with "Unknown"
data$WebServer[is.na(data$WebServer) | data$WebServer == ""] <- "Unknown"

# Create a mapping of web servers to their short forms
short_names <- list(
  "Apache" = "Apa", "nginx" = "Ngx", "LiteSpeed" = "LSpd", 
  "IIS/4.0" = "IIS4", "IIS/5.0" = "IIS5", "IIS/5.1" = "IIS5_1", "IIS/6.0" = "IIS6", "IIS/7.0" = "IIS7",
  "IIS/7.5" = "IIS7_5", "IIS/8.0" = "IIS8", "Unknown" = "N/A"
)

# Convert web server names to short forms
data$WebServer <- sapply(data$WebServer, function(x) {
  if (!is.null(short_names[[x]])) { 
    return(short_names[[x]])
  } else {
    return("OTHERS")  # Group all unknown web servers into "OTHERS"
  }
})

# Merge "N/A" (Unknown) into "OTHERS"
data$WebServer[data$WebServer == "N/A"] <- "OTHERS"

# Count occurrences of each web server
webserver_counts <- data %>%
  group_by(WebServer) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Identify the top web servers (same as the 2D version)
threshold <- 10  # Number of web servers to keep
top_webservers <- webserver_counts %>%
  top_n(threshold, Count) %>%
  pull(WebServer)

# Group all other web servers into "OTHERS"
data$WebServer <- ifelse(data$WebServer %in% top_webservers, data$WebServer, "OTHERS")

# Merge "N/A" into "OTHERS"
data$WebServer[data$WebServer == "N/A"] <- "OTHERS"

# Recount after grouping
webserver_counts <- data %>%
  group_by(WebServer) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Compute percentages (following the 2D chart logic)
webserver_counts <- webserver_counts %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Dynamically select the top 5 web servers based on count
top_5_webservers <- webserver_counts %>%
  top_n(5, Count) %>%
  arrange(desc(Count))

# Create 3D Lollipop Chart
p <- plot_ly() %>%
  add_trace(
    type = "scatter3d",
    mode = "lines+markers",
    x = ~top_5_webservers$WebServer, 
    y = ~top_5_webservers$Count, 
    z = ~top_5_webservers$Percentage,
    line = list(color = "darkgray", width = 3),  # Connecting line
    marker = list(size = 6, color = "darkmagenta")  # Lollipop head
  ) %>%
  layout(
    title = "Web Server Distribution - 3D Lollipop Chart",
    scene = list(
      xaxis = list(title = "Web Server", tickangle = 45),
      yaxis = list(title = "Count"),
      zaxis = list(title = "Percentage (%)")
    ),
    margin = list(l = 10, r = 10, b = 10, t = 10)
  )

# Print the 3D plot
p


# 3.1.2 Analysis 1-2: Bar Chart: The distribution of Top 10 Webserver across Ransom Attacks
# Load required libraries
library(ggplot2)
library(dplyr)
library(stringr)

# Ensure dataset exists
if (!exists("data")) {
  stop("Error: 'data' does not exist. Please load or create the dataset.")
}

# Ensure 'WebServer' column exists
if (!"WebServer" %in% colnames(data)) {
  stop("Error: 'WebServer' column does not exist in 'data'. Please check the dataset.")
}

# Replace empty strings and missing values with "Unknown"
data$WebServer[is.na(data$WebServer) | data$WebServer == ""] <- "Unknown"

# Create a mapping of web servers to their short forms
short_names <- list(
  "Apache" = "Apa", "nginx" = "Ngx", "LiteSpeed" = "LSpd", 
  "IIS/4.0" = "IIS4", "IIS/5.0" = "IIS5", "IIS/5.1" = "IIS5_1", "IIS/6.0" = "IIS6", "IIS/7.0" = "IIS7",
  "IIS/7.5" = "IIS7_5", "IIS/8.0" = "IIS8", "IIS/8.5" = "IIS8_5", "IIS/9.0" = "IIS9", "IIS" = "IIS",
  "GWS" = "GWS", "Zeus" = "Zeu", "Tomcat" = "Tmc", "SunONE WebServer" = "SunONE",
  "IBM_HTTP_Server" = "IBM", "Oracle AS" = "OraAS", "Oracle-iPlanet-Web-Server/7.0" = "OraIPWS",
  "Microsoft-IIS/9.0 mod_ruid2/0.9" = "IIS9_mod", "CoffeeMaker" = "CofMk",
  "WebServer" = "WebS", "IdeaWebServer" = "IdWS", "BigIP" = "BIGIP",
  "Power MOD by web4host.net" = "PModW", "cloudflare-nginx" = "CFNgx",
  "SamBar" = "SamB", "YTS" = "YTS", "lighttpd" = "Ltpd", "ATS" = "ATS",
  "Varnish" = "Vrn", "RedHatVN config by info@redhatvn.net" = "RHVN",
  "Rackcorpcdn/2.1" = "RackCDN", "Tengine" = "Tng", "CherryPy/2.3.0" = "ChPy",
  "Squid" = "Sq", "Resin" = "Res", "Caddy" = "Cdy", "Zope" = "Zop",
  "Lotus-Domino" = "LotD", "MiniGun" = "MnGn", "WP Engine/4.0" = "WPE",
  "Oversee Turing v1.0.0" = "OvTur", "X-Artvisual-serverlinux15.artvisual.net" = "XArt",
  "Sputnik2.aus" = "Sp2", "Sputnik6.aus" = "Sp6", "sputnik3.aus" = "Sp3",
  "sputnik4.aus" = "Sp4", "SCICUBE_LIMITED" = "SCI", "Safe3 Web Firewall" = "S3WF",
  "TB-Server" = "TBs", "X-KRYPTON" = "X-K", "fs5" = "FS5", "mBog" = "mBog",
  "PNS Box HTTP Proxy Service" = "PNSBox", "Depdiknas WebServer/1.0" = "DepWS",
  "Secured By FS4HOST.COM" = "FS4H", "Dimofinf Hosting" = "DimH",
  "Hosting" = "Host", "Dungeon9" = "Dgn9", "ExaServer" = "ExaS",
  "NOYB" = "NOYB", "Unknown" = "N/A"
)

# Convert web server names to short forms
data$WebServer <- sapply(data$WebServer, function(x) {
  if (!is.null(short_names[[x]])) { 
    return(short_names[[x]])
  } else {
    return(substr(x, 1, 5))  
  }
})

# Count ransom attacks per web server
webserver_counts <- data %>%
  group_by(WebServer) %>%
  summarise(AttackCount = n()) %>%
  arrange(desc(AttackCount))

# Identify web servers with zero attacks
threshold <- 10  # Number of web servers to keep
top_webservers <- webserver_counts %>% top_n(threshold, AttackCount) %>% pull(WebServer)

# Group all other web servers into "OTHERS"
data$WebServer <- ifelse(data$WebServer %in% top_webservers, data$WebServer, "OTHERS")

# Recount after grouping and sort in descending order
final_counts <- data %>%
  group_by(WebServer) %>%
  summarise(AttackCount = n()) %>%
  arrange(desc(AttackCount))  # Sort in descending order

# Set factor levels based on the sorted data to ensure correct ordering in the plot
final_counts$WebServer <- factor(final_counts$WebServer, levels = final_counts$WebServer)

# Convert WebServer to a factor with proper ordering
data$WebServer <- factor(data$WebServer, levels = c(top_webservers, "OTHERS"))

# Define 10 distinct high-contrast colors for WebServers
webserver_colors <- c("darkorchid1", "#FDBF6F", "yellow3", "skyblue2", "#FB9A99", 
                      "palegreen2", "#6A3D9A", "darkturquoise", "springgreen4", "darkblue")

# Create the bar plot
p <- ggplot(final_counts, aes(x = WebServer, y = AttackCount, fill = WebServer)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  scale_fill_manual(values = webserver_colors) +
  labs(title = "Distribution of Ransom Attacks Across Web Servers",
       x = "Web Server (Short Name)",
       y = "Count of Ransom Attacks") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plot
print(p)


# 3.1.3 Analysis 1-3: Histogram:  The distribution of Top 10 Countries across Ransom Attacks 
library(tidyverse)
library(ggplot2)
library(readr)

# Ensure 'Downtime' is numeric
data$Downtime <- as.numeric(data$Downtime)

# Load required libraries
library(ggplot2)
library(dplyr)

# Aggregate total downtime per country
country_downtime <- data %>%
  group_by(Country) %>%
  summarise(Total_Downtime = sum(Downtime, na.rm = TRUE)) %>%
  arrange(desc(Total_Downtime))

# Display the count of downtime per country
print(country_downtime)

# Select the top 10 countries
top_10_countries <- country_downtime %>% top_n(10, Total_Downtime)

# Plot histogram with a suitable scale for y-axis
histogram_plot <- ggplot(top_10_countries, aes(x = reorder(Country, -Total_Downtime), y = Total_Downtime, fill = Country)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Countries with Highest Downtime",
       x = "Country",
       y = "Total Downtime") +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, max(top_10_countries$Total_Downtime), by = 500000)) +  # Custom y-axis breaks
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# Display the plot
print(histogram_plot)


# 3.1.4 Analysis 1-4: Pie Chart: The distribution of Top 5 Countries across Ransom Attacks
library(dplyr)
library(ggplot2)
library(countrycode)
library(knitr)
library(stringdist)

# Create a copy of data to avoid modifying the original
data_pie <- data

# Ensure required columns exist
if (!all(c("Country", "Ransom") %in% colnames(data_pie))) {
  stop("Error: Required columns missing from the dataset.")
}

# Convert country names to uppercase and remove extra spaces
data_pie$Country <- toupper(trimws(data_pie$Country))

# Replace missing, empty, and "UNKNOWN" values
data_pie$Country[data_pie$Country == "" | is.na(data_pie$Country) | data_pie$Country == "UNKNOWN"] <- "UNKNOWN"

# Get a list of valid country names
valid_countries <- unique(countrycode::codelist$country.name.en)

# Function to find closest country match
find_closest_match <- function(country) {
  distances <- stringdist::stringdist(country, valid_countries, method = "jw")  # Jaro-Winkler similarity  
  return(valid_countries[which.min(distances)])  # Return the closest match  
}

# Correct known country name errors
data_pie$Country <- sapply(data_pie$Country, find_closest_match)

# Remove duplicate country entries
data_pie <- data_pie %>%
  distinct()

# Summarize ransom attacks per country
country_ransom_summary <- data_pie %>%
  group_by(Country) %>%
  summarise(TotalRansom = sum(Ransom, na.rm = TRUE)) %>%
  arrange(desc(TotalRansom))

# Select top 5 countries (including UNKNOWN if applicable)
top_5_countries <- country_ransom_summary %>%
  slice_head(n = 5)

# Format ransom values with commas for readability
top_5_countries$Label <- paste0(top_5_countries$Country, " (", format(top_5_countries$TotalRansom, big.mark = ","), ")")

# Define distinct colors for each country
country_colors <- setNames(
  c("violetred3", "darkmagenta", "darkgreen", "darkcyan", "royalblue4"),  # Updated color scheme
  top_5_countries$Label  # Assign colors to formatted labels
)

# Convert country names to factors for ordered plotting
top_5_countries$Label <- factor(top_5_countries$Label, levels = top_5_countries$Label)

# Create the pie chart
pie_chart <- ggplot(top_5_countries, aes(x = "", y = TotalRansom, fill = Label)) +
  geom_bar(stat = "identity", width = 1, color = "black") +  # Creates pie chart
  coord_polar(theta = "y", start = 0) +  # Converts to circular pie chart
  scale_fill_manual(values = country_colors) +  # Apply colors
  theme_void() +  # Remove unnecessary background elements
  labs(title = "Top 5 Countries with Ransom Attacks", fill = "Country (Total Ransom)") +
  theme(legend.position = "right")  # Position legend to the right

# Display the pie chart
print(pie_chart)

# Print summary table with Total Ransom Attacks
top_5_countries %>%
  select(Country, TotalRansom) %>%
  kable(caption = "Ransom Attacks per Country (Top 5)")


# 3.1.5 Analysis 1-5: Bar Chart with Line Graph:  Web Servers and Ransom Attacks based

# Ensure necessary columns exist
if (!all(c("WebServer", "Downtime", "Ransom") %in% colnames(data))) {
  stop("Error: Required columns missing from the dataset.")
}

# Replace missing values in Downtime and Ransom
data$Downtime[is.na(data$Downtime)] <- 0
data$Ransom[is.na(data$Ransom)] <- 0

# Summarize data by WebServer
downtime_summary <- data %>%
  group_by(WebServer) %>%
  summarise(TotalDowntime = sum(Downtime), TotalRansomAttacks = sum(Ransom)) %>%
  arrange(desc(TotalDowntime))

# Print the count and percentage for each WebServer
downtime_summary <- downtime_summary %>%
  mutate(DowntimePercentage = round((TotalDowntime / sum(TotalDowntime)) * 100, 1),
         RansomPercentage = round((TotalRansomAttacks / sum(TotalRansomAttacks)) * 100, 1))

print(downtime_summary)

# Define appropriate y-axis range and breaks
y_max <- max(downtime_summary$TotalDowntime) * 1.1  # Add 10% padding for better visualization
y_breaks <- seq(0, y_max, by = 500000)  # Adjusting interval to 500,000 (or another suitable value)

# Create a stacked bar chart with a line graph and red points
stacked_line_chart <- ggplot(downtime_summary, aes(x = reorder(WebServer, -TotalDowntime))) +
  geom_bar(aes(y = TotalDowntime, fill = factor(TotalRansomAttacks)), stat = "identity") +
  geom_line(aes(y = TotalDowntime, group = 1), color = "darkslateblue", size = 1) +  
  geom_point(aes(y = TotalDowntime), color = "red", size = 1.5) +  # Add red dots
  scale_y_continuous(limits = c(0, y_max), breaks = y_breaks) +  
  labs(title = "Web Servers and Ransom Attacks Based on Downtime",
       x = "Web Server",
       y = "Total Downtime",
       fill = "Number of Ransom Attacks",
       color = "Downtime") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the stacked bar chart with red points on the line graph
print(stacked_line_chart)


# 3.1.6 Analysis 1-6: Clustered Bar Chart with Line Graph: Web Server, Ransom Attacks, Downtime, and Country.
library(dplyr)
library(ggplot2)
library(forcats)
library(knitr)
library(DT)

# Ensure necessary columns exist
if (!all(c("WebServer", "Country", "Downtime", "Ransom") %in% colnames(data))) {
  stop("Error: Required columns missing from the dataset.")
}

# Replace missing values in Downtime and Ransom
data$Downtime[is.na(data$Downtime)] <- 0
data$Ransom[is.na(data$Ransom)] <- 0

# Ensure country names are in uppercase
data$Country <- toupper(data$Country)

# Filter data for United States only
us_data <- data %>% filter(Country == "UNITED STATES")

# Create a mapping of web servers to their short forms
short_names <- c("Apache" = "Apa", "nginx" = "Ngx", "LiteSpeed" = "LSpd", 
                 "IIS/4.0" = "IIS4", "IIS/5.0" = "IIS5", "IIS/5.1" = "IIS5_1", "IIS/6.0" = "IIS6",
                 "IIS/7.0" = "IIS7", "IIS/7.5" = "IIS7_5", "IIS/8.0" = "IIS8", "IIS/8.5" = "IIS8_5",
                 "IIS/9.0" = "IIS9", "GWS" = "GWS", "Zeus" = "Zeu", "Tomcat" = "Tmc", 
                 "SunONE WebServer" = "SunONE", "IBM_HTTP_Server" = "IBM", "Oracle AS" = "OraAS",
                 "Oracle-iPlanet-Web-Server/7.0" = "OraIPWS", "Microsoft-IIS/9.0 mod_ruid2/0.9" = "IIS9_mod",
                 "WebServer" = "WebS", "IdeaWebServer" = "IdWS", "BigIP" = "BIGIP", "Tengine" = "Tng",
                 "Squid" = "Sq", "Resin" = "Res", "Caddy" = "Cdy", "Zope" = "Zop", "Lotus-Domino" = "LotD",
                 "Safe3 Web Firewall" = "S3WF", "Dungeon9" = "Dgn9", "ExaServer" = "ExaS",
                 "NOYB" = "NOYB", "Unknown" = "N/A")

# Rename web servers using short forms
us_data$WebServer <- recode(us_data$WebServer, !!!short_names)

# Summarize data by WebServer
downtime_summary <- us_data %>%
  group_by(WebServer) %>%
  summarise(TotalDowntime = sum(Downtime), TotalRansom = sum(Ransom), .groups = "drop") %>%
  arrange(desc(TotalDowntime)) %>%
  slice_head(n = 10)  # Select top 10 web servers by Downtime

# Define appropriate scale factor for ransom so it fits with downtime
scale_factor <- max(downtime_summary$TotalDowntime, na.rm = TRUE) / max(downtime_summary$TotalRansom, na.rm = TRUE)

# Apply scale factor to TotalRansom for table
downtime_summary$ScaledRansom <- downtime_summary$TotalRansom * scale_factor

# Print the summary table (including scaled ransom values)
summary_table <- downtime_summary %>%
  select(WebServer, TotalDowntime, TotalRansom)

# Display summary as a static table
kable(summary_table, caption = "Summary of WebServer, Ransom, and Downtime (Top 10 in the United States)")

# Alternatively, display an interactive table using DT
datatable(summary_table, options = list(pageLength = 5))

# Define 10 distinct high-contrast colors for WebServers
webserver_colors <- c("#FB9A99", "#FDBF6F", "yellow3", "skyblue2", "darkblue", 
                      "palegreen2", "#6A3D9A", "darkturquoise", "springgreen4", "darkorchid1")

# Assign colors to WebServers
downtime_summary$Color <- webserver_colors[1:nrow(downtime_summary)]

# Add an index column for proper label adjustment
downtime_summary <- downtime_summary %>%
  arrange(desc(TotalDowntime)) %>%
  mutate(Index = row_number())  # Create an index column

# Create a clustered bar chart with line graph (red dot for Ransom)
clustered_bar_line_chart <- ggplot(downtime_summary, aes(x = reorder(WebServer, -TotalDowntime), y = TotalDowntime, fill = WebServer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +  # Bars for Downtime
  geom_line(aes(y = TotalRansom * scale_factor, group = 1), color = "black", size = 1) +  # Red Line for Ransom
  geom_point(aes(y = TotalRansom * scale_factor), color = "red", size = 3) +  # Red Dots for Ransom
  geom_text(aes(x = WebServer, y = TotalRansom * scale_factor, label = TotalRansom), 
            color = "black", 
            vjust = ifelse(downtime_summary$Index == 1, -1.4, -1.8),  # Keep first label at -1.4, adjust others
            angle = ifelse(downtime_summary$Index == 1, 0, 8)) +  # Keep first label at angle 0, others at 8
  scale_fill_manual(values = setNames(downtime_summary$Color, downtime_summary$WebServer)) +  # Apply custom colors
  scale_y_continuous(name = "Total Downtime") +  
  labs(title = "Clustered Bar Chart of Downtime by WebServer (USA) with Ransom Trends",
       x = "Web Server",
       fill = "Web Server",
       caption = "Note: Red line & dots represent ransom attacks (scaled for visualization)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the final graph
print(clustered_bar_line_chart)

total_rows <- nrow(us_data)
print(paste("Total number of rows used in the analysis:", total_rows))


# Conclusion
# 3.1.6 Analysis 1-6: Clustered Bar Chart with Line Graph: Web Server, Ransom Attacks, Downtime, and Country.
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Prevent scientific notation for better readability
options(scipen = 999)

# Ensure necessary columns exist
required_columns <- c("WebServer", "Country", "Downtime", "Ransom")
if (!all(required_columns %in% colnames(data))) {
  stop("Error: Required columns missing from the dataset.")
}

# Convert categorical variables to factors
data <- data %>%
  mutate(Country = as.factor(Country),
         WebServer = as.factor(WebServer))

# ---------------- Chi-Square Test ---------------- #
# Checking if WebServer distribution depends on Country
webserver_country_table <- table(data$WebServer, data$Country)

if (any(webserver_country_table < 5)) {
  chi_sq_test <- if (sum(webserver_country_table) < 200) {
    fisher.test(webserver_country_table)  # Fisher's Exact Test for small tables
  } else {
    chisq.test(webserver_country_table, simulate.p.value = TRUE)  # Simulate p-value if necessary
  }
} else {
  chi_sq_test <- chisq.test(webserver_country_table)  # Standard Chi-Square Test
}
print(chi_sq_test)

# ---------------- T-Test / ANOVA ---------------- #
# Checking normality before deciding test type (limit to 5000 samples for efficiency)
sample_size <- min(5000, nrow(data))

shapiro_ransom <- shapiro.test(sample(data$Ransom, sample_size))$p.value
shapiro_downtime <- shapiro.test(sample(data$Downtime, sample_size))$p.value

# Define appropriate statistical test based on normality
ransom_test <- if (shapiro_ransom > 0.05) {
  aov(Ransom ~ WebServer, data = data)
} else {
  kruskal.test(Ransom ~ WebServer, data = data)
}
print(summary(ransom_test))

downtime_test <- if (shapiro_downtime > 0.05) {
  aov(Downtime ~ WebServer, data = data)
} else {
  kruskal.test(Downtime ~ WebServer, data = data)
}
print(summary(downtime_test))

# ---------------- Correlation Test ---------------- #
# Load necessary library for data.table
library(data.table)

# Convert data to data.table for faster operations
data <- as.data.table(data)

# Remove missing values for accurate correlation analysis
filtered_data <- data[!is.na(Ransom) & !is.na(Downtime)]

# Downsample the data (e.g., take 5000 random rows for testing)
set.seed(42)  # Set seed for reproducibility
sample_size <- min(5000, nrow(filtered_data))  # Limit to 5000 rows
filtered_data_sample <- filtered_data[sample(1:nrow(filtered_data), sample_size), ]

# Use Kendall's correlation for non-parametric data
# Load necessary library for data.table
library(data.table)

# Convert data to data.table for faster operations
data <- as.data.table(data)

# ---------------- Correlation Test ---------------- #
# Remove missing values for accurate correlation analysis
filtered_data <- data[!is.na(Ransom) & !is.na(Downtime)]

# Downsample the data (e.g., take 5000 random rows for testing)
set.seed(42)  # Set seed for reproducibility
sample_size <- min(5000, nrow(filtered_data))  # Limit to 5000 rows
filtered_data_sample <- filtered_data[sample(1:nrow(filtered_data), sample_size), ]

# Use Kendall's correlation for non-parametric data
correlation_test <- cor.test(filtered_data_sample$Ransom, filtered_data_sample$Downtime, method = "kendall")
print(correlation_test)

#=================================================
#EAZEN
#=================================================

# 2. Import original data
data <- read.csv("4.hackingdata.csv")

# 3. Inspect data structure
str(data)   # Check the structure of the dataset
dim(data)   # Check the number of rows and columns
head(data)  # View the first few rows
summary(data) 

# 4. Data cleaning
# Convert relevant columns to numeric, removing special characters

colnames(data)
names(data) <- trimws(names(data))  # Remove extra spaces
names(data) <- tolower(names(data)) # Convert to lowercase for consistency
data <- read.csv("4.hackingdata.csv")

data <- data %>%
  mutate(
    Ransom = as.numeric(gsub("[^0-9.]", "", as.character(Ransom))),
    Loss = as.numeric(gsub("[^0-9.]", "", as.character(Loss)))
  )
data <- read.csv("4.hackingdata.csv")

# Handle missing values by replacing them with the median
num_cols <- sapply(data, is.numeric)
data[, num_cols] <- lapply(data[, num_cols], function(col) {
  ifelse(is.na(col), median(col, na.rm = TRUE), col)
})

# Fill empty categorical values with the most frequent value
fill_most_frequent <- function(column) {
  column[column == ""] <- NA
  column[is.na(column)] <- names(which.max(table(column)))
  return(column)
}

data$Lang <- fill_most_frequent(data$Lang)
data$WebServer <- fill_most_frequent(data$WebServer)
data$Country <- fill_most_frequent(data$Country)

# Convert Date column to proper format
data$Date <- as.Date(data$Date, format = "%d/%m/%Y")

# Get a list of valid country names
Country <- unique(countrycode::codelist$country.name.en)

# Sample data with incorrect country names and missing spaces
df <- data.frame(
  Country = c("Untied States", "UNITED STATE", "Germnay", "Barzil", "RUSSIAN FEDE",   
              "UNITED KINGD", "NETHERLANDS\"", "NEWZEALAND", "PUERTORICO", "CZECH REPUBL",
              "SOUTHAMERICA", "VIET NAM"),  
  count = c(1555, 257, 6908, 6625, 68, 257, 75, 77, 10, 23, 100, 200)  
)

# Function to find the closest correct country name
find_closest_match <- function(country) {  
  distances <- stringdist::stringdist(country, Country, method = "jw")  # Jaro-Winkler similarity  
  return(Country[which.min(distances)])  # Return the closest match  
}  

# Apply function to correct country names
df$Country <- sapply(df$Country, find_closest_match)

# Define a dictionary mapping countries to their primary languages
Country_to_lang <- c(
  "UNITED STATES" = "English",
  "GERMANY" = "German",
  "BRAZIL" = "Portuguese",
  "RUSSIA" = "Russian",
  "UNITED KINGDOM" = "English",
  "NETHERLANDS" = "Dutch",
  "NEW ZEALAND" = "English",
  "PUERTO RICO" = "Spanish",
  "CZECH REPUBLIC" = "Czech",
  "SOUTH AMERICA" = "Spanish",
  "VIETNAM" = "Vietnamese"
)

# Ensure country names are in uppercase to match the dictionary keys
data$Country <- toupper(data$Country)

# Fix spacing issues (e.g., SOUTHAMERICA → SOUTH AMERICA)
df$Country <- gsub("([A-Z])([A-Z]+)", "\\1 \\2", df$Country)

# Map corrected country names to languages
df$lang <- Country_to_lang[df$Country]

# Handle missing language values by assigning the most frequent language
if (sum(!is.na(df$lang)) > 0) {  # Check if there are non-NA values
  most_frequent_lang <- names(which.max(table(na.omit(df$lang))))  # Get most frequent language
  df$lang[is.na(df$lang)] <- most_frequent_lang  # Assign to missing values
} else {
  df$lang[is.na(df$lang)] <- "UNKNOWN"  # Fallback if all values are NA
}

# Convert all text to uppercase
df$Country <- toupper(df$Country)
df$lang <- toupper(df$lang)

# Remove duplicate entries
data <- unique(data)

# 5. Outlier detection and handling

# Define outlier detection function first
outlier_detection <- function(column) {
  if (sum(!is.na(column)) == 0) {  # If all values are NA, return column unchanged
    return(column)
  }
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  column[column < lower_bound | column > upper_bound] <- NA
  
  # Replace outliers with median to maintain the same length
  return(ifelse(is.na(column), median(column, na.rm = TRUE), column))
}

# Ensure column names exist
colnames(data)

# Check missing values and total rows
sum(is.na(data$Downtime))  
length(data$Downtime)      

# Apply outlier detection only if the column exists
if ("Downtime" %in% colnames(data)) {
  data$Downtime <- outlier_detection(data$Downtime)
}

# Replace missing values with median
if ("Downtime" %in% colnames(data)) {
  data$Downtime[is.na(data$Downtime)] <- median(data$Downtime, na.rm = TRUE)
}

if ("Ransom" %in% colnames(data)) {
  data$Ransom <- outlier_detection(data$Ransom)
}

# Ensure no negative or zero values
if ("Ransom" %in% colnames(data)) {
  data$Ransom[data$Ransom <= 0 | is.na(data$Ransom)] <- NA
}

if ("Downtime" %in% colnames(data)) {
  data$Downtime[data$Downtime <= 0 | is.na(data$Downtime)] <- NA
}

# Ensure `num_cols` exists before applying the function
if (exists("num_cols")) {
  data[, num_cols] <- lapply(data[, num_cols], function(col) {
    ifelse(is.na(col), median(col, na.rm = TRUE), col)
  })
}

# Check summary of Downtime to confirm no errors
summary(data$Downtime)

# 6. LOG TRANSFORMATION FOR LARGE VALUES
# Rename column if necessary
colnames(data)[colnames(data) == "DownTime"] <- "Downtime"

# Convert Downtime and Ransom to numeric, removing non-numeric characters
data$Downtime <- as.numeric(gsub("[^0-9.]", "", as.character(data$Downtime)))
data$Ransom <- as.numeric(gsub("[^0-9.]", "", as.character(data$Ransom)))

# Check for missing values
sum(is.na(data$Downtime))  # Count NA values
sum(is.na(data$Ransom))    # Count NA values

# Impute missing values with median
if ("Downtime" %in% colnames(data)) {
  data$Downtime[is.na(data$Downtime)] <- median(data$Downtime, na.rm = TRUE)
}
if ("Ransom" %in% colnames(data)) {
  data$Ransom[is.na(data$Ransom)] <- median(data$Ransom, na.rm = TRUE)
}

# Print summary to verify transformations
summary(data$Downtime)
summary(data$Ransom)

# Ensure `corrected_country` exists in `data`
if (!"Country" %in% colnames(data)) {
  data$Country <- toupper(data$Country)  # Use existing 'Country' column
}

# Now count occurrences and sort
data %>%
  count(Country) %>%
  arrange(desc(n))

# Step 7: Save cleaned data
write.csv(df, "4.hackingdata.csv", row.names = FALSE)

# Display summary statistics
summary(data)





# HYPOTESIS:Hacking Activities have increased over the years, with certain attack types becoming more prevalent due to evolving cyber security vulnerabilities.
# OBJECTIVE: To analyze the most Web Server with most Ransom Attacks and the Downtime based on Countries.
# Manreen Chart Part

# 1.1: 2D Lollipop Chart for Web Server Distribution 
# Load required libraries
library(ggplot2)
library(dplyr)

# Ensure dataset exists
if (!exists("data")) {
  stop("Error: 'data' does not exist. Please load or create the dataset.")
}

# Ensure 'WebServer' column exists
if (!"WebServer" %in% colnames(data)) {
  stop("Error: 'WebServer' column does not exist in 'data'. Please check the dataset.")
}

# Replace empty strings and missing values with "Unknown"
data$WebServer[is.na(data$WebServer) | data$WebServer == ""] <- "Unknown"

# Create a mapping of web servers to their short forms
short_names <- list(
  "Apache" = "Apa", "nginx" = "Ngx", "LiteSpeed" = "LSpd", 
  "IIS/4.0" = "IIS4", "IIS/5.0" = "IIS5", "IIS/5.1" = "IIS5_1", "IIS/6.0" = "IIS6", "IIS/7.0" = "IIS7",
  "IIS/7.5" = "IIS7_5", "IIS/8.0" = "IIS8", "IIS/8.5" = "IIS8_5", "IIS/9.0" = "IIS9", "IIS" = "IIS",
  "GWS" = "GWS", "Zeus" = "Zeu", "Tomcat" = "Tmc", "SunONE WebServer" = "SunONE",
  "IBM_HTTP_Server" = "IBM", "Oracle AS" = "OraAS", "Oracle-iPlanet-Web-Server/7.0" = "OraIPWS",
  "Microsoft-IIS/9.0 mod_ruid2/0.9" = "IIS9_mod", "CoffeeMaker" = "CofMk",
  "WebServer" = "WebS", "IdeaWebServer" = "IdWS", "BigIP" = "BIGIP",
  "Power MOD by web4host.net" = "PModW", "cloudflare-nginx" = "CFNgx",
  "SamBar" = "SamB", "YTS" = "YTS", "lighttpd" = "Ltpd", "ATS" = "ATS",
  "Varnish" = "Vrn", "RedHatVN config by info@redhatvn.net" = "RHVN",
  "Rackcorpcdn/2.1" = "RackCDN", "Tengine" = "Tng", "CherryPy/2.3.0" = "ChPy",
  "Squid" = "Sq", "Resin" = "Res", "Caddy" = "Cdy", "Zope" = "Zop",
  "Lotus-Domino" = "LotD", "MiniGun" = "MnGn", "WP Engine/4.0" = "WPE",
  "Oversee Turing v1.0.0" = "OvTur", "X-Artvisual-serverlinux15.artvisual.net" = "XArt",
  "Sputnik2.aus" = "Sp2", "Sputnik6.aus" = "Sp6", "sputnik3.aus" = "Sp3",
  "sputnik4.aus" = "Sp4", "SCICUBE_LIMITED" = "SCI", "Safe3 Web Firewall" = "S3WF",
  "TB-Server" = "TBs", "X-KRYPTON" = "X-K", "fs5" = "FS5", "mBog" = "mBog",
  "PNS Box HTTP Proxy Service" = "PNSBox", "Depdiknas WebServer/1.0" = "DepWS",
  "Secured By FS4HOST.COM" = "FS4H", "Dimofinf Hosting" = "DimH",
  "Hosting" = "Host", "Dungeon9" = "Dgn9", "ExaServer" = "ExaS",
  "NOYB" = "NOYB", "Unknown" = "N/A"
)

# Convert web server names to short forms
data$WebServer <- sapply(data$WebServer, function(x) {
  if (!is.null(short_names[[x]])) { 
    return(short_names[[x]])
  } else {
    return(substr(x, 1, 5))  
  }
})

# Count occurrences of each web server
webserver_counts <- data %>%
  group_by(WebServer) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Identify top web servers based on occurrence count
threshold <- 10  # Number of web servers to keep
top_webservers <- webserver_counts %>%
  top_n(threshold, Count) %>%
  pull(WebServer)

# Group all other web servers into "OTHERS"
data$WebServer <- ifelse(data$WebServer %in% top_webservers, data$WebServer, "OTHERS")

# Merge "N/A" into "OTHERS"
data$WebServer[data$WebServer == "N/A"] <- "OTHERS"

# Recount after grouping
webserver_counts <- data %>%
  group_by(WebServer) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Convert WebServer to a factor with proper ordering
data$WebServer <- factor(data$WebServer, levels = c(top_webservers, "OTHERS"))

# Compute percentages
webserver_counts <- webserver_counts %>%
  mutate(Percentage = Count / sum(Count) * 100)

#1. Web Server & Attack Frequency (Lollipop Chart)


p1 <- ggplot(webserver_counts, aes(x = reorder(WebServer, Count), y = Count)) +
  geom_segment(aes(xend = WebServer, yend = 0), color = "black") +
  geom_point(color = "darkred", size = 4) +
  geom_text(aes(label = Count), hjust = -0.2, size = 3, check_overlap = TRUE) +
  coord_flip() +
  labs(title = "Web Server vs. Attack Frequency", x = "Web Server", y = "Number of Attacks") +
  theme_minimal()
print(p1)

#2. Web Server & Downtime (Boxplot)
downtime_data <- data %>%
  group_by(WebServer) %>%
  summarise(Average_Downtime = mean(Downtime, na.rm = TRUE)) %>%
  arrange(desc(Average_Downtime))

p2 <- ggplot(downtime_data, aes(x = reorder(WebServer, -Average_Downtime), y = Average_Downtime)) +
  geom_col(fill = "steelblue") +  # Use bar chart instead
  labs(title = "Web Server vs. Downtime", x = "Web Server", y = "Average Downtime (hours)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p2)


#3. Downtime & Country (Bar Chart)

country_downtime <- data %>%
  group_by(Country) %>%
  summarise(Average_Downtime = mean(Downtime, na.rm = TRUE)) %>%
  arrange(desc(Average_Downtime))

p3 <- ggplot(country_downtime, aes(x = reorder(Country, Average_Downtime), y = Average_Downtime)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Average Downtime by Country", x = "Country", y = "Downtime (hours)") +
  theme_minimal()
print(p3)

# Select top 5 longest and 5 shortest downtime countries
top_longest <- country_downtime %>% top_n(10, Average_Downtime)
top_shortest <- country_downtime %>% top_n(-10, Average_Downtime)


# Plot
p3_1 <- ggplot(top_longest, aes(x = reorder(Country, Average_Downtime), y = Average_Downtime)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 10 Longest Downtime by Country", x = "Country", y = "Downtime (hours)") +
  theme_minimal()

print(p3_1)

p3_2 <- ggplot(top_shortest, aes(x = reorder(Country, Average_Downtime), y = Average_Downtime)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 10 Shortest Downtime by Country", x = "Country", y = "Downtime (hours)") +
  theme_minimal()

print(p3_2)

#4. Web Server, Attack Frequency, and Downtime (Scatter Plot)

server_attack_downtime <- data %>%
  group_by(WebServer) %>%
  summarise(Attack_Frequency = n(),
            Average_Downtime = mean(Downtime, na.rm = TRUE)) %>%
  arrange(desc(Attack_Frequency))


p4 <- ggplot(server_attack_downtime, aes(x = Attack_Frequency, y = Average_Downtime, color = WebServer)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Attack Frequency vs Downtime by Web Server", x = "Number of Attacks", y = "Downtime (hours)") +
  theme_minimal()
print(p4)

#5. Web Server, Attack Frequency, Downtime, and Notification Methods (Faceted Bar Chart)

p5_data <- data %>%
  group_by(WebServer, Notify) %>%
  summarise(
    Attack_Frequency = n(),
    Average_Downtime = mean(Downtime, na.rm = TRUE),
    .groups = "drop" # Ensure no unwanted grouping
  )


# Ensure there are no missing values in Average_Downtime
p5_data <- p5_data %>% drop_na(Average_Downtime)

# Create the bar chart
p5 <- ggplot(p5_data, aes(x = WebServer, y = Average_Downtime, fill = Attack_Frequency)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Color scale for Attack Frequency
  labs(title = "Impact of Attack Frequency on Downtime", x = "Web Server", y = "Downtime (hours)") +
  theme_minimal()

print(p5)


calculate_total_downtime <- function(data) {
  total_downtime <- data %>%
    group_by(WebServer) %>%
    summarise(Total_Average_Downtime = sum(Average_Downtime, na.rm = TRUE))
  
  return(total_downtime)
}

#NOT WORKING
##total_downtime_result <- calculate_total_downtime(notification_data)
##print(total_downtime_result)

#=================================================
#KAI YING
#=================================================
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(gridExtra)
library(lubridate)
library(reshape2)
data<- read.csv("4.hackingdata.csv")  # read the csv file
df<-data

#1
df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
df <- df[!is.na(df$Date), ]
df$Year <- as.integer(format(df$Date, "%Y"))
df <- df %>% filter(Year >= 2003)

incident_count <- df %>%
  group_by(Year) %>%
  summarise(Incidents = n())

unique_ips <- df %>%
  group_by(Year) %>%
  summarise(Unique_IPs = n_distinct(IP))

attack_trends <- merge(incident_count, unique_ips, by="Year")

ggplot(attack_trends, aes(x = Year)) +
  geom_line(aes(y = Incidents, color = "Total Attacks"), size = 1) +  
  geom_line(aes(y = Unique_IPs, color = "Unique IPs"), size = 1) +  
  geom_point(aes(y = Incidents), color = "blue", size = 2) +  
  geom_point(aes(y = Unique_IPs), color = "red", size = 2) +  
  scale_color_manual(values = c("Total Attacks" = "blue", "Unique IPs" = "red")) +
  scale_x_continuous(breaks = seq(2003, max(df$Year), by = 1)) +  
  labs(title = "Hacking Trends Over Time (2003+)",
       x = "Year",
       y = "Count",
       color = "Legend") +
  theme_minimal()

yearly_stats <- df %>%group_by(Year) %>%summarise(
  Total_Attacks = n(),Unique_IPs = n_distinct(IP))

print(yearly_stats)

#2
df$Ransom <- as.numeric(df$Ransom)
df$Downtime <- as.numeric(df$Downtime)

attack_count <- df %>%
  group_by(OS) %>%
  summarise(Attack_Count = n()) %>%
  filter(OS != "Unknown") %>%
  arrange(desc(Attack_Count)) %>%
  top_n(10, Attack_Count)

df_filtered <- df %>%
  filter(OS %in% attack_count$OS & OS != "Unknown") %>%
  filter(!is.na(Ransom) & !is.na(Downtime))

avg_ransom <- df_filtered %>%
  group_by(OS) %>%
  summarise(Average_Ransom = mean(Ransom, na.rm = TRUE)) %>%
  left_join(attack_count, by = "OS") %>%
  arrange(desc(Attack_Count))

avg_downtime <- df_filtered %>%
  group_by(OS) %>%
  summarise(Average_Downtime = mean(Downtime, na.rm = TRUE)) %>%
  left_join(attack_count, by = "OS") %>%
  arrange(desc(Attack_Count))

p1 <- ggplot(attack_count, aes(x = reorder(OS, -Attack_Count), y = Attack_Count, fill = OS)) +
  geom_bar(stat = "identity") +
  scale_y_log10(labels = comma) +
  labs(title = "Top 10 Most Attacked OS (Log Scale)", x = "Operating System", y = "Attack Count (log10)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(avg_ransom, aes(x = reorder(OS, -Attack_Count), y = Average_Ransom, fill = OS)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = dollar, limits = c(min(avg_ransom$Average_Ransom) - 10, max(avg_ransom$Average_Ransom) + 10), oob = scales::rescale_none) +
  labs(title = "Average Ransom by OS (Sorted by Attack Count)", x = "Operating System", y = "Average Ransom ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3 <- ggplot(avg_downtime, aes(x = reorder(OS, -Attack_Count), y = Average_Downtime, fill = OS)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = comma, limits = c(min(avg_downtime$Average_Downtime) - 1, max(avg_downtime$Average_Downtime) + 1), oob = scales::rescale_none) +
  labs(title = "Average Downtime by OS (Sorted by Attack Count)", x = "Operating System", y = "Average Downtime (hours)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(p1, p2, p3, ncol = 1)

#3
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")  
data <- data %>% filter(Date >= as.Date("2005-01-01"))

#1. Plot: Top 10 Most Frequent Attacking IPs
top_ips <- data %>%
  filter(IP != "" & !is.na(IP)) %>%
  count(IP, sort = TRUE) %>%
  top_n(10, n)

ggplot(top_ips, aes(x = reorder(IP, n), y = n)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  labs(title = "Top 10 Most Frequent Attacking IPs", x = "IP Address", y = "Attack Count")


#2. Plot: Attack Frequency by Country and Year
# Extract year and filter for countries with top 10 attacks
data <- data %>%
  mutate(Year = year(Date)) %>%
  filter(Year >= 2005)

top_countries <- data %>%
  count(Country, sort = TRUE) %>%
  top_n(10, n) %>%
  pull(Country)

filtered_data <- data %>%
  filter(Country %in% top_countries) %>%
  count(Year, Country)

ggplot(filtered_data, aes(x = Year, y = Country, size = n, color = Country)) +
  geom_point(alpha = 0.7) +
  scale_x_continuous(breaks = seq(2005, max(filtered_data$Year), 1)) +
  labs(title = "Attack Frequency by Country and Year", x = "Year", y = "Country", size = "Attack Count") +
  theme_minimal()


#3. Plot: Downtime vs. Attack Count for Top 10 Countries
clean_data <- data %>%
  filter(!is.na(IP) & IP != "Unknown",
         !is.na(Country) & Country != "Unknown",
         !is.na(Downtime),
         as.integer(format(as.Date(Date, "%Y-%m-%d"), "%Y")) >= 2005)

top_countries <- clean_data %>%
  count(Country, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(Country)

filtered_data <- clean_data %>%
  filter(Country %in% top_countries)

top_ips <- filtered_data %>%
  group_by(IP, Country) %>%
  summarise(attack_count = n(), total_downtime = sum(Downtime, na.rm = TRUE)) %>%
  arrange(desc(attack_count)) %>%
  slice_head(n = 10)

ggplot(top_ips, aes(x = attack_count, y = total_downtime, color = Country, label = IP)) +
  geom_point(size = 4) +  
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +  
  scale_x_continuous(limits = c(min(top_ips$attack_count) * 0.9, max(top_ips$attack_count) * 1.1)) +  
  scale_y_continuous(limits = c(min(top_ips$total_downtime) * 0.9, max(top_ips$total_downtime) * 1.1)) +  
  labs(title = "Top 10 High-Impact Attackers (2005+): Downtime vs. Attack Count",
       x = "Number of Attacks",
       y = "Total Downtime (minutes)",
       color = "Country") +
  theme_minimal()


#4. Plot: DDoS Attack Trends Over Time
data <- data %>% filter(Date >= as.Date("2005-01-01"))
monthly_attacks <- data %>%
  mutate(YearMonth = floor_date(Date, "month")) %>%
  count(YearMonth)

ggplot(monthly_attacks, aes(x = YearMonth, y = n)) +
  geom_line(color = "blue", size = 1) +  
  geom_smooth(method = "loess", color = "red", linetype = "dashed") +
  labs(title = "DDoS Attack Trends Over Time (2005+)", 
       x = "Year", y = "Number of Attacks") +
  theme_minimal()

#4
df_filtered <- df %>%
  filter(WebServer != "Unknown")

attack_counts <- df_filtered %>%
  group_by(WebServer) %>%
  summarise(Attack_Count = n()) %>%
  arrange(desc(Attack_Count)) %>%
  head(10)

top_10_web_servers <- attack_counts$WebServer

ransom_summary <- df_filtered %>%
  filter(WebServer %in% top_10_web_servers) %>%
  group_by(WebServer) %>%
  summarise(Average_Ransom = round(mean(Ransom, na.rm = TRUE), 2)) %>%
  arrange(desc(Average_Ransom))

loss_summary <- df_filtered %>%
  filter(WebServer %in% top_10_web_servers) %>%
  group_by(WebServer) %>%
  summarise(Average_Loss = round(mean(Loss, na.rm = TRUE), 2)) %>%
  arrange(desc(Average_Loss))

ggplot(attack_counts, aes(x = reorder(WebServer, Attack_Count), y = Attack_Count, fill = WebServer)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Most Attacked Web Servers (Excluding Unknown)", x = "Web Server", y = "Attack Count") +
  theme_minimal()

ggplot(ransom_summary, aes(x = reorder(WebServer, Average_Ransom), y = Average_Ransom, fill = WebServer)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Web Servers with Highest Average Ransom (Excluding Unknown)",
       x = "Web Server", y = "Average Ransom (USD)") +
  theme_minimal() +
  geom_text(aes(label = scales::dollar(Average_Ransom)), hjust = -0.2, size = 3)

ggplot(loss_summary, aes(x = reorder(WebServer, Average_Loss), y = Average_Loss, fill = WebServer)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Web Servers with Highest Average Financial Loss (Excluding Unknown)",
       x = "Web Server", y = "Average Loss (USD)") +
  theme_minimal() +
  geom_text(aes(label = scales::dollar(Average_Loss)), hjust = -0.2, size = 3)

#5
data <- data %>%
  mutate(Year = year(Date)) %>%
  filter(Year >= 2005)

top_countries <- data %>%
  count(Country, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(Country)

filtered_data <- data %>%
  filter(Country %in% top_countries)

country_impact <- filtered_data %>%
  group_by(Country) %>%
  summarise(avg_ransom = mean(Ransom, na.rm = TRUE),
            avg_loss = mean(Loss, na.rm = TRUE))

ggplot(country_impact, aes(x = Country, y = avg_ransom, group = 1)) +
  geom_line(color = "purple", size = 1) +
  geom_point(color = "purple", size = 3) +
  labs(title = "Average Ransom by Country (2005+)",
       x = "Country",
       y = "Average Ransom (USD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(country_impact, aes(x = Country, y = avg_loss, group = 1)) +
  geom_line(color = "orange", size = 1) +
  geom_point(color = "orange", size = 3) +
  labs(title = "Average Financial Loss by Country (2005+)",
       x = "Country",
       y = "Average Financial Loss (USD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#======================================
#Brayden Yoong Policarpio
#TPNum: TP079094
#======================================
#============================================
#Cleaning OS Column
#=============================================
# Standardize OS names
data$OS <- tolower(data$OS)
data$OS <- gsub("win (2000|2003|xp|nt9x|nt|7|vista|8|10|2012|2008|n|x|\\.net)", "windows", data$OS)
data$OS <- gsub("windows 2000|win 2", "windows", data$OS)
data$OS <- gsub("solaris (8|9/10|sunos)", "solaris", data$OS)
data$OS <- gsub("freeb(sd|)", "freebsd", data$OS)
data$OS <- gsub("netbsdnetbsdopenbsddnetbsdopenbsdd|netbsdopenbsd|netbsdnetbsdopenbsdd", "openbsd", data$OS)
data$OS <- gsub("linux|LINUX", "linux", data$OS)
data$OS <- gsub("macosx|macos", "macos", data$OS)
data$OS <- gsub("aix", "aix", data$OS)
data$OS <- gsub("bsd(os|i)", "bsdos", data$OS)
data$OS <- gsub("hp-ux", "hp-ux", data$OS)
data$OS <- gsub("citrix.*", "citrix", data$OS)
data$OS <- gsub("f5.*", "f5", data$OS)
data$OS <- gsub("microsoft windows.*", "windows", data$OS)
data$OS <- gsub("sco unix", "sco unix", data$OS)
data$OS <- gsub("unknown|unkno|Unknown", "misc", data$OS)

# Further cleaning
data$OS <- gsub("openbsd", "openbsd", data$OS)
data$OS <- gsub("netgear.*|hp.*|juniper.*|cisco.*|fortinet.*|linksys.*|dlink.*|epson.*|samsung.*|thecus.*|avtech.*|engenius.*|konica minolta.*|netopia.*|htc.*|motorola.*", "embedded os", data$OS)
data$OS <- gsub("android.*", "android", data$OS)
data$OS <- gsub("tru64|digital unix|compaq tru64", "tru64", data$OS)
data$OS <- gsub("freenas.*", "freebsd", data$OS)
data$OS <- gsub("unix", "other unix", data$OS)
data$OS <- gsub(".*media device.*|.*voip adapter.*|.*control system.*|.*vpn gateway.*|.*router.*", "misc", data$OS)

# Remove special characters
data$OS <- gsub("[^a-z0-9 ]", "", data$OS)

# Count unique OS values
os_counts <- data %>% 
  group_by(OS) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# Print results
print(os_counts, n =100)


#=============================================
#Analysis 1 (OS & Web Server Vulnerability)
#Examines the relationship between operating systems and web servers to 
#identify which combinations are most frequently targeted by ransom attacks.
#=============================================

# Summarize Ransom Attacks and Downtime per OS-WebServer Combination
os_web_summary <- data %>%
  group_by(OS, WebServer) %>%
  summarise(
    Total_Attacks = n(),
    Total_Ransom = sum(Ransom, na.rm = TRUE),
    Avg_Ransom = mean(Ransom, na.rm = TRUE),
    Total_Downtime = sum(Downtime, na.rm = TRUE),
    Avg_Downtime = mean(Downtime, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Attacks))

# Display the summarized data
print(os_web_summary, n = 20)

# Identify Top 10 Most Targeted OS-WebServer Combinations
top_os_web <- os_web_summary %>% top_n(10, Total_Attacks)

# ==============================================
# VISUALIZATION 1: Heatmap of Attacks by OS and Web Server
# ==============================================

heatmap_data <- os_web_summary %>% 
  select(OS, WebServer, Total_Attacks) %>% 
  spread(key = WebServer, value = Total_Attacks, fill = 0)

# Convert to matrix for correlation plot
heatmap_matrix <- as.matrix(heatmap_data[, -1])
rownames(heatmap_matrix) <- heatmap_data$OS

# Plot heatmap
corrplot(heatmap_matrix, is.corr = FALSE, col = colorRampPalette(c("blue", "yellow", "red"))(200),
         tl.col = "black", tl.srt = 45, cl.lim = c(0, max(heatmap_matrix, na.rm = TRUE)))

# ==============================================
# VISUALIZATION 2: Bar Plot of Web Server Distribution by OS
# ==============================================

ggplot(os_web_summary, aes(y = WebServer, x = Total_Attacks, fill = OS)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(option = "plasma") +
  theme_minimal() +
  labs(title = "Distribution of Web Servers by OS (Grouped)",
       x = "Number of Instances",
       y = "Web Server",
       fill = "Operating System") +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold"))
#=================================
#TESTS
#=================================

# Create a contingency table of WebServer and OS
os_web_table <- table(data$WebServer, data$OS)


# Summarize counts per OS
os_counts <- colSums(os_web_table)  

# Filter OS with more than 5 occurrences
valid_os <- names(os_counts[os_counts > 5])  

# Subset the contingency table to include only valid OS
filtered_os_web_table <- os_web_table[, valid_os]
# Collapse OS with low counts
data$OS <- ifelse(data$OS %in% valid_os, data$OS, "Other")

# Collapse Web Servers with low counts
data$WebServer <- ifelse(data$WebServer %in% names(which(table(data$WebServer) > 5)), 
                         data$WebServer, "Other")

# create the contingency table
collapsed_os_web_table <- table(data$WebServer, data$OS)
chi_sq_test <- chisq.test(collapsed_os_web_table, simulate.p.value = TRUE)

# Print results
print(chi_sq_test)


#=============================================
# Analysis 2: OS, Web Server & Downtime
# Goal: Identify if certain OS-WebServer combinations suffer longer downtimes.
#=============================================

# Summarize downtime per OS-WebServer Combination
downtime_summary <- data %>%
  group_by(OS, WebServer) %>%
  summarise(
    Total_Downtime = sum(Downtime, na.rm = TRUE),
    Avg_Downtime = mean(Downtime, na.rm = TRUE),
    Median_Downtime = median(Downtime, na.rm = TRUE)
  ) %>%
  arrange(desc(Avg_Downtime))

# Display the summarized data
print(downtime_summary, n = 20)

# ==============================================
# VISUALIZATION 1: Plot of Downtime by OS
# ==============================================
os_web_downtime <- data %>%
  group_by(OS, WebServer) %>%
  summarise(
    Avg_Downtime = mean(Downtime, na.rm = TRUE),
    Median_Downtime = median(Downtime, na.rm = TRUE),
    Total_Downtime = sum(Downtime, na.rm = TRUE),
    Count = n()
  ) %>%
  filter(Count > 5) %>%  # Filter out rare OS-Web Server combinations
  arrange(desc(Median_Downtime))

# Convert OS and WebServer to factors for ordered plotting
os_web_downtime$OS <- factor(os_web_downtime$OS, levels = unique(os_web_downtime$OS))
os_web_downtime$WebServer <- factor(os_web_downtime$WebServer, levels = unique(os_web_downtime$WebServer))

# Plot Downtime Distribution by OS and Web Server using colored dots
ggplot(os_web_downtime, aes(x = OS, y = Median_Downtime, color = WebServer)) +
  geom_point(size = 4, alpha = 0.7) +  # Colored circular dots
  scale_color_viridis_d(option = "plasma") +
  theme_minimal() +
  labs(title = "Downtime Distribution by OS & Web Server",
       x = "Operating System",
       y = "Median Downtime (Hours)",
       color = "Web Server") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold"))


#=============================================
#Tests
#=============================================
# Check normality of Downtime (sample 5000 rows for efficiency)
sample_size <- min(5000, nrow(data))
shapiro_downtime <- shapiro.test(sample(data$Downtime, sample_size))$p.value

# Select appropriate test
if (shapiro_downtime > 0.05) {
  # Data is normally distributed, use ANOVA
  downtime_test <- aov(Downtime ~ OS * WebServer, data = data)
} else {
  # Data is NOT normally distributed, use Kruskal-Wallis test
  downtime_test <- kruskal.test(Downtime ~ interaction(OS, WebServer), data = data)
}

print(downtime_test)



#=============================================
#Analysis 3: OS, Web Server & Financial Losses
#Goal: Examine which OS-WebServer combinations lead 
#to the highest financial losses due to ransom attacks.
#==============================================

# Summarize financial loss per OS-WebServer Combination
loss_summary <- data %>%
  group_by(OS, WebServer) %>%
  summarise(
    Total_Loss = sum(Loss, na.rm = TRUE),
    Avg_Loss = mean(Loss, na.rm = TRUE),
    Median_Loss = median(Loss, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Loss))

print(loss_summary, n = 20)

# ==============================================
# VISUALIZATION: Bar Plot of Financial Losses by OS & Web Server
# ==============================================
# Ensure loss_summary is in the correct format (convert WebServer to factor)
loss_summary <- loss_summary %>%
  mutate(WebServer = as.factor(WebServer))

# Create the bar plot
ggplot(loss_summary, aes(x = reorder(paste(OS, WebServer, sep = " - "), Total_Loss), 
                         y = Total_Loss, fill = OS)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip for better readability
  labs(title = "Total Financial Losses by OS & Web Server",
       x = "OS - Web Server Combination",
       y = "Total Financial Loss ($)",
       fill = "Operating System") +
  theme_minimal() +
  scale_fill_viridis_d(option = "plasma")  # Use a color palette for better distinction


#==================================
# Analysis 4: OS, Web Server, and Attack Trends Over Time
# Goal: Track ransom attack trends over the last 15 years to detect emerging vulnerabilities.
#===================================
#Convert Date column to proper format
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# Extract year from Date
data$Year <- format(data$Date, "%Y")

# Summarize attacks over time
attack_trends <- data %>%
  group_by(Year, OS, WebServer) %>%
  summarise(
    Total_Attacks = n(),
    Total_Ransom = sum(Ransom, na.rm = TRUE),
    Avg_Ransom = mean(Ransom, na.rm = TRUE),
    Total_Downtime = sum(Downtime, na.rm = TRUE)
  ) %>%
  arrange(desc(Year))

# Display the summarized data
print(attack_trends, n = 20)

# ==============================================
# VISUALIZATION: Line Plot of Attack Trends Over Time
# ==============================================

ggplot(attack_trends, aes(x = as.numeric(Year), y = Total_Attacks, color = OS, group = OS)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Ransom Attack Trends Over Time",
       x = "Year",
       y = "Total Attacks",
       color = "Operating System") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold"))

#aljk,ncsalkclasknckla
# Group by Year, OS, and WebServer to summarize attack trends
attack_trends <- data %>%
  group_by(Year, OS, WebServer) %>%
  summarise(Total_Attacks = n(), .groups = "drop") %>%
  arrange(desc(Total_Attacks))

# Filter for combinations with more than 5 attacks per year
attack_trends_filtered <- attack_trends %>% filter(Total_Attacks > 5)

# Identify the top OS-WebServer combinations
top_attack_trends <- attack_trends_filtered %>%
  group_by(OS, WebServer) %>%
  summarise(Total_Attacks = sum(Total_Attacks), .groups = "drop") %>%
  arrange(desc(Total_Attacks)) %>%
  head(10)  # Top 10 most attacked combinations

# Visualization: Bar plot of top OS-Web Server combinations
ggplot(top_attack_trends, aes(x=reorder(paste(OS, WebServer, sep=" - "), Total_Attacks), y=Total_Attacks, fill=OS)) +
  geom_bar(stat="identity") +
  coord_flip() +  # Flip for better readability
  theme_minimal() +
  scale_fill_viridis_d(option="plasma") +  # Use a color palette for better distinction
  labs(title="Top OS-Web Server Combinations by Attack Trends",
       x="OS - Web Server Combination",
       y="Total Attacks",
       fill="Operating System") +
  theme(axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        plot.title = element_text(size=14, face="bold"))

#==================================
#TESTS
#==================================

# Ensure Date is in proper format
data$Year <- as.numeric(format(as.Date(data$Date, format = "%Y-%m-%d"), "%Y"))

# Summarize total attacks per year
attack_trends <- data %>%
  group_by(Year) %>%
  summarise(Total_Attacks = sum(Ransom, na.rm = TRUE))

# Perform Kendall’s Tau correlation test
correlation_test <- cor.test(attack_trends$Year, attack_trends$Total_Attacks, method = "kendall")


print(correlation_test)
# ==============================================
# ANALYSIS 5: OS, Web Server, Ransom Attacks & Country-Specific Trends
# Goal: Compare attack patterns across countries to determine if certain OS-WebServer configurations are more vulnerable.
# ==============================================

# Summarize attacks per country
country_summary <- data %>%
  group_by(Country, OS, WebServer) %>%
  summarise(
    Total_Attacks = n(),
    Total_Ransom = sum(Ransom, na.rm = TRUE),
    Avg_Ransom = mean(Ransom, na.rm = TRUE),
    Total_Downtime = sum(Downtime, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Attacks))

# Display the summarized data
print(country_summary, n = 20)

# ==============================================
# VISUALIZATION: Heatmap of Ransom Attacks by Country & OS
# ==============================================

ggplot(country_summary, aes(x = Country, y = OS, fill = Total_Attacks)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(title = "Ransom Attacks by Country & OS",
       x = "Country",
       y = "Operating System",
       fill = "Total Attacks") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold"))
#==========================
#TESTS
#==========================

# Create a contingency table of OS, Country, and Web Server
country_os_table <- table(data$Country, data$OS)

# Run Chi-Square Test
if (any(country_os_table < 5)) {
  chi_sq_country <- if (sum(country_os_table) < 200) {
    fisher.test(country_os_table)  # Use Fisher's Exact Test for small tables
  } else {
    chisq.test(country_os_table, simulate.p.value = TRUE)  # Simulated Chi-Square Test
  }
} else {
  chi_sq_country <- chisq.test(country_os_table)  # Standard Chi-Square Test
}


print(chi_sq_country)

#=============================
#VISUAL 2, INDIVIDUAL WEB SERVER
#=============================

library(dplyr)
library(ggplot2)
library(viridis)
library(tidyr)
library(ggforce)

# Summarize attacks per country, OS, and Web Server
country_summary <- data %>%
  group_by(Country, OS, WebServer) %>%
  summarise(
    Total_Attacks = n(),
    Total_Ransom = sum(Ransom, na.rm = TRUE),
    Avg_Ransom = mean(Ransom, na.rm = TRUE),
    Total_Downtime = sum(Downtime, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Attacks))

# Convert WebServer to a factor for faceting
country_summary$WebServer <- as.factor(country_summary$WebServer)

# Filter top 15 most attacked countries for better readability
top_countries <- country_summary %>%
  group_by(Country) %>%
  summarise(Total = sum(Total_Attacks)) %>%
  arrange(desc(Total)) %>%
  slice_head(n = 15)  # Keep top 15 attacked countries

country_summary <- country_summary %>% filter(Country %in% top_countries$Country)

# Filter top 10 most attacked WebServers for clarity
top_webservers <- country_summary %>%
  group_by(WebServer) %>%
  summarise(Total = sum(Total_Attacks)) %>%
  arrange(desc(Total)) %>%
  slice_head(n = 10)  # Keep only top 10 attacked Web Servers

country_summary <- country_summary %>% filter(WebServer %in% top_webservers$WebServer)

# Create a named vector to map Web Server numbers to actual names
webserver_labels <- setNames(as.character(unique(country_summary$WebServer)), unique(country_summary$WebServer))

# Total number of pages needed (each page contains 2 Web Servers)
webservers_list <- unique(country_summary$WebServer)
num_pages <- ceiling(length(webservers_list) / 2)

# Loop through pages and generate plots
for (i in 1:num_pages) {
  # Get the web servers shown on this page
  ws_on_page <- webservers_list[((i - 1) * 2 + 1) : min(i * 2, length(webservers_list))]
  ws_title <- paste(ws_on_page, collapse = ", ")  # Create a readable title
  
  p <- ggplot(country_summary, aes(x = Country, y = OS, fill = Total_Attacks)) +
    geom_tile() +
    scale_fill_viridis_c(option = "plasma") +  # Use color gradient for attack intensity
    ggforce::facet_wrap_paginate(~ WebServer, ncol = 1, nrow = 2, page = i, labeller = labeller(WebServer = webserver_labels)) +  # Use actual Web Server names
    theme_minimal() +
    labs(title = paste("Ransom Attacks by Country, OS & Web Server: ", ws_title, sep = ""),
         x = "Country",
         y = "Operating System",
         fill = "Total Attacks") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          axis.text.y = element_text(size = 10),
          strip.text = element_text(size = 10, face = "bold"),  # WebServer facet titles
          plot.title = element_text(size = 14, face = "bold"))
  
  print(p)  # Print each plot to the output
}
