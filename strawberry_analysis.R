# MA415 Midterm Project R Script
# Run this file in RStudio. Ensure the CSV and helper function file are in the same directory.


library(knitr)  
# library(kableExtra)
library(tidyverse)
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)



strawberry <- read_csv("/Users/suakshi/Downloads/strawb_mar6.csv", 
                       col_names = TRUE,
                       show_col_types = FALSE)
data <- strawberry

source("/Users/suakshi/Downloads/my_functions.R")



                                                                        # assume data is a tibble
# n_show is the number of rows to show



show_unique <- function(data, nrows=10 ){
  # make a tibble items to hold the data to show
  # browser()
  a <- nrows * dim(data)[2]  # number of cells in items
  items <- rep(" ", a) # items will coerce everything to char
  dim(items) <- c(nrows ,dim(data)[2]) # shape items
  items <- as_tibble(items)
  colnames(items) <- colnames(data)
  # browser()
  for(i in 1:dim(data)[2]){
    
    col_items <- unique(data[,i])
    # row_ex is the number of rows needed 
    # to make the column length conformable with items
    row_ex <- nrows - dim(col_items)[1] 
    if(row_ex >= 0){
      ex_rows <- tibble(rep(" ",row_ex))
      colnames(ex_rows) <- colnames(col_items)
      col_add <- rbind2(col_items, ex_rows)
      
    } else if(row_ex < 0){
      col_add <- col_items[1:10,]
      
    }
    
    items[,i] <- col_add
    
  }
  
  return(items)
}

test <- show_unique(data, 10)
#View(test)

## PROGRAM

census <- data |> filter(Program == "CENSUS")

survey <- data |> filter(Program == "SURVEY")

nrow(data) == (nrow(census) + nrow(survey))



census <- census |> drop_one_value_col(prt_val = TRUE) 
colnames(census)
dim(census) # YEAR OF CENSUS PROGRAM IS SAME THROUGHOUT THE DATASET 


survey <- survey |> drop_one_value_col(prt_val = TRUE)
colnames(survey)
dim(survey) # SURVEY DATA IS HAVING COMMODITY AS STRAWBERRIES ONLY (NO INCOME, NET CASH FARM)


unique_sur <- survey |> show_unique(nrows = 10)
#View(unique_sur)

unique_cen <- census |> show_unique(nrows = 10)
#View(unique_cen)


census <- census |> select(-`State ANSI`)

survey <- survey |> select(-`State ANSI`, -`Week Ending`, -Period)

# rm(s_census, s_survey, strawberry, strawb)



commod <- census$Commodity |> unique()

# CALIFORNIA AND FLORIDA 
cen_f <- census |> filter(State == "FLORIDA")
sur_f <- survey |> filter(State == "FLORIDA")
cen_c <- census |> filter(State == "CALIFORNIA")
sur_c <- survey |> filter(State == "CALIFORNIA")

##### CENSUS DATA TO STRRAWBERRIES AND INCOME DATA 
strawb_cen_f <- cen_f |> filter(Commodity == "STRAWBERRIES")
income_cen_f <- cen_f |> filter(Commodity == "INCOME, NET CASH FARM")
strawb_cen_c <- cen_c |> filter(Commodity == "STRAWBERRIES")
income_cen_c <- cen_c |> filter(Commodity == "INCOME, NET CASH FARM")


strawb_cen_f <- strawb_cen_f |> drop_one_value_col(prt_val = TRUE)
income_cen_f <- income_cen_f |> drop_one_value_col(prt_val = TRUE)
strawb_cen_c <- strawb_cen_c |> drop_one_value_col(prt_val = TRUE)
income_cen_c <- income_cen_c |> drop_one_value_col(prt_val = TRUE)

# For Survey program, we have only strawberries (so let's assign the sur_f and sur_c as strawb_sur_f and strawb_sur_c)
strawb_sur_f <- sur_f
strawb_sur_c <- sur_c
strawb_sur_f <- strawb_sur_f |> drop_one_value_col(prt_val = TRUE)
strawb_sur_c <- strawb_sur_c |> drop_one_value_col(prt_val = TRUE)

### STRAWBERRIES (strawb_sur_c, strawb_sur_f, strawb_cen_c, strawb_cen_f)
## strawb_sur_c 

str(strawb_sur_c)

table(strawb_sur_c$Year)
table(strawb_sur_c$`Data Item`)
table(strawb_sur_c$Domain)
table(strawb_sur_c$`Domain Category`)
summary(strawb_sur_c$Value)


# Create a new column 'commas' that counts the number of commas in 'Data Item'
strawb_sur_c$commas <- stringr::str_count(strawb_sur_c$`Data Item`, ",")
# Check the new column
head(strawb_sur_c)
table(strawb_sur_c$commas)

table(strawb_sur_c %>% filter(commas == 0) %>% pull(`Data Item`))
table(strawb_sur_c %>% filter(commas == 1) %>% pull(`Data Item`))
table(strawb_sur_c %>% filter(commas == 2) %>% pull(`Data Item`))
table(strawb_sur_c %>% filter(commas == 3) %>% pull(`Data Item`))

strawb_sur_c_0 <- strawb_sur_c |> filter(commas == 0)
strawb_sur_c_1 <- strawb_sur_c |> filter(commas == 1)
strawb_sur_c_2 <- strawb_sur_c |> filter(commas == 2)
strawb_sur_c_3 <- strawb_sur_c |> filter(commas == 3)



# 3 Commas 
# Split the 'Data Item' column into multiple columns
strawb_sur_c_3 <- strawb_sur_c_3 |>
  separate_wider_delim(
    cols = `Data Item`,              # Column to split
    delim = ",",                     # Delimiter is a comma
    names = c("Fruit", "Category", "Item", "Metric"), # New column names
    too_many = "error",              # Stop if too many pieces
    too_few = "align_start"          # Align missing pieces from the start
  )
head(strawb_sur_c_3)
strawb_sur_c_3 <- strawb_sur_c_3 |> drop_one_value_col(prt_val = TRUE)

table(strawb_sur_c_3$Domain)
strawb_sur_c_3_fert <- strawb_sur_c_3 |> filter(Domain == "FERTILIZER")


strawb_sur_c_3_fert <- strawb_sur_c_3_fert |>
  separate_wider_delim(
    cols = `Domain Category`,              # Column to split
    delim = ":",                     # Delimiter is a colon
    names = c("Domain_1", "Domain Category"), # New column names
    too_many = "error",              # Stop if too many pieces
    too_few = "align_start"          # Align missing pieces from the start
  )
head(strawb_sur_c_3_fert)




strawb_sur_c_3_chem <- strawb_sur_c_3 |> filter(Domain != "FERTILIZER")
strawb_sur_c_3_chem <- strawb_sur_c_3_chem |>
  separate_wider_delim(
    cols = `Domain Category`,              # Column to split
    delim = ":",                     # Delimiter is a colon
    names = c("Domain_1", "Domain Category"), # New column names
    too_many = "error",              # Stop if too many pieces
    too_few = "align_start"          # Align missing pieces from the start
  )
strawb_sur_c_3_chem <- strawb_sur_c_3_chem |> select(-'Domain_1')
strawb_sur_c_3_chem <- strawb_sur_c_3_chem |>
  separate_wider_delim(
    cols = `Domain`,              # Column to split
    delim = ",",                     # Delimiter is a colon
    names = c("Domain", "Chemical"), # New column names
    too_many = "error",              # Stop if too many pieces
    too_few = "align_start"          # Align missing pieces from the start
  )


View(strawb_sur_c_3_chem)
strawb_sur_c_3_chem <- strawb_sur_c_3_chem |> drop_one_value_col(prt_val = TRUE)
dim(strawb_sur_c_3_chem)
str(strawb_sur_c_3_chem)
table(strawb_sur_c_3_chem$Year)
table(strawb_sur_c_3_chem$Chemical)

unique_domain_counts <- strawb_sur_c_3_chem %>%
  group_by(Chemical) %>%
  summarise(Unique_Domain_Categories = n_distinct(`Domain Category`))
print(unique_domain_counts)


strawb_sur_c_3_chem <- strawb_sur_c_3_chem |> filter(Value != "(NA)")
strawb_sur_c_3_chem_D <- strawb_sur_c_3_chem |> filter(Value == "(D)")
dim(strawb_sur_c_3_chem_D)
str(strawb_sur_c_3_chem_D)
table(strawb_sur_c_3_chem_D$Year)
table(strawb_sur_c_3_chem_D$Chemical)

strawb_sur_c_3_chem <- strawb_sur_c_3_chem |> filter(Value != "(D)")
dim(strawb_sur_c_3_chem)
str(strawb_sur_c_3_chem)
table(strawb_sur_c_3_chem$Year)
table(strawb_sur_c_3_chem$Chemical)


strawb_sur_c_3_chem$Value <- as.numeric(strawb_sur_c_3_chem$Value)
colSums(is.na(strawb_sur_c_3_chem))

# No missing values were found in the dataset 


summary_by_chemical <- strawb_sur_c_3_chem %>%
  group_by(Chemical) %>%
  summarise(
    Count = n(),
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE)
  )
print(summary_by_chemical)



# Summary by Year and Chemical Type
summary_by_year_chemical <- strawb_sur_c_3_chem %>%
  group_by(Year, Chemical) %>%
  summarise(Total_Value = sum(Value, na.rm = TRUE)) %>%
  tidyr::spread(Chemical, Total_Value)
print(summary_by_year_chemical)

# Plot: Chemical usage over time
ggplot(strawb_sur_c_3_chem, aes(x = as.factor(Year), y = Value, fill = Chemical)) +
  geom_boxplot() +
  labs(title = "Chemical Usage Over Years", x = "Year", y = "Value (lb/acre)") +
  theme_minimal()

# Plot: Total chemical usage by type
ggplot(summary_by_chemical, aes(x = reorder(Chemical, -Mean), y = Mean, fill = Chemical)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Chemical Usage by Type", x = "Chemical Type", y = "Average Value (lb/acre)") +
  theme_minimal() +
  coord_flip()

ggplot(strawb_sur_c_3_chem, aes(x = Category, fill = Category)) +
  geom_bar() +
  labs(title = "Count of Observations by Category", x = "Category", y = "Count") +
  theme_minimal()


ggplot(strawb_sur_c_3_chem, aes(x = Category, y = Value, fill = Category)) +
  geom_boxplot() +
  labs(title = "Distribution of Values by Category", x = "Category", y = "Value (lb/acre)") +
  theme_minimal()

mean_values_by_category <- strawb_sur_c_3_chem %>%
  group_by(Category) %>%
  summarise(Mean_Value = mean(Value, na.rm = TRUE))

ggplot(mean_values_by_category, aes(x = Category, y = Mean_Value, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Value by Category", x = "Category", y = "Mean Value (lb/acre)") +
  theme_minimal()

mean_values_by_category <- strawb_sur_c_3_chem %>%
  group_by(Category) %>%
  summarise(Mean_Value = mean(Value, na.rm = TRUE))

ggplot(mean_values_by_category, aes(x = Category, y = Mean_Value, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Value by Category", x = "Category", y = "Mean Value (lb/acre)") +
  theme_minimal()

# stacked bar plot for chemical distribution across categories 
# This shows the count of different chemicals in each category
ggplot(strawb_sur_c_3_chem, aes(x = Category, fill = Chemical)) +
  geom_bar(position = "stack") +
  labs(title = "Chemical Distribution Across Categories", x = "Category", y = "Count") +
  theme_minimal()

# Boxplot : value distribution of chemicals per category 
# This shows how chemical usage differs across categories 
ggplot(strawb_sur_c_3_chem, aes(x = Category, y = Value, fill = Chemical)) +
  geom_boxplot() +
  labs(title = "Chemical Usage by Category", x = "Category", y = "Value (lb/acre)") +
  theme_minimal()
# 2 commas 







# Create a new column 'commas' that counts the number of commas in 'Data Item'
strawb_sur_f$commas <- stringr::str_count(strawb_sur_f$`Data Item`, ",")
head(strawb_sur_f)
table(strawb_sur_f$commas)


table(strawb_sur_f %>% filter(commas == 0) %>% pull(`Data Item`))
table(strawb_sur_f %>% filter(commas == 1) %>% pull(`Data Item`))
table(strawb_sur_f %>% filter(commas == 2) %>% pull(`Data Item`))
table(strawb_sur_f %>% filter(commas == 3) %>% pull(`Data Item`))

strawb_sur_f_0 <- strawb_sur_f |> filter(commas == 0)
strawb_sur_f_1 <- strawb_sur_f |> filter(commas == 1)
strawb_sur_f_2 <- strawb_sur_f |> filter(commas == 2)
strawb_sur_f_3 <- strawb_sur_f |> filter(commas == 3)

# 3 Commas 
strawb_sur_f_3 <- strawb_sur_f_3 |> separate_wider_delim(
  cols = `Data Item`,              # Column to split
  delim = ",",                     # Delimiter is a comma
  names = c("Fruit", "Category", "Item", "Metric"), # New column names
  too_many = "error",              # Stop if too many pieces
  too_few = "align_start"          # Align missing pieces from the start
)
strawb_sur_f_3 <- strawb_sur_f_3 |> drop_one_value_col(prt_val = TRUE)

table(strawb_sur_f_3$Domain)
strawb_sur_f_3_fert <- strawb_sur_f_3 |> filter(Domain == "FERTILIZER")

strawb_sur_f_3_fert <- strawb_sur_f_3_fert |> separate_wider_delim(
  cols = `Domain Category`,              # Column to split
  delim = ":",                     # Delimiter is a colon
  names = c("Domain_1", "Domain Category"), # New column names
  too_many = "error",              # Stop if too many pieces
  too_few = "align_start"          # Align missing pieces from the start
)

strawb_sur_f_3_chem <- strawb_sur_f_3 |> filter(Domain != "FERTILIZER")
strawb_sur_f_3_chem <- strawb_sur_f_3_chem |> separate_wider_delim(
  cols = `Domain Category`,              # Column to split
  delim = ":",                     # Delimiter is a colon
  names = c("Domain_1", "Domain Category"), # New column names
  too_many = "error",              # Stop if too many pieces
  too_few = "align_start"          # Align missing pieces from the start
)
strawb_sur_f_3_chem <- strawb_sur_f_3_chem |> select(-'Domain_1')
strawb_sur_f_3_chem <- strawb_sur_f_3_chem |> separate_wider_delim(
  cols = `Domain`,              # Column to split
  delim = ",",                     # Delimiter is a colon
  names = c("Domain", "Chemical"), # New column names
  too_many = "error",              # Stop if too many pieces
  too_few = "align_start"          # Align missing pieces from the start
)

strawb_sur_f_3_chem <- strawb_sur_f_3_chem |> drop_one_value_col(prt_val = TRUE)

# Filtering and data processing
strawb_sur_f_3_chem <- strawb_sur_f_3_chem |> filter(Value != "(NA)")
strawb_sur_f_3_chem <- strawb_sur_f_3_chem |> filter(Value != "(D)")

strawb_sur_f_3_chem$Value <- as.numeric(strawb_sur_f_3_chem$Value)

# Summary statistics
summary_by_chemical <- strawb_sur_f_3_chem %>%
  group_by(Chemical) %>%
  summarise(
    Count = n(),
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE)
  )
print(summary_by_chemical)

# Summary by Year and Chemical Type
summary_by_year_chemical <- strawb_sur_f_3_chem %>%
  group_by(Year, Chemical) %>%
  summarise(Total_Value = sum(Value, na.rm = TRUE)) %>%
  tidyr::spread(Chemical, Total_Value)
print(summary_by_year_chemical)

# Plots
ggplot(strawb_sur_f_3_chem, aes(x = as.factor(Year), y = Value, fill = Chemical)) +
  geom_boxplot() +
  labs(title = "Chemical Usage Over Years", x = "Year", y = "Value (lb/acre)") +
  theme_minimal()

ggplot(summary_by_chemical, aes(x = reorder(Chemical, -Mean), y = Mean, fill = Chemical)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Chemical Usage by Type", x = "Chemical Type", y = "Average Value (lb/acre)") +
  theme_minimal() +
  coord_flip()

ggplot(strawb_sur_f_3_chem, aes(x = Category, fill = Category)) +
  geom_bar() +
  labs(title = "Count of Observations by Category", x = "Category", y = "Count") +
  theme_minimal()

ggplot(strawb_sur_f_3_chem, aes(x = Category, y = Value, fill = Category)) +
  geom_boxplot() +
  labs(title = "Distribution of Values by Category", x = "Category", y = "Value (lb/acre)") +
  theme_minimal()

ggplot(strawb_sur_f_3_chem, aes(x = Category, fill = Chemical)) +
  geom_bar(position = "stack") +
  labs(title = "Chemical Distribution Across Categories", x = "Category", y = "Count") +
  theme_minimal()

ggplot(strawb_sur_f_3_chem, aes(x = Category, y = Value, fill = Chemical)) +
  geom_boxplot() +
  labs(title = "Chemical Usage by Category", x = "Category", y = "Value (lb/acre)") +
  theme_minimal()

###### INCOME, NET CASH FARM () INCOME (income_cen_c, income_cen_f))

# income_cen_c
str(income_cen_c)
sum(is.na(income_cen_c$Value))

# Convert Value column to numeric in 1000's unit
income_cen_c$Value <- as.numeric(gsub(",", "", income_cen_c$Value)) / 1000
summary(income_cen_c$Value)
income_cen_c$`CV (%)` <- as.numeric(income_cen_c$`CV (%)`)
income_cen_c <- income_cen_c |> separate_wider_delim(
  cols = `Data Item`,              # Column to split
  delim = ",",                     # Delimiter is a colon
  names = c("Commodity", "Farm", "Type", "Measure"), # New column names
  too_many = "error",              # Stop if too many pieces
  too_few = "align_start"          # Align missing pieces from the start
)
head(income_cen_c)


income_cen_c_total <- income_cen_c |> filter(Domain == "TOTAL")
income_cen_c <- income_cen_c |> filter(Domain != "TOTAL")

income_cen_c <- income_cen_c |> separate_wider_delim(
  cols = `Domain Category`,              # Column to split
  delim = ":",                     # Delimiter is a colon
  names = c("Domain1", "Category"), # New column names
  too_many = "error",              # Stop if too many pieces
  too_few = "align_start"          # Align missing pieces from the start
)
income_cen_c <- income_cen_c |> select(-"Domain1")
head(income_cen_c)

table(income_cen_c$Commodity)
table(income_cen_c$Farm)
table(income_cen_c$Type)
table(income_cen_c$Measure)
table(income_cen_c$Domain)
table(income_cen_c$Category)
summary(income_cen_c$Value)
summary(income_cen_c$`CV (%)`)
str(income_cen_c)

# 1. Histogram & Density Plot (Distribution of Income)
ggplot(income_cen_c, aes(x = Value)) +
  scale_x_continuous(labels = scales::comma) +  # Format x-axis
  scale_y_continuous(labels = scales::comma) +  # Format y-axis
  geom_histogram(binwidth = 50000, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Income (Value)", x = "Income ($)", y = "Frequency") +
  theme_minimal()

ggplot(income_cen_c, aes(x = Value)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  scale_x_continuous(labels = scales::comma) +  # Format x-axis
  scale_y_continuous(labels = scales::comma) +  # Format y-axis
  labs(title = "Density Plot of Income", x = "Income ($)", y = "Density") +
  theme_minimal()

# 2. Box Plot (Income Across Types)
ggplot(income_cen_c, aes(x = Type, y = Value)) +
  geom_boxplot(fill = "orange", outlier.color = "red") +
  labs(title = "Income Distribution Across Categories", x = "Category", y = "Income ($)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 3. Bar Chart (Income by Type)
ggplot(income_cen_c, aes(x = Type, y = Value, fill = Type)) +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis
  geom_bar(stat = "identity") +
  labs(title = "Income by Type", x = "Type", y = "Income ($)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4. Scatter Plot (Income vs CV %)
ggplot(income_cen_c, aes(x = `CV (%)`, y = Value)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(title = "Income vs Coefficient of Variation (CV %)", x = "CV (%)", y = "Income ($)") +
  theme_minimal()

# Scatter Plot (Income vs CV %) with Type as Color and formatted y-axis
ggplot(income_cen_c, aes(x = `CV (%)`, y = Value, color = Type)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis
  labs(title = "Value vs Coefficient of Variation (CV %) by Type",
       x = "CV (%)",
       y = "Value ($)",
       color = "Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

library(dplyr)

# Summary for income_cen_c
summary_c_values <- income_cen_c |> group_by(Type) |> summarise(
  Mean_Value = mean(Value, na.rm = TRUE),
  Median_Value = median(Value, na.rm = TRUE),
  SD_Value = sd(Value, na.rm = TRUE),
  CV_Avg = mean(`CV (%)`, na.rm = TRUE)
)
print(summary_c_values)







#### 
str(income_cen_f)
sum(is.na(income_cen_f$Value))


income_cen_f$Value <- as.numeric(gsub(",", "", income_cen_f$Value)) / 1000
income_cen_f$`CV (%)` <- as.numeric(income_cen_f$`CV (%)`)
summary(income_cen_f$Value)


income_cen_f <- income_cen_f |> separate_wider_delim(
  cols = `Data Item`,
  delim = ",",
  names = c("Commodity", "Farm", "Type", "Measure"),
  too_many = "error",
  too_few = "align_start"
)
head(income_cen_f)


income_cen_f_total <- income_cen_f |> filter(Domain == "TOTAL")
income_cen_f <- income_cen_f |> filter(Domain != "TOTAL")

income_cen_f <- income_cen_f |> separate_wider_delim(
  cols = `Domain Category`,
  delim = ":",
  names = c("Domain1", "Category"),
  too_many = "error",
  too_few = "align_start"
)

income_cen_f <- income_cen_f |> select(-"Domain1")
head(income_cen_f)

table(income_cen_f$Commodity)
table(income_cen_f$Farm)
table(income_cen_f$Type)
table(income_cen_f$Measure)
table(income_cen_f$Domain)
table(income_cen_f$Category)
summary(income_cen_f$Value)
summary(income_cen_f$`CV (%)`)
str(income_cen_f)




ggplot(income_cen_f, aes(x = Value)) +
  scale_x_continuous(labels = scales::comma) +
  geom_histogram(binwidth = 50000, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Income (Value) for Florida", x = "Income ($)", y = "Frequency") +
  theme_minimal()

ggplot(income_cen_f, aes(x = Value)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Density Plot of Income for Florida", x = "Income ($)", y = "Density") +
  theme_minimal()

ggplot(income_cen_f, aes(x = Type, y = Value)) +
  geom_boxplot(fill = "orange", outlier.color = "red") +
  labs(title = "Income Distribution Across Categories for Florida", x = "Category", y = "Income ($)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(income_cen_f, aes(x = Type, y = Value, fill = Type)) +
  scale_y_continuous(labels = scales::comma) +
  geom_bar(stat = "identity") +
  labs(title = "Income by Type for Florida", x = "Type", y = "Income ($)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(income_cen_f, aes(x = `CV (%)`, y = Value)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(title = "Income vs Coefficient of Variation (CV %) for Florida", x = "CV (%)", y = "Income ($)") +
  theme_minimal()


ggplot(income_cen_f, aes(x = `CV (%)`, y = Value, color = Type)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Value vs Coefficient of Variation (CV %) by Type for Florida",
       x = "CV (%)",
       y = "Value ($)",
       color = "Type") +
  theme_minimal() +
  theme(legend.position = "bottom")



summary_f_values <- income_cen_f |> group_by(Type) |> summarise(
  Mean_Value = mean(Value, na.rm = TRUE),
  Median_Value = median(Value, na.rm = TRUE),
  SD_Value = sd(Value, na.rm = TRUE),
  CV_Avg = mean(`CV (%)`, na.rm = TRUE)
)
print(summary_f_values)
