getwd()
data <- read.csv("Conditions_Contributing_to_COVID-19_Deaths.csv", sep=",")
library(ggplot2)
library(stringr)
library(dplyr)
# Descriptive Statistics 
nrow(data)
ncol(data)
names(data)
head(data)
tail(data)
summary(data)
# Check for missing values
colSums(is.na(data))

#Summarize mental health care utilization by Indicator and view the summary 
Indicatorsummary <- data %>%
  group_by(Indicator) %>%
  summarize(
    mean_utilization = mean(Value, na.rm = TRUE),
    median_utilization = median(Value, na.rm = TRUE),
    sd_utilization = sd(Value, na.rm = TRUE))
print(Indicatorsummary)

#Visualize Indicator summary as a bar chart. Use actual mean_utilization values (rather than counting occurrences(stat = "identity")).
ggplot(Indicatorsummary, aes(x = reorder(Indicator, -mean_utilization), y = mean_utilization)) +
  geom_bar(stat = "identity", fill = "lightblue", color="black") +
  labs(title = "Mean Mental Health Care Utilization by Indicator",
       x = "Indicators ",
       y = "Mean Utilization") +
  theme_minimal() + theme(legend.position = "none",  # Hide legend if not needed
                          axis.text.y = element_text(size = 10)) +  # Adjust font size
  scale_y_continuous(labels = scales::comma) +  # Format y-axis numbers
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))  # Wrap text

#Create a box plot to visualize the spread of mental health care utilization across indicators
ggplot(data, aes(x = Indicator, y = Value)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Distribution of Mental Health Care Utilization by Indicator",
       x = "Indicator",
       y = "Utilization") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  scale_y_continuous(labels = scales::comma) +  # Format y-axis numbers
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))  # Wrap text for readability

#Summarize mental health care utilization by state and view the summary 
state_summary <- data %>%
  group_by(State) %>%
  summarize(
    mean_utilization = mean(Value, na.rm = TRUE),
    median_utilization = median(Value, na.rm = TRUE),
    sd_utilization = sd(Value, na.rm = TRUE))
print(state_summary)

#Visualize State summary as a column chart. It also reorders states so that those with the highest mean_utilization appear at the top.Use actual mean_utilization values (rather than counting occurrences(stat = "identity")).
ggplot(state_summary, aes(x = reorder(State, -mean_utilization), y = mean_utilization)) +
  geom_bar(stat = "identity", fill = "pink", color="black") +
  coord_flip() +
  labs(title = "Mean Mental Health Care Utilization by State",
       x = "State",
       y = "Mean Utilization") +
  theme_minimal()

#Create a box plot that shows the distribution / spread and variation of mental health care utilization across the different states
ggplot(data, aes(x = State, y = Value)) +
  geom_boxplot(fill = "pink", color = "black") +
  coord_flip() +
  labs(title = "Distribution of Mental Health Care Utilization by State",
       x = "State",
       y = "Utilization Rate") +
  theme_minimal()

# Regression analysis, correlation analysis and hypothesis testing
# ================= REGRESSION ANALYSIS ================= #
# Convert categorical state variable to numeric factor for regression
state_summary$StateNumeric <- as.numeric(as.factor(state_summary$State))

# Perform linear regression
regression_model1 <- lm(mean_utilization ~ StateNumeric, data = state_summary)
summary(regression_model1)

# ================= CORRELATION ANALYSIS ================= #
# Spearman correlation test (since State is categorical)
correlation_test1 <- cor.test(state_summary$StateNumeric, state_summary$mean_utilization, method = "spearman")
print(correlation_test1)

# ================= HYPOTHESIS TESTING ================= #
# Checking normality assumption
shapiro_test1 <- shapiro.test(state_summary$mean_utilization)  # If p-value < 0.05, data is NOT normal
print(shapiro_test1)

# If data is NOT normal, use Kruskal-Wallis test instead of ANOVA
kruskal_test1 <- kruskal.test(mean_utilization ~ as.factor(State), data = state_summary)
print(kruskal_test1)

#Summarize average utilization by the different groups and view the summary
group_summary <- data %>%
  group_by(Group) %>%
  summarize(mean_utilization = mean(Value, na.rm = TRUE),
            median_utilization = median(Value, na.rm = TRUE),
            sd_utilization = sd(Value, na.rm = TRUE))
print(group_summary)

#Visualize the mental health service utlization of the different groups
ggplot(group_summary, aes(x = reorder(Group, -mean_utilization), y = mean_utilization, fill = Group)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip for better readability
  labs(title = "Mean Mental Health Care Utilization by Group",
       x = "Group",
       y = "Mean Utilization") +
  theme_minimal() +
  theme(legend.position = "none",  # Hide legend if not needed
        axis.text.y = element_text(size = 10)) +  # Adjust font size
  scale_y_continuous(labels = scales::comma) +  # Format y-axis numbers
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))  # Wrap text
#Create a box plot to see the distribution of utilization across different groups
ggplot(data, aes(x = Group, y = Value, fill = Group)) +
  geom_boxplot() +
  labs(title = "Distribution of Mental Health Care Utilization by Group",
       x = "Groups",
       y = "Utilization Rate") +
  theme_minimal() +
  theme(legend.position = "none",  # Hide legend if not needed
        axis.text.y = element_text(size = 10)) +  # Adjust font size
  scale_y_continuous(labels = scales::comma) +  # Format y-axis numbers
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20) #wrap the text
  ) + coord_flip()  # Flip for readability

#Hypothesis testing for data item group
# Checking normality assumption
shapiro_test2 <- shapiro.test(group_summary$mean_utilization)  # If p-value < 0.05, data is NOT normal
print(shapiro_test2)

# If data is NOT normal, use Kruskal-Wallis test instead of ANOVA
kruskal_test2 <- kruskal.test(mean_utilization ~ as.factor(Group), data = group_summary)
print(kruskal_test2)

#Summarize average utilization by the different subgroups and view the summary
subgroup_summary <- data %>%
  group_by(Subgroup) %>%
  summarize(mean_utilization = mean(Value, na.rm = TRUE),
            median_utilization = median(Value, na.rm = TRUE),
            sd_utilization = sd(Value, na.rm = TRUE))
print(subgroup_summary)

#Calculate mean utilization for each group and subgroup and sort by group
groupsummary <- data %>%
  group_by(Group, Subgroup) %>%
  summarize(mean_utilization = mean(Value, na.rm = TRUE)) %>%
  arrange(Group, desc(mean_utilization))  # Sort by Group first, then by utilization
print(groupsummary)

#Remove the state summary since they are examined separately
top_subgroups <- groupsummary %>%
  filter(Group != "By State") %>%  # Remove states since they are examined alone
  group_by(Group) %>%
  top_n(10, mean_utilization)  # Keep only the top 10 subgroups per Group 
print(top_subgroups)

#Select color for all the subgroups to be able to filter out the specific subgroups and plot them.
subgroup_colors <- c(
  "18 - 29 years" = "#66c2a5", "30 - 39 years" ="orange","40 - 49 years"="#a6d854", "50 - 59 years"="brown", "60 - 69 years"="lightblue", "70 - 79 years"="red", "80 years and above" ="#e5c494",
  "Male" = "blue", "Female" = "pink", "With disability" ="green", "Without disability"="orange",
  "Some college/Associate's degree"="darkorange", "Bachelor's degree or higher"="orange", "Less than a high school diploma"="#ffd92f", "High school diploma or GED"="#e5c494",
  "Cis-gender female"="pink", "Cis-gender male"="lightblue", "Transgender"="green","Experienced symptoms of anxiety/depression in past 4 weeks"="red", "Did not experience symptoms of anxiety/depression in the past 4 weeks"="green", 
  "Non-Hispanic, other races and multiple races"="brown", "Non-Hispanic White, single race"="#a6d854","Hispanic or Latino"="orange", "Non-Hispanic Black, single race"= "black", "Non-Hispanic Asian, single race"="gray",
  "Bisexual" = "#a6d854","Gay or lesbian" = "#ffd92f","Straight" = "#e5c494")

#Display the different subgroups arranged by the groups. 
ggplot(top_subgroups, aes(x = reorder(Subgroup, mean_utilization), y = mean_utilization, fill = Subgroup)) +
  geom_col() +
  facet_wrap(~ Group, scales = "free_y", labeller = labeller(Group = function(x) stringr::str_wrap(x, width = 15))) +  
  theme_minimal() +
  labs(
    title = "The Mean Utilization of The Different Groups", 
    x = "Subgroups", 
    y = "Mean Utilization"
  ) +
  theme(
    axis.text.y = element_text(size = 4.5),  
    axis.text.x = element_text(size = 8, vjust = 1),  
    strip.text = element_text(size = 7, margin = margin(b = 5)),  
    legend.position = "none"  
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = subgroup_colors) +  
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) +  
  coord_flip() 

#Filter for specific subgroups by age and visualize it.
filtered_data <- top_subgroups %>%
  filter(Subgroup %in% c("18 - 29 years", "30 - 39 years", "40 - 49 years", "50 - 59 years", "60 - 69 years", "70 - 79 years", "80 years and above"))
#plot
ggplot(filtered_data, aes(x = reorder(Subgroup, -mean_utilization), y = mean_utilization, fill = Subgroup)) +
  geom_col() +
  facet_wrap(~ Group, scales = "free_y") +  
  theme_minimal() +
  labs(title = "Mean Utilization for Subgroups Arouped by Age", 
       x = "Subgroup", 
       y = "Mean Utilization") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = subgroup_colors)  # Keep custom colors

#Age group analysis and the different indicators and the visualization of the age group and indicators and their average utlizations. 
# Compute the average values for each age group and indicator
age_data <- data %>%
  filter(Group == "By Age") %>%
  group_by(Subgroup, Indicator) %>%
  summarise(Average_Value = mean(Value, na.rm = TRUE), .groups = "drop")

# Scatter plot (no regression since Subgroup is categorical)
ggplot(age_data, aes(x = Subgroup, y = Average_Value, color = Indicator)) +
  geom_point(size = 4, position = position_jitter(width = 0.2)) +    
  labs(
    title = "Average Mental Health Care Utilization by Age Group",
    x = "Age Group",
    y = "Average Value",
    color = "Indicator"
  ) +  geom_smooth(method = "lm", se = FALSE, aes(group = Indicator), linetype = "dashed") +  # Add trendlines
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x labels for readability
    legend.text = element_text(size = 8)  # Adjust legend text size
  ) +
  scale_color_discrete(labels = function(x) stringr::str_wrap(x, width = 20))  # Wrap legend text
# Linear Regression Model
regression_model_age <- lm(Average_Value ~ Subgroup, data = age_data)
summary(regression_model_age)
# Correlation analysis
cor_test <- cor.test(age_data$Average_Value, as.numeric(as.factor(age_data$Subgroup)), method = "spearman")
print(cor_test)
# Checking normality assumption
shapiro_test3 <- shapiro.test(age_data$Average_Value)  # If p-value < 0.05, data is NOT normal
print(shapiro_test3)
# Kruskal-Wallis Test (since `Subgroup` is categorical)
kruskal_test3 <- kruskal.test(Average_Value ~ Subgroup, data = age_data)
print(kruskal_test3)

#Filter for specific subgroups by sex and visualize it.
filtered_data2 <- top_subgroups %>%
  filter(Subgroup %in% c("Male", "Female"))

ggplot(filtered_data2, aes(x = reorder(Subgroup, -mean_utilization), y = mean_utilization, fill = Subgroup)) +
  geom_col() +
  facet_wrap(~ Group, scales = "free_y") +  
  theme_minimal() +
  labs(title = "Mean Utilization for Subgroups Grouped by Sex", 
       x = "Subgroup", 
       y = "Mean Utilization") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  scale_fill_manual(values = subgroup_colors)  # Keep custom colors

#Sex analysis and the different indicators average utilization and its visualization.
sex_data <- data %>%
  filter(Group == "By Sex") %>%
  group_by(Subgroup, Indicator) %>%
  summarise(Average_Value = mean(Value, na.rm = TRUE), .groups = "drop")

# Scatter plot
ggplot(sex_data, aes(x = Subgroup, y = Average_Value, color = Indicator)) +
  geom_point(size = 4, position = position_jitter(width = 0.2)) +  
  theme_minimal() +
  labs(
    title = "Mental Health Care Utilization By Sex",
    x = "Sex",
    y = "Average Utilization Value",
    color = "Mental Health Indicator"
  ) +  geom_smooth(method = "lm", se = FALSE, aes(group = Indicator), linetype = "dashed") +  # Add trendlines
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.text.x = element_text(angle = 0, hjust = .5, size = 8),
        legend.text = element_text(size = 8)) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) +
  scale_color_discrete(labels = function(x) stringr::str_wrap(x, width = 20))

# Linear Regression Model
sex_data$Subgroup <- as.factor(sex_data$Subgroup)
sex_model <- lm(Average_Value ~ Subgroup, data = sex_data)
summary(sex_model)

# Correlation analysis
cor_test2 <- cor.test(sex_data$Average_Value, as.numeric(as.factor(sex_data$Subgroup)), method = "spearman")
print(cor_test2)

#hypothesis testing
if (length(unique(sex_data$Subgroup)) == 2) {
  t_test_result <- t.test(Average_Value ~ Subgroup, data = sex_data)
  print(t_test_result)
} else {
  anova_result <- aov(Average_Value ~ Subgroup, data = sex_data)
  print(summary(anova_result))
}

#Filter for specific subgroups by disability and visualize only the filtered subgroups. 
filtered_data3 <- top_subgroups %>%
  filter(Subgroup %in% c("With disability", "Without disability"))

ggplot(filtered_data3, aes(x = reorder(Subgroup, -mean_utilization), y = mean_utilization, fill = Subgroup)) +
  geom_col() +
  facet_wrap(~ Group, scales = "free_y") +  
  theme_minimal() +
  labs(title = "Mean Utilization for Subgroups Grouped by Disability", 
       x = "Subgroup", 
       y = "Mean Utilization") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  scale_fill_manual(values = subgroup_colors)  # Keep custom colors

#Disability analysis and the different indicators average utilization and their visualization.
# Filter and aggregate the data
disability_data <- data %>%
  filter(Group == "By Disability status") %>%
  group_by(Subgroup, Indicator) %>%
  summarise(Average_Value = mean(Value, na.rm = TRUE), .groups = "drop")

# Scatter plot (no regression since `Subgroup` is categorical)
ggplot(disability_data, aes(x = Subgroup, y = Average_Value, color = Indicator)) +
  geom_point(size = 4, position = position_jitter(width = 0.2)) +  
  theme_minimal() +
  labs(
    title = "Mental Health Care Utilization by Disability Status",
    x = "Disability Status",
    y = "Average Utilization Value",
    color = "Mental Health Indicator"
  ) +  geom_smooth(method = "lm", se = FALSE, aes(group = Indicator), linetype = "dashed") +  # Add trendlines
  theme(
    axis.text.x = element_text(angle = 0, hjust = .5),  # Rotate x labels
    legend.text = element_text(size = 8)  # Adjust legend text size
  ) +
  scale_color_discrete(labels = function(x) stringr::str_wrap(x, width = 20))  # Wrap legend text

# Linear Regression Model
disability_data$Subgroup <- as.factor(disability_data$Subgroup)
disability_model <- lm(Average_Value ~ Subgroup, data = disability_data)
summary(disability_model)

# Correlation analysis
cor_test3 <- cor.test(disability_data$Average_Value, as.numeric(as.factor(disability_data$Subgroup)), method = "spearman")
print(cor_test3)

# T-test if only 2 groups exist, otherwise ANOVA
if (length(unique(disability_data$Subgroup)) == 2) {
  t_test_result1 <- t.test(Average_Value ~ Subgroup, data = disability_data)
  print(t_test_result1)
} else {
  anova_result1 <- aov(Average_Value ~ Subgroup, data = disability_data)
  print(summary(anova_result1))
}

#Filter for specific subgroups by education and visualize it.
filtered_data4 <- top_subgroups %>%
  filter(Subgroup %in% c("Some college/Associate's degree", "Bachelor's degree or higher", "Less than a high school diploma", "High school diploma or GED"))

ggplot(filtered_data4, aes(x = reorder(Subgroup, -mean_utilization), y = mean_utilization, fill = Subgroup)) +
  geom_col() +
  facet_wrap(~ Group, scales = "free_y") +  
  theme_minimal() +
  labs(title = "Mean Utilization for Subgroups Grouped by Education", 
       x = "Subgroup", 
       y = "Mean Utilization") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) +  # Wrap long labels
  scale_fill_manual(values = subgroup_colors)  # Keep custom colors

#Education analysis and the different indicators average utilization and their visualization.
education_data <- data %>%
  filter(Group == "By Education") %>%
  group_by(Subgroup, Indicator) %>%
  summarise(Average_Value = mean(Value, na.rm = TRUE), .groups = "drop")

# Scatter plot
ggplot(education_data, aes(x = Subgroup, y = Average_Value, color = Indicator)) +
  geom_point(size = 4, position = position_jitter(width = 0.2)) +  
  theme_minimal() +
  labs(
    title = "Mental Health Care Utilization by Education",
    x = "Education Level",
    y = "Average Utilization Value",
    color = "Mental Health Indicator"
  )  +  geom_smooth(method = "lm", se = FALSE, aes(group = Indicator), linetype = "dashed") +  # Add trendlines
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(size = 8)) +
  scale_color_discrete(labels = function(x) stringr::str_wrap(x, width = 20))

# Correlation analysis
cor_test4 <- cor.test(education_data$Average_Value, as.numeric(as.factor(education_data$Subgroup)), method = "spearman")
print(cor_test4)

# Regression and normality tests
education_data$Subgroup <- as.factor(education_data$Subgroup)
education_model <- lm(Average_Value ~ Subgroup, data = education_data)
summary(education_model)

if (nrow(education_data) < 5000) {
  shapiro.test(education_data$Average_Value)
} else {
  ks.test(education_data$Average_Value, "pnorm", mean(education_data$Average_Value), sd(education_data$Average_Value))
}

kruskal_test4 <- kruskal.test(Average_Value ~ Subgroup, data = education_data)
print(kruskal_test4)


#Filter for specific subgroups by gender identity and visualize it.
filtered_data5 <- top_subgroups %>%
  filter(Subgroup %in% c("Cis-gender female", "Cis-gender male", "Transgender"))

ggplot(filtered_data5, aes(x = reorder(Subgroup, -mean_utilization), y = mean_utilization, fill = Subgroup)) +
  geom_col() +
  facet_wrap(~ Group, scales = "free_y") +  
  theme_minimal() +
  labs(title = "Mean Utilization for Subgroups Grouped by Gender Identity", 
       x = "Subgroup", 
       y = "Mean Utilization") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  scale_fill_manual(values = subgroup_colors)  # Keep custom colors

#Gender identity analysis and the different indicators average utilization and their visualization.
gender_data <- data %>%
  filter(Group == "By Gender identity") %>%
  group_by(Subgroup, Indicator) %>%
  summarise(Average_Value = mean(Value, na.rm = TRUE), .groups = "drop")

# Scatter plot
ggplot(gender_data, aes(x = Subgroup, y = Average_Value, color = Indicator)) +
  geom_point(size = 4, position = position_jitter(width = 0.2)) +  
  theme_minimal() +
  labs(
    title = "Mental Health Care Utilization by Gender Identity",
    x = "Gender Identity",
    y = "Average Utilization Value",
    color = "Mental Health Indicator"
  ) +  geom_smooth(method = "lm", se = FALSE, aes(group = Indicator), linetype = "dashed") +  # Add trendlines
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(size = 8)) +
  scale_color_discrete(labels = function(x) stringr::str_wrap(x, width = 20))
# regression model
gender_data$Subgroup <- as.factor(gender_data$Subgroup)
gender_model <- lm(Average_Value ~ Subgroup, data = gender_data)
summary(gender_model)
# Correlation analysis
cor_test5 <- cor.test(gender_data$Average_Value, as.numeric(as.factor(gender_data$Subgroup)), method = "spearman")
print(cor_test5)
# hypothesis testing 
if (nrow(gender_data) < 5000) {
  shapiro.test(gender_data$Average_Value)
} else {
  ks.test(gender_data$Average_Value, "pnorm", mean(gender_data$Average_Value), sd(gender_data$Average_Value))
}

kruskal_test5 <- kruskal.test(Average_Value ~ Subgroup, data = gender_data)
print(kruskal_test5)

#Filter for specific subgroups by race/ ethinicity and visualize it.
filtered_data6 <- top_subgroups %>%
  filter(Subgroup %in% c("Non-Hispanic, other races and multiple races", "Non-Hispanic White, single race", "Hispanic or Latino", "Non-Hispanic Black, single race", "Non-Hispanic Asian, single race"))

ggplot(filtered_data6, aes(x = reorder(Subgroup, -mean_utilization), y = mean_utilization, fill = Subgroup)) +
  geom_col() +
  facet_wrap(~ Group, scales = "free_y") +  
  theme_minimal() +
  labs(title = "Mean Utilization for Subgroups Grouped by Race/ Hispanic Ethnicity", 
       x = "Subgroup", 
       y = "Mean Utilization") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) +  # Wrap long labels
  scale_fill_manual(values = subgroup_colors)  # Keep custom colors

# Compute the average values for each age group and indicator
# Filter and aggregate the data
race_data <- data %>%
  filter(Group == "By Race/Hispanic ethnicity") %>%
  group_by(Subgroup, Indicator) %>%
  summarise(Average_Value = mean(Value, na.rm = TRUE), .groups = "drop")
# Scatter plot (no regression since Subgroup is categorical)
ggplot(race_data, aes(x = Subgroup, y = Average_Value, color = Indicator)) +
  geom_point(size = 4, position = position_jitter(width = 0.2)) +  
  labs(
    title = "Average Mental Health Care Utilization by Race/ Ethnicity",
    x = "Race/Hispanic ethnicity",
    y = "Average Value",
    color = "Indicator"
  ) +  geom_smooth(method = "lm", se = FALSE, aes(group = Indicator), linetype = "dashed") +  # Add trendlines
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x labels for readability
    legend.text = element_text(size = 8)  # Adjust legend text size
  ) +
  scale_color_discrete(labels = function(x) stringr::str_wrap(x, width = 20))  # Wrap legend text

# Linear Regression Model
regression_model_race <- lm(Average_Value ~ Subgroup, data = race_data)
summary(regression_model_race)

# Correlation analysis
cor_test6 <- cor.test(race_data$Average_Value, as.numeric(as.factor(race_data$Subgroup)), method = "spearman")
print(cor_test6)
# Checking normality assumption
shapiro_test4 <- shapiro.test(race_data$Average_Value)  # If p-value < 0.05, data is NOT normal
print(shapiro_test4)
# Kruskal-Wallis Test (since `Subgroup` is categorical)
kruskal_test6 <- kruskal.test(Average_Value ~ Subgroup, data = race_data)
print(kruskal_test6)

#Filter for specific subgroups by sexual identity  and visualize it.
filtered_data7 <- top_subgroups %>%
  filter(Subgroup %in% c("Gay or lesbian", "Straight", "Bisexual"))

ggplot(filtered_data7, aes(x = reorder(Subgroup, -mean_utilization), y = mean_utilization, fill = Subgroup)) +
  geom_col() +
  facet_wrap(~ Group, scales = "free_y") +  
  theme_minimal() +
  labs(title = "Mean Utilization for Subgroups Grouped by Sexual Identity", 
       x = "Subgroup", 
       y = "Mean Utilization") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  scale_fill_manual(values = subgroup_colors)  # Keep custom colors

#Sexual orientation analysis and the different indicators average utilization and their visualization.
# Grouping data by Sexual orientation and Indicator, then calculating mean value
orientation_data <- data %>%
  filter(Group == "By Sexual orientation") %>%
  group_by(Subgroup, Indicator) %>%
  summarise(Average_Value = mean(Value, na.rm = TRUE), .groups = "drop")

# Scatter plot (no regression since Subgroup is categorical)
ggplot(orientation_data, aes(x = Subgroup, y = Average_Value, color = Indicator)) +
  geom_point(size = 4, position = position_jitter(width = 0.2)) +  
  geom_smooth(method = "lm", se = FALSE, aes(group = Indicator), linetype = "dashed") +  # Add trendlines
  theme_minimal() +
  labs(
    title = "Mental Health Care Utilization By Sexual Orientation",
    x = "Sexual Orientation",
    y = "Average Utilization Value",
    color = "Mental Health Indicator"
  ) +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),  # Adjust title size and center it
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Rotate x labels for clarity
    legend.text = element_text(size = 8)  # Adjust legend text size
  ) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) +  # Wrap x-axis labels
  scale_color_discrete(labels = function(x) stringr::str_wrap(x, width = 20))  # Wrap legend text

# Convert Subgroup to Factor
orientation_data$Subgroup <- as.factor(orientation_data$Subgroup)

# Linear Regression Model
regression_model_orientation <- lm(Average_Value ~ Subgroup, data = orientation_data)
summary(regression_model_orientation)

# Correlation analysis
cor_test7 <- cor.test(orientation_data$Average_Value, as.numeric(as.factor(orientation_data$Subgroup)), method = "spearman")
print(cor_test7)

# Statistical Tests for Normality and Group Differences
shapiro_result <- shapiro.test(orientation_data$Average_Value)
print(shapiro_result)  # Print test results

if (shapiro_result$p.value < 0.05) {
  # If data is not normally distributed, use Kruskal-Wallis
  kruskal_result <- kruskal.test(Average_Value ~ Subgroup, data = orientation_data)
  print(kruskal_result)
} else {
  # If data is normal, ANOVA could be used (if needed)
  anova_result2 <- aov(Average_Value ~ Subgroup, data = orientation_data)
  print(summary(anova_result2))
}

#Filter for specific subgroups by presence of symptoms  and visualize it.
filtered_data8 <- top_subgroups %>%
  filter(Subgroup %in% c("Experienced symptoms of anxiety/depression in past 4 weeks", 
                         "Did not experience symptoms of anxiety/depression in the past 4 weeks"))

ggplot(filtered_data8, aes(x = reorder(Subgroup, -mean_utilization), y = mean_utilization, fill = Subgroup)) +
  geom_col() +
  facet_wrap(~ Group, scales = "free_y") +  
  theme_minimal() +
  labs(title = "Mean Utilization for Subgroups Grouped by Presence of Symptoms \n of Anxiety/Depression", 
       x = "Subgroup", 
       y = "Mean Utilization") +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, size = 10),  # Slight rotation for readability
    legend.text = element_text(size = 9),  # Adjust legend text size
    legend.key.size = unit(0.8, "cm"),  # Reduce legend key size
    legend.spacing.y = unit(0.5, "cm")  # Reduce vertical spacing between legend items
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +  # Wrap x-axis text
  scale_fill_manual(values = subgroup_colors, 
                    labels = function(x) str_wrap(x, width = 25)) +  # Wrap legend text
  guides(fill = guide_legend(ncol = 1))  # Arrange legend items in a single column

#Sympton analysis and the different indicator average utilization and their visualization.
# Filter and aggregate the data
symptom_data <- data %>%
  filter(Group == "By Presence of Symptoms of Anxiety/Depression") %>%
  group_by(Subgroup, Indicator) %>%
  summarise(Average_Value = mean(Value, na.rm = TRUE), .groups = "drop")

# Scatter plot (no regression since `Subgroup` is categorical)
ggplot(symptom_data, aes(x = Subgroup, y = Average_Value, color = Indicator)) +
  geom_point(size = 4, position = position_jitter(width = 0.2)) +  
  theme_minimal() +
  labs(
    title = "Mental Health Care Utilization by Presence of Symptoms of Anxiety/Depression",
    x = "Presence of Symptoms of Anxiety/Depression",
    y = "Average Utilization Value",
    color = "Mental Health Indicator"
  ) +  geom_smooth(method = "lm", se = FALSE, aes(group = Indicator), linetype = "dashed") +  # Add trendlines
  theme(
    axis.text.x = element_text(angle = 0, hjust = .5),  # Rotate x labels
    legend.text = element_text(size = 8)  # Adjust legend text size
  ) + scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +  # Wrap x-axis text
  scale_color_discrete(labels = function(x) stringr::str_wrap(x, width = 20))  # Wrap legend text

# Linear Regression Model
symptom_data$Subgroup <- as.factor(symptom_data$Subgroup)
symptom_model <- lm(Average_Value ~ Subgroup, data = symptom_data)
summary(symptom_model)

# Correlation analysis
cor_test8 <- cor.test(symptom_data$Average_Value, as.numeric(as.factor(symptom_data$Subgroup)), method = "spearman")
print(cor_test8)

# T-test if only 2 groups exist, otherwise ANOVA
if (length(unique(symptom_data$Subgroup)) == 2) {
  t_test_result2 <- t.test(Average_Value ~ Subgroup, data = symptom_data)
  print(t_test_result2)
} else {
  anova_result3 <- aov(Average_Value ~ Subgroup, data = symptom_data)
  print(summary(anova_result3))
}

#Summarize average utilization throughout the different phases and view the summary.
phase_summary <- data %>%
  group_by(Phase) %>%
  summarize(mean_utilization = mean(Value, na.rm = TRUE),
            median_utilization = median(Value, na.rm = TRUE),
            sd_utilization = sd(Value, na.rm = TRUE))
print(phase_summary)

#Create a Bar plot to compare the mean utilization across the different phases.
ggplot(phase_summary, aes(x = Phase, y = mean_utilization, fill = Phase)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Mental Health Care Utilization by Phase",
       x = "Phase",
       y = "Mean Utilization") +
  theme_minimal() + theme(axis.text.x = element_text(size = 7.5)) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10))  # Wrap labels

#Create a line plot to showing trend of utilization throught the phases.
ggplot(phase_summary, aes(x = as.factor(Phase), y = mean_utilization, group = 1)) +
  geom_line(color = "black", size = 1) + 
  geom_smooth(method = "lm",se = FALSE, color = "blue") +
  geom_point(size = 3, color = "red") +
  labs(title = "Trend of Mental Health Care Utilization Across Phases",
       x = "Phase",
       y = "Mean Utilization") +
  theme_minimal() +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10))  # Wrap labels

#Create a box plot to show full distribution of data points in each phase.
ggplot(data, aes(x = as.factor(Phase), y = Value, fill = Phase)) +
  geom_boxplot() +
  labs(title = "Distribution of Mental Health Care Utilization by Phase",
       x = "Phase",
       y = "Utilization Rate") +
  theme_minimal() + theme(axis.text.x = element_text(size = 7.5)) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10))  # Wrap labels

# ================== REGRESSION ANALYSIS ================== #
# Linear Regression: Predicting mean_utilization using Phase
regression_model2 <- lm(mean_utilization ~ as.numeric(Phase), data = phase_summary)
regression_summary <- summary(regression_model2)
print(regression_summary)

# ================== CORRELATION ANALYSIS ================== #
# Spearman correlation since Phase is categorical (ordinal)
correlation_test2 <- cor.test(as.numeric(phase_summary$Phase), phase_summary$mean_utilization, method = "spearman")
print(correlation_test2)

# ================== HYPOTHESIS TESTING ================== #
# Normality test (Shapiro-Wilk)
shapiro_test5 <- shapiro.test(phase_summary$mean_utilization)  # If p < 0.05, data is NOT normal
print(shapiro_test5)

anova_test2 <- aov(mean_utilization ~ as.factor(Phase), data = phase_summary)
summary(anova_test2)