###### OUT OF SCHOOL RATE STATISTICAL ANALYSIS 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(dplyr)
library(gridExtra)

# set working directory path
df = read.csv("OOS_Rate_Countries.csv")
head(df)

# Renaming columns names
colnames(df) <- c("Country_Name", "Country_Code", "level", "sex", "year", "value", "lower", "upper")

head(df)



# Statistical Analysis

# Descriptive Statistics

DS <- filter(df,level == 'all', sex != 'total') %>%
  group_by(year) %>%
  summarise(mean_value = mean(value), std = sd(value))
print(DS)



combined_plot <- ggplot(DS, aes(x = as.factor(year))) +
  # bar plot for mean values
  geom_bar(aes(y = mean_value), stat = "identity", fill = "skyblue", alpha = 0.5) +
  # line plot for standard deviation
  geom_line(aes(y = std, group = 1), color = "red") +
  labs(title = "Mean Values and Standard Deviation by Year", x = "Year", y = "") +
  theme_minimal()
combined_plot <- combined_plot + theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(combined_plot)



#T-Test

group_1 <- filter(df,level == 'all', sex == 'female') %>%
  group_by(year) %>%
  summarise(mean_value = mean(value))
print(group_1)

group_2 <- filter(df,level == 'all', sex == 'male') %>%
  group_by(year) %>%
  summarise(mean_value = mean(value))
print(group_2)


# two-sample t-test
t_test_result <- t.test(group_1$mean_value, group_2$mean_value)

# t-test result
print(t_test_result)

# p-value = 0.319. As the p-value is greater than 0.05 we can conclude that
# statistically there is no significant difference between the means of Group 1 and Group 2
# So we can conclude there is no significant gender bias.


# density plots for group_1 and group_2

# density plot for group_1
density_plot_group_1 <- ggplot(group_1, aes(x = mean_value, fill = "Group 1")) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot for Group 1",
       x = "Mean Value", y = "Density") +
  theme_minimal() +
  scale_fill_manual(values = "blue")

# density plot for group_2
density_plot_group_2 <- ggplot(group_2, aes(x = mean_value, fill = "Group 2")) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot for Group 2",
       x = "Mean Value", y = "Density") +
  theme_minimal() +
  scale_fill_manual(values = "red")

# density plots side by side
library(gridExtra)
grid.arrange(density_plot_group_1, density_plot_group_2, ncol = 2)


# density plot for both groups overlapped
density_plot_both <- ggplot() +
  geom_density(data = group_1, aes(x = mean_value, fill = "Group 1"), alpha = 0.5) +
  geom_density(data = group_2, aes(x = mean_value, fill = "Group 2"), alpha = 0.5) +
  labs(title = "Density Plot for Group 1 and Group 2",
       x = "Mean Value", y = "Density") +
  theme_minimal() +
  scale_fill_manual(values = c("Group 1" = "blue", "Group 2" = "red"))

# Display the overlapped density plot
print(density_plot_both)


#ANOVA


# Load the necessary library (if not already loaded)
library(stats)

data <- df %>%
  filter(level != "all" & sex == "total") %>%
  group_by(level,year) %>%
  summarise(mean_value = mean(value))
print(data)

# Perform a one-way ANOVA
anova_model <- aov(mean_value ~ level, data = data)

# Display the ANOVA results
summary(anova_model)
# The ANOVA results you provided indicate that there is a statistically significant difference in the means of the 'mean_value' variable among the different levels ('level').
#`Pr(>F)`: The p-value is less than 2e-16 (almost zero)
# which means that there is strong evidence to reject the null hypothesis, indicating that at least one group mean is different from the others.
# Significance codes: '***' indicates highly significant differences.
# Based on these results, we can conclude that there are significant differences in the 'mean_value' among the different levels ('level').


