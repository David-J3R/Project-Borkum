# Load data
borkum_data <- read.csv('Borkum Project/borkum_data_full 240627.csv')

# loading libraries
library(ggplot2)
library(dplyr)

# verify that all data load correctly
head(borkum_data)
# View the structure of the data
str(borkum_data)
# Summary statistics 
summary(borkum_data)

# defining my theme
mysize <- 15
mytheme <- theme(
  axis.title = element_text(size=mysize),
  axis.text = element_text(size=mysize),
  legend.title = element_text(size=mysize),
  legend.text = element_text(size=mysize))
my_colors <- c("#F26419", "#A9CBB7")
color_label = "#070B0E"
color_main = "#F26419"
color_sec = "#A9CBB7"





### Pie chart (guest/residents)

# Summarize the counts
guestResident_data <- borkum_data %>%
  group_by(VB03) %>% #create a data set with the count of guest(1) and residents (2)
  summarise(count = n())

# Convert 1 and 2 to 'Guest' and 'Resident'
guestResident_data$VB03 <- factor(guestResident_data$VB03, levels = c(1, 2), labels = c("Guest", "Resident"))

# Calculate percentages
guestResident_data <- guestResident_data %>%
  mutate(percentage = count / sum(count) * 100) #add a new column with the percentage to data set

# Create the pie chart
piechart <- ggplot(guestResident_data, aes(x = "", y = count, fill = VB03)) + #Fill with complete data
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") + #transform to pie chart
  geom_text(aes(label = paste0(round(percentage, 1), "%"), fontface = "bold"), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Distribution of Guests and Residents", x = "", y = "") + #format
  theme_void() + 
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = my_colors)

# Save the plot with transparent background
ggsave("piechart.png", plot = piechart, bg = "transparent")



### Average of sustainable behavior across ages at vacation

# Create groups of ages
#Create 6 groups to better visualization
borkum_data$age_group <- cut(borkum_data$VB12_01, #add a new column
                             breaks = c(15, 20, 30, 40, 50, 60, 80), #c() to combine vectors
                             labels = c("15-20", "21-30", "31-40", "41-50", "51-60", '61+'),
                             right = FALSE)

# Calculate mean engagement by age group
#using dplyr
mean_sustainable_by_age <- borkum_data %>%  #use dplyr to chain data
  group_by(age_group) %>%
  summarize(mean_engagement = mean(NA02_Mean))

# plot the data
#visualize and check how do data look
ggplot(mean_sustainable_by_age, aes(x = age_group, y = mean_engagement)) +
  geom_col(fill = "skyblue") +
  geom_text(aes(label = round(mean_engagement, 2)), vjust = -0.5) +  # Add mean value labels
  labs(
    title = "Average Sustainability Behavior Across Age Groups",
    x = "Age Group",
    y = "Mean Percentage Engagement"
  ) +
  theme_minimal() #make minimalist plot

# Create the plot
boxplot_vacation <- ggplot(borkum_data, aes(x = age_group, y = NA02_Mean)) + #save the plot in a variable for later export
  geom_boxplot(fill = color_main, alpha = 0.6, outlier.shape = NA) +  # Boxplot without outliers
  geom_jitter(width = 0.2, color = "darkblue", alpha = 0.5) +  # Jittered points
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, color = color_label) +  # Highlight mean
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)), vjust = -1.5,
               hjust = 2.8, fontface = "bold", color = color_label) +  # Label mean
  labs(
    title = "Average Sustainable Behavior Across Age at vacation",
    x = "Age Group",
    y = "Sustainable Behavior"
  ) +
  theme_minimal() + mytheme
# Save the plot with transparent background
ggsave("boxplot_vacation.png", plot = boxplot_vacation, bg = "transparent")



### Average of sustainable behavior across ages at home

# same process of the plot above
# Calculate mean engagement by age group
#using dplyr
mean_homesustainable_by_age <- borkum_data %>% 
  group_by(age_group) %>%
  summarize(mean_engagement = mean(UV01_Mean))

# plot the data
ggplot(mean_homesustainable_by_age, aes(x = age_group, y = mean_engagement)) +
  geom_col(fill = "skyblue") +
  geom_text(aes(label = round(mean_engagement, 2)), vjust = -0.5) +  # Add mean value labels
  labs(
    title = "Average Sustainability Behavior Across Age Groups at home",
    x = "Age Group",
    y = "Mean Percentage Engagement"
  ) +
  theme_minimal()

# Create the plot
boxplot_home <- ggplot(borkum_data, aes(x = age_group, y = UV01_Mean)) +
  geom_boxplot(fill = color_main, alpha = 0.6, outlier.shape = NA) +  # Boxplot without outliers
  geom_jitter(width = 0.2, color = "darkblue", alpha = 0.5) +  # Jittered points
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, color = color_label) +  # Highlight mean
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)), vjust = -1.5,
               hjust = 2.8, fontface = "bold", color = color_label) +  # Label mean
  labs(
    title = "Average Sustainable Behavior Across Age at Home",
    x = "Age Group",
    y = "Sustainable Behavior"
  ) +
  theme_minimal() + mytheme
# Save the plot with transparent background
ggsave("boxplot_home.png", plot = boxplot_home, bg = "transparent")



### Create the plot with overlapping bar charts
# see the overlapping between both behaviors
# Combine mean values with labels
mean_homeVac <- rbind( #create a data set with the average behaviors 
  transform(mean_homesustainable_by_age, dataset = "Home"),
  transform(mean_sustainable_by_age, dataset = "Vacation"))


stackedplot <- ggplot() + #use different geom_bar() to overlap the bars
  geom_bar(data = mean_homesustainable_by_age, aes(x = age_group, y = mean_engagement, fill = "Home"),
           stat = "identity", alpha = 0.6, position = position_dodge(width = 0.75)) + #format position
  geom_bar(data = mean_sustainable_by_age, aes(x = age_group, y = mean_engagement, fill = "vacation"),
           stat = "identity", alpha = 1, position = position_dodge(width = 0.75)) +
  geom_text(data = mean_homeVac, aes(x = age_group, y = mean_engagement, 
                                  label = paste("Mean:", round(..y.., 2)), group = dataset), #label() show mean of each bar
            position = position_dodge(width = 0.75), vjust = -0.5, size = 3, color = color_label, fontface = "bold") +
  labs(
    title = "Comparison of Sustainable Behaviors Between Home and Vacation",
    x = "Age Group",
    y = "Sustainable Behavior"
  ) +
  scale_fill_manual(name = "Behavior Context", values = c("Home" = color_sec, "vacation" = color_main)) +  # fill colors
  theme_minimal() +
  mytheme
# Save the plot with transparent background
ggsave("stackedplot.png", plot = stackedplot, bg = "transparent")



### Why traveling to Borkum Barplot

# Subset borkum data to include only VB09 (why traveling to Borkum)
columns_of_vb09 <- c("VB09_01", "VB09_02", "VB09_03", "VB09_04", "VB09_05", 
                         "VB09_06", "VB09_07", "VB09_08", "VB09_09", "VB09_10",
                         "VB09_11", "VB09_12")
data_vb09 <- borkum_data[, columns_of_vb09] #create new data set

# Calculate mean of each column
means <- colMeans(data_vb09, na.rm = TRUE) #na.rm for 'Na' data

# Create a data frame for plotting
# assign names to variables
names_vb09 <- c("Reachability", "Gastronomy", "Low crime", "Climate", "Cost", 
                     "Companion", "Sustainability", "Culture", "Personal req.", "Medical care",
                     "Accom. standards", "Nature")
mean_vb09 <- data.frame(
  column = names_vb09,
  mean_value = means
)
# sorting in descending order
mean_vb09 <- mean_vb09[order(mean_vb09$mean_value), ] #important to have barplots in order

# Convert 'column' to factor with correct levels for plotting
mean_vb09$column <- factor(mean_vb09$column, levels = mean_vb09$column)

# Create bar plot using ggplot2
barplot <- ggplot(mean_vb09, aes(x = mean_value, y = column)) +
  geom_bar(stat = "identity", fill = color_main) +
  geom_text(aes(label = round(mean_value, 2)), vjust = -0.3, size = 3.5, fontface = "bold") +  # Add labels to bars
  labs(
    title = "Reasons for Traveling to Borkum",
    x = "Influences",
    y = "Factors"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) + # Rotate x-axis labels
  mytheme
# Save the plot with transparent background
ggsave("barplot.png", plot = barplot, bg = "transparent")




### Average of Eco-stress levels
# Calculate mean
mean_ecostress <- mean(borkum_data$ES_Mean)

# Create ggplot object with histogram and density plot
histogram <- ggplot(borkum_data, aes(x = ES_Mean)) +
  geom_histogram(aes(y = ..density..), bins = 20, color = color_sec, fill = color_main, alpha = 0.7) +
  geom_density(color = "darkblue", size = 1) +
  geom_vline(xintercept = mean_ecostress, color = color_label, linetype = "dashed", size = 1) +  # Add mean line
  annotate("text", x = mean_ecostress, y = 0.019, label = paste("Mean:", round(mean_ecostress, 2)), 
           color = color_label, vjust = -0.5, hjust = 0, fontface="bold") +  # Add text label for mean
  labs(
    title = "Eco-stress Distribution",
    x = "Level of Eco-stress",
    y = "Density"
  ) +
  theme_minimal() +
  mytheme
# Save the plot with transparent background
ggsave("histogram.png", plot = histogram, bg = "transparent")



### mapping of color

# Create the ggplot object for the tile plot with matching style
colormap <- ggplot(borkum_data, aes(x = OU02_01, y = OU02_02, fill = OU01_01)) + 
  geom_tile(size = 1, width = 1, height = 1) +
  scale_fill_gradient(low = "#FEF2EC", high = "#F26419") +  # Adjust the fill gradient
  labs(
    title = "Relationship of Mental & Physical Health to Feeling Embedded in Nature",
    x = "Physical Health",
    y = "Mental Health",
    fill = "Feeling immersed
    in nature"
  ) +
  theme_minimal() + #minimalistic style
  theme( #Manual theme, not mytheme
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )
# Save the plot with transparent background
ggsave("colormap.png", plot = colormap, bg = "transparent")

  