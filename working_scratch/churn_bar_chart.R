# 1. Install and load the ggplot2 library.
# If you don't have ggplot2 installed, uncomment the line below and run it.
# install.packages("ggplot2")
library(ggplot2)
library(plotly)
library(htmlwidgets)

# 2. Create a sample data frame that includes customer name, fiscal period, and expected churn.
# The `fiscal_period` is converted to a factor to ensure proper ordering on the x-axis.
data <- op_list$renewals %>%
  select(`Account Name`,`Fiscal Period`,Amount,expected_churn)

data$`Fiscal Period`=factor(data$`Fiscal Period`,levels=fiscal.period)

c=load_constants()
plan_df <- c$churn_plan



aggregated_data <- data %>%
  group_by(`Fiscal Period`) %>%
  summarise(Total_Amount = sum(expected_churn, na.rm = TRUE), .groups = "drop")

aggregated_data$`Fiscal Period`=factor(aggregated_data$`Fiscal Period`,levels=fiscal.period)

p <- ggplot(data, aes(x = `Fiscal Period`, y = expected_churn, fill = expected_churn, text = paste("Account Name: ",`Account Name`,", Amount: $", Amount,", Expected Churn: ",expected_churn))) +
  geom_bar(stat = "identity") +
  # Add clear and descriptive labels to the chart.
  labs(
    title = "Expected Churn by Customer and Fiscal Period",
    x = "Fiscal Period",
    y = "Expected Churn ($)"
  ) +
  # Use a clean and professional theme.
  theme_minimal()+
  scale_fill_gradient(low = "gray80", high = "gray20")+
  theme(legend.position = "none")

# 4. Convert the static ggplot chart to an interactive plotly chart.
# The `ggplotly()` function is used with the `tooltip` argument to specify which aesthetics
# should be displayed on hover. This ensures the custom 'Amount' field is shown.
p<-ggplotly(p, tooltip = c("text"))



# Assuming your plot is named 'p' as in your script,
# use the saveWidget function to save it to an HTML file.
saveWidget(p, "output/interactive_churn_plot.html",selfcontained=TRUE)



# --- User-Provided Plan Data ---
# This vector represents the planned amount of churn for each quarter.

# Merge the aggregated data with the plan data frame
plot_data <- left_join(aggregated_data, plan_df, by = "Fiscal Period")

# --- 2. Plotting with ggplot2 ---
# Create the bar plot using the aggregated data
p <- ggplot(plot_data, aes(x = `Fiscal Period`, y = Total_Amount)) +

  # Create the bar chart
  geom_bar(stat = "identity", fill = "#83A9C8", width = 0.7) +

  # --- ADDITION 1: Add the red horizontal plan lines ---
  # This uses geom_segment to draw a line for each bar at the 'plan_amount' value.
  # The x and xend values are calculated to center the line on the bar.
  #geom_segment(aes(y = plan_amount, yend = plan_amount,
  #                 x = as.numeric(as.factor(`Fiscal Period`)) - 0.35,
  #                 xend = as.numeric(as.factor(`Fiscal Period`)) + 0.35),
  #             color = "#C85A5A", linetype = "dashed", linewidth = 1.2) +

  # --- ADDITION 2: Add the totals on top of the bars ---
  # This uses geom_text to place the 'Total_Amount' label.
  # The 'vjust' parameter moves the text slightly above the bar.
  geom_text(aes(label = paste0("$", round(Total_Amount, 0))),
            vjust = -0.5,
            color = "black",
            size = 4) +

  # --- Customize Plot Appearance ---
  labs(
    title = "Expected Churn by Fiscal Period",
    x = "Fiscal Period",
    y = "Total Expected Churn Amount ($)"
  ) +

  theme_minimal(base_size = 12) +

  # Adjust y-axis limits to ensure the labels are visible
  scale_y_continuous(limits = c(0, max(plot_data$Total_Amount, plot_data$plan_amount) * 1.15))

# 3. Convert to an interactive plotly chart
# The tooltip now shows the total amount for the bar and the plan amount for the line.
ggplotly(p, tooltip = c("x", "y"))


