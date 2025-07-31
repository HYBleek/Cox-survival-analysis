## ----include=FALSE, warning=FALSE--------------------------------------------------------------------------------
data = read.csv('data.csv')


## ----echo=FALSE, warning=FALSE-----------------------------------------------------------------------------------
# Custom theme for better visualization
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(viridis)
library(patchwork)

# Create custom theme
my_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Custom color palette
survival_colors <- c("#3498db", "#e74c3c")


## ----echo=FALSE, warning=FALSE-----------------------------------------------------------------------------------
# Count of death events
p1 <- ggplot(data, aes(x = factor(DEATH_EVENT, labels = c("Survived", "Died")))) +
  geom_bar(fill = survival_colors, width = 0.6) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Distribution of Outcomes",
       x = "Outcome",
       y = "Count") +
  my_theme

# Calculate percentages for pie chart
outcome_counts <- table(data$DEATH_EVENT)
outcome_pct <- round(100 * outcome_counts / sum(outcome_counts), 1)
pie_data <- data.frame(
  outcome = c("Survived", "Died"),
  count = as.numeric(outcome_counts),
  pct = as.numeric(outcome_pct)
)

# Pie chart
p2 <- ggplot(pie_data, aes(x = "", y = count, fill = outcome)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = survival_colors) +
  labs(title = "Percentage of Outcomes",
       fill = "Outcome") +
  geom_text(aes(label = paste0(pct, "%")), 
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5))

# Combine plots
p1 + p2 + plot_layout(ncol = 2)


## ----echo=FALSE, warning=FALSE-----------------------------------------------------------------------------------
# Create a function for histogram + density plots
hist_density_plot <- function(variable, title, color, binwidth = NULL) {
  p <- ggplot(data, aes(x = !!sym(variable))) +
    geom_histogram(aes(y = after_stat(density)), 
                  fill = color, color = "white", alpha = 0.7,
                  binwidth = binwidth) +
    geom_density(alpha = 0.2, fill = color) +
    labs(title = title,
         x = variable,
         y = "Density") +
    my_theme
  return(p)
}

# Function for boxplots by outcome
boxplot_by_outcome <- function(variable, title, y_label) {
  p <- ggplot(data, aes(x = factor(DEATH_EVENT, labels = c("Survived", "Died")), 
                       y = !!sym(variable),
                       fill = factor(DEATH_EVENT, labels = c("Survived", "Died")))) +
    geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.fill = "white") +
    scale_fill_manual(values = survival_colors) +
    labs(title = title,
         x = "Outcome",
         y = y_label,
         fill = "Outcome") +
    my_theme +
    theme(legend.position = "none")
  return(p)
}

# Age plots
p1 <- hist_density_plot("age", "Age Distribution", "#3498db")
p2 <- boxplot_by_outcome("age", "Age by Outcome", "Age (years)")

# Ejection fraction plots
p3 <- hist_density_plot("ejection_fraction", "Ejection Fraction", "#2ecc71")
p4 <- boxplot_by_outcome("ejection_fraction", "Ejection Fraction by Outcome", "Ejection Fraction (%)")

# Serum creatinine plots
p5 <- hist_density_plot("serum_creatinine", "Serum Creatinine", "#9b59b6")
p6 <- boxplot_by_outcome("serum_creatinine", "Serum Creatinine by Outcome", "Serum Creatinine (mg/dL)")

# Combine plots
(p1 + p2) / (p3 + p4) / (p5 + p6)


## ----echo=FALSE, warning=FALSE-----------------------------------------------------------------------------------
# Platelets plots
p1 <- hist_density_plot("platelets", "Platelets Distribution", "#f1c40f")
p2 <- boxplot_by_outcome("platelets", "Platelets by Outcome", "Platelets (kiloplatelets/mL)")

# Creatinine phosphokinase plots
p3 <- hist_density_plot("creatinine_phosphokinase", "CPK Distribution", "#e67e22", binwidth = 100)
p4 <- boxplot_by_outcome("creatinine_phosphokinase", "CPK by Outcome", "CPK (mcg/L)")

# Serum sodium plots
p5 <- hist_density_plot("serum_sodium", "Serum Sodium Distribution", "#16a085")
p6 <- boxplot_by_outcome("serum_sodium", "Serum Sodium by Outcome", "Serum Sodium (mEq/L)")

# Combine plots
(p1 + p2) / (p3 + p4) / (p5 + p6)


## ----echo=FALSE, warning=FALSE, fig.width=10, fig.height=8, out.width="100%"-------------------------------------
# Function to create enhanced bar charts for binary variables
plot_binary_variable <- function(variable, var_label, color1, color2) {
  # Prepare data
  counts_data <- as.data.frame(table(data[[variable]], data$DEATH_EVENT))
  names(counts_data) <- c("Variable", "Outcome", "Count")
  counts_data$Variable <- ifelse(counts_data$Variable == 0, "No", "Yes")
  counts_data$Outcome <- ifelse(counts_data$Outcome == 0, "Survived", "Died")
  
  # Create plot
  p <- ggplot(counts_data, aes(x = Variable, y = Count, fill = Outcome)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    scale_fill_manual(values = c(color1, color2)) +
    geom_text(aes(label = Count), 
              position = position_dodge(width = 0.7), 
              vjust = -0.5, size = 3) +
    labs(title = var_label,
         x = NULL,
         y = "Count") +
    my_theme
  return(p)
}

# Create plots for all binary variables
p1 <- plot_binary_variable("anaemia", "Anaemia", "#3498db", "#e74c3c")
p2 <- plot_binary_variable("diabetes", "Diabetes", "#3498db", "#e74c3c")
p3 <- plot_binary_variable("high_blood_pressure", "High Blood Pressure", "#3498db", "#e74c3c")
p4 <- plot_binary_variable("sex", "Sex (1=Male, 0=Female)", "#3498db", "#e74c3c")
p5 <- plot_binary_variable("smoking", "Smoking", "#3498db", "#e74c3c")

# Arrange plots in a grid
(p1 + p2) / (p3 + p4) / (p5 + plot_spacer()) 


## ----fig.width=10, fig.height=8,echo=FALSE, warning=FALSE--------------------------------------------------------
library(ggplot2)
library(reshape2)
library(viridis)

# Calculate correlation matrix
cor_matrix <- cor(data)

# Format the correlation values to display
cor_matrix_text <- round(cor_matrix, 3)

# Melt the correlation matrix for ggplot
melted_cor <- melt(cor_matrix)
colnames(melted_cor) <- c("Var1", "Var2", "value")

# Add text labels from the rounded matrix
melted_cor$text_label <- melt(cor_matrix_text)$value

# Create the correlation heatmap
ggplot(melted_cor, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = text_label), color = "black", size = 3) +
  scale_fill_gradient2(
    low = "#4575b4",    # Blue for negative correlations
    mid = "white",      # White for zero
    high = "#d73027",   # Red for positive correlations
    midpoint = 0,
    limit = c(-1, 1),
    name = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) +
  coord_fixed() +
  labs(title = "The correlation among features")

