#install and load proper packages necessary for data process and handling
install_and_load_packages <- function(packages) {
  # Check if the package is already installed, if not, install it
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
  }
  
  # Load all the installed packages
  invisible(lapply(packages, library, character.only = TRUE))
}

# Define a vector of package names
packages_to_install <- c("psych", "ggplot2", "dplyr", "tidyr", "reshape2", "gridExtra", "plotly", "corrplot", "patchwork")

# Call the function to install and load the packages
install_and_load_packages(packages_to_install)

#data processing
data = read.csv('C:/Users/HP/Downloads/life_expectancy.csv')
dim(data)
describe(data)
View(data)
variable_names <- names(data)
print(variable_names)
str(data)
data_frame(data)
head(data)
colnames(data)

columns <- colnames(data)
for (column in columns) {
  data[[column]] <- ifelse(trimws(data[[column]]) == "",
                           NA, data[[column]])
}
colSums(is.na(data))

data$Population = ifelse(is.na(data$Population),
                  ave(data$Population, FUN = function (x)mean(x, na.rm = TRUE)),
                  data$Population)
data$Hepatitis.B = ifelse(is.na(data$Hepatitis.B),
                  ave(data$Hepatitis.B, FUN = function (x)mean(x, na.rm = TRUE)),
                  data$Hepatitis.B)
data$Polio = ifelse(is.na(data$Polio),
            ave(data$Polio, FUN = function (x)mean(x, na.rm = TRUE)),
            data$Polio)
data$Diphtheria = ifelse(is.na(data$Diphtheria),
                    ave(data$Diphtheria, FUN = function (x)mean(x, na.rm = TRUE)),
                    data$Diphtheria)
data$Total.expenditure = ifelse(is.na(data$Total.expenditure),
                    ave(data$Total.expenditure, FUN = function (x)mean(x, na.rm = TRUE)),
                    data$Total.expenditure)
data$GDP= ifelse(is.na(data$GDP),
                    ave(data$GDP, FUN = function (x)mean(x, na.rm = TRUE)),
                    data$GDP)
data$BMI = ifelse(is.na(data$BMI),
                    ave(data$BMI, FUN = function (x)mean(x, na.rm = TRUE)),
                    data$BMI)
data$thinness..1.19.years = ifelse(is.na(data$thinness..1.19.years),
                    ave(data$thinness..1.19.years, FUN = function (x)mean(x, na.rm = TRUE)),
                    data$thinness..1.19.years)
data$Alcohol = ifelse(is.na(data$Alcohol),
                    ave(data$Alcohol, FUN = function (x)mean(x, na.rm = TRUE)),
                    data$Alcohol)
data$Schooling = ifelse(is.na(data$Schooling),
                    ave(data$Schooling, FUN = function (x)mean(x, na.rm = TRUE)),
                    data$Schooling)

unique_countries <- data[seq(1, nrow(data), by = 15), 1]
print(unique_countries)


if ('Status' %in% colnames(data)) {
  developed_countries <- unique(data$Country[data$Status == "Developed"])
  developing_countries <- unique(data$Country[data$Status == "Developing"])

  cat("Developed Countries:\n")
  print(developed_countries)
  
  cat("\nDeveloping Countries:\n")
  print(developing_countries)
} else {
  cat("No 'Status' column found in the dataset.")
}

#plotting proccess
# import necessary library
library(reshape2)
library(ggplot2)

plot_disease_sum <- function(data) {
  # Filter data for developed and developing countries
  developed_data <- data[data$Status == "Developed", ]
  developing_data <- data[data$Status == "Developing", ]
  
  # Calculate the sum of diseases for each country type
  sum_developed <- colSums(developed_data[, c('Hepatitis.B', 'Measles', 'Polio', 'Diphtheria', 'HIV.AIDS')], na.rm = TRUE)
  sum_developing <- colSums(developing_data[, c('Hepatitis.B', 'Measles', 'Polio', 'Diphtheria', 'HIV.AIDS')], na.rm = TRUE)
  
  # Combine the sums into a single data frame
  combined_sums <- data.frame(
    CountryType = c("Developed", "Developing"),
    HepatitisB = c(sum_developed['Hepatitis.B'], sum_developing['Hepatitis.B']),
    Measles = c(sum_developed['Measles'], sum_developing['Measles']),
    Polio = c(sum_developed['Polio'], sum_developing['Polio']),
    Diphtheria = c(sum_developed['Diphtheria'], sum_developing['Diphtheria']),
    HIV_AIDS = c(sum_developed['HIV.AIDS'], sum_developing['HIV.AIDS'])
  )
  
  # Reshape data for plotting
  combined_sums_melted <- melt(combined_sums, id.vars = "CountryType", variable.name = "Disease", value.name = "Sum")
  
  # Plotting
  plot <- ggplot(combined_sums_melted, aes(x = CountryType, y = Sum, fill = Disease)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    labs(title = "Sum of Diseases in Developed and Developing Countries", y = "Sum", x = "Country Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(trans = 'log10', labels = scales::comma) +  # Format y-axis labels as integers
    scale_fill_manual(values = c("#1f78b4", "yellowgreen", "red", "orange", "yellow"))
  
  return(plot)
}

plot1 <- plot_disease_sum(data)
plot1

# Load necessary libraries
library(plotly)
library(dplyr)

# Define a function to plot the comparison of Total Expenditure to GDP Percentage and Life Expectancy
plot_expenditure_to_gdp_life_expectancy <- function(data) {
  
  # Calculate percentages for developed and developing countries from 2000 to 2015
  percentages <- lapply(2000:2015, function(year) {
    data_year <- data %>% filter(Year == year)
    developed_percentage <- sum(data_year[data_year$Status == "Developed", "Total.expenditure"]) / sum(data_year[data_year$Status == "Developed", "GDP"]) * 100
    developing_percentage <- sum(data_year[data_year$Status == "Developing", "Total.expenditure"]) / sum(data_year[data_year$Status == "Developing", "GDP"]) * 100
    return(data.frame(Year = year, Developed_Percentage = developed_percentage, Developing_Percentage = developing_percentage))
  })
  
  # Combine percentages into a single data frame
  combined_percentages <- do.call(rbind, percentages)
  
  # Calculate average life expectancy for developed and developing countries from 2000 to 2015
  avg_life_expectancy <- data %>%
    group_by(Year, Status) %>%
    summarise(avg_Life_Expectancy = mean(Life.expectancy)) %>%
    filter(Year %in% 2000:2015) %>%
    pivot_wider(names_from = Status, values_from = avg_Life_Expectancy)
  
  # Create a plotly plot
  plot2 <- plot_ly() %>%
    # Add trace for Developed Percentage (Expenditure to GDP)
    add_trace(
      data = combined_percentages,
      x = ~Year,
      y = ~Developed_Percentage,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = 'coral', width = 6),
      marker = list(size = 6, color = 'black'),
      name = 'Developed (Expenditure to GDP)',
      yaxis="y2"
    ) %>%
    # Add trace for Developing Percentage (Expenditure to GDP)
    add_trace(
      data = combined_percentages,
      x = ~Year,
      y = ~Developing_Percentage,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = 'aquamarine', width = 6),
      marker = list(size = 6, color = 'black'),
      name = 'Developing (Expenditure to GDP)',
      yaxis="y2"
    ) %>%
    # Add bars for Developed (Avg. Life Expectancy)
    add_bars(
      data = avg_life_expectancy,
      x = ~Year,
      y = ~Developed,
      name = 'Developed (Avg. Life Expectancy)',
      marker = list(color = 'yellowgreen')
    ) %>%
    # Add bars for Developing (Avg. Life Expectancy)
    add_bars(
      data = avg_life_expectancy,
      x = ~Year,
      y = ~Developing,
      name = 'Developing (Avg. Life Expectancy)',
      marker = list(color = 'pink')
    ) %>%
    # Define the layout for the plot
    layout(
      title="Comparison of Total Expenditure to GDP Percentage and Life Expectancy",
      xaxis = list(title = "Year", tickvals = 2000:2015),
      yaxis = list(
        title = list(
          text = "Average Life Expectancy", 
          font = list(size = 13),
          margin = list(r = 70, l = 70)  # Adjust right and left margins for y-axis title
        ),
        side = "left", 
        tickfont = list(size = 12)
      ),
      yaxis2 = list(
        title = list(
          text = "Total percentage Life Expectancy to GDP", 
          font = list(size = 13),
          margin = list(r = 70, l = 70)  # Adjust right and left margins for y-axis title
        ),
        side = "right", overlaying = "y",
        tickfont = list(size = 12)
      ),
      margin = list(b = 70, t = 50, l = 50, r = 50),
      legend = list(
        x = 1.08,  # Adjust the x position of the legend
        y = 1,  # Adjust the y position of the legend
        traceorder = "normal",
        font = list(size = 10)
      )
    )
  return(plot2)
}

# Call the function with the 'data' dataframe
plot2 <- plot_expenditure_to_gdp_life_expectancy(data)
plot2


#loading necessary library
library(dplyr)
library(ggplot2)
library(tidyr)  

plot_total_comparison <- function(data) {
  # Filter data into developed and developing countries for the years 2000 to 2015
  developed_data <- data %>%
    filter(Status == "Developed") %>%
    group_by(Year) %>%
    summarise(Total_Expenditure = sum(Total.expenditure), Total_GDP = sum(GDP), Status = "Developed")
  
  developing_data <- data %>%
    filter(Status == "Developing") %>%
    group_by(Year) %>%
    summarise(Total_Expenditure = sum(Total.expenditure), Total_GDP = sum(GDP), Status = "Developing")
  
  
  x <-c( 1,2,3)
  combined_data <- rbind(developed_data, developing_data)
  
  # Reshape data for easier plotting
  combined_data_long <- combined_data %>%
    gather(key = "Variable", value = "Value", Total_Expenditure, Total_GDP)
  
  # Plotting combined chart for Total Expenditure and Total GDP with numeric y-axis for Total GDP
  plot <- ggplot(combined_data_long, aes(x = as.factor(Year), y = Value, fill = Status)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Variable, scales = "free_y", ncol = 1) +
    labs(title = "Total Expenditure and GDP ",
         x = "Year",
         y = "Total Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("#1f78b4", "yellowgreen"))+
    scale_y_continuous(labels = function(x) ifelse(x < 1000, format(x, scientific = FALSE), format(x, scientific = FALSE, big.mark = ",")))
  
  print(plot)
}
plot3 <- plot_total_comparison(data)
plot3

# Load necessary library
library(plotly)

# Function to generate an interactive circular heatmap
generate_interactive_circular_heatmap <- function(data, columns) {
  # Subset the data to the selected columns
  correlation_data <- data[, columns]
  
  # Calculate the correlation matrix
  cor_matrix <- cor(correlation_data, use = "complete.obs")
  
  # Get the indices of selected columns in the correlation matrix
  column_indices <- match(columns, colnames(cor_matrix))
  
  # Reorder columns to start with "Life.expectancy" and end with "Schooling"
  col_order <- c(
    column_indices[column_indices == "Life.expectancy"], 
    column_indices[column_indices != "Life.expectancy" & column_indices != "Schooling"], 
    column_indices[column_indices == "Schooling"]
  )
  
  # Rearrange the correlation matrix based on column order
  cor_matrix <- cor_matrix[col_order, col_order]
  
  # Create a Plotly interactive heatmap
  heatmap_plotly <- plot_ly(
    z = cor_matrix, x = colnames(cor_matrix), y = colnames(cor_matrix), 
    type = "heatmap", colorscale = "Warm",
    text = round(cor_matrix, 2),  # Display correlation values with two decimals
    hoverinfo = "text"  # Show text on hover
  )
  
  # Set layout parameters for the heatmap
  heatmap_plotly <- layout(
    heatmap_plotly, 
    title = "Interactive Correlation Heatmap", 
    xaxis = list(title = ""), yaxis = list(title = ""),
    polar = list(
      radialaxis = list(visible = FALSE),
      angularaxis = list(direction = "clockwise")
    ),
    margin = list(t = 50)  # Adding top margin to the entire plot
  )
  
  return(heatmap_plotly)
}

# Define columns for the heatmap
selected_columns <- c(
  "Life.expectancy", "Population", "Hepatitis.B", "Measles", "Polio", "Diphtheria", "HIV.AIDS",
  "infant.deaths", "under.five.deaths", "Total.expenditure", "GDP", "BMI",
  "thinness..1.19.years", "Alcohol", "Schooling")

# Generate the interactive heatmap using the function
plot4 <- generate_interactive_circular_heatmap(data, selected_columns)
plot4

#loading necessary library
library(ggplot2)

life_expectancy_distribution_boxplot <- function(data) {
  developed_countries <- unique(data$Country[data$Status == "Developed"])
  developing_countries <- unique(data$Country[data$Status == "Developing"])
  
  # Filter data for developed and developing countries
  developed_data <- data[data$Country %in% developed_countries, ]
  developing_data <- data[data$Country %in% developing_countries, ]
  
  # Add a new column to distinguish between developed, developing, and overall population
  developed_data$Distribution <- "Developed"
  developing_data$Distribution <- "Developing"
  data$Distribution <- "Population"
  
  # Merge data for all categories
  combined_data <- rbind(developed_data, developing_data, data)
  
  # Create a boxplot comparing life expectancy among categories
  plot <- ggplot(combined_data, aes(x = Distribution, y = Life.expectancy, fill = Distribution)) +
    geom_boxplot() +
    labs(x = "Distribution standards", y = "Life Expectancy", title = "Overall distribution of Life Expectancy") +
    scale_fill_manual(values = c("Developed" = "lightblue", "Developing" = "coral", "Population" = "pink")) +
    theme_minimal() +
    theme(plot.title = element_text(margin = margin(b = 20)),  # Padding for the title
          axis.title.x = element_text(margin = margin(t = 10)),  # Padding for x-axis label
          axis.title.y = element_text(margin = margin(r = 10)))   # Padding for y-axis label))  # Adding padding to the title
  
  return(plot)
}

plot5 <- life_expectancy_distribution_boxplot(data)
plot5

