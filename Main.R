library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(readr)
library(ggplot2)
library(tidyverse)
library(plotly)

connectivity <- read_csv("/Users/ness/Downloads/SchoolDownloads/DATA350/finalproject/Digital_Inequality - P2.csv")
gdp <- read_csv("/Users/ness/Downloads/SchoolDownloads/DATA350/finalproject/GDP(percapita) - P2.csv")
education <- read_csv("/Users/ness/Downloads/SchoolDownloads/DATA350/finalproject/Education_Expendatures - P2.csv")

connectivity$Total <- as.numeric(gsub("%", "", connectivity$Total))
connectivity$Residence_rural <- as.numeric(gsub("%", "", connectivity$Residence_rural))
connectivity$Residence_urban <- as.numeric(gsub("%", "", connectivity$Residence_urban))
connectivity$Wealth_quintile_poor <- as.numeric(gsub("%", "", connectivity$Wealth_quintile_poor))
connectivity$Wealth_quintile_rich <- as.numeric(gsub("%", "", connectivity$Wealth_quintile_rich))

ui <- fluidPage(
  titlePanel("Digital Inequality for Students in the Modern Age Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("incomeGroup", "Select Income Group:",
                  choices =  unique(connectivity$Income_Group),
                  selected = "All"),
      sliderInput("gdpRange", "Select GDP per Capita Range:", 
                  min = min(gdp$GDP_per_capita, na.rm = TRUE), 
                  max = max(gdp$GDP_per_capita, na.rm = TRUE), 
                  value = c(min(gdp$GDP_per_capita, na.rm = TRUE),
                            max(gdp$GDP_per_capita, na.rm = TRUE))),
      selectInput("region", "Select Region:", 
                  choices = c("All",
                              unique(connectivity$Region))),
      sliderInput("yearRange", "Select Year Range:",
                  min = min(gdp$Year, na.rm = TRUE),
                  max = max(gdp$Year, na.rm = TRUE), 
                  value = c(min(gdp$Year, na.rm = TRUE),
                            max(gdp$Year, na.rm = TRUE)))
    ),
    
    tabsetPanel(
      tabPanel("Geospatial Map", 
               h4("Geospatial Map:"),
               p("This map displays digital access across various countries. Each country is represented by a circle, where the circle’s size correlates with the digital access percentage (larger circles represent higher digital access). You can zoom in, pan, and click on circles to get more information on each country’s digital access."),
               leafletOutput("geoMap")),
      
      tabPanel("GDP vs Digital Access", 
               h4("GDP vs Digital Access:"),
               p("This scatter plot shows the relationship between GDP per capita and digital access across different countries. The red trendline indicates the general direction of this relationship. You can hover over the points to see specific country names and their corresponding values."),
               plotlyOutput("gdpPlot")),
      
      tabPanel("Inequality Analysis", 
               h4("Inequality Analysis:"),
               p("This faceted bar chart visualizes the differences in digital access between wealthier and poorer populations across different regions. The chart is split by regions (each region gets its own facet) and compares digital access for different wealth quintiles (Poor vs. Rich). Hover over the bars to see the specific values."),
               plotlyOutput("facetedBar")),
      
      tabPanel("Education Spending vs Digital Access", 
               h4("Education Spending vs Digital Access:"),
               p("This plot compares the average government expenditure on education (as a percentage of GDP) against the average digital access percentage for different regions. Hover over the points to see the specific education spending and digital access values for each region."),
               plotlyOutput("trendPlot")),
      
      tabPanel("Education Spending Heatmap", 
               h4("Education Spending Heatmap:"),
               p("This heatmap visualizes the relationship between regions and the average education spending as a percentage of GDP, showing how government spending on education varies across regions. Hover over the cells to see the exact value of education spending for each region."),
               plotOutput("heatmap")),
      
      tabPanel("GDP Heatmap", 
               h4("GDP Heatmap:"),
               p("This heatmap visualizes the GDP per capita for different countries and regions. The map shows how wealth (as measured by GDP per capita) varies across the world. Hover over the cells to see the exact GDP per capita value for each country and region."),
               plotOutput("gdpHeatmap"))
    )
  )
)

server <- function(input, output, session) {
  
  # Filtered data reactive pipeline
  filteredData <- reactive({
    merged_data <- connectivity %>%
      inner_join(gdp, by = c("Country_Code", "Region"))
    
    # Apply filters
    if (input$incomeGroup != "All") {
      merged_data <- merged_data %>% filter(Income_Group == input$incomeGroup)
    }
    
    if (input$region != "All") {
      merged_data <- merged_data %>% filter(Region == input$region)
    }
    
    merged_data <- merged_data %>%
      filter(Year >= input$yearRange[1], Year <= input$yearRange[2])
    
    merged_data
  })
  
  # Data Summary output
  get_mode <- function(x) {
    uniqx <- unique(x)
    uniqx[which.max(tabulate(match(x, uniqx)))]  # Find the mode
  }

  # Data Summary output
  output$dataSummary <- renderPrint({
    data <- filteredData()  # Get the filtered merged data
    
    # Check if required columns exist
    if ("Total" %in% colnames(data)) {
      
      # Summary statistics
      stats <- data %>%
        group_by(Region) %>%
        summarise(
          Min = min(Total, na.rm = TRUE),
          Median = median(Total, na.rm = TRUE),
          Mode = get_mode(Total),
          Max = max(Total, na.rm = TRUE),
          Range = max(Total, na.rm = TRUE) - min(Total, na.rm = TRUE),
          `1st Percentile` = quantile(Total, 0.01, na.rm = TRUE),
          `2nd Percentile` = quantile(Total, 0.02, na.rm = TRUE),
          `3rd Percentile` = quantile(Total, 0.03, na.rm = TRUE)
        )
      
      # Print the summary statistics
      print("Summary Statistics for Digital Access by Region:")
      print(stats)
      
      # Perform a t-test or ANOVA (if you want to compare across regions)
      anova_result <- aov(Total ~ Region, data = data)
      p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]  # Extract p-value
      
      # Format the result
      significance <- ifelse(p_value < 0.05, "Significant", "Not Significant")
      
      cat("\n\nANOVA Results:\n")
      cat(paste("ANOVA P-value: ", round(p_value, 4), "\n"))
      cat(paste("Significance: ", significance, "\n"))
      
    } else {
      print("Required columns not found in the data.")
    }
  })
  
  
  
  # Geospatial Map
  output$geoMap <- renderLeaflet({
    leaflet(data = filteredData()) %>%
      addTiles() %>%
      addCircles(lng = ~Longitude, lat = ~Latitude, 
                 weight = 1, 
                 radius = ~Total * 1000,
                 popup = ~paste(Country_Code, 
                                "Digital Access:", 
                                Total, "%"))
  })
  
  # GDP vs Digital Access Scatterplot
  output$gdpPlot <- renderPlotly({
    plot <- ggplot(filteredData(), 
                   aes(x = GDP_per_capita,
                       y = Total, 
                       color = Region)) +
      geom_point() +
      geom_smooth(method = "lm",
                  se = FALSE,
                  color = "red") +
      labs(title = "GDP per Capita vs Digital Access", 
           x = "GDP per Capita", 
           y = "Digital Access (%)")
    
    ggplotly(plot)
  })
  
  # Faceted Inequality Bar Chart (by both wealth quintiles: Poor and Rich)
  output$facetedBar <- renderPlotly({
    # Plot for Rich Quintile
    plot_rich <- ggplot(filteredData(), 
                        aes(x = Wealth_quintile_rich, 
                            y = Total, 
                            fill = Wealth_quintile_rich)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~Region) +
      labs(title = "Digital Access by Wealth Quintile (Rich)", 
           x = "Wealth Quintile (Rich)", 
           y = "Digital Access (%)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x labels for better readability
    
    plot_combined <- subplot(
      ggplotly(plot_rich),
      nrows = 1,
      titleX = TRUE, titleY = TRUE
    )
    
    plot_combined  # Display combined plot
  })
  
  # Education Spending vs Digital Access
  output$trendPlot <- renderPlotly({
    # Merge the datasets
    mergedData <- education %>%
      inner_join(connectivity, by = c("Country_Code", "Region"))
    
    # Prepare data: aggregate by Region
    plotData <- mergedData %>%
      mutate(Total = as.numeric(gsub("%", "", Total))) %>%  # Convert Total to numeric
      group_by(Region) %>%
      summarise(
        Avg_Education_Spending = mean(Government_expenditure_on_education_as_percent_of_GDP, na.rm = TRUE),
        Avg_Digital_Access = mean(Total, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = c(Avg_Education_Spending, Avg_Digital_Access), 
                   names_to = "Metric", 
                   values_to = "Value")
    
    # Create the bar plot
    plot <- ggplot(plotData, aes(x = Region, y = Value, fill = Metric)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Comparison of Education Spending and Digital Access by Region",
           x = "Region", 
           y = "Average Value (%)",
           fill = "Metric") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(plot)
  })
  
  
  # Regional Connectivity Averages
  output$regionBar <- renderPlot({
    avgData <- connectivity %>%
      group_by(Region) %>%
      summarise(Average_Access = mean(Total, na.rm = TRUE))
    
    ggplot(avgData, aes(x = Region, 
                        y = Average_Access, 
                        fill = Region)) +
      geom_bar(stat = "identity") +
      labs(title = "Average Connectivity by Region", 
           x = "Region", 
           y = "Average Digital Access (%)") +
      theme(axis.text.x = element_text(angle = 45,
                                       hjust = 1))
  })
  
  # Education Spending Heatmap
  output$heatmap <- renderPlot({
    ggplot(education, aes(x = Year, 
                          y = Region,  
                          fill = Government_expenditure_on_education_as_percent_of_GDP)) +
      geom_tile() +
      scale_fill_gradientn(colors = c("red", "orange", "yellow", "green", "blue")) +
      labs(title = "Education Spending Heatmap", 
           x = "Year", 
           y = "Region", 
           fill = "Spending (% GDP)") +
      theme(axis.text.y = element_text(size = 6)) + 
      theme_minimal()
  })
  
  output$gdpHeatmap <- renderPlot({
    ggplot(gdp, aes(x = Year, 
                    y = Region,  
                    fill = GDP_per_capita)) + 
      geom_tile() +
      scale_fill_gradientn(colors = c("red", "orange", "yellow", "green", "blue")) +
      labs(title = "GDP Over Time per Region", 
           x = "Year", 
           y = "Region", 
           fill = "GDP per Capita (Current USD)") +
      theme(axis.text.y = element_text(size = 6)) + 
      theme_minimal()
  })
}

# Run the app
shinyApp(ui, server)
