# Digital Inequality Analysis in the Modern Age

## Project Overview
This project explores digital inequality among students worldwide using three datasets related to **connectivity, GDP per capita, and education expenditure**. The Shiny application developed for this project provides interactive visualizations that help analyze the relationship between a country's economic standing, education investment, and digital access. Users can filter data based on income groups, regions, and years to gain deeper insights.

## Datasets Used
This analysis integrates three datasets:

1. **Connectivity Dataset**
   - Contains information on digital access (% of the population with internet access) across different regions and income groups.
   - Key columns:
     - `Country_Code`: Unique identifier for each country.
     - `Region`: Geographic region of the country.
     - `Income_Group`: Classification of countries based on income level.
     - `Total`: Percentage of the population with internet access.
     - `Residence_rural` & `Residence_urban`: Internet access percentages in rural and urban areas.
     - `Wealth_quintile_poor` & `Wealth_quintile_rich`: Internet access percentages for the poorest and richest quintiles.

2. **GDP Dataset**
   - Provides GDP per capita (current USD) for each country, enabling economic analysis.
   - Key columns:
     - `Country_Code`: Unique identifier for each country.
     - `Region`: Geographic region of the country.
     - `Year`: Year of recorded data.
     - `GDP_per_capita`: GDP per capita in current USD.

3. **Education Expenditure Dataset**
   - Contains government expenditure on education as a percentage of GDP.
   - Key columns:
     - `Country_Code`: Unique identifier for each country.
     - `Region`: Geographic region of the country.
     - `Year`: Year of recorded data.
     - `Government_expenditure_on_education_as_percent_of_GDP`: Percentage of GDP allocated to education.

## Shiny App Features
The Shiny app consists of interactive visualizations and summaries, including:
- **Data Summary:** Displays statistics such as min, median, mode, max, range, and percentiles for digital access by region.
- **Geospatial Map:** Shows digital access levels for countries with an interactive map.
- **GDP vs Digital Access Scatterplot:** Highlights the relationship between GDP per capita and digital access.
- **Inequality Analysis:** Uses faceted bar charts to compare digital access between rich and poor wealth quintiles.
- **Education Spending vs Digital Access:** Analyzes how education expenditure impacts digital access across regions.
- **Regional Connectivity Boxplot:** Displays distribution of digital access across different regions.
- **Heatmaps:** Visualizes education expenditure and GDP per capita trends over time.

## How to Use the Shiny App
1. Clone this repository.
2. Install necessary R packages: `shiny`, `ggplot2`, `dplyr`, `leaflet`, `plotly`, and `tidyverse`.
3. Run the app using `shinyApp(ui, server)` in RStudio.
4. Use filters to explore data dynamically and interact with the plots.

This project provides insights into global digital inequality and how economic and educational factors influence digital access. By using interactive visualizations, users can better understand disparities and identify trends across regions and income groups.

