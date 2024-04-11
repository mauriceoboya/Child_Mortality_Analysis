# Childhood Mortality Analysis Shiny App

This Shiny app provides a user-friendly interface for analyzing childhood mortality data. It includes features for data visualization, filtering, and spatial analysis. Childhood mortality spatial analysis allows us to understand the geographical distribution of various factors affecting children's lives, from healthcare accessibility to educational resources. 📈 In Kenya, this analysis holds immense importance as we strive to ensure every child has equal opportunities for growth and development, regardless of their location.

## Installation

To run the app locally, follow these steps:

1. **Clone this repository** to your local machine:
   ```bash
   git clone https://github.com/mauriceoboya/childhood-mortality-analysis.git
   ```

2. **Navigate to the project directory**:
   ```bash
   cd childhood-mortality-analysis
   ```

3. **Install the required R packages** listed in `dependencies.R`. You can install them manually or use the following command:
   ```bash
   Rscript dependencies.R
   ```

4. **Place your main data file** (`main_data.xlsx`) **and the spatial map file** (`spatialdata.png`) **in the project directory**.

## Usage

Once the installation is complete, you can run the Shiny app by executing the following command in R or RStudio:

```R
shiny::runApp()
```

This will launch the app in your default web browser. You can then interact with the app using the provided interface.

## Features

### Data Summary

- View an overview of the childhood mortality data.
- Filter data based on various parameters such as mother's education, wealth index, age at first birth range, and outcome (deceased or not).

### Mortality Overview

- Visualize child mortality rates by mother's education level.
- Apply filters to focus on specific subsets of the data.

### Spatial Analysis

- Select a county to view mortality rates on a spatial map.
- Use the dropdown menu to choose a specific county or view data for all counties.

## Data Sources

The main data file (`main_data.xlsx`) contains the childhood mortality data used in this analysis. It should be structured with appropriate columns for analysis.

The spatial map (`spatialdata.png`) provides a visual representation of mortality rates by county.
