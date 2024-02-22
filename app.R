library(shinydashboard)
library(readxl)
library(dplyr)
library(sf)
library(ggplot2)
library(viridis)
library(stringr)
library(plotly)
library(tibble)
library(DT)

# Read the main data
data <- read_excel('main_data.xlsx', sheet ='maindata')
data$ResidenceCounty <- str_to_title(data$ResidenceCounty)

# Read shapefile
shapefile <- st_read("./shapefiles/County.shp")
shapefile <- st_make_valid(shapefile)

# Join data
joined_data <- inner_join(shapefile, data, by = c("Name" = "ResidenceCounty"))

# Calculate mortality count
mortality_count <- joined_data %>%
  group_by(Name) %>%
  summarise(MortalityCount = sum(Outcome == "Dead"))

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Childhood Mortality Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Summary", tabName = "data_view", icon = icon("home")),
      menuItem("Mortality Overview", tabName = "overview", icon = icon("check")),
      menuItem("Spatial Analysis", tabName = "filter_data", icon = icon("map-marker"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              uiOutput("overview_content")
      ),
      tabItem(tabName = "filter_data",
              uiOutput("filter_content")
      ),
      tabItem(tabName = 'data_view',
              uiOutput("page_content")
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$map <- renderPlot({
    filtered_data <- mortality_count
    if (input$county != "All") {
      filtered_data <- filter(filtered_data, Name %in% input$county)
    }
    ggplot() +
      geom_sf(data = filtered_data, aes(fill = MortalityCount)) +
      scale_fill_viridis_c() +
      labs(fill = "Mortality Count") +
      theme_minimal() +
      ggtitle("Spatial Distribution of Mortality Count")
  })
  
  output$filter_content <- renderUI({
    fluidPage(
      titlePanel("Filter Data"),
      sidebarLayout(
        sidebarPanel(
          selectInput("county", "Select County:", choices = c("All", unique(mortality_count$Name)))
        ),
        mainPanel(
          plotOutput("map")
        )
      )
    )
  })
  
  output$overview_content <- renderUI({
    fluidRow(
      column(width = 4,
             box(
               title = "Filter Data",
               status = "primary",
               solidHeader = TRUE,
               width = 12,
               selectInput("education", "Mother's Education", choices = c("All", unique(data$MotherEducation))),
               selectInput("wealth", "Wealth Index", choices = c("All", unique(data$WealthIndex))),
               sliderInput("age_range", "Age at First Birth Range",
                           min = min(data$AgeAtFirstBirth), max = max(data$AgeAtFirstBirth),
                           value = c(min(data$AgeAtFirstBirth), max(data$AgeAtFirstBirth))),
               checkboxInput("only_deceased", "Show only deceased children", value = FALSE)
             )
      ),
      column(width = 8,
             box(
               title = "Child Mortality Overview",
               status = "info",
               solidHeader = TRUE,
               width = 12,
               plotlyOutput("mortality_plotly")
             )
      )
    )
  })
  
  output$mortality_plotly <- renderPlotly({
    filtered <- data
    
    # Apply filters
    if (input$education != "All") {
      filtered <- filtered %>% filter(MotherEducation == input$education)
    }
    if (input$wealth != "All") {
      filtered <- filtered %>% filter(WealthIndex == input$wealth)
    }
    if (input$age_range[1] != min(data$AgeAtFirstBirth) || input$age_range[2] != max(data$AgeAtFirstBirth)) {
      filtered <- filtered %>% filter(AgeAtFirstBirth >= input$age_range[1] & AgeAtFirstBirth <= input$age_range[2])
    }
    if (input$only_deceased == TRUE) { 
      filtered <- filtered %>% filter(Outcome == "Dead")
    }
    
    plot_ly(filtered, x = ~MotherEducation, color = ~Outcome) %>%
      add_histogram() %>%
      layout(title = "Child Mortality by Mother's Education",
             xaxis = list(title = "Mother's Education"),
             yaxis = list(title = "Count"),
             barmode = "group")
  })
  
  ###############################################################data#####
  
  generate_table <- function(data) {
    datatable(data, options = list(pageLength = 10))
  }
  
  # Render table with first 10 rows
  output$page_content <- renderUI({
    generate_table(data)
  })
  
  ###########################################################################
  
}

shinyApp(ui = ui, server = server)
