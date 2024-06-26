library(shiny)
library(readxl)
library(dplyr)
library(sf)
library(ggplot2)
library(viridis)
library(stringr)
library(shinydashboard)
library(plotly)
library(tibble)
library(shinyauthr)

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

user_base <- tibble::tibble(
  user = c("admin"),
  password = c("admin"),
  permissions = c("admin"),
  name = c("admin")
)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Childhood Mortality Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Mortality Overview", tabName = "overview", icon = icon("home")),
      menuItem("Spatial Analysis", tabName = "filter_data", icon = icon("filter"))
    ),
    div(class = "pull-right", shinyauthr::logoutUI(id = "logout"))
  ),
  dashboardBody(
    shinyauthr::loginUI(id = "login"),
    tabItems(
      tabItem(tabName = "overview",
              uiOutput("overview_content")
      ),
      tabItem(tabName = "filter_data",
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
      )
    )
  )
)

server <- function(input, output, session) {
  
  # user and password cols and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = "user",
    pwd_col = "password",
    log_out = reactive(logout_init())
  )
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  output$map <- renderPlot({
    filtered_data <- mortality_count
    if (input$county != "All") {
      filtered_data <- filter(mortality_count, Name %in% input$county)
    }
    print(filtered_data)  # Check if filtered_data is correct
    ggplot() +
      geom_sf(data = filtered_data, aes(fill = MortalityCount)) +
      scale_fill_viridis_c() +
      labs(fill = "Mortality Count") +
      theme_minimal() +
      ggtitle("Spatial Distribution of Mortality Count")
  })
  
  output$overview_content <- renderUI({
    if (credentials()$user_auth) {
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
    }
  })
  
  output$mortality_plotly <- renderPlotly({
    if (credentials()$user_auth) {
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
      if (input$only_deceased == TRUE) { # Convert to logical
        filtered <- filtered %>% filter(Outcome == "Dead")
      }
      
      plot_ly(filtered, x = ~MotherEducation, color = ~Outcome) %>%
        add_histogram() %>%
        layout(title = "Child Mortality by Mother's Education",
               xaxis = list(title = "Mother's Education"),
               yaxis = list(title = "Count"),
               barmode = "group")
    }
  })
}

shinyApp(ui = ui, server = server)