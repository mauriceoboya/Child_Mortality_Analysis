library(shiny)
library(shinydashboard)
library(shinymanager)
library(readxl)
library(dplyr)
library(sf)
library(ggplot2)
library(viridis)
library(stringr)
library(plotly)
library(tibble)
library(DT)
library(ggplot2)
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
    uiOutput("sidebar_menu"),
    uiOutput("logout")
  ),
  dashboardBody(
    uiOutput("login"),
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
  
  # Define some basic credentials
  credentials <- data.frame(
    user = c("shiny", "shinymanager"), 
    password = c("azerty", "12345"), 
    start = c("2019-04-15", NA), 
    expire = c(NA, "2019-12-31"),
    admin = c(FALSE, TRUE),
    comment = "Simple and secure authentication mechanism for single ‘Shiny’ applications.",
    stringsAsFactors = FALSE
  )
  
  # Function to check if the user is authenticated
  is_authenticated <- reactive({
    req(input$user(), input$password())  # Ensure input$user() and input$password() are not NULL
    
    user_index <- which(credentials$user == input$user() & credentials$password == input$password())
    
    if (length(user_index) > 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  
  output$login <- renderUI({
    if (!is_authenticated()) {
      tagList(
        textInput("user", "Username:"),
        passwordInput("password", "Password:"),
        actionButton("login", "Log in")
      )
    }
  })
  
  observeEvent(input$login, {
    if (is_authenticated()) {
      showModal(modalDialog(
        title = "Success",
        "Successfully logged in.",
        footer = NULL
      ))
    } else {
      showModal(modalDialog(
        title = "Error",
        "Invalid username or password. Please try again."
      ))
    }
  })
  
  output$logout <- renderUI({
    if (is_authenticated()) {
      actionButton("logout_button", "Log out")
    }
  })
  
  observeEvent(input$logout_button, {
    updateTextInput(session, "user", value = "")
    updateTextInput(session, "password", value = "")
  })
  
  output$sidebar_menu <- renderUI({
    if (is_authenticated()) {
      sidebarMenu(
        menuItem("Data Summary", tabName = "data_view", icon = icon("home")),
        menuItem("Mortality Overview", tabName = "overview", icon = icon("check")),
        menuItem("Spatial Analysis", tabName = "filter_data", icon = icon("map-marker"))
      )
    }
  })
  
  output$filter_content <- renderUI({
    if (is_authenticated()){
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
    }
  })
  
  output$overview_content <- renderUI({
    if (is_authenticated()) {
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
    if (is_authenticated()) {
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
    }
  })
  
  ###############################################################data#####
  datas <- reactive({
    if (is_authenticated()) {
      datas <- read_excel('main_data.xlsx', sheet ='maindata')
    } else {
      return(NULL)
    }
  })
  
  output$page_content <- renderUI({
    if (!is.null(datas())) {
      generate_table(datas())
    } else {
      NULL
    }
  })
  
  generate_table <- function(datas) {
    datatable(datas, options = list(pageLength = 7))
  }
  
  ###########################################################################
  
}

shinyApp(ui = ui, server = server)
