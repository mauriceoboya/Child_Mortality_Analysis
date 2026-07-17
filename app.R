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

# ---------------------------------------------------------------------------
# Data loading (done once at app start, not repeated inside reactives)
# ---------------------------------------------------------------------------
data <- read_excel("main_data.xlsx", sheet = "maindata")
data$ResidenceCounty <- str_to_title(data$ResidenceCounty)

shapefile <- st_read("./shapefiles/County.shp")
shapefile <- st_make_valid(shapefile)

joined_data <- inner_join(shapefile, data, by = c("Name" = "ResidenceCounty"))

mortality_count <- joined_data %>%
  group_by(Name) %>%
  summarise(MortalityCount = sum(Outcome == "Dead"))


credentials <- data.frame(
  user = c("shiny", "admin"),
  password = c(Sys.getenv("SHINY_USER_PW", "12345"),
               Sys.getenv("SHINY_ADMIN_PW", "12345")),
  start = c("2019-04-15", NA),
  expire = c(NA, "2019-12-31"),
  admin = c(FALSE, TRUE),
  comment = "Simple and secure authentication mechanism for single 'Shiny' applications.",
  stringsAsFactors = FALSE
)

message("Credential source for 'shiny': ",
        if (nzchar(Sys.getenv("SHINY_USER_PW"))) "env var SHINY_USER_PW" else "fallback default")
message("Credential source for 'shinymanager': ",
        if (nzchar(Sys.getenv("SHINY_ADMIN_PW"))) "env var SHINY_ADMIN_PW" else "fallback default")

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
      tabItem(tabName = "data_view",
              uiOutput("page_content")
      )
    )
  )
)

server <- function(input, output, session) {
  
  logged_in <- reactiveVal(FALSE)
  
  output$login <- renderUI({
    if (!logged_in()) {
      tagList(
        textInput("user", "Username:"),
        passwordInput("password", "Password:"),
        actionButton("login", "Log in")
      )
    }
  })
  
  observeEvent(input$login, {
    req(input$user, input$password)
    
    typed_user <- trimws(input$user)
    typed_password <- trimws(input$password)
    
    match_idx <- which(
      credentials$user == typed_user &
        credentials$password == typed_password
    )
    
    if (length(match_idx) > 0) {
      logged_in(TRUE)
      showNotification("Successfully logged in.", type = "message", duration = 3)
    } else {
      showModal(modalDialog(
        title = "Error",
        "Invalid username or password. Please try again.",
        footer = modalButton("Dismiss")
      ))
    }
  })
  
  output$logout <- renderUI({
    if (logged_in()) {
      actionButton("logout_button", "Log out")
    }
  })
  
  observeEvent(input$logout_button, {
    logged_in(FALSE)
    updateTextInput(session, "user", value = "")
    updateTextInput(session, "password", value = "")
  })
  
  output$sidebar_menu <- renderUI({
    if (logged_in()) {
      sidebarMenu(
        menuItem("Data Summary", tabName = "data_view", icon = icon("home")),
        menuItem("Mortality Overview", tabName = "overview", icon = icon("check")),
        menuItem("Spatial Analysis", tabName = "filter_data", icon = icon("map-marker"))
      )
    }
  })
  
  # -------------------------------------------------------------------------
  # Spatial Analysis tab
  # -------------------------------------------------------------------------
  output$filter_content <- renderUI({
    if (logged_in()) {
      fluidPage(
        titlePanel("Filter Data"),
        sidebarLayout(
          sidebarPanel(
            selectInput("county", "Select County:",
                        choices = c("All", sort(unique(mortality_count$Name))))
          ),
          mainPanel(
            plotOutput("map")
          )
        )
      )
    }
  })
  
  # This output was referenced in the UI (plotOutput("map")) but was never
  # implemented, and the county filter was never actually applied.
  output$map <- renderPlot({
    req(logged_in())
    
    map_data <- mortality_count
    if (!is.null(input$county) && input$county != "All") {
      map_data <- map_data %>% filter(Name == input$county)
    }
    
    ggplot(map_data) +
      geom_sf(aes(fill = MortalityCount)) +
      scale_fill_viridis_c(name = "Deaths") +
      labs(title = "Child Mortality Count by County") +
      theme_minimal()
  })
  
  # -------------------------------------------------------------------------
  # Mortality Overview tab
  # -------------------------------------------------------------------------
  output$overview_content <- renderUI({
    if (logged_in()) {
      fluidRow(
        column(width = 4,
               box(
                 title = "Filter Data",
                 status = "primary",
                 solidHeader = TRUE,
                 width = 12,
                 selectInput("education", "Mother's Education",
                             choices = c("All", unique(data$MotherEducation))),
                 selectInput("wealth", "Wealth Index",
                             choices = c("All", unique(data$WealthIndex))),
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
    req(logged_in())
    
    filtered <- data
    
    if (input$education != "All") {
      filtered <- filtered %>% filter(MotherEducation == input$education)
    }
    if (input$wealth != "All") {
      filtered <- filtered %>% filter(WealthIndex == input$wealth)
    }
    if (input$age_range[1] != min(data$AgeAtFirstBirth) ||
        input$age_range[2] != max(data$AgeAtFirstBirth)) {
      filtered <- filtered %>%
        filter(AgeAtFirstBirth >= input$age_range[1] & AgeAtFirstBirth <= input$age_range[2])
    }
    if (isTRUE(input$only_deceased)) {
      filtered <- filtered %>% filter(Outcome == "Dead")
    }
    
    plot_ly(filtered, x = ~MotherEducation, color = ~Outcome) %>%
      add_histogram() %>%
      layout(title = "Child Mortality by Mother's Education",
             xaxis = list(title = "Mother's Education"),
             yaxis = list(title = "Count"),
             barmode = "group")
  })
  
  # -------------------------------------------------------------------------
  # Data Summary tab
  # No longer re-reads the Excel file from disk on every reactive
  # invalidation -- reuses the `data` object already loaded at app start.
  # -------------------------------------------------------------------------
  output$page_content <- renderUI({
    req(logged_in())
    DT::datatable(data, options = list(pageLength = 7))
  })
  
}

shinyApp(ui = ui, server = server)