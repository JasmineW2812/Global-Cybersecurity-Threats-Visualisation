library(shiny)
library(readr)
library(ggplot2)
library(dplyr)

data(World) 

Cybersecurity <- read_csv("Global_Cybersecurity_Threats_2015-2024.csv") %>%
  mutate(Country = case_when(
    Country == "USA" ~ "United States of America",
    Country == "UK" ~ "United Kingdom",
    TRUE ~ Country
  ))

# UI
ui <- fluidPage(
  
  titlePanel("Global Cybersecurity Threats (2015-2024)"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("range","Year:",
                  min = 2015, max= 2024, step=1, value=2014),
      selectInput("country", "Country:", choices = unique(Cybersecurity$Country)),
      tableOutput("financialLoss"),
      uiOutput("googleLink")
    ),
  
    mainPanel(
      plotOutput("attackCountPlot"),
      plotOutput("map")

    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    Cybersecurity %>%
      filter(Year == input$range)
  })
  
  World_data <- reactive({
    World %>%
      left_join(filtered_data(), by = c("name" = "Country"))
  })
  
  
  output$attackCountPlot <- renderPlot({
    ggplot(filtered_data(), aes(x=`Attack Type`, fill =`Security Vulnerability Type`)) +
      geom_bar()+
      xlab("Attack Type") +
      ylab("Number of Attacks") 
    
  })
  
  output$map <- renderPlot({
    qtm(World_data(), fill='Number of Affected Users')
      
    
  })
  
  output$financialLoss <- renderTable({
    filtered_data() %>%
      group_by(`Target Industry`) %>%
      summarise(total = sum(`Financial Loss (in Million $)`, na.rm = TRUE))%>%
                  arrange(desc(total))
  })
  
  output$googleLink <-renderUI({
    country <- input$country
    year <- input$range
    
    url <- paste0("https://www.google.com/search?q=cybersecurity+attacks+", 
                  URLencode(country), "+", year)
    
    tags$a(href = url, 
           paste("🔍 Search Google for", country, "attacks in", year), 
           target = "_blank")
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)