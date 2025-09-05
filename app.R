library(shiny)
library(readr)
library(ggplot2)

Cybersecurity<- read_csv("Global_Cybersecurity_Threats_2015-2024.csv")
# UI
ui <- fluidPage(
  
  titlePanel("Global Cybersecurity Threats (2015-2024)"),
  
  mainPanel(
    plotOutput("attackCountPlot"),
  )
  
)

# Server logic
server <- function(input, output) {
  
  
  output$attackCountPlot <- renderPlot({
    ggplot(Cybersecurity, aes(Year))+
      geom_bar()
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)