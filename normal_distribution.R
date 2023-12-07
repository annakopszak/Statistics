library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Variable Distribution"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("mean", "Mean Variable:", min = 80, max = 120, value = 100, step = 1),
      sliderInput("blue_sd", "blue Standard Deviation:", min = 1, max = 20, value = 10, step = 1),
      sliderInput("red_sd", "red Standard Deviation:", min = 1, max = 20, value = 5, step = 1)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    x <- seq(60, 140, length.out = 100)
    
    blue_y <- dnorm(x, mean = input$mean, sd = input$blue_sd)
    red_y <- dnorm(x, mean = input$mean, sd = input$red_sd)
    
    blue_data <- data.frame(x, blue_y)
    red_data <- data.frame(x, red_y)
    
    blue_label <- paste0("blue (SD = ", input$blue_sd, ")")
    red_label <- paste0("red (SD = ", input$red_sd, ")")
    
    ggplot() +
      geom_line(data = blue_data, aes(x = x, y = blue_y, color = blue_label)) +
      geom_line(data = red_data, aes(x = x, y = red_y, color = red_label)) +
      labs(x = "Variable", y = "Density") +
      ggtitle("Variable Distribution by Colour") +
      theme_minimal() +
      scale_color_manual(values = c("blue", "red"), labels = c(blue_label, red_label))
  })
}

shinyApp(ui = ui, server = server)
