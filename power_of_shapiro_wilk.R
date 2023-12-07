library(shiny)
library(ggplot2)
library(fGarch)

ui <- fluidPage(
  titlePanel("What sample size is enough for the Shapiro-Wilk test to detect the deviations from normality in the data?"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("stala", "Choose skewness parameter:", min = -5, max = 5, value = 1, step= 0.1),
      actionButton("plotButton", "Generate Plots")
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.stala != 0",
        plotOutput("densityPlot"),
        plotOutput("shapiroPlot")
      ),
      conditionalPanel(
        condition = "input.stala == 0",
        verbatimTextOutput("normalDistributionText")
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$plotButton, {
    stala <- input$stala
    
    if (stala == 0) {
      output$normalDistributionText <- renderPrint({
        "Normal distribution"
      })
      return()
    }
    
    n <- 1000

    density_plot <- ggplot(data.frame(X = rsnorm(n, mean = 0, sd = 2, xi = stala)),
                           aes(x = X)) +
      geom_density(fill = "blue", alpha = 0.5) +
      labs(title = "The data generated for testing normality is a n-long sample from a distribution with density:",
           x = "X",
           y = "Density") +
      theme_minimal()
    
    simulate_shapiro <- function(n, num_simulations = 100, stala) {
      correct_results <- numeric(num_simulations)
      for (i in 1:num_simulations) {
        X <- rsnorm(n, mean = 0, sd = 2, xi = stala)
        shapiro_result <- shapiro.test(X)
        correct_results[i] <- ifelse(shapiro_result$p.value < 0.05, 1, 0)
      }
      proportion_correct <- sum(correct_results) / length(correct_results)
      return(proportion_correct)
    }
    
    n_values <- seq(3, 250, by = 1)
    proportions_correct <- sapply(n_values, simulate_shapiro, stala = stala)
    
    shapiro_plot <- ggplot(data.frame(n = n_values, proportion_correct = proportions_correct),
                           aes(x = n, y = proportion_correct)) +
      geom_point() +
      geom_hline(yintercept = 0.8, linetypes = "dashed", color = "red") +
      labs(x = "Sample Size (n)", y = "Proportion reject",
           title = "Proportion of how many times out of 100 Shapiro-Wilk Test rejects normality") +
      scale_x_continuous(limits = c(0, NA)) +
      scale_y_continuous(limits = c(0, 1)) +
      theme_minimal()
    
    output$densityPlot <- renderPlot({ density_plot })
    output$shapiroPlot <- renderPlot({ shapiro_plot })
    

    output$normalDistributionText <- renderPrint({
      NULL
    })
  })
  
  observe({

    choices <- seq(-5, 5, by = 0.1)
    choices <- choices[choices != 0]
    

    updateSliderInput(session, "stala", min = min(choices), max = max(choices), value = choices[1])
  })
}

shinyApp(ui = ui, server = server)
