library(shiny)
library(ggplot2)
library(nortest)

# Define simulation functions
simulate_shapiro <- function(n, num_simulations = 100) {
  cat("Calling simulate_shapiro with n =", n, "\n")  # Added print statement
  correct_results <- numeric(num_simulations)
  
  for (i in 1:num_simulations) {
    X <- runif(n, 0, 1)
    shapiro_result <- shapiro.test(X)
    
    correct_results[i] <- ifelse(shapiro_result$p.value < 0.05, 1, 0)
  }
  
  proportion_correct_sw <- sum(correct_results) / length(correct_results)
  return(proportion_correct_sw)
}

simulate_KS <- function(n, num_simulations = 100) {
  cat("Calling simulate_KS with n =", n, "\n")  # Added print statement
  correct_results <- numeric(num_simulations)
  
  for (i in 1:num_simulations) {
    X <- runif(n, 0, 1)
    correct_results[i] <- ifelse(as.logical(ks.test(X, "pnorm")[2] < 0.05), 1, 0)
  }
  
  proportion_correct_sw <- sum(correct_results) / length(correct_results)
  return(proportion_correct_sw)
}

simulate_lilliefors <- function(n, num_simulations = 100) {
  cat("Calling simulate_lilliefors with n =", n, "\n")  # Added print statement
  correct_results <- numeric(num_simulations)
  
  for (i in 1:num_simulations) {
    X <- runif(n, 0, 1)
    correct_results[i] <- ifelse(as.logical(as.numeric(lillie.test(X)[2]) < 0.05), 1, 0)
  }
  
  proportion_correct_sw <- sum(correct_results) / length(correct_results)
  return(proportion_correct_sw)
}

# Define UI
ui <- fluidPage(
  titlePanel("Heavy tailed distribution detection using normality tests"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("test", "Choose a test:",
                  choices = c("Shapiro-Wilk" = "sw", "Kolmogorov-Smirnov" = "ks", "Lilliefors" = "lf"))
    ),
    
    mainPanel(
      plotOutput("powerPlot", width = 800), position = "right"
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$powerPlot <- renderPlot({
    n_values <- seq(5, 150, by = 1)
    
    if (input$test == "sw") {
      proportions_correct <- sapply(n_values, simulate_shapiro)
      title <- "Proportion of Correct Shapiro-Wilk Test Results for n-long sample from the uniform distribution"
    } else if (input$test == "ks") {
      proportions_correct <- sapply(n_values, simulate_KS)
      title <- "Proportion of Correct Kolmogorov-Smirnov Test Results"
    } else if (input$test == "lf") {
      proportions_correct <- sapply(n_values, simulate_lilliefors)
      title <- "Proportion of Correct Lilliefors Test Results"
    }
    
    results <- data.frame(n = n_values, proportion_correct = proportions_correct)
    
    ggplot(results, aes(x = n, y = proportion_correct)) +
      geom_point() +
      geom_hline(yintercept = 0.8, linetypes = "dashed", color = "red") +
      labs(x = "Sample Size (n)", y = "Proportion Correct (power)", title = title) +
      scale_x_continuous(limits = c(0, NA)) +
      scale_y_continuous(limits = c(0, 1)) +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui, server)
