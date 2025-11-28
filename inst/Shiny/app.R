# Load necessary packages
library(shiny)
library(ggplot2)
library(stats4)

# Define UI
ui <- fluidPage(
  titlePanel("Maximum Likelihood Estimation for Common Distributions"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Select Distribution:",
                  choices = c("Normal", "Exponential", "Poisson", "Binomial", "Uniform")),
      numericInput("n", "Sample Size:", value = 100, min = 10),
      uiOutput("paramUI"),
      actionButton("simulate", "Generate Data")
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      verbatimTextOutput("mleOutput")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Dynamic UI for parameters
  output$paramUI <- renderUI({
    switch(input$dist,
           "Normal" = tagList(
             numericInput("mean", "True Mean (μ):", value = 0),
             numericInput("sd", "True SD (σ):", value = 1, min = 0.1)
           ),
           "Exponential" = numericInput("rate", "True Rate (λ):", value = 1, min = 0.01),
           "Poisson" = numericInput("lambda", "True λ:", value = 4, min = 0.1),
           "Binomial" = tagList(
             numericInput("size", "Number of Trials (n):", value = 10, min = 1),
             numericInput("prob", "Success Probability (p):", value = 0.5, min = 0, max = 1)
           ),
           "Uniform" = tagList(
             numericInput("min", "Minimum (a):", value = 0),
             numericInput("max", "Maximum (b):", value = 1)
           )
    )
  })
  
  # Reactive data generator
  data_gen <- eventReactive(input$simulate, {
    n <- input$n
    switch(input$dist,
           "Normal" = rnorm(n, input$mean, input$sd),
           "Exponential" = rexp(n, input$rate),
           "Poisson" = rpois(n, input$lambda),
           "Binomial" = rbinom(n, input$size, input$prob),
           "Uniform" = runif(n, input$min, input$max)
    )
  })
  
  # MLE estimation
  output$mleOutput <- renderPrint({
    x <- data_gen()
    if (is.null(x)) return(NULL)
    
    # Define log-likelihood and estimate parameters
    switch(input$dist,
           "Normal" = {
             ll <- function(mean, sd) -sum(dnorm(x, mean, sd, log = TRUE))
             fit <- mle(ll, start = list(mean = mean(x), sd = sd(x)), method = "L-BFGS-B",
                        lower = c(-Inf, 0.001))
             summary(fit)
           },
           "Exponential" = {
             ll <- function(rate) -sum(dexp(x, rate, log = TRUE))
             fit <- mle(ll, start = list(rate = 1 / mean(x)))
             summary(fit)
           },
           "Poisson" = {
             ll <- function(lambda) -sum(dpois(x, lambda, log = TRUE))
             fit <- mle(ll, start = list(lambda = mean(x)))
             summary(fit)
           },
           "Binomial" = {
             size <- input$size
             ll <- function(prob) -sum(dbinom(x, size, prob, log = TRUE))
             fit <- mle(ll, start = list(prob = mean(x) / size), method = "L-BFGS-B",
                        lower = 0.001, upper = 0.999)
             summary(fit)
           },
           "Uniform" = {
             ll <- function(min, max) -sum(dunif(x, min, max, log = TRUE))
             fit <- mle(ll, start = list(min = min(x), max = max(x)), method = "L-BFGS-B",
                        lower = c(-Inf, -Inf), upper = c(Inf, Inf))
             summary(fit)
           }
    )
  })
  
  # Plot data and fitted PDF/PMF
  output$distPlot <- renderPlot({
    x <- data_gen()
    if (is.null(x)) return(NULL)
    
    df <- data.frame(x = x)
    
    ggplot(df, aes(x)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "white") +
      labs(title = paste("Distribution:", input$dist),
           x = "x", y = "Density") +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui, server)

