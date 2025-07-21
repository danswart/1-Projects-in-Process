library(shiny)
library(plotly)
library(colorspace)
library(dplyr)

# Define available distributions
distributions <- c("Normal" = "norm",
                  "Uniform" = "unif",
                  "Exponential" = "exp",
                  "Gamma" = "gamma",
                  "Beta (scaled)" = "beta")

# Define UI
ui <- fluidPage(
  titlePanel("Student Performance Distribution Comparisons"),

  # Add CSS to increase font size throughout the app
  tags$head(
    tags$style(HTML("
      body, label, input, button, select {
        font-size: 16px !important;
      }
      h4 {
        font-size: 20px !important;
        font-weight: bold;
      }
      .shiny-input-container {
        margin-bottom: 25px;
      }
    "))
  ),

  sidebarLayout(
    sidebarPanel(
      # Distribution 1 controls
      h4("Student Group 1 (Blue)"),
      selectInput("dist1", "Distribution Type:", distributions),
      sliderInput("mean1", "Success Rate (%):", min = 1, max = 100, value = 50, step = 1),
      sliderInput("sd1", "Std Dev:", min = 0.1, max = 20, value = 6.1, step = 0.1),

      # Distribution 2 controls
      h4("Student Group 2 (Red)"),
      selectInput("dist2", "Distribution Type:", distributions),
      sliderInput("mean2", "Success Rate (%):", min = 1, max = 100, value = 60, step = 1),
      sliderInput("sd2", "Std Dev:", min = 0.1, max = 20, value = 6.1, step = 0.1),

      # Distribution 3 controls
      h4("Student Group 3 (Green)"),
      selectInput("dist3", "Distribution Type:", distributions),
      sliderInput("mean3", "Success Rate (%):", min = 1, max = 100, value = 70, step = 1),
      sliderInput("sd3", "Std Dev:", min = 0.1, max = 20, value = 6.1, step = 0.1),

      # Additional parameters for certain distributions
      conditionalPanel(
        condition = "input.dist1 == 'gamma' || input.dist2 == 'gamma' || input.dist3 == 'gamma'",
        sliderInput("gamma_shape", "Gamma Shape Parameter:", min = 0.1, max = 10, value = 2, step = 0.1)
      ),

      conditionalPanel(
        condition = "input.dist1 == 'beta' || input.dist2 == 'beta' || input.dist3 == 'beta'",
        sliderInput("beta_shape1", "Beta Shape1 Parameter:", min = 0.1, max = 10, value = 2, step = 0.1),
        sliderInput("beta_shape2", "Beta Shape2 Parameter:", min = 0.1, max = 10, value = 2, step = 0.1)
      ),

      # Add option to toggle overlap highlighting
      checkboxInput("highlightOverlap", "Highlight Overlaps in Red", value = TRUE)
    ),

    mainPanel(
      plotlyOutput("densityPlot", height = "650px")  # Increased height for title margin
    )
  )
)

# Define server
server <- function(input, output) {

  # Define Okabe-Ito colors manually
  colors <- c("#0072B2", "#D55E00", "#009E73")

  # Generate density values for specified distribution
  generate_density <- function(dist_type, mean_val, sd_val, x_values) {
    if(dist_type == "norm") {
      return(dnorm(x_values, mean = mean_val, sd = sd_val))
    } else if(dist_type == "unif") {
      min_val <- mean_val - sd_val * sqrt(12) / 2
      max_val <- mean_val + sd_val * sqrt(12) / 2
      return(dunif(x_values, min = min_val, max = max_val))
    } else if(dist_type == "exp") {
      rate <- 1 / sd_val
      shift <- mean_val - sd_val
      return(dexp(x_values - shift, rate = rate))
    } else if(dist_type == "gamma") {
      shape <- input$gamma_shape
      scale <- sd_val / sqrt(shape)
      shift <- mean_val - shape * scale
      return(dgamma(x_values - shift, shape = shape, scale = scale))
    } else if(dist_type == "beta") {
      shape1 <- input$beta_shape1
      shape2 <- input$beta_shape2
      # Scale beta to [1, 100]
      mean_beta <- shape1 / (shape1 + shape2)
      var_beta <- (shape1 * shape2) / ((shape1 + shape2)^2 * (shape1 + shape2 + 1))
      sd_beta <- sqrt(var_beta)

      # Scale factor to match desired sd
      scale_factor <- sd_val / sd_beta

      # Calculate scaled x values (transformed to [0,1] for beta)
      x_scaled <- (x_values - 1) / 99

      # Shift to center at mean_val
      shift <- mean_val - (mean_beta * 99 + 1)
      x_centered <- (x_values - shift - 1) / 99

      # Return density values for valid range
      result <- dbeta(x_centered, shape1, shape2) / 99
      result[x_centered < 0 | x_centered > 1] <- 0
      return(result)
    }
    return(rep(0, length(x_values)))
  }

  output$densityPlot <- renderPlotly({
    # Create x values
    x_values <- seq(1, 100, length.out = 1000)

    # Generate density values for each distribution
    y1 <- generate_density(input$dist1, input$mean1, input$sd1, x_values)
    y2 <- generate_density(input$dist2, input$mean2, input$sd2, x_values)
    y3 <- generate_density(input$dist3, input$mean3, input$sd3, x_values)

    # Create plot
    p <- plot_ly()

    # Find the minimum of all distributions at each point (for overlap)
    y_min_12 <- pmin(y1, y2)
    y_min_13 <- pmin(y1, y3)
    y_min_23 <- pmin(y2, y3)
    y_min_123 <- pmin(y1, y2, y3)

    # If highlighting is enabled, render overlap areas first
    if (input$highlightOverlap) {
      # Highlight overlap between all three distributions
      overlap_indices_123 <- which(y_min_123 > 0)
      if (length(overlap_indices_123) > 0) {
        p <- p %>% add_trace(
          x = x_values[overlap_indices_123],
          y = y_min_123[overlap_indices_123],
          name = "Triple Overlap",
          type = 'scatter',
          mode = 'none',
          fill = 'tozeroy',
          fillcolor = 'rgba(255,0,0,0.7)',
          showlegend = FALSE
        )
      }

      # Highlight pairwise overlaps (excluding triple overlap)
      # 1-2 overlap (excluding triple overlap)
      overlap_indices_12 <- which(y_min_12 > 0 & (y3 <= 0 | y_min_12 < y3))
      if (length(overlap_indices_12) > 0) {
        p <- p %>% add_trace(
          x = x_values[overlap_indices_12],
          y = y_min_12[overlap_indices_12],
          name = "Overlap 1-2",
          type = 'scatter',
          mode = 'none',
          fill = 'tozeroy',
          fillcolor = 'rgba(255,0,0,0.5)',
          showlegend = FALSE
        )
      }

      # 1-3 overlap (excluding triple overlap)
      overlap_indices_13 <- which(y_min_13 > 0 & (y2 <= 0 | y_min_13 < y2))
      if (length(overlap_indices_13) > 0) {
        p <- p %>% add_trace(
          x = x_values[overlap_indices_13],
          y = y_min_13[overlap_indices_13],
          name = "Overlap 1-3",
          type = 'scatter',
          mode = 'none',
          fill = 'tozeroy',
          fillcolor = 'rgba(255,0,0,0.5)',
          showlegend = FALSE
        )
      }

      # 2-3 overlap (excluding triple overlap)
      overlap_indices_23 <- which(y_min_23 > 0 & (y1 <= 0 | y_min_23 < y1))
      if (length(overlap_indices_23) > 0) {
        p <- p %>% add_trace(
          x = x_values[overlap_indices_23],
          y = y_min_23[overlap_indices_23],
          name = "Overlap 2-3",
          type = 'scatter',
          mode = 'none',
          fill = 'tozeroy',
          fillcolor = 'rgba(255,0,0,0.5)',
          showlegend = FALSE
        )
      }
    }

    # Add filled areas with 50% transparency
    p <- p %>% add_trace(
      x = x_values,
      y = y1,
      name = paste("Blue Students (", input$mean1, "%)"),
      type = 'scatter',
      mode = 'lines',
      fill = 'tozeroy',
      fillcolor = paste0(substr(colors[1], 1, 7), "80"),  # 50% transparency
      line = list(color = colors[1], width = 4),
      showlegend = TRUE
    )

    p <- p %>% add_trace(
      x = x_values,
      y = y2,
      name = paste("Orange Students (", input$mean2, "%)"),
      type = 'scatter',
      mode = 'lines',
      fill = 'tozeroy',
      fillcolor = paste0(substr(colors[2], 1, 7), "80"),  # 50% transparency
      line = list(color = colors[2], width = 4),
      showlegend = TRUE
    )

    p <- p %>% add_trace(
      x = x_values,
      y = y3,
      name = paste("Green Students (", input$mean3, "%)"),
      type = 'scatter',
      mode = 'lines',
      fill = 'tozeroy',
      fillcolor = paste0(substr(colors[3], 1, 7), "80"),  # 50% transparency
      line = list(color = colors[3], width = 4),
      showlegend = TRUE
    )

    # Add vertical lines for means with percentage labels
    max_y_value <- max(c(max(y1), max(y2), max(y3)))

    # Group 1 mean line and label
    p <- p %>% add_segments(x = input$mean1, xend = input$mean1, y = 0, yend = max(y1),
                           line = list(color = colors[1], width = 2, dash = 'dash'),
                           showlegend = FALSE)
    p <- p %>% add_annotations(
      x = input$mean1,
      y = max(y1) + max_y_value * 0.05,
      text = paste0(input$mean1, "%"),
      showarrow = FALSE,
      font = list(family = "Arial", size = 16, color = colors[1])
    )

    # Group 2 mean line and label
    p <- p %>% add_segments(x = input$mean2, xend = input$mean2, y = 0, yend = max(y2),
                           line = list(color = colors[2], width = 2, dash = 'dash'),
                           showlegend = FALSE)
    p <- p %>% add_annotations(
      x = input$mean2,
      y = max(y2) + max_y_value * 0.05,
      text = paste0(input$mean2, "%"),
      showarrow = FALSE,
      font = list(family = "Arial", size = 16, color = colors[2])
    )

    # Group 3 mean line and label
    p <- p %>% add_segments(x = input$mean3, xend = input$mean3, y = 0, yend = max(y3),
                           line = list(color = colors[3], width = 2, dash = 'dash'),
                           showlegend = FALSE)
    p <- p %>% add_annotations(
      x = input$mean3,
      y = max(y3) + max_y_value * 0.05,
      text = paste0(input$mean3, "%"),
      showarrow = FALSE,
      font = list(family = "Arial", size = 16, color = colors[3])
    )

    # Configure layout with larger fonts, bolder axes, and proper margins for title
    p <- p %>% layout(
      title = list(
        text = "Student Achievement Distribution Comparison",
        font = list(size = 24, family = "Arial", color = "black"),
        y = 0.95  # Move title down from the top edge
      ),
      margin = list(
        t = 100,  # Top margin for title
        l = 80,   # Left margin for y-axis title
        r = 40,   # Right margin
        b = 80    # Bottom margin for x-axis title
      ),
      xaxis = list(
        title = list(
          text = "Percentage of Students Approaching Grade Level or Higher",
          font = list(size = 20, family = "Arial", color = "black")
        ),
        range = c(0, 100),
        tickfont = list(size = 16),
        linewidth = 2,
        gridwidth = 2,
        ticksuffix = "%"  # Add % to x-axis ticks
      ),
      yaxis = list(
        title = list(
          text = "Distribution Density",
          font = list(size = 20, family = "Arial", color = "black")
        ),
        tickfont = list(size = 16),
        linewidth = 2,
        gridwidth = 2
      ),
      legend = list(
        font = list(size = 16, family = "Arial"),
        bgcolor = "rgba(255,255,255,0.8)"
      ),
      hovermode = "closest"
    )

    return(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
