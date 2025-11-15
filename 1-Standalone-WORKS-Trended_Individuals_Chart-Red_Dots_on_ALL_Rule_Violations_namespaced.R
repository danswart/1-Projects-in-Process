# TRENDED CONTROL CHART WITH RULE #1 AND ANHOEJ RUN-RULES ---------------------------------------------------

# install.packages(c("mapview", "survey", "srvyr", "arcgislayers"))

# census_api_key("95496766c51541ee6f402c1e1a8658581285b759", install = TRUE, overwrite = TRUE)

# load libraries

# Force dplyr's select to take precedence
select <- dplyr::select
filter <- dplyr::filter

# Options
base::options(scipen = 999)
base::options(qic.clshade = T) # NO LONGER NEEDED; CHARTS ALL PREPARED WITH GGPLOT2 ONLY
base::options(qic.linecol = 'black') # NO LONGER NEEDED; CHARTS ALL PREPARED WITH GGPLOT2 ONLY
base::options(qic.signalcol = "red") # NO LONGER NEEDED; CHARTS ALL PREPARED WITH GGPLOT2 ONLY
base::options(qic.targetcol = "purple") # NO LONGER NEEDED; CHARTS ALL PREPARED WITH GGPLOT2 ONLY
base::options(DT.options = base::list(dom = 'pBlfrti')) # Add buttons, filtering, and top (t) pagination controls
base::options(shiny.maxRequestSize = 50 * 1024^2) # Set upload maximum to 50 MB
base::options(tigris_use_cache = TRUE)


# Set global theme for consistent plots
ggplot2::theme_set(
  ggplot2::theme_minimal(base_size = 20) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 26),
      plot.subtitle = ggplot2::element_text(face = "bold", size = 24),
      axis.title.x = ggplot2::element_text(face = "bold", size = 22),
      axis.title.y = ggplot2::element_text(face = "bold", size = 22),
      axis.text.x = ggplot2::element_text(
        face = "bold",
        size = 22,
        angle = 45,
        hjust = 1
      ),
      legend.position = "bottom",
      strip.text = ggplot2::element_text(face = "bold"),
      panel.spacing.x = grid::unit(1.5, "cm"),
      panel.spacing.y = grid::unit(1.5, "cm"),
      plot.margin = ggplot2::margin(20, 20, 20, 20, "pt")
    )
)


# Set seed for reproducibility
base::set.seed(123)


# Create a toy dataset

n <- 30
x <- 1:n
y <- 10 + 0.5 * x + stats::rnorm(n, mean = 0, sd = 1)
data <- base::data.frame(x = x, y = y)


# Introduce a perturbation to shift the mean higher after point 15
# data$y[data$x > 15] <- data$y[data$x > 15] + 3  # Add 3 to the y values after point 15

# Fit a linear model and create a summary object
model <- stats::lm(y ~ x, data = data)

model_summary <- base::summary(model)

# Calculate a standard deviation that reflects the trend
residuals_sd <- stats::sd(stats::residuals(model))

# Calculate control limits (3 sigma, NOT 3 std dev)
data$centerline <- stats::predict(model, newdata = data)
data$ucl <- data$centerline + 3 * (residuals_sd / 1.128)
data$lcl <- data$centerline - 3 * (residuals_sd / 1.128)

# Set Shewhart Rule #1 boundaries
sigma.signals <- y < data$lcl | y > data$ucl # this is a logical vector

# Add check for unusually long runs or unusually few crossings.

# add runs analysis
runs <- base::sign(y - data$centerline)
runs <- runs[runs != 0]
runs <- base::rle(runs)$lengths
n.obs <- base::sum(runs)
longest.run <- base::max(runs, na.rm = TRUE)
n.runs <- base::length(runs)
n.crossings <- n.runs - 1

longest.run.max <- base::round(base::log2(n.obs) + 3)
n.crossings.min <- stats::qbinom(.05, n.obs - 1, 0.5)

runs.signal <- longest.run > longest.run.max | n.crossings < n.crossings.min # this is a logical vector


# Initialize a Plot for a trended control chart
ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +

  # Connect dots with lines
  ggplot2::geom_line() +

  # Draw CL, UCL & LCL
  ggplot2::geom_line(
    ggplot2::aes(y = centerline),
    color = 'royalblue',
    linetype = runs.signal + 1,
    linewidth = 2
  ) +

  ggplot2::geom_line(
    ggplot2::aes(y = ucl),
    color = "red",
    linetype = "solid",
    linewidth = 2
  ) +

  ggplot2::geom_line(
    ggplot2::aes(y = lcl),
    color = "red",
    linetype = "solid",
    linewidth = 2
  ) +

  # Convert a logical vector to a numeric value that sets the color of the point to red

  # Plot with conditional color based on sigma.signals
  # geom_point(aes(col = factor(sigma.signals)),  # Convert logical to factor (TRUE = 1, FALSE = 0)
  #            shape = 19,
  #            size = 5) +
  # scale_color_manual(values = c("black", "red")) +  # Map FALSE to black and TRUE to red

  ggplot2::geom_point(
    ggplot2::aes(x = x, y = y),
    shape = 19,
    size = 5,
    col = sigma.signals + 1
  ) +

  # Add labels
  ggplot2::labs(
    title = "Trended I-Chart",
    subtitle = "with Shewhart Rule #1 and Anhoej Run Rules",
    caption = "Data is synthetic",
    x = "Observation",
    y = "Value"
  )
