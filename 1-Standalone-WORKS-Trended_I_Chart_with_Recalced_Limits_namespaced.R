# install.packages(c("mapview", "survey", "srvyr", "arcgislayers"))

# census_api_key("95496766c51541ee6f402c1e1a8658581285b759", install = TRUE, overwrite = TRUE)

# load libraries

# Force dplyr's select to take precedence
select <- dplyr::select
filter <- dplyr::filter

# Options
options(scipen = 999)
options(qic.clshade = T) # NO LONGER NEEDED; CHARTS ALL PREPARED WITH GGPLOT2 ONLY
options(qic.linecol = 'black') # NO LONGER NEEDED; CHARTS ALL PREPARED WITH GGPLOT2 ONLY
options(qic.signalcol = "firebrick") # NO LONGER NEEDED; CHARTS ALL PREPARED WITH GGPLOT2 ONLY
options(qic.targetcol = "purple") # NO LONGER NEEDED; CHARTS ALL PREPARED WITH GGPLOT2 ONLY
options(DT.options = list(dom = 'pBlfrti')) # Add buttons, filtering, and top (t) pagination controls
options(shiny.maxRequestSize = 50 * 1024^2) # Set upload maximum to 50 MB
options(tigris_use_cache = TRUE)


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
set.seed(123)


# Create a toy dataset

n <- 30
x <- 1:n
y <- 10 + 0.5 * x + stats::rnorm(n, mean = 0, sd = 1)
data <- base::data.frame(x = x, y = y)

# Introduce a perturbation to shift the mean higher after point 15
data$y[data$x > 15] <- data$y[data$x > 15] + 5 # Add 15 to the y values after point 15

# Split the data into two segments
data_first_15 <- data[data$x <= 15, ]
data_remaining <- data[data$x > 15, ]

# Fit linear models for each segment
model_first_15 <- stats::lm(y ~ x, data = data_first_15)
model_remaining <- stats::lm(y ~ x, data = data_remaining)

# Calculate standard deviations for each segment
residuals_sd_first_15 <- stats::sd(stats::residuals(model_first_15))
residuals_sd_remaining <- stats::sd(stats::residuals(model_remaining))

# Calculate control limits for each segment
data$centerline_first_15 <- stats::predict(model_first_15, newdata = data)
data$ucl_first_15 <- data$centerline_first_15 +
  3 * (residuals_sd_first_15 / 1.128)
data$lcl_first_15 <- data$centerline_first_15 -
  3 * (residuals_sd_first_15 / 1.128)

data$centerline_remaining <- stats::predict(model_remaining, newdata = data)
data$ucl_remaining <- data$centerline_remaining +
  3 * (residuals_sd_remaining / 1.128)
data$lcl_remaining <- data$centerline_remaining -
  3 * (residuals_sd_remaining / 1.128)

# Set Shewhart Rule #1 boundaries
sigma.signals <- y < data$lcl_first_15 |
  y > data$ucl_first_15 |
  y < data$lcl_remaining |
  y > data$ucl_remaining

# Add check for unusually long runs or unusually few crossings.

# runs analysis
runs <- base::sign(
  y - base::ifelse(x <= 15, data$centerline_first_15, data$centerline_remaining)
)
runs <- runs[runs != 0]
runs <- base::rle(runs)$lengths
n.obs <- base::sum(runs)
longest.run <- base::max(runs, na.rm = TRUE)
n.runs <- base::length(runs)
n.crossings <- n.runs - 1

longest.run.max <- base::round(base::log2(n.obs) + 3)
n.crossings.min <- stats::qbinom(.05, n.obs - 1, 0.5)

runs.signal <- longest.run > longest.run.max | n.crossings < n.crossings.min

# Plot a trended control chart
ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_point(
    ggplot2::aes(x = x, y = y),
    shape = 19,
    size = 3,
    col = sigma.signals + 1
  ) +
  ggplot2::geom_line() +

  # Draw CL, UCL & LCL for the first 15 points
  ggplot2::geom_line(
    ggplot2::aes(y = base::ifelse(x <= 15, centerline_first_15, NA)),
    color = "blue",
    linetype = "solid",
    linewidth = 1
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = base::ifelse(x <= 15, ucl_first_15, NA)),
    color = "red",
    linetype = "dotted",
    linewidth = 1
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = base::ifelse(x <= 15, lcl_first_15, NA)),
    color = "red",
    linetype = "dotted",
    linewidth = 1
  ) +

  # Draw CL, UCL & LCL for the remaining points
  ggplot2::geom_line(
    ggplot2::aes(y = base::ifelse(x > 15, centerline_remaining, NA)),
    color = "green",
    linetype = "solid",
    linewidth = 1
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = base::ifelse(x > 15, ucl_remaining, NA)),
    color = "red",
    linetype = "dotted",
    linewidth = 1
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = base::ifelse(x > 15, lcl_remaining, NA)),
    color = "red",
    linetype = "dotted",
    linewidth = 1
  ) +

  # Add labels
  ggplot2::labs(
    title = "Trended Control Chart with Recalced Limits",
    subtitle = "Using Shewhart Rule #1 and Anhoej Run Rules",
    caption = "Perturbation of 3 at Point 16",
    x = "Observation",
    y = "Value"
  )
