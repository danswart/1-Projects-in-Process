# TRENDED CONTROL CHART WITH RULE #1 AND ANHOEJ RUN-RULES ---------------------------------------------------

# install.packages(c("mapview", "survey", "srvyr", "arcgislayers"))

# census_api_key("95496766c51541ee6f402c1e1a8658581285b759", install = TRUE, overwrite = TRUE)

# load libraries

# library(BayesFactor) # Computation of Bayes Factors for Common Designs
# library(boot) # Bootstrap Functions
# library(broom) # Convert Statistical Objects into Tidy Tibbles
# library(camcorder) # Record Your Plot History
# library(car) # Companion to Applied Regression
# library(corrplot) # Visualization of a Correlation Matrix
# library(cowplot) # Streamlined Plot Theme and Plot Annotations for 'ggplot2'
# library(dagitty) # Graphical Analysis of Structural Causal Models
# library(DiagrammeR) # Graph/Network Visualization
# library(dlookr) # Tools for Data Diagnosis, Exploration, Transformation
library(dplyr) # A Grammar of Data Manipulation
# library(DT) # A Wrapper of the JavaScript Library 'DataTables' # A Wrapper of the JavaScript Library 'DataTables'
# library(dutchmasters) # Color Palettes based on Famous Paintings
# library(flexdashboard) # R Markdown Format for Flexible Dashboards
# library(flextable) # Functions for Tabular Reporting
# library(forcats) # Tools for Working with Categorical Variables (Factors)
# library(gganimate) # A Grammar of Animated Graphics
# library(ggcorrplot) # Visualization of a Correlation Matrix using 'ggplot2'
# library(ggdag) # Analyze and Create Elegant Directed Acyclic Graphs
# library(ggforce) # Accelerating 'ggplot2'
# library(gghighlight) # Highlight Lines and Points in 'ggplot2'
# library(ggplot2) # Create Elegant Data Visualizations Using the Grammar of Graphics
# library(ggokabeito) # 'Okabe-Ito' Scales for 'ggplot2' and 'ggraph'
# library(ggpubr) # 'ggplot2' Based Publication Ready Plots
# library(ggrepel) # Automatically Position Non-Overlapping Text Labels with 'ggplot2'
# library(ggtext) # Improved Text Rendering Support for 'ggplot2'
# library(ggthemes) # Extra Themes, Scales and Geoms for 'ggplot2'
# library(glue) # Interpreted String Literals
# library(grid) # The Grid Graphics Package
# library(gt) # Easily Create Presentation-Ready Display Tables
# library(gtExtras) # Extending 'gt' for Beautiful HTML Tables
# library(haven) # Import and Export 'SPSS', 'Stata' and 'SAS' Files
# library(here) # A Simpler Way to Find Your Files
# library(htmltools) # Tools for HTML
# library(htmlwidgets) # HTML Widgets for R
# library(janitor) # Simple Tools for Examining and Cleaning Dirty Data
# library(kableExtra) # Construct Complex Table with 'kable' and Pipe Syntax
# library(knitr) # A General-Purpose Package for Dynamic Report Generation in R
# library(lavaan) # Latent Variable Analysis
# library(lubridate) # Make Dealing with Dates a Little Easier
# library(monochromeR) # Easily Create, View and Use Monochrome Color Palettes
# library(paletteer) # Comprehensive Collection of Color Palettes
# library(patchwork) # The Composer of Plots
# library(plotly) # Create Interactive Web Graphics via 'plotly.js' # Create Interactive Web Graphics via 'plotly.js'
# library(ppcor) # Partial and Semi-Partial (Part) Correlation
# library(prettycode) # Pretty Print R Code in the Terminal
# library(purrr) # Functional Programming Tools
# library(pwr) # Basic Functions for Power Analysis
# library(qgraph) # Graph Plotting Methods, Psychometric Data Visualization and Graphical Model Estimation
# library(qicharts2) # Quality Improvement Charts
# library(RColorBrewer) # ColorBrewer Palettes
# library(readr) # Read Rectangular Text Data
# library(readxl) # Read Excel Files
# library(rethinking) # Statistical Rethinking book package
# library(rlang) # Functions for Base Types and Core R and 'Tidyverse' Features
# library(scales) # Scale Functions for Visualization
# library(shiny) # Web Application Framework for R
# library(shinyobjects) # Access Reactive Data Interactively
# library(skimr) # Compact and Flexible Summaries of Data
# library(stringr) # Simple, Consistent Wrappers for Common String Operations
# library(tibble) # Simple Data Frames
# library(tidycensus) # Load US Census Boundary and Attribute Data as 'tidyverse' and 'sf'-Ready Data Frames
# library(tidylog) # Logging for 'dplyr' and 'tidyr' Functions
# library(tidyr) # Tidy Messy Data
# library(tidytext) # Text Mining using 'dplyr', 'ggplot2', and Other Tidy Tools
# library(tsibble) # Tidy Temporal Data Frames and Tools
# library(viridis) # Colorblind-Friendly Color Maps for R
# library(visdat) # Preliminary Visualization of Data
# library(vroom) # Read and Write Rectangular Text Data Quickly
# library(wesanderson) # A Wes Anderson Palette Generator
# library(writexl) # Export Data Frames to Excel 'xlsx' Format
# library(WRS2) # A Collection of Robust Statistical Methods

# Force dplyr's select to take precedence
select <- dplyr::select
filter <- dplyr::filter

# Options
options(scipen = 999)
options(qic.clshade = T) # NO LONGER NEEDED; CHARTS ALL PREPARED WITH GGPLOT2 ONLY
options(qic.linecol = 'black') # NO LONGER NEEDED; CHARTS ALL PREPARED WITH GGPLOT2 ONLY
options(qic.signalcol = "red") # NO LONGER NEEDED; CHARTS ALL PREPARED WITH GGPLOT2 ONLY
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

# # Load libraries
# library(tidyverse) # Easily Install and Load the 'Tidyverse'
# library(plotly) # Create Interactive Web Graphics via 'plotly.js'
# library(DT) # A Wrapper of the JavaScript Library 'DataTables'

# Create a toy dataset
set.seed(123)
n <- 30
x <- 1:n
y <- 10 + 0.5 * x + stats::rnorm(n, mean = 0, sd = 1)
data <- data.frame(x = x, y = y)


# Introduce a perturbation to shift the mean higher after point 15
# data$y[data$x > 15] <- data$y[data$x > 15] + 3  # Add 3 to the y values after point 15

# Fit a linear model and create a summary object
model <- stats::lm(y ~ x, data = data)

model_summary <- summary(model)

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
runs <- sign(y - data$centerline)
runs <- runs[runs != 0]
runs <- rle(runs)$lengths
n.obs <- sum(runs)
longest.run <- max(runs, na.rm = TRUE)
n.runs <- length(runs)
n.crossings <- n.runs - 1

longest.run.max <- round(log2(n.obs) + 3)
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
