## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, collapse = TRUE)
library(dplyr)
library(maraca)

## ----maraca1, eval = TRUE-----------------------------------------------------
file_path <- system.file("extdata", "hce_scenario_b.csv", package = "maraca")

data <- read.csv(file_path, stringsAsFactors = FALSE)
colnames(data)

## ----maraca2, eval = TRUE-----------------------------------------------------
column_names <- c(
    outcome = "GROUP",
    arm = "TRTP",
    value = "AVAL0"
)

## ----maraca3, eval = TRUE-----------------------------------------------------
unique(data[["GROUP"]])

## ----maraca4, eval = TRUE-----------------------------------------------------
tte_outcomes <- c(
  "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
)
continuous_outcome <- "Continuous outcome"

## ----maraca5, eval = TRUE-----------------------------------------------------
unique(data[["TRTP"]])

## ----maraca6, eval = TRUE-----------------------------------------------------
arm_levels = c(active = "Active", control = "Control")

## ----maraca7, eval = TRUE-----------------------------------------------------
mar <- maraca(
  data, tte_outcomes, continuous_outcome, arm_levels, column_names, compute_win_odds = TRUE
)

## ---- eval = TRUE-------------------------------------------------------------
mar$win_odds

## ----maraca8, eval = TRUE, fig.width = 7, fig.height = 6----------------------
plot(mar, continuous_grid_spacing_x = 20)

## ----maraca9, eval = TRUE, fig.width = 7, fig.height = 6----------------------
plot(mar, continuous_grid_spacing_x = 20, density_plot_type = "box")

## ----maraca10, eval = TRUE, fig.width = 7, fig.height = 6---------------------
plot(mar, continuous_grid_spacing_x = 20, density_plot_type = "scatter", vline_type = "mean")

## ----maraca11, eval = TRUE, fig.width = 7, fig.height = 6, message=FALSE, warning=FALSE----
p <- plot_maraca(mar, continuous_grid_spacing_x = 20, density_plot_type = "scatter", vline_type = "mean")
p +
  theme_bw() + scale_color_manual(values=c("#999999", "#E69F00")) +
  theme(axis.text.x.bottom = element_text(angle = c(90,90,90,90,0), vjust = 0.5, hjust = 0.5))

## ----maraca12, eval = TRUE, fig.width = 7, fig.height = 6, message=FALSE, warning=FALSE----
plot_tte_components(mar)

## ----maraca13, eval = TRUE, fig.width = 7, fig.height = 6, message=FALSE, warning=FALSE----
plot_tte_composite(mar)

