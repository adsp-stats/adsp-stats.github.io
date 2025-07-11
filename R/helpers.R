```r
# This file contains reusable R functions for your book

monthly_plot <- function(ts) {
  library(ggplot2)
  autoplot(ts) + ggtitle("Monthly Time Series Plot")
}