## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  dpi = 100
)

## ----setup--------------------------------------------------------------------
library(correlationfunnel)
library(dplyr)

## -----------------------------------------------------------------------------
data("customer_churn_tbl")

customer_churn_tbl %>% glimpse()

## -----------------------------------------------------------------------------
customer_churn_binarized_tbl <- customer_churn_tbl %>%
  select(-customerID) %>%
  mutate(TotalCharges = ifelse(is.na(TotalCharges), MonthlyCharges, TotalCharges)) %>%
  binarize(n_bins = 5, thresh_infreq = 0.01, name_infreq = "OTHER", one_hot = TRUE)

customer_churn_binarized_tbl %>% glimpse()

## -----------------------------------------------------------------------------
customer_churn_corr_tbl <- customer_churn_binarized_tbl %>%
  correlate(Churn__Yes)

customer_churn_corr_tbl

## ---- fig.height=12, fig.width=8, out.height="70%", out.width="70%", fig.align="center"----
customer_churn_corr_tbl %>%
  plot_correlation_funnel()

