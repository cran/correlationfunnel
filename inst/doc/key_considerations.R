## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  dpi = 100,
  out.width = "60%",
  out.height = "60%",
  fig.height = 4,
  fig.width = 4,
  fig.align = "center"
)

## ----setup--------------------------------------------------------------------
library(correlationfunnel)
library(dplyr)
library(tibble)
library(ggplot2)
library(stringr)
library(forcats)
library(knitr)

## ---- echo=F------------------------------------------------------------------
tribble(
  ~"", ~`y=0`, ~`y=1`,
  "x=0", "f00", "f01",
  "x=1", "f10", "f11"
) %>% knitr::kable()

## -----------------------------------------------------------------------------
# Generate Data
set.seed(1)
linear_data <- tibble(
  sales = rnorm(100, mean = 10, sd = 5) + seq(1, 200,length.out = 100) * 1e6,
  macro_indicator = rnorm(100, mean = 1, sd = 2) + seq(5, 20, length.out = 100)
  ) %>%
  mutate_all(~round(., 2))

# Plot Relationship
linear_data %>%
  ggplot(aes(macro_indicator, sales)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  labs(title = "Data with Linear Relationship")
  

## -----------------------------------------------------------------------------
linear_data %>%
  correlate(target = sales) 

## -----------------------------------------------------------------------------
linear_data_corr_tbl <- linear_data %>%
  binarize() %>%
  correlate(sales__150250005.6625_Inf) 

linear_data_corr_tbl

## -----------------------------------------------------------------------------
linear_data_corr_tbl %>%
  plot_correlation_funnel()

## -----------------------------------------------------------------------------
# Generate synthetic data
set.seed(1)
nonlinear_data <- tibble(
  sales = c(
    rnorm(100, mean = 10, sd = 25),
    rnorm(100, mean = 50, sd = 100),
    rnorm(100, mean = 2,  sd = 40)),
  age = c(
    runif(100, min = 20, max = 29),
    runif(100, min = 30, max = 39),
    runif(100, min = 39, max = 59)
  )
) %>%
  mutate(
    sales = ifelse(sales < 0, 0, sales) %>% round(2),
    age   = floor(age)
    )

# Visualize the nonlinear relationship
nonlinear_data %>%
  ggplot(aes(age, sales)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_y_continuous(labels = scales::dollar_format()) +
  expand_limits(y = 300) +
  labs(title = "Data with Non-Linear Relationship")

## -----------------------------------------------------------------------------
nonlinear_data %>%
  correlate(sales)

## -----------------------------------------------------------------------------
nonlinear_data_corr_tbl <- nonlinear_data %>%
  binarize(n_bins = 5) %>%
  correlate(sales__46.552_Inf) 

nonlinear_data_corr_tbl

## -----------------------------------------------------------------------------
nonlinear_data_corr_tbl %>%
  plot_correlation_funnel(limits = c(-0.2, 1))

## -----------------------------------------------------------------------------
data("marketing_campaign_tbl")

marketing_campaign_tbl 

## -----------------------------------------------------------------------------
marketing_campaign_tbl %>%
  pull(CAMPAIGN) %>%
  quantile()

## -----------------------------------------------------------------------------
marketing_campaign_tbl %>%
  ggplot(aes(CAMPAIGN)) +
  geom_histogram() +
  labs(title = "Skewed Data")

## -----------------------------------------------------------------------------
marketing_campaign_tbl %>%
  select(CAMPAIGN) %>%
  binarize()

## -----------------------------------------------------------------------------
marketing_campaign_tbl %>%
  pull(PDAYS) %>%
  quantile()

## -----------------------------------------------------------------------------
marketing_campaign_tbl %>%
  ggplot(aes(PDAYS)) +
  geom_histogram() +
  labs(title = "Highly Skewed Data")

## -----------------------------------------------------------------------------
marketing_campaign_tbl %>%
  select(PDAYS) %>%
  binarize()

## -----------------------------------------------------------------------------
marketing_campaign_tbl %>%
  select(EDUCATION)

## -----------------------------------------------------------------------------
marketing_campaign_tbl %>%
  select(EDUCATION) %>%
  binarize(one_hot = TRUE)

## -----------------------------------------------------------------------------
marketing_campaign_tbl %>%
  select(EDUCATION) %>%
  binarize(one_hot = FALSE)

## -----------------------------------------------------------------------------
marketing_campaign_tbl %>%
  select(JOB) %>%
  binarize(thresh_infreq = 0) %>% 
  glimpse()

## ---- fig.height=8, fig.width=5-----------------------------------------------
marketing_campaign_tbl %>%
  # Count Frequency
  count(JOB) %>%
  mutate(prop = n / sum(n)) %>%
  
  # Format columns for plot
  mutate(JOB = as.factor(JOB) %>% fct_reorder(n)) %>%
  mutate(label_text = str_glue("JOB: {JOB}
                               Count: {n}
                               Prop: {scales::percent(prop)}")) %>%
  
  # Creat viz
  ggplot(aes(JOB, n)) +
  geom_point(aes(size = n)) +
  geom_segment(aes(yend = 0, xend = JOB)) +
  geom_label(aes(label = label_text), size = 3, hjust = "inward") +
  coord_flip() +
  labs(title = "Frequency of Each Level in JOB Feature")

## -----------------------------------------------------------------------------
marketing_campaign_tbl %>%
  select(JOB) %>%
  binarize(thresh_infreq = 0.06, name_infreq = "-OTHER") %>% 
  glimpse()

## ---- fig.height=8------------------------------------------------------------
marketing_campaign_tbl %>%
  binarize(n_bins = 5, thresh_infreq = 0.06, name_infreq = "-OTHER") %>%
  correlate(TERM_DEPOSIT__yes) %>%
  plot_correlation_funnel(alpha = 0.7)

