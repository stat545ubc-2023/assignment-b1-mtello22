Assignment B-1
================
Marco Tello
2023-10-30

``` r
library(tidyverse)
library(testthat)
set.seed(123)
```

# Exercise 1: Make a Function (25 points)

In this exercise, you’ll be making a function and fortifying it. The
function need not be complicated. The function need not be “serious”,
but shouldn’t be nonsense.

## Function to evaluate predictions from a regression model.

``` r
evaluate_regressor <- function(df, correlation = NULL, 
                               pred_name = "pred", 
                               obs_name = "obs"){
  if(!(correlation %in% c("pearson", "kendall", "spearman"))){
    return("Please specify a valid correlation for the function cor() between: \"pearson\", \"kendall\", or \"spearman\" ")
  }
  # Extract predictions and observations
  predictions <- df %>%
    select(all_of(pred_name)) %>%
    unlist() %>%
    as.numeric()
  
  observations <- df %>%
    select(all_of(obs_name)) %>%
    unlist() %>%
    as.numeric()
  
  
  # Verify non missing values in predictions or observations
  if(any(is.na(predictions)) | any(is.na(observations))) {
    return("Please remove missing values from the predictions and observations")
  }
  # Verify numeric values in predictions or observations
  if(!all(is.numeric(predictions)) | !all(is.numeric(observations))) {
    return("Please remove non-numeric values from the predictions and observations")
  }
  # Calculate correlation
  correlation_score <- cor(x = predictions, y = observations, method = correlation)
  
  # Calculate rmse
  rmse_score <- sqrt(mean((observations - predictions)^2))
  
  # Calculate R-squared
  SSR <- sum((observations - predictions)^2)
  SST <- sum((observations - mean(observations))^2)
  rsquared_score <- 1 - (SSR /SST)
  
  # Make output data frame
  output <- data.frame(score_name = c("RMSE", "r_squared", 
                                      paste(correlation, "correlation", 
                                            sep = "_")),
                       score = c(rmse_score, rsquared_score, correlation_score))
  return(output)
}
```

# Exercise 2: Document your Function (20 points)

In the same code chunk where you made your function, document the
function using roxygen2 tags. Be sure to include:

1.  Title.

2.  Function description: In 1-2 brief sentences, describe what the
    function does.

3.  Document each argument with the `@param` tag, *making sure to
    justify why you named the parameter as you did*.

<!-- -->

    -   (Justification for naming is not often needed, but we want to hear your reasoning.)

4.  What the function returns, using the `@return` tag.

``` r
#' Evaluate regressor  
#' @description
#' This function calculates three common scores for evaluation of regression models: R-squared, Root Mean Squared Error (RMSE), and a correlation score among "pearson", "kendall", or "spearman". Takes as input a data.frame object with two independent columns for the observed ("obs") and predicted values ("pred").  
#' 
#' @param df Data frame with the predictions and observations as independent columns. The name was chosen to indicate the input is a data frame object
#' @param correlation Correlation method to pass cor() function. Accepted values: "pearson", "kendall", "spearman". Default NULL.
#' @param pred_name Name of the column containing the predictions. The name was selected to guide the user about what column to select. Default "pred"
#' @param obs_name Name of the column containing the observations or prediction targets The name was selected to guide the user about what column to select. Default "obs".
#' @return A dataframe with two columns: first the name of the score calculated, second the score numeric value.
evaluate_regressor <- function(df, correlation = NULL, 
                               pred_name = "pred", 
                               obs_name = "obs"){
  if(!(correlation %in% c("pearson", "kendall", "spearman"))){
    return("Please specify a valid correlation for the function cor() between: \"pearson\", \"kendall\", or \"spearman\" ")
  }
  # Extract predictions and observations
  predictions <- df %>%
    select(all_of(pred_name)) %>%
    unlist() %>%
    as.numeric()
  
  observations <- df %>%
    select(all_of(obs_name)) %>%
    unlist() %>%
    as.numeric()
  
  
  # Verify non missing values in predictions or observations
  if(any(is.na(predictions)) || any(is.na(observations))) {
    return("Please remove missing and non-numeric values from the predictions and observations")
  }

  # Calculate correlation
  correlation_score <- cor(x = predictions, y = observations, method = correlation)
  
  # Calculate rmse
  rmse_score <- sqrt(mean((observations - predictions)^2))
  
  # Calculate R-squared
  SSR <- sum((observations - predictions)^2)
  SST <- sum((observations - mean(observations))^2)
  rsquared_score <- 1 - (SSR /SST)
  
  # Make output data frame
  output <- data.frame(score_name = c("RMSE", "r_squared", 
                                      paste(correlation, "correlation", 
                                            sep = "_")),
                       score = c(rmse_score, rsquared_score, correlation_score))
  return(output)
}
```

# Exercise 3: Include examples (15 points)

Demonstrate the usage of your function with a few examples. Use one or
more new code chunks, describing what you're doing.

Note: If you want to deliberately show an error, you can use
`error = TRUE` in your code chunk option.

``` r
# Calculate Spearman correlation 
my_pred <- data.frame(obs = sort(rnorm(n = 100)), 
                      pred = sort(rnorm(n = 100, mean = 0.6, sd = 1)))
evaluate_regressor(my_pred, correlation = "spearman")
```

    ##             score_name     score
    ## 1                 RMSE 0.4405764
    ## 2            r_squared 0.7646897
    ## 3 spearman_correlation 1.0000000

``` r
# Calculate Pearson correlation 
my_pred <- data.frame(obs = sort(rnorm(n = 100)), 
                      pred = sort(rnorm(n = 100, mean = 0.2, sd = 2)))
evaluate_regressor(my_pred, correlation = "pearson")
```

    ##            score_name      score
    ## 1                RMSE  1.1454762
    ## 2           r_squared -0.4689275
    ## 3 pearson_correlation  0.9864073

# Exercise 4: Test the Function (25 points)

Running examples is a good way of checking by-eye whether your function
is working as expected. But, having a formal "yes or no" check is useful
when you move on to other parts of your analysis.

Write formal tests for your function. You should use at least three
non-redundant uses of an `expect_()` function from the `testthat`
package, and they should be contained in a `test_that()` function (or
more than one). They should all pass.

``` r
example1 <- data.frame(obs = sort(rnorm(n = 100)), 
                       pred = c(sort(rnorm(n = 99, mean = 0.6, sd = 1)), NA))
example1 <- evaluate_regressor(example1, correlation = "pearson")

example2 <- data.frame(obs = sort(rnorm(n = 100)), 
                       pred = c(sort(rnorm(n = 99, mean = 0.6, sd = 1)), "AB"))
example2 <- evaluate_regressor(example2, correlation = "pearson")

example3 <- data.frame(obs = sort(rnorm(n = 100)), 
                      pred = c(sort(rnorm(n = 99, mean = 0.6, sd = 1)), "0.3"))
example3 <- evaluate_regressor(my_pred, correlation = "spearman")
```

The first two examples have instances where the calculation of scores
should not be performed, returning a message.

The third example is expected to run because the function would convert
the string “0.3” to a numeric version. So it should generate a data
frame with the appropriate dimensions and classes for each column.

``` r
test_that("Testing function", {
  expect_equal(example1, 
               "Please remove missing and non-numeric values from the predictions and observations")
  expect_equal(example2, 
               "Please remove missing and non-numeric values from the predictions and observations")
  expect_equal(dim(example3), c(3,2))
  expect_true(is.numeric(example3$score))
  expect_true(is.character(example3$score_name))
})
```

    ## Test passed 🥇
