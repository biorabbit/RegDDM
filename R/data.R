#' Example of LS task dataset from our previous Cognitive Reserve study
#'
#' This is a dataset with 49 subjects and 6032 trials. Each trial, the subject
#' is presented with different number of letters (`memload` variable). After 7
#' seconds, the subect is presented with another letter, and asked to decide
#' whether the letter is in the letter set at the beginning. The response (correct = 1
#' , incorrect = 0) and response time (`rt`, in seconds) is recorded. The age
#'  and education of the subject (both in years) are also recorded. `y` is the
#'  IQ of the subject estimated using the American NART.

#'
#' @format A list with two items
#' \describe{
#'   \item{data1}{Subject-level dataframe with 49 rows and 4 columns: `id`, `y`, `age` and `education`}
#'   \item{data2}{Trial-level dataframe with 6032 rows and 4 columns: `id`, `memload`, `response`, `rt`}
#' }
#'
#' @examples
#' data("regddm_data")
#' head(regddm_data$data1)
#' head(regddm_data$data2)
"regddm_data"





#' Simulated example dataset
#' @description
#' This is the same dataset used in the tutorial part of the RegDDM paper
"regddm_tutorial"

