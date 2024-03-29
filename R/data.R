#' spam7
#'
#' @name spam7
#' @title Spam E-mail Data
#' @docType data
#' @description The data consist of 4601 email items, of which 1813 items were
#'              identified as spam.
#' @format This data frame contains the following columns:
#' \itemize{
#' \item \code{crl.tot}: total length of words in capitals
#' \item \code{dollar}:  number of occurrences of the $ symbol
#' \item \code{bang}: number of occurrences of the ! symbol
#' \item \code{money}: number of occurrences of the word `money'
#' \item \code{n000}: number of occurrences of the string `000'
#' \item \code{make}: number of occurrences of the word `make'
#' \item \code{yesno}: outcome variable, a factor with levels \code{n} not spam,
#'       \code{y} spam
#' }
#' @examples
#' require(rpart)
#' spam.rpart <- rpart(formula = yesno ~ crl.tot + dollar + bang +
#'                         money + n000 + make, data=spam7)
#' plot(spam.rpart)
#' text(spam.rpart)
#' @source
#' George Forman, Hewlett-Packard Laboratories
#'
#' These data are available from the University of California at Irvine Repository
#' of Machine Learning Databases and Domain Theories. The address is:
#' \url{https://archive.ics.uci.edu/ml/index.php}
#'
#' Also available in the DAAG R package.
#'
#' John H. Maindonald and W. John Braun (2019). DAAG: Data Analysis and Graphics
#' Data and Functions.
#' R package version 1.22.1. https://CRAN.R-project.org/package=DAAG
NULL
