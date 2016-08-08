# Helpers used when making a tree from a Text Analysis item.
#' @importFrom flipFormat DataTableWithRItemFormat
#' @export
print.textPredictiveTree <- function(x, ...){
    if (!x$print.table)
    {
        # Default to normal print function defined for CART
        classes <- class(x)
        class(x) <- classes[classes != "textPredictiveTree"]
        print(x)
    } else {
        dd <- data.frame("OutcomeVariable" = x$outcome.variable, "Original Text" = x$original.text, "Transformed Text" = x$transformed.text)
        dd <- DataTableWithRItemFormat(dd, allow.length.change = FALSE)
        print(dd)
    }
}

#' \code{CheckDataForTextTree} Check that there are enough cases to compute the
#' tree from the term document matrix.
#' @param data A data frame where the first column is the outcome variable and the subsequent
#' columns are terms from the term document matrix.
#' @param weights An option vector containing sampling weights for each row of \code{data}.
#' @param subset An option vector describing the subset of rows from \code{data} that are to be used to generate the tree.
#' @inheritParams CART
#' @importFrom flipData ErrorIfMissingDataFound
#' @export
CheckDataForTextTree <- function(data, weights = NULL, subset = NULL, missing = "Exclude cases with missing data")
{
    if (is.null(weights))
    {
        weights <- rep(1, nrow(data))
    }

    if (length(subset) == 1 || is.null(subset))
    {
        subset <- rep(TRUE, nrow(data))
    }

    subset <- subset & (weights > 0)
    subset.data <- data[subset, ]

    if (missing == "Error if missing data")
    {
        ErrorIfMissingDataFound(data.subset)
        num.valid <- nrow(subset.data)
    } else if (missing == "Imputation") {
        num.valid <- nrow(subset.data)
    } else if (missing == "Use partial data (pairwise correlations)" || missing == "Use partial data") {
        num.valid <- nrow(flipData:::removeCasesWithAllNA(subset.data))
    } else if (missing == "Exclude cases with missing data") {
        num.valid <- nrow(flipData:::removeCasesWithAnyNA(subset.data))
    }

    if (num.valid < ncol(data))
    {
        stop("The predictive tree requires that there are fewer words than cases in the data. To reduce the number of words in the analysis, increase the word frequency specified in the initial text analysis.")
    }
    return(TRUE)
}





#' \code{CreateTextTree} Generate a predictive tree from an outcome variable and a Term Document Matrix.
#' @param tdm A Term Document Matrix.
#' @param outcome.variable The dependent variable used when creating \code{tree}.
#' @param original.text A vector containing the source text that was used to create the TDM.
#' @param transformed.text A vector containing the processed text.
#' @param output A string indicating how the result should be printed. Options
#' are \code{"Sankey"}, \code{"Tree"}, \code{"Text"}, or \code{"Table"}. The first three
#' match the output options of \code{\link{CART}}, and "Table" prints an HTML widget table
#' which shows the outcome variable next to the text.
#' #' @inheritParams CART
#' @export
CreateTextTree <- function(tree,
                           outcome.variable,
                           original.text,
                           transformed.text,
                           output = "Sankey")
{
    class(tree) <- c("textPredictiveTree", class(tree))
    tree$outcome.variable <- outcome.variable
    tree$original.text <- original.text
    tree$transformed.text <- transformed.text
    tree$print.table <- output == "Table"
    return(tree)
}

