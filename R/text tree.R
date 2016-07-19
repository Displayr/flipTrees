# Helpers used when making a tree from a Text Analysis item.

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

prepareTextTreeOptions <- function(tree, outcome.variable, original.text, transformed.text, print.table)
{
    class(tree) <- c("textPredictiveTree", class(tree))
    tree$outcome.variable <- outcome.variable
    tree$original.text <- original.text
    tree$transformed.text <- transformed.text
    tree$print.table <- print.table
    return(tree)
}
