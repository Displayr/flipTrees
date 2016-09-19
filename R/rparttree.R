#' @importFrom flipU OutcomeName
rPartToTreeFrame <- function(obj)
{
    frame <- obj$frame
    n.nodes <- nrow(frame)
    splits <- matrix("", n.nodes, 2)
    c <- 1
    for (i in 1:n.nodes)
    {
        var.name <- as.character(frame$var[i])
        if (var.name != "<leaf>")
        {
            if (is.factor(obj$model[[var.name]]))
            {
                if (ncol(obj$csplit) > length(letters))
                    stop("There are too many levels in the factor to be represented by letters.")
                lttrs <- letters[1:ncol(obj$csplit)]
                levels.split <- obj$csplit[c, ]
                splits[i, 1] <- paste0(":", paste(lttrs[levels.split == 1], collapse = ""))
                splits[i, 2] <- paste0(":", paste(lttrs[levels.split == 3], collapse = ""))
            }
            else
            {
                nms <- colnames(obj$splits)
                break.val <- obj$splits[c, nms == "index"]
                if (obj$splits[c, nms == "ncat"] < 0)
                {
                    splits[i, 1] <- paste0("<", break.val)
                    splits[i, 2] <- paste0(">", break.val)
                }
                else
                {
                    splits[i, 1] <- paste0(">", break.val)
                    splits[i, 2] <- paste0("<", break.val)
                }
            }
            c <- c + frame$ncompete[i] + frame$nsurrogate[i] + 1
        }
    }
    frame$splits <- splits

    outcome.var <- obj$model[[OutcomeName(obj$terms)]]

    if (is.factor(outcome.var))
        frame$yval <- levels(outcome.var)[frame$yval]

    if (!is.null(frame$yval2))
    {
        n.levels <- length(levels(outcome.var))
        yprob <- frame$yval2[, (n.levels + 2):(2 * n.levels + 1)]
        colnames(yprob) <- levels(outcome.var)
        frame$yprob <- yprob
    }
    frame
}
