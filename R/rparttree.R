#' @importFrom flipU OutcomeName
#' @importFrom stats formula
rPartToTreeFrame <- function(obj)
{
    frame <- obj$frame
    n.nodes <- nrow(frame)
    splits <- matrix("", n.nodes, 2)
    nms <- colnames(obj$splits)
    index.i <- nms == "index"
    ncat.i <- nms == "ncat"
    c <- 1
    for (i in 1:n.nodes)
    {
        var.name <- as.character(frame$var[i])
        if (var.name != "<leaf>")
        {
            if (is.factor(obj$model[[var.name]]))
            {
                extended.letters <- extendLetters(length(levels(obj$model[[var.name]])))
                lttrs <- extended.letters[1:length(levels(obj$model[[var.name]]))]
                levels.split <- obj$csplit[obj$splits[c, index.i], ]
                splits[i, 1] <- paste0(":", paste(lttrs[levels.split == 1], collapse = ""))
                splits[i, 2] <- paste0(":", paste(lttrs[levels.split == 3], collapse = ""))
            }
            else
            {
                break.val <- obj$splits[c, index.i]
                if (obj$splits[c, ncat.i] < 0)
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

    outcome.var <- obj$model[[OutcomeName(formula(obj$terms))]]

    if (is.factor(outcome.var))
        frame$yval <- levels(outcome.var)[frame$yval]

    if (!is.null(frame$yval2))
    {
        fitted.levels <- levels(droplevels(outcome.var[obj$subset]))
        all.levels <- levels(outcome.var)
        n.levels <- length(fitted.levels)
        yprob <- frame$yval2[, (n.levels + 2):(2 * n.levels + 1)]
        empty.levels <- all.levels[!all.levels %in% fitted.levels]
        yprob <- cbind(yprob, matrix(0, nrow = nrow(yprob), ncol = length(empty.levels)))
        colnames(yprob) <- c(fitted.levels, empty.levels)
        frame$yprob <- yprob
    }
    frame
}
