#' @importFrom partykit party
convertTreeToParty <- function(tree)
{
    tf <- tree$frame
    df <- data.frame()

    node.hash <- getNodeHash(attributes(tree))

    not.leaf <- tf$var != "<leaf>"
    n.splits <- sum(not.leaf)
    non.leaf.indices <- (1:nrow(tf))[not.leaf]
    var.names <- as.character(tf$var[non.leaf.indices])
    numeric.breaks <- rep(NA, n.splits)

    for (i in 1:n.splits)
    {
        v <- tree$model[[var.names[i]]]
        idx <- non.leaf.indices[i]
        left.text <- tf$splits[idx, 1]
        right.text <- tf$splits[idx, 2]

        if (is.numeric(v))
        {
            df[[i]] <- numeric(0)
            numeric.breaks[i] <- parseNumericSplitsText(left.text)
        }
        else if (is.factor(v))
        {
            levels.hash <- node.hash[[2]][[node.hash[[1]][[var.names[i]]]]]
            left.label <- factorSplitsLabel(left.text, levels.hash)
            right.label <- factorSplitsLabel(right.text, levels.hash)
            df[[i]] <- factor(NULL, levels = c(left.label, right.label))
        }
        else
            stop(paste0("Unhandled variable class: ", class(v)))
    }
    colnames(df) <- var.names

    c <- 1L
    split.c <- 1L
    nd <- getNode(c, split.c, tf, numeric.breaks)
    party(nd$node, df)
}

parseNumericSplitsText <- function(t)
{
    as.numeric(substr(t, 2, nchar(t)))
}

factorSplitsLabel <- function(t, levels.hash)
{
    labels <- character(length(t) - 1)
    for (i in 2:nchar(t))
    {
        ch <- substr(t, i, i)
        labels[i - 1] <- levels.hash[[ch]]
    }
    paste(labels, collapse = ",\n")
}

#' @importFrom partykit partynode partysplit
getNode <- function(c, split.c, tf, numeric.breaks)
{
    if (tf$var[c] == "<leaf>")
    {
        node <- partynode(c, info = paste0("Mean = ", FormatAsReal(tf$yval[c])))
        c <- c + 1L
        list(node = node, c = c, split.c = split.c)
    }
    else
    {
        br <- numeric.breaks[split.c]
        splt <- if (is.na(br))
            partysplit(split.c, index = 1:2)
        else
            partysplit(split.c, breaks = br)

        c <- c + 1L
        split.c <- split.c + 1L

        left.child <- getNode(c, split.c, tf, numeric.breaks)
        right.child <- getNode(left.child$c, left.child$split.c, tf, numeric.breaks)
        node <- partynode(right.child$c, split = splt, kids = list(left.child$node, right.child$node))
        list(node = node, c = right.child$c, split.c = right.child$split.c)
    }
}
