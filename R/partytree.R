#' @importFrom partykit party
#' @importFrom flipU OutcomeName
#' @importFrom stats formula
treeFrameToParty <- function(frame, xlevels, model, terms, labels)
{
    df <- data.frame()

    node.hash <- getNodeHash(xlevels)

    not.leaf <- frame$var != "<leaf>"
    n.splits <- sum(not.leaf)
    non.leaf.indices <- (1:nrow(frame))[not.leaf]
    var.names <- as.character(frame$var[non.leaf.indices])
    numeric.breaks <- rep(NA, n.splits)
    numeric.breaks.reversed <- logical(n.splits)

    for (i in 1:n.splits)
    {
        v <- model[[var.names[i]]]
        idx <- non.leaf.indices[i]
        left.text <- frame$splits[idx, 1]
        right.text <- frame$splits[idx, 2]

        if (is.numeric(v))
        {
            df[[i]] <- numeric(0)
            numeric.breaks[i] <- parseNumericSplitsText(left.text)
            numeric.breaks.reversed[i] <- grepl(">", left.text)
        }
        else if (is.factor(v))
        {
            levels.hash <- node.hash[[2]][[node.hash[[1]][[var.names[i]]]]]
            left.label <- factorSplitsLabel(left.text, levels.hash)
            right.label <- factorSplitsLabel(right.text, levels.hash)
            df[[i]] <- factor(NULL, levels = c(left.label, right.label))
        }
        else if (is.logical(v))
            df[[i]] <- factor(NULL, levels = c("FALSE", "TRUE"))
        else
            stop(paste0("Unhandled variable class: ", class(v)))
    }
    colnames(df) <- sapply(var.names, function(name) {
        truncateLabel(if (!is.null(labels)) unname(labels[name]) else name)
    })

    yval <- frame$yval
    outcome.name <- truncateLabel(if (!is.null(labels)) unname(labels[OutcomeName(formula(terms))])
                                  else OutcomeName(formula(terms)), 10)

    nd <- getPartyNodes(1L, 1L, not.leaf, yval, numeric.breaks, numeric.breaks.reversed, outcome.name)
    party(nd$node, df)
}

parseNumericSplitsText <- function(t)
{
    num <- as.numeric(trimws(gsub("[<>=]", "", t)))
    if (is.na(num))
        stop(paste("The following could not be parsed:", t))
    num
}

factorSplitsLabel <- function(t, levels.hash)
{
    labels <- character(nchar(t))
    prev <- NULL

    for (i in 2:nchar(t))
    {
        c <- substr(t, i, i)
        if (grepl("[[:lower:]]", c))
        {
            if (!is.null(prev))
            {
                ch <- substr(t, prev, i - 1)
                labels[i - 1] <- levels.hash[[ch]]
            }
            prev <- i
        }
    }
    labels[nchar(t)] <- levels.hash[[substr(t, prev, nchar(t))]]

    paste(labels, collapse = ", ")
}

#' @importFrom partykit partynode partysplit
getPartyNodes <- function(c, split.c, not.leaf, yval, numeric.breaks, numeric.breaks.reversed, outcome.name)
{
    if (not.leaf[c])
    {
        br <- numeric.breaks[split.c]
        is.reversed <- numeric.breaks.reversed[split.c]
        splt <- if (is.na(br))
            partysplit(split.c, index = 1:2)
        else
            partysplit(split.c, breaks = br)

        c <- c + 1L
        split.c <- split.c + 1L

        left.child <- getPartyNodes(c, split.c, not.leaf, yval, numeric.breaks, numeric.breaks.reversed,
                              outcome.name)
        right.child <- getPartyNodes(left.child$c, left.child$split.c, not.leaf, yval, numeric.breaks,
                               numeric.breaks.reversed, outcome.name)
        kids <- if (is.reversed)
            list(right.child$node, left.child$node)
        else
            list(left.child$node, right.child$node)
        node <- partynode(right.child$c, split = splt, kids = kids)
        list(node = node, c = right.child$c, split.c = right.child$split.c)
    }
    else
    {
        info <- if (is.numeric(yval))
            paste0(outcome.name, ":\n", FormatAsReal(yval[c]), "\n")
        else
            paste0(outcome.name, ":\n", truncateLabel(as.character(yval[c])), "\n")
        node <- partynode(c, info = info)
        c <- c + 1L
        list(node = node, c = c, split.c = split.c)
    }
}

truncateLabel <- function(label, truncation.length = 20)
{
    if (nchar(label) > truncation.length)
        paste0(substr(label, 1, truncation.length - 2), "...")
    else
        label
}

extractNodeInfo <- function(node.list, node, outcome.var, row.i, var.names, node.i)
{
    result <- list()
    result[["isleaf"]] <- is.null(node$split)
    result[["node"]] <- node.i

    if (!result[["isleaf"]])
    {
        result[["var"]] <- var.names[node$split$varid]
        result[["varid"]] <- node$split$varid
        s <- node$split
        if (!is.null(s$breaks))
            result[["breaks"]] <-  s$breaks
        else if (!is.null(s$index))
            result[["index"]] <- s$index
        else
            stop(paste("Unhandled split", s))
    }

    if (is.factor(outcome.var))
    {
        node.row.i <- as.integer(row.names(node$info$estfun))
        result[["yprob"]] <- computeYProb(outcome.var, row.i, node.row.i)
        lvls <- levels(outcome.var)
        result[["yval"]] <- factor(lvls[which.max(result[["yprob"]])], lvls)
    }
    else if (is.numeric(outcome.var))
        result[["yval"]] <- unname(node$info$coefficients)
    else
        stop(paste("Outcome class not handled:", class(outcome.var)))

    result[["n"]] <- unname(node$info$nobs)

    node.list[[length(node.list) + 1]] <- result

    if (!is.null(node$kids))
    {
        node.list <- extractNodeInfo(node.list, node$kids[[1]], outcome.var, row.i, var.names, node.i * 2)
        node.list <- extractNodeInfo(node.list, node$kids[[2]], outcome.var, row.i, var.names, node.i * 2 + 1)
    }

    node.list
}

computeYProb <- function(outcome.var, row.i, node.row.i)
{
    lvls <- levels(outcome.var)
    yfreq <- numeric(length(lvls))
    c <- 1
    for (i in 1:length(row.i))
    {
        if (node.row.i[c] == row.i[i])
        {
            j <- (1:length(lvls))[outcome.var[i] == lvls]
            yfreq[j] <- yfreq[j] + 1
            if (c == length(node.row.i))
                break
            else
                c <- c + 1
        }
    }
    yprob <- prop.table(yfreq)
    names(yprob) <- lvls
    yprob
}

#' @importFrom flipU OutcomeName
partyToTreeFrame <- function(obj)
{
    outcome.var <- obj$data[[OutcomeName(obj$terms)]]
    row.i <- as.integer(row.names(obj$data))
    node.list <- extractNodeInfo(list(), obj$node, outcome.var, row.i, colnames(obj$data), 1)
    n.nodes <- length(node.list)
    var.names <- unlist(lapply(node.list, function(x) {
        if (!is.null(x$var))
            x$var
        else
            "<leaf>"
    }))
    n <- unlist(lapply(node.list, function(x) x$n))
    yval <- unlist(lapply(node.list, function(x) x$yval))
    splits <- matrix("", n.nodes, 2)
    for (i in 1:n.nodes)
    {
        nd <- node.list[[i]]
        if (!is.null(nd$breaks))
        {
            splits[i, 1] <- paste0(" <= " , nd$breaks)
            splits[i, 2] <- paste0(" > " , nd$breaks)
        }
        else if (!is.null(nd$index))
        {
            idx <- nd$index
            if (length(idx) > length(letters))
                stop("There are too many levels in the factor to be represented by letters.")
            lttrs <- letters[1:length(idx)]
            splits[i, 1] <- paste0(":", paste(lttrs[!is.na(idx) & idx == 1], collapse = ""))
            splits[i, 2] <- paste0(":", paste(lttrs[!is.na(idx) & idx == 2], collapse = ""))
        }
    }

    frame <- data.frame(var = var.names, n = n, yval = yval, splits = rep(0, length(var.names)))
    frame$splits <- splits # the matrix will be split into columns if we assign it to frame in the line above

    outcome.is.factor <- !is.null(node.list[[1]]$yprob)
    if (outcome.is.factor)
    {
        yprob <- matrix(NA, n.nodes, length(node.list[[1]]$yprob))
        for (i in 1:n.nodes)
            yprob[i, ] <- node.list[[i]]$yprob
        colnames(yprob) <- names(node.list[[1]]$yprob)
        frame$yprob <- yprob
    }
    rownames(frame) <- as.character(unlist(lapply(node.list, function(x) x$node)))
    frame
}

getXLevels <- function(obj)
{
    var.names <- attr(obj$terms, "term.labels")
    result <- vector("list", length(var.names))
    names(result) <- var.names
    for (nm in var.names)
        if (is.factor(obj$data[[nm]]))
            result[[nm]] <- levels(obj$data[[nm]])
    result
}
