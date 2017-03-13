globalVariables(c(".weight.1232312", ".estimation.data"))

#' \code{CART} Creats a classification or regression tree.
#' @param formula A formula expression. The left-hand-side (response) should
#' be either a numerical vector when a regression tree will be fitted or
#' a factor, when a classification tree is produced. The right-hand-side should
#' be a series of numeric or factor variables separated by \code{+}; there should be
#' no interaction terms. Both \code{.} and \code{-} are allowed: regression trees can have
#' offset terms
#' @param data A data frame in which to preferentially interpret formula, weights and subset
#' @param subset An optional vector specifying a subset of observations to be used in the fitting process or
#' the name of a variable in \code{data}. It may not be an expression.
#' @param weights An optional vector of sampling weights or the name
#' of a variable in \code{data}. It may not be an expression.
#' @param output How the tree is represented: \code{"Sankey"}, \code{"Tree"}, \code{"Text"}, or \code{"Prediction-Accuracy Table"}.
#' @param missing How missing data is to be treated in the regression. Options are:
#' \code{"Error if missing data"}, \code{"Exclude cases with missing data"},
#' \code{"Use partial data"},and \code{"Imputation (replace missing values with estimates)"}.
#' @param method A character string giving the method to use. The only other useful value is "model.frame".
#' @param algorithm The algorithm used to generate the tree. Takes the values "tree", "rpart" and "party".
#' @param auxiliary.data A data frame containing additional variables to be used in imputation.
#' @param show.labels Shows the variable labels, as opposed to the names, in the outputs, where a
#' variables label is an attribute (e.g., attr(foo, "label")).
#' @param predictor.level.treatment How predictor factor labels are displayed: \code{"Letters"}, \code{"Abbreviated labels"} or \code{"Full labels"}.
#' @param outcome.level.treatment How outcome factor labels are displayed: \code{"Letters"}, \code{"Abbreviated labels"} or \code{"Full labels"}.
#' @param seed The random number seed.
#' @param ... Additional arguments that are passed to  \code{\link{tree}}
#' and \code{\link{tree.control}}. Normally used for mincut, minsize or mindev
#' @details Creates a \code{\link{tree}} and plots it as a \code{\link{SankeyTree}}
#' @importFrom flipData GetData CalibrateWeight
#' @importFrom flipData EstimationData
#' @importFrom flipFormat Labels
#' @importFrom flipRegression ConfusionMatrix
#' @importFrom flipU OutcomeName
#' @importFrom partykit glmtree lmtree mob
#' @importFrom rpart rpart
#' @importFrom tree tree
#' @importFrom stats na.exclude binomial predict as.formula
#' @importFrom colorspace diverge_hsv
#' @importFrom utils capture.output
#' @export

CART <- function(formula,
                 data = NULL,
                 subset = NULL,
                 weights = NULL,
                 output = "Sankey",
                 missing = "Use partial data",
                 method = "recursive.partition",
                 algorithm = "tree",
                 auxiliary.data = NULL,
                 show.labels = FALSE,
                 predictor.level.treatment = "Abbreviated labels",
                 outcome.level.treatment = "Full labels",
                 seed = 12321,
                 ...)
{
    cl <- match.call()
    .formula <- formula # Hack to work past scoping issues in car package: https://cran.r-project.org/web/packages/car/vignettes/embedding.pdf.
    subset.description <- try(deparse(substitute(subset)), silent = TRUE) #We don't know whether subset is a variable in the environment or in data.
    subset <- eval(substitute(subset), data, parent.frame())
    if (!is.null(subset))
    {
        if (is.null(subset.description) | (class(subset.description) == "try-error") | !is.null(attr(subset, "name")))
            subset.description <- Labels(subset)
        if (is.null(attr(subset, "name")))
            attr(subset, "name") <- subset.description
    }
    weights <- eval(substitute(weights), data, parent.frame())
    data <- GetData(.formula, data, auxiliary.data)

    # Get rid of escape characters in formula and data
    colnames(data) <- make.names(colnames(data))
    fstr <- paste(colnames(data)[1], paste(colnames(data)[-1], collapse=" + "), sep=" ~ ")
    formula <- as.formula(fstr)

    if (method == "model.frame")
        return(data)
    set.seed(seed)
    outcome.name <- OutcomeName(formula)
    data <- shortenFactorLevels(data, outcome.name, predictor.level.treatment, outcome.level.treatment)
    processed.data <- EstimationData(formula, data, subset, weights, missing)
    unfiltered.weights <- processed.data$unfiltered.weights
    estimation.data <- processed.data$estimation.data
    outcome.is.factor <- is.factor(estimation.data[[outcome.name]])

    if (algorithm == "tree")
    {
        if (is.null(weights))
            result <- tree(formula, data = estimation.data, model = FALSE, ...)
        else
        {
            weights <- CalibrateWeight(processed.data$weights)
            result <- do.call("tree", list(formula, data = estimation.data, weights = weights, model = FALSE, ...))
        }
        class(result) <- append("CART", class(result))
    }
    else if (algorithm == "rpart")
    {
        if (is.null(weights))
            result <- rpart(formula, data = estimation.data, model = FALSE, ...)
        else
        {
            weights <- CalibrateWeight(processed.data$weights)
            result <- do.call("rpart", list(formula, data = estimation.data, weights = weights, model = FALSE, ...))
        }
        class(result) <- append("CART", class(result))
    }
    else if (algorithm == "party")
    {
        if (is.null(weights))
        {
            if (outcome.is.factor)
                result <- glmtree(formula, data = estimation.data, na.action = na.exclude,
                                  family = binomial, inner = "estfun", terminal = "estfun", maxit=100)
            else
                result <- lmtree(formula, data = estimation.data, na.action = na.exclude)
        }
        else
        {
            weights <- CalibrateWeight(processed.data$weights)
            if (outcome.is.factor)
                result <- do.call("glmtree", list(formula, data = estimation.data, weights = weights,
                                  na.action = na.exclude, family = binomial, inner = "estfun",
                                  terminal = "estfun"))
            else
                result <- do.call("lmtree", list(formula, data = estimation.data, weights = weights,
                                 na.action = na.exclude))
        }

        # Un-remove levels with no cases
        if (outcome.is.factor)
        {
            outcome.var <- result$data[[outcome.name]]
            lvls <- levels(outcome.var)
            correct.lvls <- levels(data[[outcome.name]])
            n.lvls <- length(correct.lvls)
            num <- as.numeric(outcome.var)
            for (l in lvls)
                num[outcome.var == l] <- (1:n.lvls)[correct.lvls == l]
            result$data[[outcome.name]] <- factor(num, 1:n.lvls, labels = correct.lvls)
        }

        result$frame <- partyToTreeFrame(result)
        nds <- predict(result, newdata = data, type = "node")
        result$predicted <- result$frame$yval[nds]

        if (outcome.is.factor)
            result$probabilities <- result$frame$yprob[nds, ]

        result$nodetext <- paste(capture.output(result$node), collapse = "\n")
        result$node <- NULL

        class(result) <- "CART"
    }
    else
        stop(paste("Unhandled algorithm:", algorithm))

    result$input.data <- data
    result$model <- data           # duplicate for naming consistency with other fitted objects
    result$sample.description <- processed.data$description
    result$outcome.numeric <- !outcome.is.factor
    result$algorithm <- algorithm
    result$output <- output
    result$outcome.name <- outcome.name
    if (is.null(subset))
        subset <- rep(TRUE, nrow(data))
    result$subset <- subset

    if (result$show.labels <- show.labels)
    {
        result$labels <- Labels(data)
        result$outcome.label <- result$labels[match(outcome.name, names(data))]
    }
    else
        result$outcome.label <- outcome.name

    result$confusion <- ConfusionMatrix(result, subset, unfiltered.weights)

    return(result)
}

# This function generates hash tables to facilitate uniqueness checking and searching, etc.
# Returns a list where the first element is a hash table that maps variable names to an index
# in the second element which is a vector of hashes, which map letters to factor levels.
getNodeHash <- function(xlevels)
{
    xlevels.fac <- xlevels[!sapply(xlevels, is.null)] # strip null
    if (length(xlevels.fac) == 0)
        return(NULL)

    # make hash tables to search for names
    # check uniqueness
    features.hash = hash(keys = names(xlevels.fac), values = 1:length(xlevels.fac))
    xlevels.hash = c()
    for(node.texts in xlevels.fac) {
        # this approach will fail if more than 26 levels
        h = hash(keys = letters[1:length(node.texts)], values = node.texts)
        xlevels.hash = c(xlevels.hash, h)
    }
    result = list(features.hash, xlevels.hash)
}

getShortenedLevels <- function(lvls)
{
    .appendNum <- function(text, text.hash, c) {
        text1 <- paste0(text,c)
        if (has.key(text1, text.hash)) {
            text1 <- .appendNum(text, text.hash, c+1)
        }
        return(text1)
    }

    # replace all non alphanumeric letters
    lvls <- gsub("[^a-zA-Z0-9]", " ", lvls)

    # replace first letter of all words with upper case
    lvls <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", lvls, perl=TRUE)

    # get the first two or three letters of the words
    text.hash = hash()
    node.texts <- rep("",length(lvls))
    for (j in 1:length(lvls)) {
        text <- lvls[j]
        text.len <- sapply(gregexpr("[[:alpha:]]+", text), function(x) sum(x > 0)) # count number of words
        if (text.len == 1) {
            nchars <- nchar(text)
            node.text <- ifelse(nchars > 3, substr(text,1,3), text) # one word
        } else {
            text1 <- strsplit(text," ")[[1]]     # more than one word
            node.text <- rep("",length(text1))
            nchars <- nchar(text1)
            for(l in 1:length(nchars)) {
                node.text[l] = ifelse(nchars[l] > 2, substr(text1[l],1,2), text1[l])
            }
        }
        node.text <- paste(node.text, collapse = "")
        if (!has.key(node.text, text.hash)) {
            .set(text.hash, keys=node.text, values=TRUE)
        } else {
            node.text <- .appendNum(node.text, text.hash, 1)
        }
        node.texts[j] <- node.text
    }
    clear(text.hash)
    node.texts
}

shortenFactorLevels <- function(data, outcome.name, predictor.level.treatment, outcome.level.treatment)
{
    result <- data
    nms <- colnames(data)
    for (name in nms)
        if (is.factor(data[[name]]))
        {
            if (name == outcome.name)
            {
                if (outcome.level.treatment == "Abbreviated labels")
                    levels(result[[name]]) <- getShortenedLevels(levels(data[[name]]))
                else if (outcome.level.treatment == "Letters")
                    levels(result[[name]]) <- letters[seq(levels(data[[name]]))]
            }
            else
            {
                if (predictor.level.treatment == "Abbreviated labels")
                    levels(result[[name]]) <- getShortenedLevels(levels(data[[name]]))
                else if (predictor.level.treatment == "Letters")
                    levels(result[[name]]) <- letters[seq(levels(data[[name]]))]
            }
        }
    result
}

textTreeWithLabels <- function(text, labels, model, algorithm)
{
    result <- text
    if (algorithm == "tree")
    {
        if (!is.null(labels))
            for (i in seq(labels))
            {
                name <- names(labels[i])
                if (is.factor(model[[name]]))
                    result <- gsub(paste0(") ", name, ":"), paste0(") ", unname(labels[i]), ":"), result)
                else
                    result <- gsub(paste0(") ", name, " "), paste0(") ", unname(labels[i]), " "), result)
            }
    }
    else if (algorithm == "rpart")
    {
        if (!is.null(labels))
            for (i in seq(labels))
            {
                name <- names(labels[i])
                if (is.factor(model[[name]]))
                 result <- gsub(paste0(") ", name, "="), paste0(") ", unname(labels[i]), "="), result)
                else
                {
                    result <- gsub(paste0(") ", name, ">"), paste0(") ", unname(labels[i]), ">"), result)
                    result <- gsub(paste0(") ", name, "<"), paste0(") ", unname(labels[i]), "<"), result)
                }
            }
    }
    else if (algorithm == "party")
    {
        for (i in seq(model))
        {
            name <- colnames(model)[i]
            displayed.name <- if (!is.null(labels)) unname(labels[name]) else name
            result <- gsub(paste0("] V", i, " "), paste0("] ", displayed.name, " "), result)
        }
    }
    else
        stop(paste("Algorithm not handled:", algorithm))

    result
}

#' predict.CART
#'
#' Predicts values for numeric outcomes and group membership for categories based on \code{newdata}
#' and a fitted CART \code{object}.  A value (which may be NA) is returned for every instance
#' including those with missing data and for the fitted \code{data} before filtering in the case
#' that \code{newdata} is not specified.  NA is returned for cases with unfitted factor levels.
#' @param object A \code{CART} object.
#' @param seed A random number seed to ensure stability of predictions.
#' @param newdata Optionally, a data frame including the variables used to fit the model.
#' If omitted, the \code{data} supplied to \code{CART()} is used before any filtering.
#' @param ... Extra parameters. Currently not used.
#' @importFrom stats na.pass
#' @importFrom flipData CheckPredictionVariables
#' @export
predict.CART <- function(object, seed = 1232, newdata = object$input.data, ...)
{
    set.seed(seed)
    newdata = CheckPredictionVariables(object, newdata)

    if (object$algorithm == "tree")
    {
        class(object) <- "tree"
        if(object$outcome.numeric)
            predict(object, type = "vector", newdata = newdata)
        else
            predict(object, type = "class", newdata = newdata)
    }
    else if (object$algorithm == "rpart")
    {
        class(object) <- "rpart"
        if(object$outcome.numeric)
            predict(object, type = "vector", newdata = newdata, na.action = na.pass)
        else
            predict(object, type = "class", newdata = newdata, na.action = na.pass)
    }
    else if (object$algorithm == "party")
    {
        if(object$outcome.numeric)
            class(object) <- c("lmtree")
        else
            class(object) <- c("glmtree")
        #nds <- predict(object, type = "node", newdata = newdata, na.action = na.pass)
        #object$frame$yval[nds]
        object$predicted
    }
    else
        stop(paste("Algorithm not handled:", object$algorithm))
}

#' Probabilities.CART
#'
#' @param object The \code{CART} object whose values are to be predicted.
#' @importFrom stats na.pass
#' @export
Probabilities.CART <- function(object)
{
    if(object$outcome.numeric)
        stop("Probabilities not available for numeric dependent variables.")
    if (object$algorithm == "tree")
    {
        class(object) <- "tree"
        predict(object, type = "vector", newdata = object$input.data)
    }
    else if (object$algorithm == "rpart")
    {
        class(object) <- "rpart"
        m <- predict(object, type = "matrix", newdata = object$input.data, na.action = na.pass)
        lvls <- levels(object$input.data[[OutcomeName(object$terms)]])
        prob <- m[, (length(lvls) + 2):(2 * length(lvls) + 1)]
        colnames(prob) <- lvls
        prob
    }
    else if (object$algorithm == "party")
        object$probabilities
    else
        stop(paste("Algorithm not handled:", object$algorithm))
}

#' @importFrom graphics plot
#' @importFrom rhtmlSankeyTree SankeyTree
#' @export
print.CART <- function(x, ...)
{
    if (x$algorithm != "tree" && x$algorithm != "rpart" && x$algorithm != "party")
        stop(paste("Algorithm not handled:", x$algorithm))

    if (nrow(x$frame) == 1)
        stop("Output tree has one node and no splits. Change the inputs to produce a useful tree.")

    if (x$output == "Sankey")
    {
        tree.list <- if (x$algorithm == "tree")
            treeFrameToList(x$frame, attr(x, "xlevels"), x$model, x$where, x$labels)
        else if (x$algorithm == "rpart")
        {
            frame <- rPartToTreeFrame(x)
            treeFrameToList(frame, attr(x, "xlevels"), x$model, x$where, x$labels)
        }
        else if (x$algorithm == "party")
            treeFrameToList(x$frame, getXLevels(x), x$data, x$fitted[[1]], x$labels)
        plt <- SankeyTree(tree.list, value = "n", nodeHeight = 100, numeric.distribution = TRUE,
                          tooltip = "tooltip", treeColors = TRUE, terminalDescription = TRUE)
        print(plt)
    }
    else if (x$output == "Tree")
    {
        prty <- if (x$algorithm == "tree")
            treeFrameToParty(x$frame, attr(x, "xlevels"), x$model, x$terms, x$labels)
        else if (x$algorithm == "rpart")
        {
            frame <- rPartToTreeFrame(x)
            treeFrameToParty(frame, attr(x, "xlevels"), x$model, x$terms, x$labels)
        }
        else if (x$algorithm == "party")
            treeFrameToParty(x$frame, getXLevels(x), x$data, x$terms, x$labels)

        plot(prty, ip_args = list(id = FALSE), tp_args = list(id = FALSE, height = 3))
    }
    else if (x$output == "Text")
    {
        if (x$algorithm == "tree")
        {
            class(x) <- "tree"
            cat(textTreeWithLabels(paste(capture.output(x), collapse = "\n"), x$labels, x$model, x$algorithm))
        }
        else if (x$algorithm == "rpart")
        {
            class(x) <- "rpart"
            cat(textTreeWithLabels(paste(capture.output(x), collapse = "\n"), x$labels, x$model, x$algorithm))
        }
        else if (x$algorithm == "party")
            cat(textTreeWithLabels(x$nodetext, x$labels, x$data, x$algorithm))
    }
    else if (x$output == "Prediction-Accuracy Table")
    {
        print(x$confusion)
    }
        else
        stop(paste("Unhandled output: ", x$output))
}
