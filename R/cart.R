globalVariables(c(".weight.1232312", ".estimation.data"))

#' Creates a classification or regression tree.
#'
#' @param formula A formula expression. The left-hand-side (response)
#'     should be either a numerical vector when a regression tree will
#'     be fitted or a factor, when a classification tree is
#'     produced. The right-hand-side should be a series of numeric or
#'     factor variables separated by \code{+}; there should be no
#'     interaction terms. Both \code{.} and \code{-} are allowed:
#'     regression trees can have offset terms
#' @param data A data frame in which to preferentially interpret
#'     formula, weights and subset
#' @param subset An optional vector specifying a subset of
#'     observations to be used in the fitting process or the name of a
#'     variable in \code{data}. It may not be an expression.
#' @param weights An optional vector of sampling weights or the name
#'     of a variable in \code{data}. It may not be an expression.
#' @param output How the tree is represented: \code{"Sankey"},
#'     \code{"Tree"}, \code{"Text"},
#'     \code{"Prediction-Accuracy Table"} or
#'     \code{"Cross Validation"}.
#' @param missing How missing data is to be treated in the
#'     regression. Options are: \code{"Error if missing data"},
#'     \code{"Exclude cases with missing data"},
#'     \code{"Use partial data"},and
#'     \code{"Imputation (replace missing values with estimates)"}.
#' @param prune How to prune the tree according to the cross-validated
#'     error.  Options are: \code{"None"}, \code{"Minimum error"} and
#'     \code{"Smallest tree"}.
#' @param early.stopping Whether or not to stop building the tree
#'     early if splits are not decreasing the lack of fit
#'     sufficiently.
#' @param auxiliary.data A data frame containing additional variables
#'     to be used in imputation.
#' @param show.labels Shows the variable labels, as opposed to the
#'     names, in the outputs, where a variables label is an attribute
#'     (e.g., attr(foo, "label")).
#' @param predictor.level.treatment How predictor factor labels are
#'     displayed: \code{"Letters"}, \code{"Abbreviated labels"} or
#'     \code{"Full labels"}.
#' @param outcome.level.treatment How outcome factor labels are
#'     displayed: \code{"Letters"}, \code{"Abbreviated labels"} or
#'     \code{"Full labels"}.
#' @param seed The random number seed.
#' @param ... Other arguments to be supplied to \code{\link{rpart}}.
#'     Normally used for mincut,
#'     minsize or mindev
#' @details Creates an \code{\link[rpart]{rpart.object}} tree and plots it as a
#'     \code{\link{SankeyTree}}
#' @importFrom flipData GetData CalibrateWeight
#' @importFrom flipData EstimationData
#' @importFrom flipFormat Labels
#' @importFrom flipRegression ConfusionMatrix
#' @importFrom flipU OutcomeName
#' @importFrom rpart rpart rpart.control prune
#' @importFrom stats na.exclude binomial predict as.formula
#' @importFrom utils capture.output
#' @export

CART <- function(formula,
                 data = NULL,
                 subset = NULL,
                 weights = NULL,
                 output = "Sankey",
                 missing = "Use partial data",
                 prune = "None",
                 early.stopping = TRUE,
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

    set.seed(seed)
    outcome.name <- OutcomeName(formula)
    data <- shortenFactorLevels(data, outcome.name, predictor.level.treatment, outcome.level.treatment)
    processed.data <- EstimationData(formula, data, subset, weights, missing)
    unfiltered.weights <- processed.data$unfiltered.weights
    estimation.data <- processed.data$estimation.data
    outcome.is.factor <- is.factor(estimation.data[[outcome.name]])

    cp <- ifelse(early.stopping, 0.01, 0)
    control <- rpart.control(cp = cp)

    if (is.null(weights))
        result <- rpart(formula, data = estimation.data, model = FALSE, control = control, ...)
    else
    {
        weights <- CalibrateWeight(processed.data$weights)
        result <- do.call("rpart", list(formula, data = estimation.data, weights = weights,
                                        model = FALSE, control = control, ...))
    }

    if (prune == "Minimum error")
    {
        result <- prune(result, cp = result$cptable[which.min(result$cptable[, "xerror"]), "CP"])
    }
    else if (prune == "Smallest tree")
    {
        i.min.xerror <- which.min(result$cptable[, "xerror"])
        xerror.threshold <- result$cptable[i.min.xerror, "xerror"] + result$cptable[i.min.xerror, "xstd"]
        i <- 1
        while (result$cptable[i, "xerror"] > xerror.threshold)
        {
            i <- i + 1
        }
        result <- prune(result, cp = result$cptable[i, "CP"])
    }

    class(result) <- c("CART", "MachineLearning", class(result))

    result$missing <- missing
    result$model <- data
    result$sample.description <- processed.data$description
    result$outcome.numeric <- !outcome.is.factor
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
        extended.letters <- extendLetters(length(node.texts))
        lttrs <- extended.letters[1:length(node.texts)]
        h = hash(keys = lttrs[1:length(node.texts)], values = node.texts)
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
    lvls <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", lvls, perl = TRUE)

    # get the first two or three letters of the words
    text.hash = hash()
    node.texts <- rep("", length(lvls))
    for (j in 1:length(lvls)) {
        text <- lvls[j]
        text.len <- sapply(gregexpr("[[:alpha:]]+", text), function(x) sum(x > 0)) # count number of words
        if (text.len == 0) {
            node.text <- "X"
        } else if (text.len == 1) {
            nchars <- nchar(text)
            node.text <- ifelse(nchars > 3, substr(text, 1, 3), text) # one word
        } else {
            text1 <- strsplit(text," ")[[1]]     # more than one word
            node.text <- rep("",length(text1))
            nchars <- nchar(text1)
            for(l in 1:length(nchars)) {
                node.text[l] = ifelse(nchars[l] > 2, substr(text1[l], 1, 2), text1[l])
            }
        }
        node.text <- paste(node.text, collapse = "")
        if (!has.key(node.text, text.hash)) {
            text.hash[[node.text]] = TRUE
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

textTreeWithLabels <- function(text, labels, model)
{
    result <- text
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

    result
}

# extend letters to be of length at least n by suffixing with integers
extendLetters <- function(n) {
    if (n <= 26)
        return(letters)
    alphabets <- (n %/% 26) + 1
    extended.letters <- paste0(rep(letters, alphabets), rep(seq(alphabets), each = 26))
    return(extended.letters)
}


#' predict.CART
#'
#' Predicts values for numeric outcomes and group membership for categories based on \code{newdata}
#' and a fitted CART \code{object}.  A value (which may be NA) is returned for every instance
#' in \code{newdata} including those with missing data. NA is returned for cases with unfitted factor levels.
#' @param object A \code{CART} object.
#' @param seed A random number seed to ensure stability of predictions.
#' @param newdata Optionally, a data frame including the variables used to fit the model.
#' If omitted, the \code{data} supplied to \code{CART()} is used before any filtering.
#' @param ... Extra parameters. Currently not used.
#' @importFrom stats na.pass na.omit complete.cases
#' @importFrom flipData CheckPredictionVariables
#' @export
predict.CART <- function(object, seed = 1232, newdata = object$model, ...)
{
    set.seed(seed)
    newdata = CheckPredictionVariables(object, newdata)
    class(object) <- "rpart"
    type <- ifelse(object$outcome.numeric, "vector", "class")

    # If error or exclude for missing data then predict NA for cases with any missing data.
    # If partial or impute for missing data then allow rpart to predict for cases with missing data.
    if (object$missing == "Error if missing data" || object$missing == "Exclude cases with missing data") {
        newdata[complete.cases(newdata), "prediction"] <-
            predict(object, type = type, newdata = newdata[complete.cases(newdata), , drop = FALSE], na.action = na.omit)
        return(newdata$prediction)
    }
    else
        predict(object, type = type, newdata = newdata, na.action = na.pass)
}

#' Probabilities.CART
#'
#' @param object The \code{CART} object whose values are to be predicted.
#' @importFrom stats na.pass
#' @importFrom flipU OutcomeName
#' @export
Probabilities.CART <- function(object)
{
    if(object$outcome.numeric)
        stop("Probabilities not available for numeric dependent variables.")

    class(object) <- "rpart"
    m <- predict(object, type = "matrix", newdata = object$model, na.action = na.pass)

    outcome.variable <- object$model[[OutcomeName(object$terms)]]
    all.levels <- levels(outcome.variable)
    fitted.levels <- levels(droplevels(outcome.variable))
    empty.levels <- all.levels[!all.levels %in% fitted.levels]
    n.levels <- length(fitted.levels)

    prob <- m[, (n.levels + 2):(2 * n.levels + 1)]
    prob <- cbind(prob, matrix(0, nrow = nrow(prob), ncol = length(empty.levels)))
    colnames(prob) <- c(fitted.levels, empty.levels)
    prob
}

#' @importFrom graphics plot
#' @importFrom rhtmlSankeyTree SankeyTree
#' @importFrom rpart plotcp
#' @export
print.CART <- function(x, ...)
{
    if (nrow(x$frame) == 1)
        stop("Output tree has one node and no splits. Either change the input data or relax early stopping or pruning to produce a larger tree.")

    if (x$output == "Sankey")
    {
        frame <- rPartToTreeFrame(x)
        tree.list <- treeFrameToList(frame, attr(x, "xlevels"), x$model, x$where, x$labels)
        plt <- SankeyTree(tree.list, value = "n", nodeHeight = 100, numeric.distribution = TRUE,
                          tooltip = "tooltip", treeColors = TRUE, terminalDescription = TRUE)
        print(plt)
    }
    else if (x$output == "Tree")
    {
        frame <- rPartToTreeFrame(x)
        prty <- treeFrameToParty(frame, attr(x, "xlevels"), x$model, x$terms, x$labels)
        plot(prty, ip_args = list(id = FALSE), tp_args = list(id = FALSE, height = 3))
    }
    else if (x$output == "Text")
    {
        class(x) <- "rpart"
        cat(textTreeWithLabels(paste(capture.output(x), collapse = "\n"), x$labels, x$model))
    }
    else if (x$output == "Prediction-Accuracy Table")
    {
        print(x$confusion)
    }
    else if (x$output == "Cross Validation")
    {
        plotcp(x, col = 4)
    }
    else
        stop(paste("Unhandled output: ", x$output))
}
