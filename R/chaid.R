#' Constructs a decision tree using the CHAID algorithm.
#'
#' The CHAID-algorithm has 5 steps. Somewhat counterintuitively, we start with
#' step 5.
#'
#' Step 1: calculates the cross-tabs for each predictor variable with the
#' outcome variable in turn and performs step 2 and step 3.
#' Step 2: Find the two predictors which show the least significant differing
#' crosstab with the outcome. If the p-value exceeds the critical value, the
#' categories are merged.
#' Step 3: For groups of 3 or more categories, split groups apart if the
#' significance of such a split would be above some threshold.
#' Step 4: computes the significance of each optimally grouped predictor. If the
#' significance of splitting using this predictor is above some threshold,
#' perform the split.
#' Step 5: Recursively construct the tree by applying steps 1 and 4 to each
#' unevaluated partition of the data, until no more partitions are formed (i.e.
#' the tree is complete).
#' @param formula A formula expression. The left-hand-side (response) should be
#'     a factor. The right-hand-side should be a series of factor variables
#'     separated by \code{+}.
#' @param data a data frame containing the variables in the model.
#' @param subset an optional vector specifying a subset of observations to be
#'     used in the fitting process.
#' @param weights an optional vector of weights to be used in the fitting
#'     process. Should be \code{NULL} or a numeric vector.
#' @param na.action a function which indicates what should happen when the data
#'     contains \code{NA}s. The default is \code{na.omit}.
#' @param control hyper parameters of the algorithm as returned by
#'     \code{\link{chaid_control}}.
#' @return An object of class \code{constparty}, see package
#'     \code{\link[partykit]{party}}.
#' @references G. V. Kass (1980). An Exploratory Technique for Investigating
#' Large Quantities of Categorical Data. \emph{Applied Statistics},
#' \bold{29}(2), 119--127.
#' @examples
#' data(adult.2000, package = "flipExampleData")
#' chaid(income ~ education + occupation + race, data = adult.2000)
#' @importFrom stats model.response model.weights terms
#' @importFrom partykit fitted_node
#' @export
chaid <- function(formula,
                  data,
                  subset,
                  weights,
                  na.action = na.omit,
                  control = chaid_control())
{
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights", "na.action"),
               names(mf),
               0)
    mf <- mf[c(1, m)]
    mf$drop.unused.levels <- FALSE
    # Changes mf so that it is a call to model.frame() instead of one to
    # chaid(), with the same arguments and construct a model frame using this
    # call
    mf[[1]] <- as.name("model.frame")
    # Evaluate the expression that constructs a model.frame out of our formula,
    # data, etc.
    m <- eval.parent(mf)
    # Like it says, extracts the LHS of the input formula (i.e. the response
    # variable). Note that this is a factor (is it always???)
    y <- model.response(m)
    # for passing to the chaid recursive function, use all the parameters that
    # aren't the weights (we'll pass those through separately)
    x <- m[, c(-1,-which(names(m) == "(weights)")), drop = FALSE]
    w <- model.weights(m)
    # Recursive construction of the tree with the CHAID algo
    chaidtree <- step5(1L, y, x, weights = w, ctrl = control)
    tree <- party(
        chaidtree,
        data = x,
        fitted = data.frame(
            "(fitted)" = fitted_node(chaidtree, data = x),
            "(response)" = y,
            check.names = FALSE
        ),
        terms = terms(formula, data = data)
    )
    if (!missing(weights))
        tree$fitted[["(weights)"]] <- w
    class(tree) <- c("constparty", "party")
    tree
}

#' Default hyperparameters for the CHAID algorithm.
#'
#' Overriden if the user specifies the 'control' parameter \code{\link{chaid}}.
#' @param alpha2 Level of significance used for merging of predictor categories
#'     (step 2).
#' @param alpha3 - If set to a positive value < 1, level of significance used
#'     for the the splitting of former merged categories of the predictor (step
#'     3). Otherwise, step 3 is omitted (the default).
#' @param alpha4 - Level of significance used for splitting of a node in the
#'     most significant predictor (step 5).
#' @param minsplit - Number of observations in splitted response at which no
#'     further split is desired.
#' @param minbucket - Minimum number of observations in terminal nodes.
#' @param minprob - Mininimum frequency of observations in terminal nodes. (What
#'     does this mean???)
#' @param stump - if TRUE, limits height to 1 (decision stump/1 rule learning)
#' @param maxheight - max height of tree. no limit if -1
chaid_control <- function(alpha2 = 0.05,
                          alpha3 = -1,
                          alpha4 = 0.05,
                          minsplit = 20,
                          minbucket = 7,
                          minprob = 0.01,
                          stump = FALSE,
                          maxheight = -1) {
    ret <- list(
        alpha2 = alpha2,
        alpha3 = alpha3,
        alpha4 = alpha4,
        minsplit = minsplit,
        minbucket = minbucket,
        minprob = minprob,
        stump = stump,
        maxheight = maxheight
    )
    class(ret) <- "chaid_control"
    return(ret)
}

#' Top-level recursive function for creating the CHAID tree
#'
#' Generates a root node for the CHAID tree generated for this data, and calls
#' itself recursively to generate the children of this node.
#' @param id ID number of the root node to be generated
#' @param response Outcome variable
#' @param x A \code{\link{model.frame}} containing formula and data
#' @param weights Optional. Number of times to count each data point. For data
#'     points that have been partitioned into a different node, this should be 0.
#'     If this is \code{NULL}, we assume all data points should be counted once.
#' @param indices Optional. List of lists, one per predictor variable,
#'     indicating which merged group each level/category in the predictor is a
#'     part of. \code{NULL} indicates that all predictors are not yet merged.
#' @param ctrl hyper parameters of the algorithm. See
#'     \code{\link{chaid_control}}.
#' @param height current height of the tree
#' @return an object of class partynode representing the root node of the tree
#' @importFrom partykit nodeids kidids_split
#' @noRd
step5 <- function(id = 1L,
                  response,
                  x,
                  weights = NULL,
                  indices = NULL,
                  ctrl = chaid_control(),
                  height = 0)
{
    # Weights are all 1 if not specified
    if (is.null(weights))
        weights <- rep.int(1, length(response))

    # Since this implementation treats weights as the number of times to repeat
    # data, the sum of the weights is the effective number of data points. Here,
    # we don't split the data further if we have less (effective) data points
    # than the threshold.
    if (sum(weights) < ctrl$minsplit)
        return(partynode(id = id))

    # Don't go deeper than one level if this is meant to be a stump
    if (ctrl$stump && id > 1)
        return(partynode(id = id))

    if (height == ctrl$maxheight)
        return(partynode(id = id))
    height <- height + 1

    # For each predictor, a list of indices describing the optimal merging of
    # categories
    indices <- step1(response, x, weights, indices = indices, ctrl)

    # The log p-values of splitting on each of the predictors (which have been
    # merged optimally)
    logpvals <- step4(response, x, weights, indices, ctrl)
    info <- list(adjpvals = exp(logpvals))

    # If most most significant split is still less significant than our
    # threshold, this is a nerminal node.
    if (exp(min(logpvals)) > ctrl$alpha4)
        return(partynode(id = id, info = info))

    # Otherwise, split using the predictor which gives us the most significant
    # difference between children
    sp <- partysplit(varid = which.min(logpvals),
                     index = as.integer(indices[[which.min(logpvals)]]))

    # For the children of this node, we reset the mergings of categories.
    # Because the childer
    newindices <- lapply(x, function(x){ 1:nlevels(x) })

    # Generate child nodes
    kidids <- kidids_split(sp, data = x)
    kids <- vector(mode = "list", length = max(sp$index))
    for (kidid in 1:max(sp$index)) {
        w <- weights
        w[kidids != kidid] <- 0
        # Give children unique ids amongst their siblings
        if (kidid > 1) {
            myid <- max(nodeids(kids[[kidid - 1]]))
        } else {
            myid <- id
        }
        # Recurse to create child nodes
        kids[[kidid]] <- step5(
            id = as.integer(myid + 1),
            response,
            x,
            weights = w,
            newindices,
            ctrl,
            height
        )
    }
    return(partynode(
        id = as.integer(id),
        split = sp,
        kids = kids,
        info = info
    ))
}

#' Optimally merge categories of all predictors
#'
#' Calculates a crosstab for each predictor variable with the outcome variable,
#' and uses these to  determine merged categories (steps 2 and 3) which have
#' similar values with respect to the outcome variable.
#' @param response Outcome variable
#' @param xvars A \code{\link{model.frame}} containing formula and data.
#' @param weights Optional. Number of times to count each data point. For data
#'     points that have been partitioned into a different node, this should be 0.
#'     If this is \code{NULL}, we assume all data points should be counted once.
#' @param indices Optional. List of lists, one per predictor variable,
#'     indicating which merged group each level/category in the predictor is a
#'     part of.
#' @param ctrl hyper parameters of the algorithm. See
#'     \code{\link{chaid_control}}.
#' @return a list of indices for each predictor, representing the optimal way to
#'     merge the levels/categories of this predictor.
#' @noRd
step1 <- function(response, xvars, weights, indices = NULL, ctrl) {
    ret <- vector(mode = "list", length = length(xvars))
    # Iterates over the predictors as factors and records the optimal grouping
    # of levels for each one.
    for (i in 1:length(xvars))
        ret[[i]] <- step1internal(response, xvars[[i]], weights, indices[[i]], ctrl)
    ret
}

#' Optimally merge categories of a predictor
#'
#' Given one of the predictors, tries to merge levels within the predictor so
#' that merged levels have similar values in the outcome variable#'
#' @return a list of group numbers, one per level of the predictor, representing
#'     the optimal way to group levels for merging.
#' @note this does not do the actual merging, we save that for when we've
#'     decided what the best thing to merge is.
#' @importFrom stats aggregate
#' @noRd
step1internal <- function(response, x, weights, index = NULL, ctrl)
{
    alpha2 <- ctrl$alpha2
    alpha3 <- ctrl$alpha3
    stopifnot(alpha2 > alpha3)
    # currently only supports factors (categorical data) as predictors
    stopifnot(is.factor(x))
    # By default, put each level in its own group
    if (is.null(index))
        index <- 1:nlevels(x)

    state <- NULL
    while (TRUE) {
        # there is nothing to do for two categories
        if (max(index) == 2) {
            break
        }

        # Find the best 2+ categories to merge into a new one
        result <- step2(response, x, weights, index, ctrl, state)
        mlev <- result$mlev
        state <- result$state

        # nothing to merge, return groupings untouched
        if (is.null(mlev))
            break()

        # is step 3 necessary?
        runstep3 <- sum(mlev[1] == index) > 1 ||
            sum(mlev[2] == index) > 1
        runstep3 <- runstep3 && (alpha3 > 0)

        # Merge levels by giving all grouped levels the same group number
        kati <- index %in% mlev
        index <- mergelevels(index, mlev)
        kat <- unique(index[kati])

        # perform step 3 if necessary:
        # This splits apart categories in a similar way to how they are merged
        # Kass (1980) notes that this is rarely used, but is needed for results
        # to be as close to optimal as possible
        if (runstep3) {
            index <- step3(x, response, weights, alpha3 = alpha3, index, kat)
            #state <- NULL
            if (is.ordered(x)) {
                if (!(all(diff(index) %in% c(0, 1)))) {
                    dum <- aggregate(index,
                                     by = list(index),
                                     FUN = length)
                    colnames(dum) <- c("index", "times")
                    dum$ind <- unique(index)

                    dum1 <- dum[, c("times", "ind")]
                    dum1 <- dum1[order(dum1$ind), ]

                    index <- rep(dum1$ind, dum1$times)
                }
            }
        }
    }
    attr(index, "state") <- state
    return(index)
}

#' For some predictor, find the best levels to merge into a new one
#'
#' We do this by comparing the crosstab of each variable with the outcome
#' variable to the crosstab of another variable with the outcome variable, and
#' testing for similarity.
#' @return an object representing the best merge to perform, and the current
#'     state of merging. \code{NULL} is no suitable merge could be found (all
#'     levels are too different to each other).
#' @importFrom stats xtabs
#' @noRd
step2 <- function(response,
                  x,
                  weights,
                  index = 1:nlevels(x),
                  ctrl,
                  state = NULL)
{
    stopifnot(is.factor(response))
    stopifnot(is.factor(x))
    if (nlevels(response[, drop = TRUE]) < 2)
        return(NULL)

    if (is.null(state)) {
        mergedx <- x
        if (nlevels(mergedx[, drop = TRUE]) < 3)
            return(NULL)
        xytab <- xtabs(weights ~ mergedx + response)
        logpmaxs <- matrix(NA, nrow = nrow(xytab), ncol = nrow(xytab))
    } else {
        xytab <- state$xytab
        mergedx <- state$mergedx
        logpmaxs <- state$logpmaxs
    }

    if (nlevels(mergedx) > 1) {
        comb <- switch(
            class(mergedx)[1],
            "factor" = lapply(1:(nlevels(mergedx) - 1), function(i)
                (i + 1):nlevels(mergedx)),
            "ordered" = lapply(1:(nlevels(mergedx) - 1), function(i)
                i + 1),
            stop("unknown class")
        )

        for (i in 1:length(comb)) {
            for (j in comb[[i]]) {
                if (is.na(logpmaxs[i, j])) {
                    X <- xytab[c(i, j),]

                    if (length(dim(X)) > 1) {
                        logpmaxs[i, j] <- logchisq.test(X)
                    } else {
                        logpmaxs[i, j] <- NA
                    }
                }
            }
        }

        logpmax <- max(logpmaxs, na.rm = TRUE)
        pos <- which.max(logpmaxs)
        levindx <-
            c(pos %% nrow(logpmaxs), as.integer(pos / nrow(logpmaxs)) + 1)

        # sample size stopping criteria
        nmin <- min(c(ceiling(ctrl$minprob * sum(weights)), ctrl$minbucket))

        if (exp(logpmax) > ctrl$alpha2 || any(rowSums(xytab) < nmin)) {
            xytab[min(levindx), ] <- colSums(xytab[levindx, ])
            mergedx[mergedx == rownames(xytab)[max(levindx)]] <-
                rownames(xytab)[min(levindx)]
            xytab <- xytab[-max(levindx), ]

            if (is.null(rownames(xytab))) {
                mergedx <- factor(mergedx,
                                  levels = 1,
                                  ordered = is.ordered(mergedx))
            } else {
                mergedx <- factor(mergedx,
                                  levels = rownames(xytab),
                                  ordered = is.ordered(mergedx))
            }

            logpmaxs[levindx, ] <- NA
            logpmaxs[, levindx] <- NA
            logpmaxs <- logpmaxs[-min(levindx),-max(levindx)]
            return(list(
                mlev = levindx,
                state = list(
                    xytab = xytab,
                    mergedx = mergedx,
                    logpmaxs = logpmaxs
                )
            ))
        }
    }
    return(NULL)
}

#' Splits two merged groups of levels within a predictor apart
#' @return the new mapping of levels to groups after the split
#' @noRd
step3 <- function(x, y, weights, alpha3 = 0.049, index, kat) {
    split_indx <- index
    if (sum(index == kat) > 2) {
        sp <- step3intern(x, y, weights, alpha3, index, kat)
        # compute minimum p-value and split
        if (!is.null(sp))
            split_indx <- splitlevels(index, level = kat, sp$split)
    }
    return(split_indx)
}

#' Performs the splitting described in step3
#' @importFrom stats xtabs
#' @noRd
step3intern <- function(x, y, weights, alpha3 = 0.05, index, kat) {
    ### determine all admissible combinations
    foo <- function(nll) {
        if (is.ordered(x)) {
            ret <- matrix(FALSE, ncol = nll, nrow = nll - 1)

            for (i in 1:(nll - 1)) {
                for (j in 1:(nll - 1))  {
                    if (i <= j) {
                        ret[j, i] <- TRUE
                    }
                }
            }
        }

        else{
            indl <- rep(FALSE, nll)
            indl[1] <- TRUE
            mi <- 2 ^ (nll - 1)
            ret <- matrix(FALSE, ncol = nll, nrow = mi - 1)
            for (i in 1:(mi - 1)) {
                ii <- i
                for (l in 1:(nll - 1)) {
                    indl[l] <- as.logical(ii %% 2)
                    ii <- ii %/% 2
                }
                ret[i, ] <- indl
            }
        }
        return(ret)
    }
    subsetx <- x %in% levels(x)[index == kat]
    ytmp <- y[subsetx, drop = TRUE]
    xtmp <- x[subsetx, drop = TRUE]
    wtmp <- weights[subsetx]
    xlev <- levels(xtmp)

    if (nlevels(xtmp) > 1) {
        ret <- foo(nlevels(xtmp))
        logp <- numeric(nrow(ret))
        for (i in 1:nrow(ret)) {
            tmpx <- as.factor(xtmp %in% xlev[ret[i,]])
            mat <- xtabs(wtmp ~ tmpx + ytmp)

            if (length(dim(mat)) > 1) {
                logp[i] <- logchisq.test(mat)
            } else {
                logp[i] <- NA
            }
        }
        logp_min <- min(logp)
        if (exp(logp_min) > alpha3)
            return(NULL)

        splitlev <- xlev[ret[which.min(logp), ]]

        return(list(
            logp = logp_min,
            split = which(levels(x) %in% splitlev)
        ))
    } else {
        return(NULL)
    }
}

#' Calculate the significance of splitting on each optimally merged predictor.
#' @return a list of log p-values, one for each predictor we could use to split
#'     the tree
#' @noRd
step4 <- function(response,
                  xvars,
                  weights,
                  indices,
                  ctrl,
                  states)
{
    states <- attr(indices, "state")
    p <- numeric(length(xvars))
    X2 <- rep(NA, length(xvars))
    for (i in 1:length(xvars)) {
        tmp <- step4internal(response, xvars[[i]], weights, indices[[i]], ctrl)
        p[i] <- tmp
        if (!is.null(attr(tmp, "Chisq")))
            X2[i] <- attr(tmp, "Chisq")
    }
    names(p) <- names(xvars)
    attr(p, "Chisq") <- X2
    return(p)
}

#' Calculates adjusted p-value for a particular way of splitting the tree
#' @noRd
step4internal <- function(response, x, weights, index, ctrl) {
    if (nlevels(response[, drop = TRUE]) < 2)
        return(0)
    state <- attr(index, "state")
    if (is.null(state))
        mx <- mergex(x, index)
    else
        mx <- state$mergedx

    nmin <-
        min(c(ceiling(ctrl$minprob * sum(weights)), ctrl$minbucket))
    if (any(table(mx[weights > 0]) < nmin))
        return(0)

    c_levels <- nlevels(x[weights > 0, drop = TRUE])
    r_levels <- nlevels(mx)

    if (is.null(state))
        xytab <- xtabs(weights ~ response + mx)
    else
        xytab <- state$xytab

    # NOTE: p-value is on log-scale
    if (length(dim(xytab)) > 1) {
        logp <- logchisq.test(xytab)
    } else{
        logp = 0
    }

    if (logp == 0)
        return(0)

    if (is.ordered(x)) {
        # formula (3.1) in Kass (1980)
        ret <- logp + lchoose(c_levels - 1, r_levels - 1)
    } else {
        i <- 0:(r_levels - 1) # formula (3.2)
        fact <- sum((-1) ^ i * ((r_levels - i) ^ c_levels) /
                        (factorial(i) * factorial(r_levels - i)))
        ret <- logp + log(fact)
    }
    attr(logp, "Chisq") <- attr(logp, "Chisq")
    return(ret)
}

#' Performs a (log) chi-squared test on a given crosstab
#' @return the log of the p-value and the chi-squared value for the crosstab
#' @importFrom stats chisq.test pchisq
#' @noRd
logchisq.test <- function(x) {
    cs <- colSums(x) > 0
    rs <- rowSums(x) > 0
    if (sum(cs) < 2 || sum(rs) < 2)
        return(0)
    if (min(x) < 10 && sum(x) < 100) {
        ctest <- chisq.test(
            x[rs, cs],
            correct = FALSE,
            simulate.p.value = TRUE,
            B = 9999
        )
        X2 <- ctest$statistic
        ret <- log(ctest$p.value)
    } else {
        suppressWarnings(ctest <- chisq.test(x[rs, cs], correct = FALSE))
        X2 <- ctest$statistic
        df <- ctest$parameter
        ret <- pchisq(X2,
                      df = df,
                      lower.tail = FALSE,
                      log.p = TRUE)
    }
    attr(ret, "Chisq") <- X2
    ret
}

#' merge two levels of a factor
#' @noRd
mergelevels <- function(index, merge) {
    stopifnot(length(unique(merge)) == 2)
    stopifnot(all(merge %in% index))

    ### which _original levels_ are to be merged
    ml <- index %in% merge
    ### which levels must be relabeled
    # WHY? Why do these need to be relabelled
    gr <- index > max(merge)

    ### merge levels
    index[ml] <- min(index[ml])
    ### relabel
    index[gr] <- index[gr] - 1

    return(index)
}

#' split a merged group into two groups
#' @noRd
splitlevels <- function(index, level, split) {
    stopifnot(sum(index == level) > length(split))

    # levels must be relabeled
    gr <- index > level
    # do the relabel
    index[gr] <- index[gr] + 1
    # record the groupings after the split
    index[split] <- level + 1

    return(index)
}

#' actually merge factor levels
#' @noRd
mergex <- function(x, index) {
    # extract levels and save observations as character
    lev <- levels(x)
    if (is.ordered(x))
        stopifnot(all(diff(index) %in% c(0, 1)))
    chrx <- as.character(x)
    newlev <- rep("", length(unique(index)))
    for (i in unique(index)) {
        indx <- index == i
        # assign merged levels to observations
        chrx[chrx %in% lev[indx]] <-
            paste(lev[indx], collapse = "+")
        # merge levels itself
        newlev[i] <- paste(lev[indx], collapse = "+")
    }
    return(factor(chrx, levels = newlev, ordered = is.ordered(x)))
}

#' check if two objects are identical and print differences otherwise
#' @noRd
isequal <- function(a, b) {
    attributes(a) <- NULL
    attributes(b) <- NULL
    if (!isTRUE(all.equal(a, b))) {
        print(a, digits = 10)
        print(b, digits = 10)
        return(FALSE)
    } else {
        return(TRUE)
    }
}
