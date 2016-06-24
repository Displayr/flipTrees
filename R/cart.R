globalVariables(c(".weight.1232312", ".estimation.data"))

#' \code{CART} Creats a classification or regression tree.
#' @param formula A formula expression. The left-hand-side (response) should
#' be either a numerical vector when a regression tree will be fitted or
#' a factor, when a classification tree is produced. The right-hand-side should
#' be a series of numeric or factor variables separated by \code{+}; there should be
#' no interaction terms. Both \code{.} and \code{-} are allowed: regression trees can have
#' offset terms
#' @param data A data frame in which to preferentially interpret formula, weights and subset
#' @param subset An optional vector specifying a subset of observations to be used in the fitting process, or,
#' the name of a variable in \code{data}. It may not be an expression.
#' \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or,
#' the name of a variable in \code{data}. It may not be an expression.
#' @param output How the tree is represented: \code{"Sankey"}, \code{"Tree"}, or \code{"Text"}.
#' @param missing How missing data is to be treated in the regression. Options are:
#' \code{"Error if missing data"}, \code{"Exclude cases with missing data"},
#' \code{"Use partial data"},and \code{"Imputation (replace missing values with estimates)"}.
#' @param method A character string giving the method to use. The only other useful value is "model.frame".
#' @param ... Additional arguments that are passed to  \code{\link{tree}}
#' and \code{\link{tree.control}}. Normally used for mincut, minsize or mindev
#'
#' @details Creates a \code{\link{tree}} and plots it as a \code{\link{SankeyTree}}
#' @importFrom flipData GetData
#' @importFrom flipData EstimationData
#' @importFrom tree tree
#' @importFrom stats na.exclude
#' @importFrom colorspace diverge_hsv
#' @export

CART <- function(formula,
                 data = NULL,
                 subset = NULL,
                 weights = NULL,
                 output = "Sankey",
                 missing = "Use partial data",
                 method = "recursive.partition",...)
{
    cl <- match.call()
    .formula <- formula # Hack to work past scoping issues in car package: https://cran.r-project.org/web/packages/car/vignettes/embedding.pdf.
    subset.description <- if (is.null(substitute(subset))) NULL else deparse(substitute(subset))
    subset <- eval(substitute(subset), data, parent.frame())
    if (!is.null(subset.description))
       attr(subset, "description") <- subset.description
    weights <- eval(substitute(weights), data, parent.frame())
    data <- GetData(.formula, data)
    if (method == "model.frame")
        return(data)
    mt <- attr(data, "terms")
    processed.data <- EstimationData(formula, data, subset, weights, missing)
    unfiltered.weights <- processed.data$unfiltered.weights
    estimation.data <- processed.data$estimation.data
    post.missing.data.estimation.sample <- processed.data$post.missing.data.estimation.sample
    estimation.subset  <- processed.data$estimation.subset
    subset <-  processed.data$subset
    if (is.null(weights))
        result <- tree(formula, data = estimation.data, model = TRUE, ...)
    else
    {
        assign(".weight.1232312", processed.data$weights, envir=.GlobalEnv)
        assign(".estimation.data", estimation.data, envir=.GlobalEnv)
        result <- tree(formula, data = .estimation.data, weights = .weight.1232312, model = TRUE, ...)
        remove(".weight.1232312",  envir=.GlobalEnv)
        remove(".estimation.data", envir=.GlobalEnv)
    }
    result$predicted <- predict(result, newdata = data, type = "tree", na.action = na.exclude)
    class(result) <- append("CART", class(result))
    result$output <- output
    return(result)
}

#' \code{treeFrameToList} Converts a \code{\link{tree}} into a format usable in
#' a sankeytree.
#'
#' @param tree The tree to convert.
#' @param max.tooltip.length The maximum length of the tooltip (determines the
#'   scale of the tree).
#' @param numeric.distribution Outputs additional diagnostics in the tooltip.
#' @param custom.color Determines the colors of tree branches; if \code{"default"}
#' generates tree colors using Q colors; if \code{"sankey"} use colors provided by sankeyTree package;
#' or can provide a vector of hex strings as RGB values, for example \code{custom.color = c("#aabbcc","#123456")}.
#' @param num.color.div positive integer in the range [2,inf]. Controls the
#'   color resolution of the tree. A higher value gives a smoother color
#'   transition.
#' @param const.bin.size logical; if \code{true}, each color spans an equal step
#'   of y-value or an equal number of points.
#' @importFrom stats quantile
#' @importFrom hash has.key .set values hash clear
#' @importFrom flipFormat FormatAsReal FormatAsPercent
#' @importFrom colorspace diverge_hsv
#' @importFrom grDevices rgb rgb2hsv col2rgb hsv
#'
treeFrameToList <- function(tree, max.tooltip.length = 150, numeric.distribution = TRUE,
                            custom.color = "default", num.color.div = 101, const.bin.size = TRUE)
{
    # Creating the names of a node from the frame.
    frame <- tree$frame
    attri <- attributes(tree)
    model <- tree$model
    assigned <- tree$where
    .terminalNode <- function(i) frame$var[i] == frame$var[nrow(frame)]

    # This function shortens categories to an abbreviation by spliting strings
    # and then generates hash tables to facilitate uniqueness checking and searching, etc.
    .getNodeHash <- function(tree.attri)
    {
        .appendNum <- function(text, text.hash, c) {
            text1 <- paste0(text,c)
            if (has.key(text1, text.hash)) {
                text1 <- .appendNum(text, text.hash, c+1)
            }
            return(text1)
        }
        xlevels <- tree.attri$xlevels
        xlevels.fac <- xlevels[!sapply(xlevels, is.null)] # strip null
        if (length(xlevels.fac) == 0)
            return(NULL)
        # replace all non alphanumeric letters
        xlevels.fac <- lapply(xlevels.fac, function(obj) gsub("[^a-zA-Z0-9]", " ", obj))
        # replace first letter of all words with upper case
        xlevels.fac <- lapply(xlevels.fac, function(obj) gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", obj, perl=TRUE))
        # get the first two or three letters of the words
        for (i in 1:length(xlevels.fac)) {
            text.hash = hash()
            node.texts <- rep("",length(xlevels.fac[[i]]))
            for (j in 1:length(xlevels.fac[[i]])) {
                text <- xlevels.fac[[i]][j]
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
            xlevels.fac[[i]] <- node.texts
        }
        # make hash tables to search for names
        # check uniqueness
        features.hash = hash(keys = names(xlevels.fac), values = 1:length(xlevels.fac))
        xlevels.hash = c()
        for(node.texts in xlevels.fac) {
            # this approach will fail if more than 26 levels
            if (length(node.texts) > 26) {
                hash.keys = c(letters, rep(0:(length(node.texts)-27)))
            } else {
                hash.keys = letters[1:length(node.texts)]
            }
            h = hash(keys = hash.keys,values = node.texts)
            xlevels.hash = c(xlevels.hash, h)
        }
        result = list(features.hash,xlevels.hash)
    }

    tree.hash <- .getNodeHash(attri)
    categoryLegend <- NULL
    xlevels <- attri$xlevels
    xlevels.fac <- xlevels[!sapply(xlevels, is.null)]
    if (length(xlevels.fac) != 0) {
        hash.l = hash::values(tree.hash[[1]])
        categoryLegend = rep("", length(hash.l))
        for (i in 1:length(hash.l)) {
            categoryShort = hash::values(tree.hash[[2]][[i]])
            categoryShort = paste(categoryShort, xlevels.fac[[i]], sep = ":")
            categoryShort = paste(categoryShort, collapse = "; ")
            categoryLegend[i] = paste0(names(xlevels.fac)[i], ": ", categoryShort)
        }
    }

    #.trim = function(string) ifelse(nchar(string) >  max.tooltip.length, paste(0,strtrim(string, max.tooltip.length),"..."), string)
    nms = names(tree$where)
    outcome.variable = tree$model[,1]
    outcome.is.factor = is.factor(outcome.variable)
    outcome.name = names(tree$model)[[1]]

    .getQColors <- function() {
        qColors <- c(rgb(91, 155, 213, maxColorValue = 255), # blue
                     rgb(237, 125, 49, maxColorValue = 255), # orange
                     rgb(30, 192, 0, maxColorValue = 255), # yelow
                     rgb(255, 35, 35, maxColorValue = 255),  # red
                     rgb(68, 114, 196, maxColorValue = 255), # darker blue
                     rgb(112, 173, 71, maxColorValue = 255), # green
                     rgb(37, 94, 145, maxColorValue = 255), # even darker blue
                     rgb(158, 72, 14, maxColorValue = 255), # blood
                     rgb(153, 115, 0, maxColorValue = 255), # brown
                     rgb(38, 68, 120, maxColorValue = 255), # very dark blue
                     rgb(67, 104, 43, maxColorValue = 255)) # darker green
        qColors
    }

    .desat <- function(col, diff, max.val) {
        col1 = rgb2hsv(col2rgb(col))
        col1[3] = 0.8 # gray scale value
        sat = diff / max.val
        col1[2] = col1[2] * sat # saturation
        hsv(col1[1], col1[2], col1[3])
    }

    .getNbins <- function(x, xmin, xmax) {
        unique.x = sort(unique(x))
        nbins = 1

        if (length(unique.x) == 0) {
            return(0)
        } else if (length(unique.x) > 1) {
            min.diff = .Machine$double.xmax
            for (i in 1:(length(unique.x)-1)) {
                min.diff = min(min.diff, unique.x[i+1] - unique.x[i])
            }
            granular = TRUE
            for (i in 1:(length(unique.x)-1)) {
                if (unique.x[i+1] - unique.x[i] %% min.diff > 0.0001) {
                    granular = FALSE
                    break
                }
            }

            if (granular && (xmax - xmin + 1) / min.diff <= 30) {
                nbins = ceiling((xmax - xmin + 1) / min.diff)
            } else {
                n.x = length(x)
                nbins = ceiling(3 + log10(n.x) * log(n.x, 2))
            }

        }

        return(nbins)
    }

    .areColors <- function(x) {
        sapply(x, function(X) {
            tryCatch(is.matrix(col2rgb(X)),
                     error = function(e) FALSE)
        })
    }

    if (outcome.is.factor)
    { # Classification tree.
        tree.type = "Classification"
        yprob = frame$yprob
        nms = colnames(yprob)
        colnames(yprob) = NULL
        node.descriptions = matrix(paste0(round(yprob*100),"% ", nms[col(yprob)]), ncol = length(nms))
        node.descriptions <- apply(node.descriptions, 1, function(x) paste0(x,collapse = "<br>"))
        node.descriptions <- paste0("<br>",node.descriptions)

        node.color <- rep("0", nrow(frame))
        l.na = sum(is.na(custom.color))
        l.col = sum(.areColors(custom.color))
        if (custom.color == "default" || (l.na == 0 && l.col == length(custom.color)))
        {
            if (custom.color == "default" || l.col < 2) {
                q.colors = .getQColors()
            } else {
                q.colors = custom.color
            }

            ncat = nlevels(outcome.variable)
            y.val.min = 1/ncat
            y.val.max = max(yprob) - y.val.min
            if (ncat <= length(q.colors)) {
                base.colors = q.colors
            } else {
                base.colors = c()
                for (i in 0:(ncat-1)) {
                    base.colors = c(base.colors, q.colors[(i %% length(q.colors)) + 1])
                }
            }

            for (i in 1:nrow(frame)) {
                y.idx <- which.max(yprob[i,])
                y.val = yprob[i, y.idx]
                y.col = base.colors[y.idx]
                if (y.val <= y.val.min)
                    y.val = y.val.min
                node.color[i] = .desat(y.col, y.val - y.val.min, y.val.max)
            }
        }
        else
        {
            for (i in 1:nrow(frame))
            {
                node.color[i] <- ""
            }
        }

        terminal.description <- paste("Highest =",frame$yval) # Change 1
    }
    else
    { # Regression tree.
        outcome.variable = as.numeric(outcome.variable)
        ymin <- min(frame$yval)
        ymax <- max(frame$yval)
        xmin <- min(outcome.variable)
        xmax <- max(outcome.variable)
        tree.type = "Regression"
        node.mean = paste0("Mean(", outcome.name, ")", " = ", FormatAsReal(frame$yval, digits = 1), ":") # Mean
        node.descriptions <- node.mean
        if (numeric.distribution)
        {
            nodes.distribution.temp = matrix(rep(NA, nrow(frame)*length(assigned)), nrow = nrow(frame))
            nodes.counts = rep(1, nrow(frame))
            for (i in 1:length(assigned)) {
                this.node = assigned[i]
                nodes.distribution.temp[this.node, nodes.counts[this.node]] = outcome.variable[i]
                nodes.counts[this.node] = nodes.counts[this.node] + 1
            }

            nodes.distribution = c()
            nbins = .getNbins(outcome.variable, xmin, xmax)
            bins.breaks = seq(xmin, xmax, (xmax-xmin)/nbins)
            bins.breaks[1] = xmin - xmin/100
            bins.breaks[length(bins.breaks)] = xmax + xmax/100
            overall.distribution = hist(outcome.variable, breaks = bins.breaks, plot = FALSE)$counts
            overall.distribution = overall.distribution/sum(overall.distribution)

            for (i in 1:nrow(frame)) {
                this.node.values = nodes.distribution.temp[i,!is.na(nodes.distribution.temp[i,])]
                if (length(this.node.values) > 0){
                    nodes.hist = hist(this.node.values, breaks = bins.breaks, plot = FALSE)$counts
                    nodes.hist = nodes.hist/sum(nodes.hist)
                } else {
                    nodes.hist = 0
                }
                nodes.distribution = c(nodes.distribution, list(nodes.hist))
            }

        } else {
            nodes.distribution = rep(NULL, nrow(frame))
        }



        eps <- 0.001 # error margin
        # if y is too small, scale it first!
        if (ymin < -eps && ymax > eps) {
            # constant bin size not avilable
            ysmall = frame$yval[frame$yval < -eps]
            ybig = frame$yval[frame$yval > eps]
            divisions = c(quantile(ysmall, seq(0, 1, 1/(num.color.div-1)*2)), quantile(ybig, seq(0, 1, 1/(num.color.div-1)*2)))
        }
        else
        {
            if (const.bin.size)
            {
                divisions = seq(ymin, ymax, by = (ymax - ymin)/num.color.div)
            }
            else
            {
                divisions = quantile(frame$yval, seq(0, 1, 1/num.color.div))
            }
        }

        node.color <- rep("0", nrow(frame))
        l.na = sum(is.na(custom.color))
        l.col = sum(.areColors(custom.color))
        if (custom.color == "default" || (l.na == 0 && l.col == length(custom.color)))
        {
            if (custom.color == "default" || l.col < 2) {
                base.colors = .getQColors()[c(5,4)]
            } else {
                base.colors = custom.color
            }

            if (num.color.div < 2) stop('number of colors for the tree cannot be < 2')
            if (num.color.div %% 2 == 0) num.color.div = num.color.div + 1
            base.colors = base.colors[1:2]
            hsv.base.colors = rgb2hsv(col2rgb(base.colors))
            # hcl.color <- rev(diverge_hcl(num.color.div,  h = c(260, 0), c = 100, l = c(50, 90)))
            hcl.color <- rev(diverge_hsv(num.color.div,  h = hsv.base.colors[1,]*360, s = 0.9, v = c(0.8, 0.6)))
            node.color[1] <- "#ccc"
            if (nrow(frame) > 1) {
                for (i in 2:nrow(frame))
                {
                    y <- frame$yval[i]
                    div.idx <- max(which(y >= divisions))
                    if (div.idx == length(divisions))
                    {
                        div.idx <- div.idx - 1
                    }
                    node.color[i] <- hcl.color[div.idx]
                }
            }

        }
        else
        {
            for (i in 1:nrow(frame))
            {
                node.color[i] <- ""
            }
        }

        terminal.description <- paste0("Mean = ",FormatAsReal(frame$yval)) # Change 2
    }

    ## create terminal description
    for (i in 1:nrow(frame)) {
        if (!.terminalNode(i))
            terminal.description[i] = "";
    }
    ## create tooltip
    ## check integer
    if (min(abs(c(frame$n %%1, frame$n %%1-1))) < 0.000001)
        node.tooltips = paste("n:", frame$n)
    else
        node.tooltips = paste("n:", FormatAsReal(frame$n, digits = 1))
    node.descriptions = paste("Description: ", node.descriptions)
    node.tooltips = paste(node.tooltips, node.descriptions, sep = "<br>")

    root.name <- outcome.name
    .constructNodeName <- function(node, i, i.parent, frame, tree.hash)
    {
        if (i == 1)
            return(root.name)
        features.hash <- tree.hash[[1]]
        xlevels.hash <- tree.hash[[2]]
        variable.name <- frame$var[i.parent]
        node.names <- frame$splits[i.parent,]
        node.name <- ifelse(node %% 2 == 0, node.names[1], node.names[2])

        if (grepl("<0.5", node.name))
        {   #Binary split (probably)
            node.name <- paste("Not", variable.name)
        }
        else if (grepl(">0.5", node.name))
        {
            node.name <- variable.name
        }
        else if (grepl("[<>]", node.name))
        {
            node.name <- paste(variable.name, node.name)
        }
        else
        {
            node.name <- sub(":", "", node.name)
            node.str <- strsplit(node.name,"") # split node string
            nd.txt <- rep("",length(node.str[[1]]))
            # for each letter generated by the tree,e.g."a","b", find its corresponding output string
            for(m in 1:length(node.str[[1]])){
                str <- node.str[[1]][m]
                feature.id <- values(features.hash, keys = as.character(variable.name))
                nd.txt[m] <- values(xlevels.hash[[feature.id]],keys = str)
            }
            node.name <- paste(nd.txt, collapse = " ")
            node.name <- paste0(variable.name, ": ", node.name)
        }
        #         if (.terminalNode(i))
        #             node.name <- paste0(node.name, terminal.description[i])
        node.name
    }
    # Function for creating a recursive list.
    .constructNodes <- function(node, nodes, frame, tree.hash) {
        parent.node <- floor(node / 2)
        i.parent <- match(parent.node, nodes)
        i <- match(node, nodes)
        if (outcome.is.factor) {
            result <- list(name = .constructNodeName(node, i, i.parent, frame, tree.hash),
                           n = frame$n[i], Percentage = FormatAsPercent(frame$n[i]/frame$n[1], digits = 1),
                           id = node, Description = node.descriptions[i],
                           tooltip = node.tooltips[i], color = node.color[i],
                           nodeDistribution = yprob[i,], overallDistribution = yprob[1,], nodeVariables = nms,
                           terminalDescription = terminal.description[i])
        } else {
            result <- list(name = .constructNodeName(node, i, i.parent, frame, tree.hash), y = frame$yval[i], y0 = frame$yval[1],
                           n = frame$n[i], Percentage = FormatAsPercent(frame$n[i]/frame$n[1], digits = 1),
                           id = node, Description = node.descriptions[i],
                           tooltip = node.tooltips[i], color = node.color[i],
                           nodeDistribution = nodes.distribution[[i]], overallDistribution = overall.distribution,
                           terminalDescription = terminal.description[i])
        }

        if((node * 2) %in% nodes) { # Adding child nodes, if they exist.
            result$children = vector("list", 2)
            for (branch in 1:2)
                result$children[[branch]] = .constructNodes(node * 2 + branch - 1, nodes, frame, tree.hash)
        }
        result
    }
    # Creating the recrusive list.
    nodes <- as.numeric(dimnames(frame)[[1]])
    tree.list <- .constructNodes(1, nodes, frame, tree.hash)
#     if (custom.color)
#     {
#         if (outcome.is.factor) {
#             tree.list <- c(list(hcl.color), list(paste0(seq(0,100,10), "%")), tree.list)
#         } else {
#             if (const.bin.size){
#                 tree.list <- c(list(hcl.color), list(FormatAsReal(seq(ymin, ymax,(ymax - ymin)/10),digits = 1)), tree.list)
#             } else {
#                 tree.list <- c(list(hcl.color), list(FormatAsReal(quantile(frame$yval, seq(0, 1, 1/10)),digits = 1)), tree.list)
#             }
#         }
#         names(tree.list)[1:2] = c("legendColor","legendText")
#     }

    if (!is.null(categoryLegend)) {
        tree.list <- c(list(categoryLegend), tree.list)
        names(tree.list)[1] <- "categoryLegend"
    }

    tree.list <- c(list(tree.type), tree.list)
    names(tree.list)[1] <- "treeType"
    tree.list
}

#' @importFrom stats predict
#' @importFrom graphics text
#' @export
predict.CART <- function(object, ...)
{
    object$predicted
}

#' @importFrom graphics plot
#' @importFrom rhtmlSankeyTree SankeyTree
#' @export
print.CART <- function(x, ...)
{
    if (x$output == "Sankey")
    {
        tree.list <- treeFrameToList(x, custom.color = "default")
        plt <- SankeyTree(tree.list, value = "n", nodeHeight = 100, numeric.distribution = TRUE,
                        tooltip = "tooltip", treeColors = TRUE, terminalDescription = TRUE)
        return(print(plt))
    }
    else if (x$output == "Tree")
    {
        plt <- plot(x)
        return(text(x))
    }
    class(x) <- "tree"
    print(x)
}

