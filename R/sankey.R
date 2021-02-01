#' \code{treeFrameToList} Converts a \code{tree} into a format usable in
#' a sankeytree.
#'
#' @param frame The tree frame.
#' @param xlevels Levels of depedent variables that are factors.
#' @param model Data used by the model.
#' @param assigned The nodes that the cases have been assigned.
#' @param labels A vector of variable labels, named by the variable names.
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
#' @importFrom graphics hist
#' @importFrom hash has.key values hash clear
#' @importFrom flipFormat FormatAsReal FormatAsPercent
#' @importFrom colorspace diverge_hsv
#' @importFrom grDevices rgb rgb2hsv col2rgb hsv
#' @importFrom verbs Sum
#'
treeFrameToList <- function(frame, xlevels, model, assigned, labels, max.tooltip.length = 150,
                            numeric.distribution = TRUE, custom.color = "default", num.color.div = 101,
                            const.bin.size = TRUE)
{
    .terminalNode <- function(i) frame$var[i] == frame$var[nrow(frame)]

    tree.hash <- getNodeHash(xlevels)
    categoryLegend <- NULL
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
    nms = names(assigned)
    outcome.variable = model[,1]
    outcome.is.factor = is.factor(outcome.variable)
    outcome.name = names(model)[[1]]

    .getQColors <- function() {
        qColors <- c(rgb(237, 125, 49, maxColorValue = 255), # orange
                     rgb(91, 155, 213, maxColorValue = 255), # blue
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
        #node.descriptions = matrix(paste0(round(yprob*100),"% ", nms[col(yprob)]), ncol = length(nms))
        #node.descriptions <- apply(node.descriptions, 1, function(x) paste0(x,collapse = "<br>"))
        #node.descriptions <- paste0("<br>",node.descriptions)

        node.color <- rep("0", nrow(frame))
        l.na = Sum(is.na(custom.color), remove.missing = FALSE)
        l.col = Sum(.areColors(custom.color), remove.missing = FALSE)
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

        ymin <- min(frame$yval)
        ymax <- max(frame$yval)
        xmin <- min(outcome.variable, na.rm = TRUE)
        xmax <- max(outcome.variable, na.rm = TRUE)
        tree.type = "Regression"
        #node.mean = paste0("Mean(", outcome.name, ")", " = ", FormatAsReal(frame$yval, digits = 1), ":") # Mean
        #node.descriptions <- node.mean
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
            bins.breaks[1] = xmin - abs(xmin)/100
            bins.breaks[length(bins.breaks)] = xmax + xmax/100
            overall.distribution = hist(outcome.variable, breaks = bins.breaks, plot = FALSE)$counts
            overall.distribution = overall.distribution/Sum(overall.distribution, remove.missing = FALSE)

            for (i in 1:nrow(frame)) {
                this.node.values = nodes.distribution.temp[i,!is.na(nodes.distribution.temp[i,])]
                if (length(this.node.values) > 0){
                    nodes.hist = hist(this.node.values, breaks = bins.breaks, plot = FALSE)$counts
                    nodes.hist = nodes.hist/Sum(nodes.hist, remove.missing = FALSE)
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
        l.na = Sum(is.na(custom.color), remove.missing = FALSE)
        l.col = Sum(.areColors(custom.color), remove.missing = FALSE)
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
    #node.descriptions = paste("Description: ", node.descriptions)
    #node.tooltips = paste(node.tooltips, node.descriptions, sep = "<br>")

    root.name <- if (!is.null(labels)) unname(labels[outcome.name]) else outcome.name
    .constructNodeName <- function(node, i, i.parent, frame, tree.hash, model)
    {
        if (i == 1)
            return(root.name)
        features.hash <- tree.hash[[1]]
        xlevels.hash <- tree.hash[[2]]
        variable.name <- as.character(frame$var[i.parent])
        displayed.name <- if (!is.null(labels)) unname(labels[variable.name]) else variable.name
        node.names <- frame$splits[i.parent,]
        node.name <- ifelse(node %% 2 == 0, node.names[1], node.names[2])
        is.binary <- isBinary(model[[variable.name]])
        # the # character is used as an indicator to make the "Not" italic
        if (is.binary && (grepl("<", node.name) || grepl("a", node.name)))
            node.name <- paste0(displayed.name, ": ", "FALSE")
        else if (is.binary && (grepl(">", node.name) || grepl("b", node.name)))
            node.name <- paste0(displayed.name, ": ", "TRUE")
        else if (grepl("[<>]", node.name))
            node.name <- paste(displayed.name, node.name)
        else
        {
            node.str <- list(splitNodeText(node.name))
            nd.txt <- rep("",length(node.str[[1]]))
            # for each letter generated by the tree,e.g."a","b", find its corresponding output string
            for(m in 1:length(node.str[[1]]))
            {
                str <- node.str[[1]][m]
                feature.id <- values(features.hash, keys = as.character(variable.name))
                nd.txt[m] <- values(xlevels.hash[[feature.id]], keys = str)
            }
            node.name <- paste(nd.txt, collapse = ", ")
            node.name <- paste0(displayed.name, ": ", node.name)
        }
        #         if (.terminalNode(i))
        #             node.name <- paste0(node.name, terminal.description[i])
        node.name
    }
    # Function for creating a recursive list.
    .constructNodes <- function(node, nodes, frame, tree.hash, model) {
        parent.node <- floor(node / 2)
        i.parent <- match(parent.node, nodes)
        i <- match(node, nodes)
        if (outcome.is.factor) {
            result <- list(name = .constructNodeName(node, i, i.parent, frame, tree.hash, model),
                           n = frame$n[i], Percentage = FormatAsPercent(frame$n[i]/frame$n[1], digits = 1),
                           id = node, #Description = node.descriptions[i], tooltip = node.tooltips[i],
                           color = node.color[i],
                           nodeDistribution = yprob[i,], overallDistribution = yprob[1,], nodeVariables = nms,
                           terminalDescription = terminal.description[i])
        } else {
            result <- list(name = .constructNodeName(node, i, i.parent, frame, tree.hash, model), y = frame$yval[i], y0 = frame$yval[1],
                           n = frame$n[i], Percentage = FormatAsPercent(frame$n[i]/frame$n[1], digits = 1),
                           id = node, #Description = node.descriptions[i], tooltip = node.tooltips[i],
                           color = node.color[i],
                           nodeDistribution = nodes.distribution[[i]], overallDistribution = overall.distribution,
                           terminalDescription = terminal.description[i])
        }

        if((node * 2) %in% nodes) { # Adding child nodes, if they exist.
            result$children = vector("list", 2)
            for (branch in 1:2)
                result$children[[branch]] = .constructNodes(node * 2 + branch - 1, nodes, frame, tree.hash, model)
        }
        result
    }
    # Creating the recursive list.
    nodes <- as.numeric(dimnames(frame)[[1]])
    tree.list <- .constructNodes(1, nodes, frame, tree.hash, model)
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

isBinary <- function(vec)
{
    u <- sort(unique(vec))
    if (is.numeric(vec))
    {
        length(u) == 2 && all(u == 0:1)
    }
    else if (is.factor(vec))
    {
        v = levels(u)
        length(v) == 2 && all(v == c("FAL", "TRU"))
    }
    else if (length(u) == 2 && all(u == c(FALSE, TRUE))) {
        TRUE
    } else
        FALSE
}
