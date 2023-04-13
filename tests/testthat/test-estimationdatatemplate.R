context("Template")
food <- structure(
    c(19.55, 27.24, 28.76, 31.19, 21.91, 27.62, 29.46, 26.35, 26.69, 30.22, 27.81,
      23.46, 23.64, 27.85, 20.62, 25.35, 28, 23.49, 27.77, 18.48, 23.01, 22.66, 23.24,
      22.82, 17.94, 26.67, 28.98, 21.48, 14.47, 28.29, 27.97, 23.53, 26.39, 30.9,
      26.05, 23.27, 29.17, 30.93, 17.55, 32.55, 28.87, 26.53, 25.26, 25.65, 29.39,
      23.26, 24.77, 25.42, 23.65, 32.22, 18.86, 21.75, 23.07, 22.3, 27.04, 22.24,
      24.87, 30.85, 21.15, 16.47, 29.05, 26.99, 21.34, 29.94, 32.95, 29.4, 22.32,
      28.36, 28.49, 22.24, 36.15, 30.62, 26.53, 27.95, 31.49, 30.24, 24.8, 26.43,
      29.35, 21.15, 29.18, 21.6, 25.39, 22.26, 24.85, 24.56, 16.35, 22.96, 25.82,
      19.46, 23.6, 33.1, 27.13, 24.4, 25.88, 27.97, 24.54, 22.66, 28.94, 30.72, 16.7,
      30.27, 26.29, 22.33, 24.85, 24.33, 24.5, 22.67, 22.28, 23.8, 25.36, 29.5, 20.19,
      20.14, 21.09, 24.78, 24.74, 22.73, 21.08, 25.7, 19.79, 16.82, 31.15, 27.84,
      22.5, 23.1, 28.26, 25.55, 16.71, 27.88, 31.07, 23.44, 28.82, 27.77, 24.54, 24.55,
      27.78, 26.14, 23.44, 26.44, 27.47, 24.94, 29.68, 24.33, 25.42, 24.64, 22.78,
      26.5, 18.71, 22.86, 25.09, 19.72, 17.05, 30.91, 25.92, 21.32, 26.18, 25.93,
      28.61, 20.54, 26.44, 29.36, 19.77, 31.69, 24.64, 22.09, 23.42, 28.63, 26.3,
      22.89, 22.68, 30.92, 20.74, 27.24, 17.12, 23.63, 20.91, 23.49, 24.86, 16.28,
      21.52, 27.22, 17.41, 16.42, 28.22, 27.52),
    questiontype = "Number",
    name = "Food.Pinching.Effeciency",
    label = "Food.Pinching.Effeciency",
    question = "Food.Pinching.Effeciency"
)

chopsticks <- structure(
    factor(rep(c("180", "210", "240", "270", "300", "330"), each = 31L)),
    questiontype = "PickOne",
    name = "Chopstick.Length_2",
    label = "Chopstick.Length (categorical)",
    question = "Chopstick.Length (categorical)"
)

set.seed(12321)
other <- rnorm(length(food))
other.factor <- factor(rep(c("A", "B", "C"), each = 31L))

# Validate the estimation data template for all variables
test_that("CART produces template for simulator", {
    default.values <- list(
        food = min(food),
        chopsticks = levels(chopsticks)[1L],
        other = min(other),
        other.factor = levels(other.factor)[1L]
    )
    cart.args <- list(
        formula = food ~ chopsticks,
        early.stopping = FALSE,
        predictor.level.treatment = "Full labels",
        outcome.level.treatment = "Full labels"
    )
    expect_silent(model <- do.call(CART, cart.args))
    # Basic template with factor predictor, no shortening
    expected.template <- structure(
        list(
            food = list(
                type = "numeric",
                questiontype = "Number",
                name = "Food.Pinching.Effeciency",
                label = "Food.Pinching.Effeciency",
                question = "Food.Pinching.Effeciency",
                default.value = default.values[["food"]]
            ),
            chopsticks = list(
                type = "factor",
                questiontype = "PickOne",
                name = "Chopstick.Length_2",
                label = "Chopstick.Length (categorical)",
                question = "Chopstick.Length (categorical)",
                levels = c("180", "210", "240", "270", "300", "330"),
                observed.levels = c("180", "210", "240", "270", "300", "330"),
                has.unobserved.levels = FALSE,
                ordered = FALSE,
                default.value = default.values[["chopsticks"]],
                levels.shortened = FALSE
            )
        ),
        outcome.name = "food"
    )
    expect_equal(model[["estimation.data.template"]], expected.template)
    # Check consistent naming
    expect_equal(names(model[["estimation.data.template"]]), c("food", "chopsticks"))
    expect_equal(names(model[["estimation.data.template"]]),
                 names(model[["model"]]))
    # Make the outcome a factor
    cart.args.rev.form <- cart.args
    cart.args.rev.form[["formula"]] <- chopsticks ~ food
    expect_silent(model.rev <- do.call(CART, cart.args.rev.form))
    rev.expected.template <- rev(expected.template)
    attr(rev.expected.template, "outcome.name") <- "chopsticks"
    expect_equal(model.rev[["estimation.data.template"]], rev.expected.template)
    # Check consistent naming
    expect_equal(names(model.rev[["estimation.data.template"]]), c("chopsticks", "food"))
    expect_equal(names(model.rev[["estimation.data.template"]]),
                 names(model.rev[["model"]]))
    # With unobserved level
    original.chopsticks <- chopsticks
    levels(chopsticks) <- c(levels(chopsticks), "Foo")
    args.with.unobs.level <- cart.args
    args.with.unobs.level[["formula"]] <- food ~ chopsticks
    expected.warn <- paste0("Some categories do not appear in the data")
    expect_warning(model.with.warn <- do.call(CART, args.with.unobs.level),
                   expected.warn)
    expected.unobs.template <- expected.template
    expected.unobs.template[["chopsticks"]][["levels"]] <- levels(chopsticks)
    expected.unobs.template[["chopsticks"]][["observed.levels"]] <- levels(original.chopsticks)
    expected.unobs.template[["chopsticks"]][["has.unobserved.levels"]] <- TRUE
    expected.unobs.template[["chopsticks"]][["levels.shortened"]] <- FALSE
    expect_equal(model.with.warn[["estimation.data.template"]],
                 expected.unobs.template)
    # Check consistent naming
    expect_equal(names(model.with.warn[["estimation.data.template"]]), c("food", "chopsticks"))
    expect_equal(names(model.with.warn[["estimation.data.template"]]),
                 names(model.with.warn[["model"]]))
    # With shortened labels
    chopsticks <- original.chopsticks
    original.levels <- levels(chopsticks) # 180, 210, 240, 270, 300, 330
    new.levels <- c(`180` = "One Hundred and Eighty", `210` = "Two Hundred and Ten",
                    `240` = "Two Hundred and Forty", `270` = "Two Hundred and Seventy",
                    `300` = "Three Hundred", `330` = "Three Hundred and Thirty",
                    `Foo` = "Foo")
    last.level <- length(new.levels)
    raw.new.levels <- unname(new.levels)
    levels(chopsticks) <- raw.new.levels
    cart.args.short.preds <- cart.args
    cart.args.short.preds[["predictor.level.treatment"]] <- "Abbreviated labels"
    expect_warning(model.short.preds <- do.call(CART, cart.args.short.preds), expected.warn)
    expected.short.template <- expected.template
    expected.short.template[["chopsticks"]][["levels"]] <- raw.new.levels
    expected.short.template[["chopsticks"]][["observed.levels"]] <- raw.new.levels[-last.level]
    expected.short.template[["chopsticks"]][["has.unobserved.levels"]] <- TRUE
    expected.short.template[["chopsticks"]][["default.value"]] <- unname(new.levels[1L])
    expected.short.template[["chopsticks"]][["levels.shortened"]] <- TRUE
    short.levels <- getShortenedLevels(raw.new.levels)
    expected.short.template[["chopsticks"]][["short.levels"]] <- short.levels
    expected.short.template[["chopsticks"]][["observed.short.levels"]] <- short.levels[-last.level]
    # Check consistent naming
    expect_equal(names(model.short.preds[["estimation.data.template"]]), c("food", "chopsticks"))
    expect_equal(names(model.short.preds[["estimation.data.template"]]),
                 names(model.short.preds[["model"]]))
    expect_equal(model.short.preds[["estimation.data.template"]],
                 expected.short.template)
    chopsticks <- original.chopsticks
    # Add other predictors
    cart.args.other.preds <- cart.args
    cart.args.other.preds[["formula"]] <- food ~ chopsticks + other
    expect_silent(model.other.preds <- do.call(CART, cart.args.other.preds))
    expected.template.other <- expected.template
    expected.template.other[["other"]] <- list(type = "numeric")
    expected.template.other[["other"]][["default.value"]] <- default.values[["other"]]
    expect_equal(model.other.preds[["estimation.data.template"]], expected.template.other)
    # Check consistent naming
    expect_equal(names(model.other.preds[["estimation.data.template"]]), c("food", "chopsticks", "other"))
    expect_equal(names(model.other.preds[["estimation.data.template"]]),
                 names(model.other.preds[["model"]]))
    # Check order is preserved
    cart.args.other.preds[["formula"]] <- food ~ other + chopsticks
    expect_silent(model.other.preds <- do.call(CART, cart.args.other.preds))
    expected.template.other <- expected.template.other[c("food", "other", "chopsticks")]
    attr(expected.template.other, "outcome.name") <- "food"
    expect_equal(model.other.preds[["estimation.data.template"]], expected.template.other)
    # Check consistent naming
    expect_equal(names(model.other.preds[["estimation.data.template"]]), c("food", "other", "chopsticks"))
    expect_equal(names(model.other.preds[["estimation.data.template"]]),
                 names(model.other.preds[["model"]]))
})
