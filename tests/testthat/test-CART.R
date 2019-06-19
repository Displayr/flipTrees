context("CART")

data("spam7", package = "DAAG")
spam.sample <- spam7[sample(seq(1,4601), 500, replace=FALSE), ]
data(cola, package = "flipExampleData")
colas <- cola
data(bank, package = "flipExampleData")
bank$fOverall <- factor(bank$Overall)
levels(bank$fOverall) <- c(levels(bank$fOverall), "8")    # add an empty factor level

test_that("saving variables",
    {
        z <- CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank,
                  subset = bank$ID > 100)
        expect_error(predict(z), NA)
        expect_error(flipData::Probabilities(z))

        z <- suppressWarnings(CART(fOverall ~ Fees + Interest + Phone + Branch + Online + ATM,
                                   data = bank, subset = bank$ID > 100))
        expect_error(predict(z), NA)
        expect_error(flipData::Probabilities(z), NA)
    })


z <- CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100)
test_that("rpart prediction",
    {
        expect_equal(unname(predict(z)[1]), 4.258064516129032)
    })


z <- suppressWarnings(CART(fOverall ~ Fees + Interest + Phone + Branch + Online + ATM,
                           data = bank, subset = bank$ID > 100))
test_that("rpart Probabilities",
    {
        expect_equal(unname(flipData::Probabilities(z)[1, 4]), 0.2444444444444445)
    })

z <- suppressWarnings(CART(fOverall ~ Fees + Interest + Phone + Branch + Online + ATM,
                           data = bank, subset = bank$ID > 100))


# Reading in the libraries so that their outputs do not pollute the test results.
library(mice)
library(hot.deck)

test_that("Error if missing data",
{
    type = "Sankey"
    # Changing data
    expect_error((CART(yesno ~ crl.tot + dollar + bang + money + n000 + make,
                       data = spam.sample, missing = "Error if missing data")),NA)
    colas$Q32[unclass(colas$Q32) == 1] <- NA
    expect_that((CART(Q32 ~ Q2, data = colas, subset = TRUE,  missing = "Error if missing data")),
                (throws_error()))
    expect_that((CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM,
                      data = bank, subset = TRUE,  weights = NULL, output = type, missing = "Error if missing data")), (throws_error()))
    # filter
    expect_that((CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM,
                      data = bank, subset = bank$ID > 100,  weights = NULL, output = type, missing = "Error if missing data")), (throws_error()))
    # weight
    expect_that((CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM,
                      data = bank, subset = TRUE,  weights = bank$ID, output = type, missing = "Error if missing data")), (throws_error()))
    # weight and filter
    expect_that((CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM,
                      data = bank, subset = bank$ID > 100,  weights = bank$ID, missing = "Error if missing")), (throws_error()))
    # DS-1525, subset creates empty level of outcome
    expect_error(suppressWarnings(CART(Q32 ~ Q2 + Q3, data = colas, subset = colas$Q32 != "Don't know")), NA)
})


for (missing in c("Exclude cases with missing data",
                  "Use partial data",
                  "Imputation (replace missing values with estimates)"))
    for (type in c("Sankey", "Tree", "Text", "Prediction-Accuracy Table", "Cross Validation"))
        test_that(paste(missing, type),
        {
            imputation <- missing == "Imputation (replace missing values with estimates)"
            expect_error((z <- suppressWarnings(CART(yesno ~ crl.tot + dollar + bang + money + n000 + make,
                                                data = spam.sample, subset = TRUE,  weights = NULL,
                                                output = type, missing = missing))), NA)

            if (type == "Prediction-Accuracy Table")
                expect_equal(attr(z, "ChartData"), ExtractChartData(z$confusion))
            else if (type == "Cross Validation")
                expect_equal(attr(z, "ChartData"), z$cptable)
            else
                expect_is(attr(z, "ChartData"), "character")

            colas$Q32[unclass(colas$Q32) == 1] <- NA
            colas.small <- colas[, colnames(colas) %in% c("Q32", "Q3", "Q2", "Q4_A", "Q4_B", "Q4_C", "Q11", "Q12")]
            colas.small$Q3[1] <- NA
            expect_error((suppressWarnings(CART(Q32 ~ Q3, data = colas.small, subset = TRUE,
                                                weights = NULL, output = type, missing = missing))), NA)
            expect_error((suppressWarnings(CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM,
                                                data = bank, subset = TRUE,  weights = NULL, output = type, missing = missing))), NA)
            # filter
            expect_error((suppressWarnings(CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM,
                                                data = bank, subset = bank$ID > 100,  weights = NULL, output = type,
                                                missing = missing))), NA)
            # weight
            expect_error((suppressWarnings(CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM,
                                                data = bank, subset = TRUE,  weights = bank$ID, output = type,
                                                missing = missing))), NA)
            # weight and filter
            expect_error((suppressWarnings(CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM,
                                                data = bank, subset = bank$ID > 100,  weights = bank$ID,
                                                output = type, missing = missing))), NA)
})


for (pruning in c("None", "Minimum error", "Smallest tree"))
    for (stopping in c(TRUE, FALSE))
        test_that(paste(missing, type),
            {
            expect_error((suppressWarnings(CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank,
                                                subset = bank$ID > 100, weights = bank$ID,
                                                output = "Sankey", missing = "Exclude cases with missing data",
                                                prune = pruning, early.stopping = stopping))), NA)
})


test_that("CART: dot in formula", {
    cart <- CART(yesno ~ ., data = spam7)
    cart2 <- CART(yesno ~ crl.tot + dollar + bang + money + n000 + make, data = spam7)
    expect_equal(cart, cart2)
})

test_that("CART: many levels", {
    many.levels <- replicate(400, paste(sample(LETTERS, 2), collapse = ""))
    spam7$new <- as.factor(sample(many.levels, nrow(spam7), replace = TRUE))
    expect_error(CART(yesno ~ ., data = spam7, early.stopping = FALSE), NA)
})

food <- structure(c(19.55, 27.24, 28.76, 31.19, 21.91, 27.62, 29.46,
            26.35, 26.69, 30.22, 27.81, 23.46, 23.64, 27.85, 20.62, 25.35,
            28, 23.49, 27.77, 18.48, 23.01, 22.66, 23.24, 22.82, 17.94, 26.67,
            28.98, 21.48, 14.47, 28.29, 27.97, 23.53, 26.39, 30.9, 26.05,
            23.27, 29.17, 30.93, 17.55, 32.55, 28.87, 26.53, 25.26, 25.65,
            29.39, 23.26, 24.77, 25.42, 23.65, 32.22, 18.86, 21.75, 23.07,
            22.3, 27.04, 22.24, 24.87, 30.85, 21.15, 16.47, 29.05, 26.99,
            21.34, 29.94, 32.95, 29.4, 22.32, 28.36, 28.49, 22.24, 36.15,
            30.62, 26.53, 27.95, 31.49, 30.24, 24.8, 26.43, 29.35, 21.15,
            29.18, 21.6, 25.39, 22.26, 24.85, 24.56, 16.35, 22.96, 25.82,
            19.46, 23.6, 33.1, 27.13, 24.4, 25.88, 27.97, 24.54, 22.66, 28.94,
            30.72, 16.7, 30.27, 26.29, 22.33, 24.85, 24.33, 24.5, 22.67,
            22.28, 23.8, 25.36, 29.5, 20.19, 20.14, 21.09, 24.78, 24.74,
            22.73, 21.08, 25.7, 19.79, 16.82, 31.15, 27.84, 22.5, 23.1, 28.26,
            25.55, 16.71, 27.88, 31.07, 23.44, 28.82, 27.77, 24.54, 24.55,
            27.78, 26.14, 23.44, 26.44, 27.47, 24.94, 29.68, 24.33, 25.42,
            24.64, 22.78, 26.5, 18.71, 22.86, 25.09, 19.72, 17.05, 30.91,
            25.92, 21.32, 26.18, 25.93, 28.61, 20.54, 26.44, 29.36, 19.77,
            31.69, 24.64, 22.09, 23.42, 28.63, 26.3, 22.89, 22.68, 30.92,
            20.74, 27.24, 17.12, 23.63, 20.91, 23.49, 24.86, 16.28, 21.52,
            27.22, 17.41, 16.42, 28.22, 27.52), questiontype = "Number",
            name = "Food.Pinching.Effeciency", label = "Food.Pinching.Effeciency",
            question = "Food.Pinching.Effeciency")

chopsticks <- structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
            1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
            1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
            2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
            2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
            3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
            4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L,
            4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 5L,
            5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L,
            5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 6L, 6L,
            6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L,
            6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L),
            class = "factor", .Label = c("180", "210", "240", "270", "300", "330"),
            questiontype = "PickOne", name = "Chopstick.Length_2",
            label = "Chopstick.Length (categorical)", question = "Chopstick.Length (categorical)")

test_that("label abbrev",
{
    res.abbrv <- CART(food~chopsticks, early.stopping = FALSE)
    res.full <- CART(food~chopsticks, early.stopping = FALSE, predictor.level.treatment = "Full labels")
    expect_equal(dim(res.abbrv$splits), dim(res.full$splits))
})


data(adult.2000, package = "flipExampleData")
set.seed(1234)
adult.2000$race[runif(2000) > 0.9] <- NA
adult.2000$age[runif(2000) > 0.9] <- -Inf
adult.2000$hrs_per_week[runif(2000) > 0.9] <- Inf

test_that("Infinity allowed in predictors but not outcome", {
    expect_error(CART(age ~ sex + race + hrs_per_week + marital,
                            data = adult.2000,
                            missing = "Exclude cases with missing data"),
                 "Variable(s) age contain infinite values. Either recode the infinities to finite values or set them as missing data.",
                 fixed = TRUE)
    expect_error(CART(marital ~ sex + race + hrs_per_week + age,
                      data = adult.2000,
                      missing = "Exclude cases with missing data"), NA)
})


test_that("Many factor levels", {

    df <- data.frame(x = as.factor(rep_len(c("A", "B"), length.out = 100)),
                     y = as.factor(rep_len(c(seq(50)), length.out = 100)),
                     z = runif(100))
    expect_error(CART(x ~ y + z, data = df, long.running.calculations = FALSE),
                 "There are more than 30 categories in predictor variable(s) y.",
                 fixed = TRUE)
})

test_that("predict method works with old outputs; DS-2488",
{
    load("oldCART.rda")
    expect_error(predict(cart), NA)
})
