context("CART")

data("spam7", package = "DAAG")
spam.sample <- spam7[sample(seq(1,4601), 500, replace=FALSE), ]
data(cola, package = "flipExampleData")
colas <- cola
data(bank, package = "flipExampleData")
bank$fOverall <- factor(bank$Overall)

for (algo in c("tree", "rpart", "party"))
    test_that("saving variables",
              {

                    z <- CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, algorithm = algo)
                    expect_error(predict(z), NA)
                    expect_error(flipData::Probabilities(z))

                    z <- suppressWarnings(CART(fOverall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, algorithm = algo))
                    expect_error(predict(z), NA)
                    expect_error(flipData::Probabilities(z), NA)
              }
    )

z <- CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, algorithm = "tree")
test_that("tree prediction",
    {
        expect_equal(unname(predict(z)[1]), 4.044871794871795)
    })
z <- CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, algorithm = "rpart")
test_that("rpart prediction",
    {
        expect_equal(unname(predict(z)[1]), 4.258064516129032)
    })
z <- CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, algorithm = "party", seed = 1)
test_that("party prediction",
    {
        expect_equal(unname(predict(z)[1]), 3.977272727272728)
    })
z <- CART(fOverall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, algorithm = "tree")
test_that("tree Probabilities",
    {
        expect_equal(unname(flipData::Probabilities(z)[1, 4]), 0.4551282051282051)
    })
z <- CART(fOverall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, algorithm = "rpart")
test_that("tree Probabilities",
    {
        expect_equal(unname(flipData::Probabilities(z)[1, 4]), 0.2444444444444445)
    })
z <- CART(fOverall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, algorithm = "party", seed = 1)
test_that("tree Probabilities",
    {
        expect_equal(unname(flipData::Probabilities(z)[1, 4]), 0.2916666666666667)
    })

# Reading in the libraries so that their outputs do not polute the test results.
library(mice)
library(hot.deck)

test_that("Error if missing data",
{
    type = "Sankey"
    # Changing data
    expect_error((CART(yesno ~ crl.tot + dollar + bang + money + n000 + make, data = spam.sample, missing = "Error if missing data")),NA)
    colas$Q32[unclass(colas$Q32) == 1] <- NA
    expect_that((CART(Q32 ~ Q2, data = colas, subset = TRUE,  missing = "Error if missing data")), (throws_error()))
    expect_that((CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = TRUE,  weights = NULL, output = type, missing = "Error if missing data")), (throws_error()))
    # filter
    expect_that((CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100,  weights = NULL, output = type, missing = "Error if missing data")), (throws_error()))
    # weight
    expect_that((CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = TRUE,  weights = bank$ID, output = type, missing = "Error if missing data")), (throws_error()))
    # weight and filter
    expect_that((CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100,  weights = bank$ID, missing = "Error if missing")), (throws_error()))
})



for (missing in c("Exclude cases with missing data",
                  "Use partial data",
                  "Imputation (replace missing values with estimates)"))
    for (type in c("Sankey", "Tree", "Text"))
        for (algo in c("tree", "rpart", "party"))
            test_that(paste(missing, type, algo),
            {
                imputation <- missing == "Imputation (replace missing values with estimates)"
                expect_error((suppressWarnings(CART(yesno ~ crl.tot + dollar + bang + money + n000 + make, data = spam.sample, subset = TRUE,  weights = NULL, output = type, missing = missing, algorithm = algo))), if (imputation) NULL else NA)
                colas$Q32[unclass(colas$Q32) == 1] <- NA
                colas.small <- colas[, colnames(colas) %in% c("Q32", "Q3", "Q2", "Q4_A", "Q4_B", "Q4_C", "Q11", "Q12")]
                colas.small$Q3[1] <- NA
                expect_error((suppressWarnings(CART(Q32 ~ Q3, data = colas.small, subset = TRUE,  weights = NULL, output = type, missing = missing, algorithm = algo))), NA)
                expect_error((suppressWarnings(CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = TRUE,  weights = NULL, output = type, missing = missing, algorithm = algo))), NA)
                # filter
                expect_error((suppressWarnings(CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100,  weights = NULL, output = type, missing = missing, algorithm = algo))), NA)
                # weight
                expect_error((suppressWarnings(CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = TRUE,  weights = bank$ID, output = type, missing = missing, algorithm = algo))), NA)
                # weight and filter
                expect_error((suppressWarnings(CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100,  weights = bank$ID, output = type, missing = missing, algorithm = algo))), NA)
           })

