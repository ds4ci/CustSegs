library(CustSegs)
library(flexclust)
context("Build Set of Random kcca Cluster Objects")

data("volunteers")
vol_ch <- volunteers[-(1:2)]
vol.mat <- as.matrix(vol_ch)
fc_cont <- new("flexclustControl")  ## flexclustControl object holds "hyperparameters"
fc_cont@tolerance <- 0.1   ## kcca only uses if classify == "weighted"
fc_cont@iter.max <- 30
fc_cont@verbose <- 0
fc_family <- "ejaccard"             ## distance metric

num_clusters <- 3
cli <- fc_rclust(vol.mat, num_clusters, fc_cont, fc_family, nrep = 20, seed = 1234)

test_that("fc_rclust generates expected output", {
  expect_equal(names(cli), c("best", "sizes", "peak_at", "tries"))
  expect_equal(cli$best$Size_1, c(1071, 1076, 1079, 1081, 1082, 1082, 1094, 1022, 1016,  935))
  expect_equal_to_reference(cli, file = "cli20.rda")
})
cat("\n *** Did scatter plot with 2D density get generated? *** \n")

