library(CustSegs)
library(flexclust)
context("flexclust kcca object re-order")

data("volunteers")
vol_ch <- volunteers[-(1:2)]
vol.mat <- as.matrix(vol_ch)
fc_cont <- new("flexclustControl")  ## flexclustControl object holds "hyperparameters"
fc_cont@tolerance <- 0.1   ## kcca only uses if classify == "weighted"
fc_cont@iter.max <- 30
fc_cont@verbose <- 0
fc_family <- "ejaccard"             ## distance metric

#####
# check reorder on object already in order does nothing at al
##
num_clusters <- 3
fc_seed <- 577 ## magic seed to get result in order
set.seed(fc_seed)
vol.cl.A <- kcca(vol.mat, k = num_clusters, save.data = TRUE,
                 control = fc_cont, family = kccaFamily(fc_family))
summary(vol.cl.A)
sizes.A <- vol.cl.A@clusinfo[[1]]
vol.cl.A.re <- fc_reorder(vol.cl.A)
test_that("fc_reorder does not clobber already ordered kcca object", {
  expect_is(vol.cl.A.re, "kcca")
  expect_equal(sizes.A, c(1078, 258, 79))
  expect_identical(vol.cl.A, vol.cl.A.re)
})

#####
# check reorder on object that needs reordering
##
fc_seed <- 243 ## magic seed to get result needing reordering
set.seed(fc_seed)
vol.cl.B <- kcca(vol.mat, k = num_clusters, save.data = TRUE,
                 control = fc_cont, family = kccaFamily(fc_family))
summary(vol.cl.B)
sizes.B <- vol.cl.B@clusinfo[[1]]
vol.cl.B.re <- fc_reorder(vol.cl.B)
sizes.B.re <- vol.cl.B.re@clusinfo[[1]]
test_that("fc_reorder does in-fact reorder clusters within kcca object", {
  expect_is((vol.cl.B.re), "kcca")
  expect_equal(sizes.B, c(260, 1080, 75))
  expect_equal(sizes.B.re, c(1080, 260, 75))
  expect_equal_to_reference(vol.cl.B.re, file = "VolClBre.rds")
})






