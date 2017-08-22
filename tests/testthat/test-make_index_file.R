context("make_index_file")

test_that("make_index_file works", {
  dir <- paste0(scopr_example_dir(), "/ethoscope_results/")
  out <- make_index_file(dir)
  ls <- list_result_files(dir)
  ls2 <- list_result_files(dir, index_file = "index.txt")
  expect_identical(ls2, ls)

})

