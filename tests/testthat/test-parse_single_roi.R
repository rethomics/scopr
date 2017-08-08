context("parse_single_roi")

test_that("...", {
  dir <- scopr_example_dir()
  test_file <- paste(dir, "ethoscope_results/029/E_029/2016-01-25_21-14-55/2016-01-25_21-14-55_029.db",sep="/")
  data <- data.table::data.table(region_id=1, experiment_id="test", path=test_file)
  a <- scopr:::parse_single_roi(data)
  a <- scopr:::parse_single_roi(data, FUN= function(x){behavr::bin_apply_all(x,y = x)})
  a[meta=T]
})

