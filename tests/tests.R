library(testthat)
library(devtools)

load_all()
testthat::test_that('data types', {
  expect_that(eq_clean_data(), is_a('tbl'))
  expect_that(eq_location_clean(), is_a('tbl'))
  expect_that(geom_timeline(), is_a('ggplot'))
  expect_that(geom_timeline_label(), is_a('ggplot'))
  expect_that(eq_map(), is_a('leaflet'))
  
})
