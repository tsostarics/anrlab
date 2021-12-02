test_that("split_instruments works", {
  instrument_db <- split_instruments(anrlab::test_data)
  expect_equal(instrument_db, anrlab:::test_insts)
})
