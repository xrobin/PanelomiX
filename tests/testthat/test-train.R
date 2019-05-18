test_that("exh.train works", {
	panels <- exh.train(aSAH, c("age", "s100b", "ndka"), "outcome", progress = FALSE)
	
	expect_is(attr(panels, "time.start.r"), "POSIXct")
	expect_is(attr(panels, "time.start.java"), "POSIXct")
	expect_is(attr(panels, "time.end"), "POSIXct")
	expect_is(attr(panels, "time"), "numeric")
	expect_gt(attr(panels, "iterations"), 12000)
	expect_lt(attr(panels, "iterations"), 14000)
	
	attr(panels, "time.start.r") <- NULL
	attr(panels, "time.start.java") <- NULL
	attr(panels, "time.end") <- NULL
	attr(panels, "time") <- NULL
	attr(panels, "iterations") <- NULL

	expect_equal(panels, expected_panels)
})
