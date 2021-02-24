library(collapsibleTree)
context("No Aggregate Function")

team <- Node$new("Lead Dev")
team$AddChild("Senior Dev")
team$AddChild("Junior Dev")
team$Set(hours = c(5, 50, 60))
print(team, "hours")

team$Do(function(x) x$parent_hours <- x$parent$hours, filterFun = isNotRoot)
team$parent_hours = 0

collapsibleTree(
  team,
  nodeSize = "hours",
  aggFun = NULL,
  tooltip = TRUE,
  attribute = "hours"
)



test_that("unlabelled root works for collapsibleTree", {
  wb <- collapsibleTree(warpbreaks, c("wool", "tension", "breaks"))
  expect_is(wb,"htmlwidget")
  expect_is(wb$x$data,"list")
  expect_is(wb$x$options$hierarchy,"character")
})

test_that("unlabelled root works for collapsibleTreeSummary", {
  wb <- collapsibleTreeSummary(warpbreaks, c("wool", "tension", "breaks"))
  expect_is(wb,"htmlwidget")
  expect_is(wb$x$data,"list")
  expect_is(wb$x$options$hierarchy,"character")
})

test_that("labeled root works for collapsibleTree", {
  wblabeled <- collapsibleTree(warpbreaks, c("wool", "tension", "breaks"), "a label")
  expect_is(wblabeled$x$data,"list")
  expect_is(wblabeled$x$options$hierarchy,"character")
})

test_that("labeled root works for collapsibleTreeSummary", {
  wblabeled <- collapsibleTreeSummary(warpbreaks, c("wool", "tension", "breaks"), "a label")
  expect_is(wblabeled$x$data,"list")
  expect_is(wblabeled$x$options$hierarchy,"character")
})
