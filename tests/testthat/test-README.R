context("repo README")

test_that("README works as expected", {
  skip_if_not(on_github_actions())
  
  root_path <- get_root_path()
  fp <- file.path(root_path, "README.Rmd")
  
  expect_message(output <- capture.output(krm <- knitr::knit(fp, output = "tmp.md")),
                 regexp = "output file: tmp.md")
  file.remove("tmp.md")
  
})