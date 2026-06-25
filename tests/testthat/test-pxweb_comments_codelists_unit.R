context("pxweb_comments_codelists_unit")

pxweb_test_v1_commented_data <- function() {
  pxweb_data(list(
    columns = list(
      list(code = "Region", text = "region", type = "c", comment = "Region column note"),
      list(code = "Tid", text = "year", type = "t"),
      list(code = "Value", text = "population", type = "d")
    ),
    comments = list(
      list(variable = "Region", value = "03", comment = "Uppsala value note")
    ),
    data = list(
      list(key = list("01", "2024"), values = list("10")),
      list(key = list("03", "2024"), values = list("20"), comment = "Observation note")
    )
  ))
}

pxweb_test_v2_comments_data <- function() {
  x <- list(
    version = "2.0",
    class = "dataset",
    label = "Population",
    id = list("Region", "Tid", "ContentsCode"),
    size = list(2L, 2L, 1L),
    dimension = list(
      Region = list(
        label = "region",
        category = list(
          index = list("01" = 0L, "03" = 1L),
          label = list("01" = "Stockholm", "03" = "Uppsala"),
          note = list("03" = "Uppsala note")
        ),
        extension = list(elimination = FALSE, show = "value")
      ),
      Tid = list(
        label = "year",
        category = list(
          index = list("2024" = 0L, "2025" = 1L),
          label = list("2024" = "2024", "2025" = "2025")
        ),
        extension = list(elimination = FALSE, show = "code")
      ),
      ContentsCode = list(
        label = "contents",
        category = list(
          index = list("POP" = 0L),
          label = list("POP" = "Population")
        ),
        extension = list(elimination = FALSE, show = "value")
      )
    ),
    value = c(10, 20, 30, 40),
    status = list("0" = "", "1" = "suppressed", "bad-name" = "ignored", "99" = "ignored")
  )
  pxweb_data_v2(x)
}

test_that("PXWEB v1 data comments include column, value, and observation comments", {
  comments <- pxweb_data_comments(pxweb_test_v1_commented_data())

  expect_equal(length(comments$pxweb_data_comments), 3)
  expect_s3_class(comments$pxweb_data_comments[[1]], "column_comment")
  expect_s3_class(comments$pxweb_data_comments[[2]], "value_comment")
  expect_s3_class(comments$pxweb_data_comments[[3]], "obs_comment")

  comments_df <- as.data.frame(comments, stringsAsFactors = FALSE)
  expect_equal(
    comments_df,
    data.frame(
      row_no = c(NA_integer_, 2L, 2L),
      col_no = c(1L, 1L, NA_integer_),
      comment_type = c("column_comment", "value_comment", "obs_comment"),
      comment = c("Region column note", "Uppsala value note", "Observation note"),
      stringsAsFactors = FALSE
    )
  )
})

test_that("PXWEB v2 note text and status comments ignore empty or invalid entries", {
  data <- pxweb_test_v2_comments_data()

  expect_null(pxweb:::pxweb_data_v2_note_text(NULL))
  expect_null(pxweb:::pxweb_data_v2_note_text(list("")))
  expect_equal(pxweb:::pxweb_data_v2_note_text(list("A", "B")), "A\nB")

  status_comments <- pxweb:::pxweb_data_v2_status_comments(data)
  expect_length(status_comments, 1)
  expect_equal(status_comments[[1]]$idx_data_frame$row_no, 2L)
  expect_equal(status_comments[[1]]$comment, "Status: suppressed")

  comments <- pxweb_data_comments(data)
  comments_df <- as.data.frame(comments, stringsAsFactors = FALSE)
  expect_true("Uppsala note" %in% comments_df$comment)
  expect_true("Status: suppressed" %in% comments_df$comment)
})

test_that("PXWEB codelists handle empty, lower-case, filters, and missing hrefs", {
  empty <- list(
    version = "2.0",
    class = "dataset",
    label = "Metadata",
    id = list("Region"),
    dimension = list(
      Region = list(
        label = "region",
        category = list(index = list("01" = 0L), label = list("01" = "Stockholm")),
        extension = list(elimination = FALSE)
      )
    ),
    value = list()
  )
  expect_equal(nrow(pxweb_codelists(empty)), 0)

  metadata <- list(
    version = "2.0",
    class = "dataset",
    label = "Metadata",
    id = list("Region", "Tid"),
    dimension = list(
      Region = list(
        label = "region",
        category = list(index = list("01" = 0L), label = list("01" = "Stockholm")),
        extension = list(
          codelists = list(
            list(id = "agg_region", label = "Region aggregation", type = "Aggregation"),
            list(
              id = "vs_region",
              label = "Region valueset",
              type = "Valueset",
              links = list(list(rel = "self"), list(href = "https://example.test/codelists/vs_region"))
            )
          )
        )
      ),
      Tid = list(
        label = "year",
        category = list(index = list("2024" = 0L), label = list("2024" = "2024")),
        extension = list(elimination = FALSE)
      )
    ),
    value = list()
  )

  code_lists <- pxweb_codelists(metadata)
  expect_equal(nrow(code_lists), 2)
  expect_equal(code_lists$href, c(NA_character_, "https://example.test/codelists/vs_region"))
  expect_equal(pxweb_codelists(metadata, variable = "REGION")$id, c("agg_region", "vs_region"))
  expect_equal(pxweb_codelists(metadata, variable = "region", type = "valueset")$id, "vs_region")
  expect_equal(pxweb:::pxweb_null_value(NULL, "fallback"), "fallback")
})
