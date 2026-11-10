# Reproduce the Statistics Sweden population case study figure.
#
# Set PXWEB_PAPER_REFRESH_DATA=true to re-download the data even when the
# cached RDS file is present.

source_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NA_character_)
args <- commandArgs(FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_file <- if (length(source_file) == 1L && !is.na(source_file) && nzchar(source_file)) {
  source_file
} else if (length(file_arg) > 0) {
  sub("^--file=", "", file_arg[[1]])
} else {
  "case-study.R"
}

candidate_dirs <- unique(c(
  getwd(),
  file.path(getwd(), "paper"),
  dirname(normalizePath(script_file, mustWork = FALSE)),
  dirname(normalizePath(file.path(getwd(), script_file), mustWork = FALSE))
))
script_dir <- candidate_dirs[file.exists(file.path(candidate_dirs, "case-study.R"))]
if (length(script_dir) < 1) {
  stop("Run case-study.R from the paper directory or the package root.", call. = FALSE)
}
script_dir <- normalizePath(script_dir[[1]])
data_dir <- file.path(script_dir, "data")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

data_path <- file.path(data_dir, "scb-population-counties-1968-2024.rds")
citation_path <- file.path(data_dir, "scb-population-citation.txt")
comments_path <- file.path(data_dir, "scb-population-comments.rds")
figure_path <- file.path(script_dir, "figs", "case-study-population.pdf")
dir.create(dirname(figure_path), showWarnings = FALSE)

if (!requireNamespace("pxweb", quietly = TRUE)) {
  stop("The pxweb package is required to run this case study.", call. = FALSE)
}
if (utils::packageVersion("pxweb") < "0.19.2") {
  stop("This case study requires pxweb >= 0.19.2.", call. = FALSE)
}

years <- as.character(1968:2024)
ages <- c(as.character(0:99), "100+")
county_codes <- c(
  "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13",
  "14", "17", "18", "19", "20", "21", "22", "23", "24", "25"
)
marital_status_codes <- c("OG", "G", "SK", "\u00c4NKL")
sex_codes <- c("1", "2")

refresh_data <- identical(tolower(Sys.getenv("PXWEB_PAPER_REFRESH_DATA")), "true")
if (file.exists(data_path) && !refresh_data) {
  cached_data <- readRDS(data_path)
  refresh_data <- !is.data.frame(cached_data) || nrow(cached_data) == 0
}

if (!file.exists(data_path) || refresh_data) {
  search_results <- pxweb::pxweb_search(
    query = "population marital status",
    api_url = "https://statistikdatabasen.scb.se/api/v2",
    lang = "en",
    page_size = 5
  )
  print(search_results[, c("id", "label")], row.names = FALSE, right = FALSE)

  metadata_url <- search_results$metadata_url[search_results$id == "TAB638"]
  if (length(metadata_url) != 1L || is.na(metadata_url)) {
    stop("Expected exactly one metadata URL for Statistics Sweden table TAB638.", call. = FALSE)
  }

  query <- list(
    Region = county_codes,
    Civilstand = marital_status_codes,
    Alder = ages,
    Kon = sex_codes,
    ContentsCode = "BE0101N1",
    Tid = years
  )

  px_data <- pxweb::pxweb_get(
    url = metadata_url,
    query = query,
    verbose = TRUE
  )
  if (is.null(px_data)) {
    stop("Could not download SCB population data from the PX-Web API.", call. = FALSE)
  }

  data <- as.data.frame(
    px_data,
    column.name.type = "code",
    variable.value.type = "code"
  )
  if (is.null(data)) {
    stop("Could not download SCB population data from the PX-Web API.", call. = FALSE)
  }

  comments <- pxweb::pxweb_data_comments(px_data)
  comments_df <- as.data.frame(comments, stringsAsFactors = FALSE)
  saveRDS(comments_df, comments_path)
  writeLines(capture.output(pxweb::pxweb_cite(px_data)), citation_path)
  saveRDS(data, data_path)
} else {
  data <- cached_data
}

data$value <- as.numeric(data$value)
data$year <- as.integer(data$Tid)
data$age <- suppressWarnings(as.integer(data$Alder))
data$age[data$Alder == "100+"] <- 100L
data$sex <- ifelse(data$Kon == "1", "Men", "Women")

total_by_year <- aggregate(value ~ year, data = data, sum, na.rm = TRUE)

age65_by_year <- aggregate(
  value ~ year + is_age65plus,
  data = transform(data, is_age65plus = age >= 65),
  sum,
  na.rm = TRUE
)
age65_wide <- reshape(
  age65_by_year,
  idvar = "year",
  timevar = "is_age65plus",
  direction = "wide"
)
age65_wide$share65 <- age65_wide$value.TRUE /
  (age65_wide$value.FALSE + age65_wide$value.TRUE)

pyramid_years <- c(1968L, 2024L)
pyramid_data <- aggregate(
  value ~ year + age + sex,
  data = data[data$year %in% pyramid_years, ],
  sum,
  na.rm = TRUE
)
pyramid_ticks <- pretty(c(-1, 1) * max(pyramid_data$value) / 1000)
pyramid_xlim <- range(pyramid_ticks)

population_pyramid <- function(year_value, main) {
  age_sex <- pyramid_data[pyramid_data$year == year_value, ]
  wide <- reshape(age_sex, idvar = "age", timevar = "sex", direction = "wide")
  wide <- wide[order(wide$age), ]
  wide$value.Men[is.na(wide$value.Men)] <- 0
  wide$value.Women[is.na(wide$value.Women)] <- 0

  male <- -wide$value.Men / 1000
  female <- wide$value.Women / 1000

  bar_midpoints <- barplot(
    male,
    horiz = TRUE,
    col = "#4C78A8",
    border = NA,
    xlim = pyramid_xlim,
    names.arg = rep("", length(male)),
    axes = FALSE,
    main = main,
    xlab = "Population, thousands",
    ylab = "Age"
  )
  par(new = TRUE)
  barplot(
    female,
    horiz = TRUE,
    col = "#F58518",
    border = NA,
    xlim = pyramid_xlim,
    names.arg = rep("", length(female)),
    axes = FALSE,
    xlab = "",
    ylab = ""
  )
  axis(1, at = pyramid_ticks, labels = abs(pyramid_ticks))
  age_labels <- ifelse(wide$age == 100L, "100+", as.character(wide$age))
  age_tick_idx <- seq(1, length(wide$age), by = 20)
  axis(2, at = bar_midpoints[age_tick_idx], labels = age_labels[age_tick_idx], las = 1)
  legend(
    "topright",
    fill = c("#4C78A8", "#F58518"),
    legend = c("Men", "Women"),
    bty = "n",
    cex = 0.85
  )
}

pdf(figure_path, width = 7.2, height = 6.8, useDingbats = FALSE)
op <- par(
  mfrow = c(2, 2),
  mar = c(4.0, 4.2, 2.8, 1.1),
  oma = c(0, 0, 1.0, 0),
  las = 1
)

plot(
  total_by_year$year,
  total_by_year$value / 1e6,
  type = "l",
  lwd = 2,
  col = "#4C78A8",
  xlab = "Year",
  ylab = "Population, millions",
  main = "A. Population, counties combined"
)
grid(col = "grey88")

plot(
  age65_wide$year,
  100 * age65_wide$share65,
  type = "l",
  lwd = 2,
  col = "#54A24B",
  xlab = "Year",
  ylab = "Share aged 65+, percent",
  main = "B. Population aged 65 or older"
)
grid(col = "grey88")

population_pyramid(pyramid_years[1], "C. Age and sex structure, 1968")
population_pyramid(pyramid_years[2], "D. Age and sex structure, 2024")

par(op)
dev.off()

print(data.frame(
  year = pyramid_years,
  population_millions = round(total_by_year$value[match(pyramid_years, total_by_year$year)] / 1e6, 2),
  share_65_plus_percent = round(100 * age65_wide$share65[match(pyramid_years, age65_wide$year)], 1)
), row.names = FALSE)

cat("Wrote data cache:", file.path("data", basename(data_path)), "\n")
cat("Wrote figure:", file.path("figs", basename(figure_path)), "\n")
