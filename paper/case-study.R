# Reproduce the Statistics Sweden population case study figure.
#
# Set PXWEB_PAPER_REFRESH_DATA=true to re-download the data even when the
# cached RDS file is present.

script_path <- tryCatch({
  args <- commandArgs(FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    normalizePath(sub("^--file=", "", file_arg[[1]]))
  } else {
    normalizePath(sys.frame(1)$ofile)
  }
}, error = function(e) NA_character_)

script_dir <- if (!is.na(script_path)) dirname(script_path) else getwd()
data_dir <- file.path(script_dir, "data")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

data_path <- file.path(data_dir, "scb-population-counties-1968-2024.rds")
figure_path <- file.path(script_dir, "case-study-population.pdf")

if (!requireNamespace("pxweb", quietly = TRUE)) {
  stop("The pxweb package is required to run this case study.", call. = FALSE)
}
if (utils::packageVersion("pxweb") < "0.19.0") {
  stop("This case study requires pxweb >= 0.19.0.", call. = FALSE)
}

metadata_url <- "https://statistikdatabasen.scb.se/api/v2/tables/TAB638/metadata?lang=en"
years <- as.character(1968:2024)
ages <- c(as.character(0:99), "100+")
county_codes <- c(
  "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13",
  "14", "17", "18", "19", "20", "21", "22", "23", "24", "25"
)
marital_status_codes <- c("OG", "G", "SK", "ÄNKL")
sex_codes <- c("1", "2")

refresh_data <- identical(tolower(Sys.getenv("PXWEB_PAPER_REFRESH_DATA")), "true")
if (file.exists(data_path) && !refresh_data) {
  cached_data <- readRDS(data_path)
  refresh_data <- !is.data.frame(cached_data) || nrow(cached_data) == 0
}

if (!file.exists(data_path) || refresh_data) {
  query <- list(
    Region = county_codes,
    Civilstand = marital_status_codes,
    Alder = ages,
    Kon = sex_codes,
    ContentsCode = "BE0101N1",
    Tid = years
  )

  data <- pxweb::pxweb_get_data(
    url = metadata_url,
    query = query,
    column.name.type = "code",
    variable.value.type = "code",
    verbose = TRUE
  )
  if (is.null(data)) {
    stop("Could not download SCB population data from the PX-Web API.", call. = FALSE)
  }

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

population_pyramid <- function(year_value, main) {
  subset_data <- data[data$year == year_value, ]
  age_sex <- aggregate(value ~ age + sex, data = subset_data, sum, na.rm = TRUE)
  wide <- reshape(age_sex, idvar = "age", timevar = "sex", direction = "wide")
  wide <- wide[order(wide$age), ]
  wide$value.Men[is.na(wide$value.Men)] <- 0
  wide$value.Women[is.na(wide$value.Women)] <- 0

  male <- -wide$value.Men / 1000
  female <- wide$value.Women / 1000
  max_value <- max(abs(c(male, female)), na.rm = TRUE)

  barplot(
    male,
    horiz = TRUE,
    col = "#4C78A8",
    border = NA,
    xlim = c(-max_value, max_value),
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
    xlim = c(-max_value, max_value),
    names.arg = rep("", length(female)),
    axes = FALSE,
    xlab = "",
    ylab = ""
  )
  axis(1, at = pretty(c(-max_value, max_value)),
       labels = abs(pretty(c(-max_value, max_value))))
  axis(2, at = seq(1, length(wide$age), by = 20),
       labels = wide$age[seq(1, length(wide$age), by = 20)], las = 1)
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

population_pyramid(1968, "C. Age and sex structure, 1968")
population_pyramid(2024, "D. Age and sex structure, 2024")

par(op)
dev.off()

cat("Wrote data cache:", data_path, "\n")
cat("Wrote figure:", figure_path, "\n")
