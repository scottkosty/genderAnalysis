library(dplyr)
library(stringr)

download.file("http://www.ssa.gov/OACT/babynames/names.zip", destfile = "names.zip")
names_d <- "names"
dir.create(names_d)
unzip("names.zip", exdir = names_d)

babyNames_l <- list()
for (file_ in list.files(names_d, pattern = "txt")) {
  oneYear <- read.csv(file.path(names_d, file_), stringsAsFactors = FALSE, header = FALSE, col.names = c("name", "gender", "freq"))
  oneYear[["year"]] <- gsub(".*(\\d\\d\\d\\d).*", "\\1", file_)
  babyNames_l[[file_]] <- oneYear
}

babyNames_df <- do.call(rbind, babyNames_l)
rownames(babyNames_df) <- NULL
# useful when matching
babyNames_df[["name"]] <- tolower(babyNames_df[["name"]])

# interesting to see names with both genders before collapsing
# TODO could have a measure of precision based on ratio
babyNames_both <- babyNames_df %.%
  group_by(name, gender) %.%
  summarise(total = sum(freq))

babyNames <- filter(babyNames_both, rank(desc(total)) == 1)

save(babyNames, file = "babyNames.Rdata")

gender <- function(name_) {
  ind_ <- match(tolower(name_), babyNames[["name"]])
  babyNames[ind_, "gender"]
}

rHelp_d <- "r_help"
rHelpNames_l <- list()
dir.create(rHelp_d)
for (yr in 1998:2013)
  for (mth in c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) {
  file_ <- file.path(rHelp_d, paste0(yr, mth, ".html"))
  # Note the "insecure" is because of the certificates
  download.file(paste0("https://mailman.stat.ethz.ch/pipermail/r-help/", yr, "-", mth, "/thread.html"), destfile = file_, method = "curl", extra = c("--insecure"))
  lines_ <- readLines(file.path(file_))
  htmlNames <- grep("^<I>", lines_, value = TRUE)
  fullNames <- unique(gsub("<I>", "", htmlNames))
  names_l <- str_split(fullNames, " ", n = 2)
  names_m <- do.call(rbind, names_l)

  # comma correction
  comma_ind <- grep(',', names_m[, 1])
  rest_ <- names_m[comma_ind, 1]
  names_m[comma_ind, 1] <- names_m[comma_ind, 2]
  names_m[comma_ind, 2] <- rest_

  names_df <- data.frame(first = names_m[, 1], rest = names_m[, 2], month = mth, year = yr, stringsAsFactors = FALSE)
  rHelpNames_l[[paste0(yr, mth)]] <- names_df
}

rHelpNames <- do.call(rbind, rHelpNames_l)
rownames(rHelpNames) <- NULL
rHelpNames[["gender"]] <- gender(rHelpNames[["first"]])
# in case the name is flipped with no comma (low precision here)
na_ind <- is.na(rHelpNames[['gender']])
rHelpNames[na_ind, "gender"] <- gender(rHelpNames[na_ind, "rest"])

save(rHelpNames, file = "rHelpNames.Rdata")
