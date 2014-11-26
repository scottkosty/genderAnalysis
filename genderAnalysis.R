library(dplyr)
library(ggplot2)

if (!require(gender)) {
    library(devtools)
    install_github("scottkosty/gender")
    library(gender)
}

rHelp <- rHelpNames
rHelp[is.na(rHelp$gender), "gender"] <- "unknown"
rHelp$date <- as.Date(paste(rHelp$year, rHelp$month, '01', sep = '-'), "%Y-%b-%d")

rHelp_sum <- rHelp %.% group_by(date, gender) %.% summarise(r_helpers = n())
plot_ <- ggplot(rHelp_sum, aes(y = r_helpers, x = date, color = gender)) + geom_line() + ggtitle("Number of unique (within month)\nposters to r-help, by gender")

print(plot_)

ggsave("gender.png", height = 5, width = 5)

