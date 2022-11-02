library(cronR)
file <- "daily_fpts.R"
cmd <- cron_rscript(file)
cron_add(cmd, frequency = "daily", at="3AM", id = "fantasy")
