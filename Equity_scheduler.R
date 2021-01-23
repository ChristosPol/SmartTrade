# Description ------------------------------------------------------------------
library(cronR)

# Path of Rscript
path <- "/media/chris/DATA/Documents/Bot_Trading/SmartTrade/Equity.R"

# Command
cmd <- cron_rscript(path)

# add frequency and intervals
cron_add(cmd, frequency = 'daily', id = 'Equity', description = 'Equity', at = '23:00')

# Check all jobs
cron_ls()

# Stop Job
# cron_clear(ask = FALSE)
cron_rm(id = "Equity")

