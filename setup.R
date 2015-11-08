# Initialize workspace -------------------------------------
rm(list = ls())
if(Sys.info()['user'] == 'dag'){
  path.data <- '/Users/dag/Dropbox/data/dembar/2015/'
  path.out <- '/Users/dag/Dropbox/data/dembar/2015/out'
  path.code <- '/Users/dag/github/auditPoster'
}
required.packages <- c('ggplot2', 'extrafont')
invisible(lapply(required.packages, library, character.only = TRUE))
loadfonts()

# fetch raw data -------------------------------------------
library(foreign)
dembar <- read.dta(
  file.path(
    path.data, 'DB_dataset_allcountries_standardizeddata.dta'
  )
)
qog <- read.dta(
  file.path(
    path.data, 'qog_std_ts_jan15.dta'
  )
)
detach(package:foreign)

# select blueprint countries -------------------------------
blueprint <- subset(dembar, Blueprint == 1)
blueprint.qog <- subset(qog,
  ccode %in% unique(blueprint$CcodeQOG) &
  (year >= 1990 & year < 2013)
)

# identify worst, average, best performing countries -------
tmp <- aggregate(
  DQ ~ Country, data = blueprint, FUN = mean, na.rm = TRUE
)
tmp[which(min(tmp$DQ) == tmp$DQ), 'Country'] # lo: Costa Rica
tmp[which(max(tmp$DQ) == tmp$DQ), 'Country'] # hi: Denmark
tmp <- within(tmp, {delta.mean <- scale(DQ, scale = FALSE)})
## Slovenia most average, but US close 2nd and more interesting
rm(tmp)

# convenience hooks ----------------------------------------
key.names <- c('Costa Rica', 'United States', 'Denmark')
key.ccodeQog <- c(188, 840, 208)
base.size <- 8 # graph font size
wzb.colors <- c('#0380B5', '#9E3173', '#619933')
# Blue-ish, Red-ish, Green-ish
plot.size <- 3
