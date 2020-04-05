#
# nyt.r 
#
# fetch state level data from NY Times curated dataset and 
# calculate rates per million population
#

pop <- readRDS("pop.rds")

data_NYT <- function() {
  # load data
  dat <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
  dat$date <- as.character(dat$date)
  ix <- which(dat$state %in% pop$NAME)
  dat <- dat[ix,]
  states <- sort(as.character(pop$NAME))
  
  # total for US as a whole
  us <- dat %>% group_by(date) %>% summarise_at(.vars = names(.)[4:5], .funs = c(sum))
  us <- cbind(as.character(us$date), "United States", NA, us$cases, us$deaths)
  colnames(us) <- colnames(dat)
  dat <- rbind(dat, us)
  dat$cases <- as.numeric(dat$cases)
  dat$deaths <- as.numeric(dat$deaths)
  
  # convert to rate per million
  ix <- match(dat$state, pop$NAME)
  dat$pop <- pop$POPESTIMATE2019[ix]
  dat$cases <- dat$cases / dat$pop * 1000000
  dat$deaths <- dat$deaths / dat$pop * 1000000
  dat
}

