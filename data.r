#
# data.r
#
# Extract, transform, and format dataset from NYT and COVID Tracking Project
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
  dat$positive <- as.numeric(dat$cases)
  dat$death <- as.numeric(dat$deaths)
  
  # convert to rate per million
  ix <- match(dat$state, pop$NAME)
  dat$pop <- pop$POPESTIMATE2019[ix]
  dat$positive_rate <- dat$positive / dat$pop * 1000000
  dat$death_rate <- dat$death / dat$pop * 1000000
  dat <- dat[ , c(1,2,6,7,8,9,10)]
  dat[order(dat$date),]
}

data_CT <- function() {
  # actual deaths from covidtracking.com
  dat <- read.csv("https://covidtracking.com/api/v1/states/daily.csv")[ , c(1,2,3,15)]
  abbr <- readRDS("state_abbr.rds")
  ix <- match(dat$state, abbr$Abbreviation)
  dat$state <- abbr$State[ix]
  
  # remove redundant data
  stdat <- paste(dat$state, dat$date)
  dat <- dat[-which(duplicated(stdat)),]

  # collate the US totals
  us <- dat %>%
    group_by(date) %>%
    summarise(state = "United States", 
              positive = sum(positive, na.rm = TRUE),
              death = sum(death, na.rm = TRUE))
  dat <- rbind(dat, us)
  ix <- which(is.na(dat$state))
  dat <- dat[-ix,]
  dat$date <- gsub("([0-9]{4,4})([0-9]{2,2})([0-9]{2,2})", "\\1-\\2-\\3", dat$date)
  
  # calculate rates
  ix <- match(dat$state, pop$NAME)
  dat$pop <- pop$POPESTIMATE2019[ix]
  dat$positive_rate <- dat$positive / dat$pop * 1000000
  dat$death_rate <- dat$death / dat$pop * 1000000
  dat[order(dat$date),]
} 

