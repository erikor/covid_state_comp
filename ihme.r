#
# ihme.r 
#
# compare actual deaths to ihme model
#

library(dplyr)
library(ggplot2)
library(scales)
library(zoo)


# ihme model

ihme1 <- read.csv("Hospitalization_all_locs_4_1.csv")
ihme5 <- read.csv("Hospitalization_all_locs_4_5.csv")
ihme7 <- read.csv("Hospitalization_all_locs_4_7.csv")
ihme9 <- read.csv("Hospitalization_all_locs_4_9.csv")
ihme12 <- read.csv("Hospitalization_all_locs_4_12.csv")
ihme17 <- read.csv("Hospitalization_all_locs_4_17.csv")
ihme22 <- read.csv("Hospitalization_all_locs_4_22.csv")

ihme1$state <- as.character(ihme1$location)
ihme1$state <- gsub("US", "United States", ihme1$state)
ihme1$state <- gsub("United States of America", "United States", ihme1$state)

ihme5$state <- as.character(ihme5$location)
ihme5$state <- gsub("US", "United States", ihme5$state)
ihme5$state <- gsub("United States of America", "United States", ihme5$state)

ihme7$state <- as.character(ihme7$location)
ihme7$state <- gsub("US", "United States", ihme7$state)
ihme7$state <- gsub("United States of America", "United States", ihme7$state)

ihme9$state <- as.character(ihme9$location)
ihme9$state <- gsub("US", "United States", ihme9$state)
ihme9$state <- gsub("United States of America", "United States", ihme9$state)

ihme12$state <- as.character(ihme12$location)
ihme12$state <- gsub("US", "United States", ihme12$state)
ihme12$state <- gsub("United States of America", "United States", ihme12$state)

ihme17$state <- as.character(ihme17$location)
ihme17$state <- gsub("US", "United States", ihme17$state)
ihme17$state <- gsub("United States of America", "United States", ihme17$state)

ihme22$state <- as.character(ihme22$location)
ihme22$state <- gsub("US", "United States", ihme22$state)
ihme22$state <- gsub("United States of America", "United States", ihme22$state)

# get range
uppers <- data.frame(upper = c(ihme1$deaths_upper,
                            ihme5$deaths_upper,
                            ihme7$deaths_upper,
                            ihme9$deaths_upper,
                            ihme12$deaths_upper,
                            ihme17$deaths_upper,
                            ihme22$deaths_upper),
                   upper_c = c(ihme1$totdea_upper,
                             ihme5$totdea_upper,
                             ihme7$totdea_upper,
                             ihme9$totdea_upper,
                             ihme12$totdea_upper,
                             ihme17$totdea_upper,
                             ihme22$totdea_upper),
                  state = c(ihme1$state,
                            ihme5$state,
                            ihme7$state,
                            ihme9$state,
                            ihme12$state,
                            ihme17$state,
                            ihme22$state))

ranges <- tapply(uppers$upper, INDEX = uppers$state, range)
ranges_c <- tapply(uppers$upper_c, INDEX = uppers$state, range)


# create the plot
ihmePlot <- function(state, actual, revision=1) {
  r <- ranges[state]
  if(revision == 1) {
    ihme <- ihme1
  } else if(revision == 2) {
    ihme <- ihme5
  } else if(revision == 3) {
    ihme <- ihme7
  } else if(revision == 4) {
    ihme <- ihme9
  } else if(revision == 5) {
    ihme <- ihme12
  } else if(revision == 6) {
    ihme <- ihme17
  } else if(revision == 7) {
    ihme <- ihme22
  }
  ix.ihme <- which(ihme$state == state)
  if(length(ix.ihme) == 0)
    return(ggplot() + theme_void())
  ix.deaths <- which(actual$state == state)
  ix <- match(ihme$date[ix.ihme], actual$date[ix.deaths])
  gd <- data.frame(predicted = ihme$deaths_mean[ix.ihme],
                   actual = c(NA, diff(actual$death[ix.deaths][ix])),
                   upper = ihme$deaths_upper[ix.ihme],
                   lower = ihme$deaths_lower[ix.ihme],
                   actual_ra = rollmean(c(NA, diff(actual$death[ix.deaths][ix])), 5, na.pad = TRUE),
                   date = as.character(ihme$date[ix.ihme]))
  colors <- c("95% CI" = "#EFC36E", "Deaths" = "#376095", "Predicted" = "#D76825", "Rolling Avg (Actual)" = "red")
  gd <- gd[which(gd$upper > 0),]
  a <-  ggplot(gd, aes(x = as.Date(date), y = predicted,
                       ymin = lower, 
                       ymax = upper)) +
    geom_ribbon(aes(color = "95% CI"), fill="#EFC36E", alpha=0.1, show.legend = FALSE) + 
    geom_line(aes(color = "Predicted")) + 
    geom_line(aes(y = actual_ra, color = "Rolling Avg (Actual)"), size=4, alpha=0.5) + 
    geom_point(aes(y=actual, fill="Deaths"), pch=21, show.legend = TRUE, color="white", size=2, alpha=0.7) + 
    labs(x="", y="COVID Deaths Per Day",
         title="IHME Model vs. Actual Deaths",
         subtitle=state) +
    scale_x_date(labels = date_format("%b %d"), breaks='7 days') +
    scale_y_continuous(breaks = seq(0,10000,400), minor_breaks = seq(0,10000, 200)) +
    ylim(0, unlist(r)[[2]]) + 
    theme(panel.grid.major = element_line(color="#ECECEC"),
          panel.grid.minor.y = element_line(color="#DBE6F7", size=0.1),
          panel.grid.minor.x = element_blank()) + 
    scale_color_manual(values = colors, name = "IHME Model",  
                       guide = guide_legend(override.aes = list(shape = c(NA), size=2))) +
    scale_fill_manual(values = colors, name = "Actual Events") + 
    theme(legend.key = element_rect(color = NA, fill = NA),
          legend.box.background = element_blank())
  a
}

ihmeCumulativePlot <- function(state, actual, revision=1) {
  r_c <- ranges_c[state]
  if(revision == 1) {
    ihme <- ihme1
  } else if(revision == 2) {
    ihme <- ihme5
  } else if(revision == 3) {
    ihme <- ihme7
  } else if(revision == 4) {
    ihme <- ihme9
  } else if(revision == 5) {
    ihme <- ihme12
  } else if(revision == 6) {
    ihme <- ihme17
  } else if(revision == 7) {
    ihme <- ihme22
  }
  ix.ihme <- which(ihme$state == state)
  if(length(ix.ihme) == 0)
    return(ggplot() + theme_void())
  ix.deaths <- which(actual$state == state)
  ix <- match(ihme$date[ix.ihme], actual$date[ix.deaths])
  gd <- data.frame(predicted = ihme$totdea_mean[ix.ihme],
                   actual = actual$death[ix.deaths][ix],
                   upper = ihme$totdea_upper[ix.ihme],
                   lower = ihme$totdea_lower[ix.ihme],
                   actual_ra = rollmean(actual$death[ix.deaths][ix], 5, na.pad = TRUE),
                   date = as.character(ihme$date[ix.ihme]))
  colors <- c("95% CI" = "#EFC36E", "Deaths" = "#376095", "Predicted" = "#D76825", "Rolling Avg (Actual)" = "red")
  gd <- gd[which(gd$upper > 0),]
  a <-  ggplot(gd, aes(x = as.Date(date), y = predicted,
                       ymin = lower, 
                       ymax = upper)) +
    geom_ribbon(aes(color = "95% CI"), fill="#EFC36E", alpha=0.1, show.legend = FALSE) + 
    geom_line(aes(color = "Predicted")) + 
    geom_line(aes(y = actual_ra, color = "Rolling Avg (Actual)"), size=4, alpha=0.5) + 
    geom_point(aes(y=actual, fill="Deaths"), pch=21, show.legend = TRUE, color="white", size=2, alpha=0.7) + 
    labs(x="", y="Cumulative COVID Deaths",
         title="IHME Model vs. Actual Deaths (Cumulative)",
         subtitle=state) +
    scale_x_date(labels = date_format("%b %d"), breaks='7 days') +
    scale_y_continuous(breaks = seq(0,10000,400), minor_breaks = seq(0,10000, 200)) +
    ylim(0, unlist(r_c)[[2]]) + 
    theme(panel.grid.major = element_line(color="#ECECEC"),
          panel.grid.minor.y = element_line(color="#DBE6F7", size=0.1),
          panel.grid.minor.x = element_blank()) + 
    scale_color_manual(values = colors, name = "IHME Model",  
                       guide = guide_legend(override.aes = list(shape = c(NA), size=2))) +
    scale_fill_manual(values = colors, name = "Actual Events") + 
    theme(legend.key = element_rect(color = NA, fill = NA),
          legend.box.background = element_blank())
  a
}
