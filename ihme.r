#
# ihme.r 
#
# compare actual deaths to ihme model
#

library(dplyr)
library(ggplot2)
library(scales)

# ihme model

ihme1 <- read.csv("Hospitalization_all_locs_4_1.csv")
ihme5 <- read.csv("Hospitalization_all_locs_4_5.csv")
ihme7 <- read.csv("Hospitalization_all_locs_4_7.csv")

ihme1$state <- as.character(ihme1$location)
ihme1$state <- gsub("US", "United States", ihme1$state)
ihme1$state <- gsub("United States of America", "United States", ihme1$state)

ihme5$state <- as.character(ihme5$location)
ihme5$state <- gsub("US", "United States", ihme5$state)
ihme5$state <- gsub("United States of America", "United States", ihme5$state)

ihme7$state <- as.character(ihme7$location)
ihme7$state <- gsub("US", "United States", ihme7$state)
ihme7$state <- gsub("United States of America", "United States", ihme7$state)


# create the plot
ihmePlot <- function(state, actual, revision=1) {
  if(revision == 1) {
    ihme <- ihme1
  } else if(revision == 2) {
    ihme <- ihme5
  } else if(revision == 3) {
    ihme <- ihme7
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
                   date = as.character(ihme$date[ix.ihme]))
  colors <- c("95% CI" = "#EFC36E", "Deaths" = "#376095", "Predicted" = "#D76825")
  gd <- gd[which(gd$upper > 0),]
  a <-  ggplot(gd, aes(x = as.Date(date), y = predicted,
                       ymin = lower, 
                       ymax = upper)) +
    geom_ribbon(aes(color = "95% CI"), fill="#EFC36E", alpha=0.1, show.legend = FALSE) + 
    geom_line(aes(color = "Predicted")) + 
    geom_point(aes(y=actual, fill="Deaths"), pch=21, show.legend = TRUE, color="white", size=3, alpha=0.8) + 
    labs(x="", y="COVID Deaths Per Day",
         title="IHME Model vs. Actual Deaths",
         subtitle=state) +
    scale_x_date(labels = date_format("%b %d"), breaks='7 days') +
    scale_y_continuous(breaks = seq(0,10000,400), minor_breaks = seq(0,10000, 200)) +
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

