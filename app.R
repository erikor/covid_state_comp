library(shiny)
library(ggplot2)
library(reshape2)
library(hrbrthemes)
library(dplyr)
library(scales)

source("data.r")
source("ihme.r")

nyt <- data_NYT()
ct <- data_CT()
dat <- ct
states <- sort(as.character(unique(dat$state)))

# figure out when data was last updated
# and return formatted for display
lastUpdate <- function(dat) {
  last <- tail(as.character(dat$date),1)
  last <- gsub("([0-9]+)\\-([0-9]+)\\-([0-9]+)", "\\2/\\3/\\1", last)
  last
}

ui <- fluidPage(
  titlePanel("State level COVID trajectories"),
  sidebarLayout(
    sidebarPanel(
           selectInput("state1", "State 1", states, "United States"),
           selectInput("state2", "State 2", states, "Michigan"),
           radioButtons("data", "Source Data",
                        choices = list("NY Times" = "nyt", 
                                       "COVID Tracking Project" = "ct"),
                        selected = "ct"),
           radioButtons("stat", "Statistic",
                        choices = list("Incidence" = "positive_rate", 
                                       "Mortality" = "death_rate"),
                        selected = "positive_rate"),
           radioButtons("scale", "Scale",
                        choices = list("Linear" = "linear", 
                                       "Logarithmic" = "log"),
                        selected = "linear"),
           radioButtons("model", "IHME Model",
                        choices = list("April 1" = "1", 
                                       "April 5" = "2",
                                       "April 7" = "3",
                                       "April 9" = "4",
                                       "April 12" = "5"),
                        selected = "5"),
           helpText(strong("Data Sources: "), 
                    tags$a(href="https://github.com/nytimes/covid-19-data/", 
                           "NY Times Covid-19 Data,"),
                    tags$a(href="https://covidtracking.com/", 
                           "The COVID Tracking Project,"),
                    tags$a(href="http://www.healthdata.org/covid/data-downloads", 
                           "IHME Projections,"),
                    "and ",
                    tags$a(href="https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/", 
                           "US Census State Populations.")),
           helpText(strong("Note: "), "curated data last updated by NY Times on ", lastUpdate(nyt), 
                    "and by the COVID Tracking Project on ", lastUpdate(ct)),
           helpText(strong("IHME Model: "), "Using the IHME models released 4/1, 4/5, and 4/7/2020"),
           
           helpText(strong("Source Code: "), 
                    tags$a(href="https://github.com/erikor/covid_state_comp", 
                           "available on github.")),
    ),
    mainPanel(
           plotOutput("distPlot"),
           splitLayout(cellWidths = c("40%", "60%"), 
                       plotOutput("ihmePlot1"),
                       plotOutput("ihmePlot2")),
           splitLayout(cellWidths = c("40%", "60%"), 
                       plotOutput("ihmePlot3"),
                       plotOutput("ihmePlot4"))
    )
  )
)

curStat <- "positive_rate"
curState1 <- ""
curState2 <- ""
curDat <- "ct"

server <- function(input, output, session) {
    observe({
        val <- 1
        if(input$state1 != curState1) {
            curState1 <<- input$state1 
        } 
        if(input$state2 != curState2) {
            curState2 <<- input$state2
        } 
        
        if(input$stat == "death_rate") {
            if(curStat == "positive_rate") {
                updateTextInput(session, "new1", value = 0)
                updateTextInput(session, "new2", value = 0)
                curStat <<- "death_rate"
            }
            val <- 0
        } else {
            if(curStat == "death_rate") {
                updateTextInput(session, "new1", value = 0)
                updateTextInput(session, "new2", value = 0)
                curStat <<- "positive_rate"
            }
        }
        
        if(input$data == "nyt") {
          if(curDat == "ct") {
            dat <<- nyt
            curDat <<- "nyt"
          }
        } else { 
          if(curDat == "nyt") {
            dat <<- ct
            curDat <<- "ct"
          }
        }
        
        output$distPlot <- renderPlot({
          st1 <- input$state1
          st2 <- input$state2
          comp_st(st1, st2, 0, 0, input$stat, input$scale)
        })
        
        output$ihmePlot1 <- renderPlot({
          ihmePlot(input$state1, dat, as.numeric(input$model)) +
            my_theme + 
            theme(axis.text.x = element_text(size=9, angle = 45, hjust = 1),
                  legend.position = "none")
        })
        
        output$ihmePlot2 <- renderPlot({
          ihmeCumulativePlot(input$state1, dat, as.numeric(input$model)) +
            my_theme + 
            theme(axis.text.x = element_text(size=9, angle = 45, hjust = 1))
        })
        
        output$ihmePlot3 <- renderPlot({
          ihmePlot(input$state2, dat, as.numeric(input$model)) +
            my_theme + 
            theme(axis.text.x = element_text(size=9, angle = 45, hjust = 1),
          legend.position = "none")
        })
        
        output$ihmePlot4 <- renderPlot({
          ihmeCumulativePlot(input$state2, dat, as.numeric(input$model)) +
            my_theme + 
            theme(axis.text.x = element_text(size=9, angle = 45, hjust = 1))
        })
        
    })
}


comp_st <- function(st1, st2, new1, new2, stat=c("positive_rate", "death_rate"), scale = "linear") {
    ix <- which(dat$state %in% c(st1, st2))
    gd <- dat[ix,]
    State1 <- dat[ , stat][which(dat$state == st1 &  dat[ , stat]>0)]
    State2 <- dat[ , stat][which(dat$state == st2 &  dat[ , stat]>0)]
    
    if(new1 > 0) {
        pop1 <- dat$pop[which(dat$state == st1)]
        State1 <- c(State1, as.numeric(new1) / pop1 * 1000000)
    }
    if(new2 > 0) {
        pop1 <- dat$pop[which(dat$state == st2)]
        State2 <- c(State2, as.numeric(new2) / pop2 * 1000000)
    }

    l <- max(length(State1), length(State2))
    if(length(State1 < l)) {
        State1 <- c(State1, rep(NA, l-length(State1)))
    }
    if(length(State2 < l)) {
        State2 <- c(State2, rep(NA, l-length(State2)))
    }
    
    days <- 1:l
    gd <- data.frame(days = days, 
                     State1 = State1,
                     State2 = State2)
    gd.m <- melt(gd, id.vars = "days")
    gd.m$variable = factor(gd.m$variable, labels=c(st1, st2))
    
    colnames(gd.m) <- c("days", "State", "Cases")
    if(stat == "positive_rate") {
        ylab <- "Cases / Million population"
        title = "Cases"
        xlab <- "Days since incidence rate > 0"
    } else{
        ylab <- "Deaths / Million population"
        title = "Deaths"
        xlab <- "Days since mortality rate > 0"
    }
    
    p <- ggplot(gd.m, aes(x=days, y=Cases, color=State)) + 
        geom_line() + 
        geom_point(fill = "white", 
                   color="white", 
                   pch=21, 
                   size=3.5, 
                   stroke=1.5) + 
        geom_point(aes(fill = State), 
                   color="white", 
                   pch=21, 
                   size=3.5, 
                   stroke=1.5,
                   alpha=0.7) + 
        labs(x=xlab, y=ylab,
             title="Outbreak trajectory comparison",
             subtitle=paste(title, "per million in", st1, "vs.", st2)) +
        my_theme  
    if(scale == "log")
        p <- p + scale_y_log10()
    p
}

my_theme <- 
    theme(scale_color_manual(values = c("#F8A431", "#0092B9"))) + 
    theme_ipsum_rc() +
    theme(axis.title.x = element_text(size=12, face=2),
          axis.title.y = element_text(size=12, face=2),
          legend.title = element_text(size=14, face=2),
          legend.text = element_text(size=12))

# Run the application 
shinyApp(ui = ui, server = server)
