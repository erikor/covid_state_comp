library(shiny)
library(ggplot2)
library(reshape2)
library(hrbrthemes)
#pop <- read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/state/detail/SCPRC-EST2019-18+POP-RES.csv")
pop <- readRDS("pop.rds")
dat <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

ui <- fluidPage(

    # Application title
    titlePanel("State level COVID trajectories"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("state1", "State 1", sort(pop$NAME), "New York"),
            selectInput("state2", "State 2", sort(pop$NAME), "Michigan"),
            sliderInput("start",
                        "Initial Rate Threshold",
                        min = 0,
                        max = 10,
                        value = 1),
            helpText("When did outbreak 'start'? Try 0 or 1 (per million) as starting values."),
            radioButtons("stat", "Statistic",
                         choices = list("Incidence" = "cases", 
                                        "Mortality" = "deaths"),
                                        selected = "cases"),
            helpText(strong("Data Sources: "), 
                     tags$a(href="https://github.com/nytimes/covid-19-data/", 
                                              "NY Times Covid-19 Data"),
                     " and ",
                     tags$a(href="https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/", 
                            "US Census State Populations.")),
            helpText(strong("Note: "), 
                            "curated data from NY Times may be about 24 hours behind current counts. ")
                     
        ),

        mainPanel(
           plotOutput("distPlot"),
           width = 6
        )
    )
)

server <- function(input, output, session) {
    output$distPlot <- renderPlot({
        st1 <- input$state1
        st2 <- input$state2
        comp_st(st1, st2, input$stat, input$start)
    })
    observe({
        val <- 1
        if(input$stat == "deaths") {
            val <- 0
        }
        # Control the value, min, max, and step.
        # Step size is 2 when input value is even; 1 when value is odd.
        updateSliderInput(session, "start", value = val)
    })
}

comp_st <- function(st1, st2, stat=c("cases", "deaths"), threshold = 0) {
    ix <- match(dat$state, pop$NAME)
    dat.rate <- data.frame(state = dat$state, rate=dat[,stat] / ( pop$POPESTIMATE2019[ix] / 1000000))
    ix1 <- which(dat.rate$state == st1 & dat.rate$rate > threshold)
    ix2 <- which(dat.rate$state == st2 & dat.rate$rate > threshold)
    
    l <- max(length(ix1), length(ix2))
    if(length(ix1 < l)) {
        ix1 <- c(ix1, rep(NA, l-length(ix1)))
    }
    if(length(ix2 < l)) {
        ix2 <- c(ix2, rep(NA, l-length(ix2)))
    }
    days <- 1:l
    gd <- data.frame(days = days, 
                     State1 = dat.rate$rate[ix1[days]],
                     State2 = dat.rate$rate[ix2[days]])
    gd.m <- melt(gd, id.vars = "days")
    gd.m$variable = factor(gd.m$variable, labels=c(st1, st2))
    
    colnames(gd.m) <- c("days", "State", "Cases")
    if(stat == "cases") {
        ylab <- "Cases / Million population"
        title = "Cases"
        xlab <- "Days since incidence crossed threshold"
    } else{
        ylab <- "Deaths / Million population"
        title = "Deaths"
        xlab <- "Days since mortality crossed threshold"
    }
    
    ggplot(gd.m, aes(x=days, y=Cases, color=State)) + 
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
