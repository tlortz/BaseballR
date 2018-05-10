library(shiny)
library(shinydashboard)
library(dplyr)
library(data.table)
library(wrapr)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- dashboardPage(
   
  dashboardHeader(title = "Historical Player Performance Trends"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      #box(plotOutput("rcViolinAggregatesPlots",height = 250)),
      box(
        title = "Player Selection Filters",
        checkboxGroupInput("posSelection","Player Position(s) Selected",
                           choices = c("P"="P","C"="C","1B"="1B","2B"="2B","3B"="3B","SS"="SS","OF"="OF"))
        # checkboxGroupInput("posSelection","Player Position(s) Selected",c("SS"="SS","1B"="1B"))
        #sliderInput("periodBegin","Beginning of Historical Period","minYear","maxYear","middleYear"),
        #sliderInput("periodEnd","End of Historical Period","minYear","maxYear","middleYear")
      )
    ),
    fluidRow(
      box(
        plotOutput("perf_violins_yoe",width = "200%")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # prep batting_enriched table
  
  # form posList, pass to output
  allPositions <- na.omit(unique(batting_enriched$primaryPos))
  # output$posList <- allPositions#named_map_builder(allPositions,allPositions)
  
  posListMap = list()
  for (p in allPositions) {
    posListMap[[p]]=p
  }

  # output$posListMap <- renderValueBox(posListMap) 
  
  # get minYear, middleYear, maxYear variables, pass to output
  minYear <- min(batting_enriched$yearID,na.rm = TRUE)
  maxYear <- max(batting_enriched$yearID,na.rm = TRUE)
  middleYear <- round(mean(c(minYear,maxYear)))
  # output$minYear <- minYear
  # output$maxYear <- maxYear
  # output$middleYear <- middleYear
  
  ## make violin plots of performance by age for the position(s) chosen
  output$perf_violins_yoe <- renderPlot(
    ggplot(batting_enriched %>% filter(primaryPos %in% input$posSelection) %>% mutate(YOE=as.factor(YOE)),aes(YOE,RC)) +
    geom_violin()
    )
  
   
}

# Run the application 
shinyApp(ui = ui, server = server)

