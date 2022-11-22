library(shiny)
library(shinythemes)
library(plotly)

# Loading datasets
library(palmerpenguins)
data(penguins)

###################################################################
### WE NEED TO LOAD TWO LIBRARIES 
library(tidyverse)
library(     ) ## Library for working with correlate() and rplot()
###################################################################


ui <- fluidPage(theme = shinytheme("united"),   #try: united, superhero, ...
        navbarPage("Penguins data",
          tabPanel("Linear Regression",
            sidebarPanel(
              tags$h3("Linear Regression"),
              shiny::selectInput(
                inputId = "v1",
                label = "Select X-axis",
                choices = c("Bill length" = "bill_length_mm",
                            "Bill depth" = "bill_depth_mm",
                            "Flipper length" = "flipper_length_mm")
              ),
            ),
            mainPanel(
              h3("Linear Regression with Body mass"),
              plotlyOutput("lmreg")
            )
          ),
          tabPanel("Histogram",
            sidebarPanel(
              shiny::sliderInput(
                inputId = "v2",
                label = "Number of bins:",
                min = 1,
                max = 40,
                value = 10)
            ),
            mainPanel(
              h3("Histogram"),
              plotOutput("hist")
            )
          ),
############################################################################
          tabPanel("Correlation Matrix",
            sidebarPanel(
              tags$h3("Correlation Matrix"),
              shiny::selectInput(
                inputId = "v3",
                label = "Select grouping variable",
                choices = c("Sex" = "sex",          ## Name the variables 
                            "Species" = "#fill# ",  ## inputs (column names)
                            "Island" = "#fill#")    ## the user can choose.
              ),
            ),
            mainPanel(
              h3("Correlation matrix grouping penguins body mass per variable selected"),
              plotOutput("#fill#")    ## fill w/output name (from 3rd output name)
            )
          ),
############################################################################
        )
)

server <- function(input, output) {

  output$lmreg <- renderPlotly({
    ggplot(aes(x=get(input$v1),y=body_mass_g),data=penguins)+
      geom_point() +
      geom_abline(slope = 1, intercept = 0) +
      ylab("Body Mass (gr.)") + xlab("Variable selected (mm)")
  })
  output$hist <- renderPlot({
    bodymass <- na.omit(penguins$body_mass_g)
    htg <- seq(min(bodymass),max(bodymass),length.out = input$v2)
    hist(bodymass, breaks = htg, xlab = 'Body Mass (gr.)',col="red",border = "black",main = NULL)
  })
##############################################################################
  output$cormatrix <- renderPlot({  ## This is the 3rd output that should be referenced in the main panel
    penguins0 <- na.omit(penguins)
    new_peng <- #fill# %>%     ## fill with the dataset name from previous row
      pivot_wider(names_from = #fill w/input3#, values_from = body_mass_g,names_prefix = "body mass ") %>%
      select_if(is.numeric) %>%
      select(-year) %>%
      correlate()
    rplot(#fill#)   ##fill with correlation object just created
  })
##############################################################################  
}

shinyApp(ui = ui, server = server)
