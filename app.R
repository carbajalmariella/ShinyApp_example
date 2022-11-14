library(shiny)
library(shinythemes)
library(plotly)
library(tidyverse)
library(corrr)

# Loading datasets
library(palmerpenguins)
data(penguins)

ui <- fluidPage(theme = shinytheme("darkly"),
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
      
          tabPanel("Matrix correlation","Loading!!!")
          
        )
)

server <- function(input, output) {

  output$lmreg <- renderPlotly({
    cc <- ggplot(aes(x=get(input$v1),y=body_mass_g),data=penguins)+
      geom_point() +
      geom_abline(slope = 1, intercept = 0) +
      ylab("Body Mass (gr.)") + xlab("Variable selected (mm)")
    
  })
  output$hist <- renderPlot({
    bodymass <- na.omit(penguins$body_mass_g)
    htg <- seq(min(bodymass),max(bodymass),length.out = input$v2)
    hist(bodymass, breaks = htg, xlab = 'Body Mass (gr.)',col="red",border = "black",main = NULL)
  })
}

shinyApp(ui = ui, server = server)
