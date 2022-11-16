library(shiny)
library(leaflet)
library(shinythemes)

navbarPage(theme = shinytheme("slate"),
           "Eco Predict", id="main",
           tabPanel("Map", leafletOutput("bbmap", height=1000)),
           #tabPanel("Data", DT::dataTableOutput("data")),
           tabPanel("Read Me",includeMarkdown("readme.md")))

