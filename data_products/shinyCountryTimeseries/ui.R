# ui.R
library(shiny)

shinyUI(fluidPage(
    titlePanel("Plotting Ebola"),

    tags$head(includeScript("google-analytics.js")),

    sidebarLayout(
        sidebarPanel("Interactive plot components",
        checkboxInput("all_countries", "Show all countries", value=T),
                   conditionalPanel(
                     condition= "input.all_countries == false",
                     uiOutput("countriesList")),
                   checkboxInput("log", "Plot y-axis on log scale")
                   ),

        mainPanel(p("This graphs the cases and deaths of each country and normalizes the onset dates all to '0' so countries can be compared"),
                  "Data was all taken from Caitlin River's 'ebola' repository",
                  a('here', href = 'https://github.com/cmrivers/ebola'),

                  plotOutput("plot")
                  )
        )
))
