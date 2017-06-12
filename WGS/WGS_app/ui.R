load("measures.RData")
load("years.RData")
load("countries.RData")

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(

  fluidPage(theme = shinytheme("paper"),

  # Application title
  titlePanel("World Gender Statistics"),

  sidebarLayout(
    sidebarPanel(

      tags$h4("Select to display in map:"),

      uiOutput("choose_measure"),

      uiOutput("choose_years"),

      radioButtons("sex", "Gender:",
                   choices = list("Male" = "M",
                                  "Female" = "F",
                                  "Ratio" = "ratio"),
                   selected = "ratio"),

      br(),

      tags$h4("Select to display in timeline:"),

      selectInput("country", "Country",
                  choices = countries)
  ),


      mainPanel(

        fluidRow(
          p("The data was downloaded from", a("The World Bank's Open Data project", href = "http://data.worldbank.org/"), "via", a("Kaggle.", href = "https://www.kaggle.com/theworldbank/world-gender-statistic"), "For more info on how I built this app check out", a("my blog.", href = "https://shiring.github.io/2017/02/06/WGS_final")),

          p("Explore the tabs below to see information about male vs female measures of 160 measurements for 164 countries over 56 years. 'World map' shows female, male or male/female values for individual years. 'Latest ratios' shows the most recent male/ female ratio of the chosen statistic. 'Timelines' shows the change of male and female values over time. 'Country information' gives an overview over the co-variates used for statistic analysis in 'Analysis - Plots' and '- Tests'.")
          ),

      tabsetPanel(
        tabPanel("World map", fluidRow(
                                  tags$h3("World map of gender statistic"),

                                  p("Select a gender statistic and year on sidebar panel and click on any country on the map to find out more about it."),

                                  plotOutput("map",
                                             click = "plot_click", height = "auto"),

                                  tableOutput("info"))),

        tabPanel("Latest ratios", fluidRow(
                                  tags$h3("World map of last recorded values"),

                                  p("Select a gender statistic on sidebar panel."),

                                  plotOutput("last_vals", height = "auto"),

                                  tags$h5("Countries with strongest bias in last value:"),

                                  tableOutput("bias1"),

                                  tableOutput("bias2"),

                                  tags$h5("Countries with biggest change over time:"),

                                  tableOutput("diff1"),

                                  tableOutput("diff2"))),

        tabPanel("Timelines", fluidRow(
                                  tags$h3("Timeline of gender statistic"),

                                  p("Select a country on sidebar panel."),

                                  plotOutput("timeline", height = "auto"),

                                  plotOutput("timeline2", height = "auto"))),

        tabPanel("Country information", fluidRow(
                                  tags$h3("World map of factors used in statistical analysis"),

                                  plotOutput("income", height = "auto"),

                                  plotOutput("economy", height = "auto"),

                                  plotOutput("population", height = "auto"),

                                  plotOutput("gdp", height = "auto"))),

        tabPanel("Analysis - Plots", fluidRow(
                                  tags$h3("Density, bar- & correlation plots"),

                                  p("Select a gender statistic on sidebar panel."),

                                  plotOutput("density"),

                                  br(),

                                  plotOutput("distribution1"),

                                  br(),

                                  plotOutput("distribution2"),

                                  br(),

                                  plotOutput("distribution3"),

                                  br(),

                                  plotOutput("cor1"),

                                  br(),

                                  plotOutput("cor2"))),

        tabPanel("Analysis - Tests", fluidRow(
                                  tags$h3("Statistical tests"),

                                  p("Select a gender statistic on sidebar panel."),

                                  tags$h5("Wilcoxon Signed-Rank Test"),
                                  tableOutput("wilcox"),

                                  br(),

                                  tags$h5("Kruskal-Wallis Test"),
                                  tableOutput("kruskal"),

                                  br(),

                                  tags$h5('ANOVA Table'),
                                  tableOutput('aovSummary')

        ))
    ))
  )
))

