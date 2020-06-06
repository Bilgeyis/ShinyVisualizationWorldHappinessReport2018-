
library(shiny)
source("dataprep.R")

shinyUI(fluidPage(
  tabsetPanel(
    tabPanel("Macro View", 
             titlePanel("World Map happiness index heatmap, 2017"),
             h4("Welcome to the Interactive Shiny App of the Happiness World Index report of 2017."),
             br(),
             h4("The first section displays the entire globe colored by the happiness index. Those regions not presenting their happiness index are presented in grey."),
             br(),
             br(),
             br(),
             plotOutput("worldPlot1", width = "100%"),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             h4("The following section displays an interactive plot that allows you to choose the regions you are interested in. The graph can display several regions simultaneously."),
             br(),
             br(),
             br(),
             sidebarLayout(
               sidebarPanel(
                 selectInput(input = 'region', 
                             label = 'Please select a region', 
                             multiple = TRUE,
                             choices=ch)),
               
               mainPanel(plotOutput('worldPlot2')))),
    

    tabPanel('Micro View',
             titlePanel('Happiness trend per selected country for all years'),
             br(),
             br(),
             
             h4("This new tab allows the user to observe the evolution of the happiness index for each country selected. In the meantime, the user could select several countries to compare the trend."),
             br(),
             br(),
             selectInput(input = 'country',
                         label = 'Please select a country name',
                         multiple = TRUE,
                         choices = ch1
                         ),
             plotOutput("worldPlot3", width = "100%"),
             h4("The top 10 happiest countries are shown in the bar race below for the year from 2005 to 2018")

             ,imageOutput('bar_chart_race',width = "100%", height = "400px", inline = FALSE)
             
             
             
            
             ),
             
    
    
    
    tabPanel('Distribution',
             titlePanel('Distribution of the selected variable'),
             selectInput(input = 'Year',
                         label = 'Please select a year',
                         choices = ch2),
             selectInput(input = 'variable',
                         label = 'Please select a variable',
                         choices = c("Life Ladder", "Population", "Log GDP per capita",  "Generosity",  "GINI index (World Bank estimate), average 2000-16")
                         
                        ),
             plotOutput("worldPlot4", width = "100%"),
             br(),
             br(),
             h4("If what you are looking for is the distribution of the variables, this tab will allow you to interact with the variables as well as their distribution, grouped by region. ")
             
             
             ),
    
    
    
    tabPanel('Box Plot',
             titlePanel('Box Plot of selected variable per selected year'),
             plotOutput("worldPlot5", width = "100%"),
             selectInput(input = 'year',
                         label = 'Please select a year',
                         choices = ch4),
             selectInput(input = 'variable1',
                         label = 'Please select a variable',
                         choices = c("Life Ladder", "Population", "Log GDP per capita",  "Generosity",  "GINI index (World Bank estimate), average 2000-16")),
             br(),
             br(),
             h4("In this tab, the visualization of basic statistics as the mean and quartiles is possible thanks to the box plot. 
                Feel free to select the year you are interested in as well as the variable.")
    ),
    
    # tabPanel('Streamgraph',
    #          titlePanel('Streamgraph of Generosity Perception'),
    #          br(),
    #          br(),
    #          h4("In this tab you can interact with the stream graph, a very useful plot if what you are looking for is the evolution of a variable over time in a more dynamic way."),
    #          br(),
    #          br(),
    #          br(),
    #          streamgraphOutput("worldPlot6", width = "100%")
    #          ),
    
    tabPanel('Regression',
             titlePanel('Linear regression happinex index~log GDP per capita and GINI index'),
             br(),
             br(),
             h4('If a regression analysis is required, down below is displayed a selector where you can choose the year. Please remember that this output represents a linear regression between the happiness index and the GDP. 
                On the next tab, you can manipulate all other variables to run regressions to will.'),
             selectInput(input='year1', label='Please select a year to display', 
                         choices=sort(unique(total$Year))),
             plotOutput('regression'),
             plotOutput('regression2')),
    
    
    tabPanel('Interactive regression',
             titlePanel('Linear regression happinex index~user variable selected'),
             br(),
             br(),
             h4('In this tab, you can have it all! Feel free to select the year, as well as the dependent and independent variable you are looking to analyze through the linear regression.'),
             selectInput(input='year_', label='Please select a year to display', 
                         choices=sort(unique(total$Year))),
             selectInput(input='user_variable', label='Please select an independent variable to model with', 
                         choices=c("Life Ladder", "Population", "Log GDP per capita",  "Healthy life expectancy at birth",
                                   "GINI index (World Bank estimate), average 2000-16","Social support","Population")),
             selectInput(input='user_variable2', label='Please select an dependent variable to model with', 
                         choices=c("Life Ladder", "Population", "Log GDP per capita",  "Healthy life expectancy at birth",
                                   "GINI index (World Bank estimate), average 2000-16","Social support","Population")),
             plotOutput('regression_combined'))
    
    #,tabPanel('Chart Race',
            # titlePanel("Bar chart race"),
             #imageOutput('bar_chart_race'),
            # br(),
           # br())
    
    )))


