
Welcome to the World Happiness Index Report Repository!
This repository was created fully on R and displayed as an interactive application by Shiny. It contains information as well as reactive programming from the information of the World Happiness Report 2019 published on: https://worldhappiness.report/ed/2019/

The present repository contains 6 files apart from this READ ME:


•	Chapter2OnlineData.xls: Containing the information of the World Happiness Index Report
•	dataprep.R: The R script with the data construction instructions
•	happy2015.csv: A csv file containing the match of countries and regions
•	melt.xls: A melted dataframe of the population to get a panel data format of the years
•	server.R: The R file rendering and containing all functions to display on the Shinny app
•	ui.R: The user input file that designs how the application will be displayed

It is important to state that in the codes, you will be able to see a section containing a streamgraph, that is a type of graph that contains a bug as it is not yet available from the CRAN mirror in R. However, if managed manually on R it is possible to show this graph. 
Finally dear R users:
You can call the application through R studio using the following code:


library(shiny)


runGitHub("Bilgeyis/ShinyVisualizationWorldHappinessReport2018-", "rstudio")
