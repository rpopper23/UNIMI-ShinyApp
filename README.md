# Shiny App
## Professor Nicolussi Exam
This Shiny app can be used to take an overview of the global pandemic situation. The user can set a date-range and select a country from a dropdown menu to visualize time and entity specific data.
The first tab contains plots of the epidemic curve and deaths on a daily basis, and an animated smoothing line to fit to the plots. Both ploths are interactive. The user can position its mouse on a specific point. Clicking allows to visualize the x and y coordinates of that point on the graph.
The second tab contains some summary statistics as well as a data table. Both elements are reactive based on the date range and country input selected on the previous tab.
The data are obtained on the web throught the European Center for Disease Prevention and Control

