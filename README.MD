# Biodiversity dashboard

The aim of this project is to create Shiny Dashboard to visualize Species on the map and also on timeline - where and when it was observed. 

### This app is based on modules:
tabPanelUI module - inputs and outputs (of one tabPanel)

tabPanelServer - server logic that uses that UI

In the dashboard there are 4 tab panels, with map and timeline for different countries:
- PL - Poland
- AT - Austria
- KE - Kenya
- NP - Nepal

The country files were created on the basis of biodiversity-data.tar.gz. dataset send to me for the purpose of the recruitment exercise

### Repository structure:
This repository consists of 4 folders:
- R - scripts sourced in app to load libraries and data
- data - csv files with countries data used for calculations
- www  - css stylesheet  
- tests - shinytests

To run the app, clone the repository and run app.R file.

This shiny dashboard was deployed to shinyapps.io, you can check link below:

https://anova24.shinyapps.io/BiodiversityDashboard/


## Author
 - [Jakub Augustynek](https://github.com/anova97)
