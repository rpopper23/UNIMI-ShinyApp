#libraries needed
library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(httr)
library(ggplot2)
library(DT)
library(scales)

#importing dataset
data <- read_csv("http://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
# convert date format and perform some adjustment
data$date_reported <- mdy(paste0(data$month,"-", data$day,"-",data$year))
data<-data %>% rename(date=date_reported, country=countryterritoryCode, `cases_per_100000`=`Cumulative_number_for_14_days_of_COVID-19_cases_per_100000`)
data <- data[order(data$country),]
continent <- data %>% 
  group_by(continentExp) %>% 
  summarise(deaths = sum(deaths), cases= sum(cases))
source("function.R", local = T)


# Define UI ---------------------------------------------------------
ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage(theme="cerulean", 
                           "Covid19 Outbreak",
                           
                           #Tab 1
                           
                           tabPanel("Trend",
                                    fluidRow(
                                      column(4,
                                             helpText("Show trends for country-specific data over a preferred time interval"),
                                             
                                             # Select country filter
                                             selectInput(inputId = "country", 
                                                         label = strong("Country"),
                                                         choices = unique(data$country),
                                                         selected = "ITA"),
                                             
                                             # Select date range to be plotted
                                             dateRangeInput("date", strong("Date range"), 
                                                            start = "2019-12-31", end = "2020-12-14",
                                                            min = "2019-12-31", max = "2020-12-14"),
                                            
                                             br(),  
                                             
                                             # Select whether to overlay smooth trend line
                                             checkboxInput(inputId = "smoother", 
                                                           label = strong("Overlay smooth trend line"), 
                                                           value = FALSE),
                                             
                                             # Display only if the smoother is checked
                                             conditionalPanel(condition = "input.smoother == true",
                                                              sliderInput(inputId = "f", label = "Smoother span:",
                                                                          min = 0.01, max = 1, value = 0.67, step = 0.01,
                                                                          animate = animationOptions(interval = 100)),
                                                              HTML("Higher values give more smoothness.")
                                                              
                                             )              
                                      ),
                                      
                                      
                                      # Output: Description, lineplot, and reference
                                      fluidRow(
                                        column(7,
                                               plotOutput(outputId = "lineplot", click = "plot_click", height = "250px"),
                                               verbatimTextOutput("info"),
                                               br(),
                                               plotOutput(outputId = "lineplot1", click = "plot_click1", height = "250px"),
                                               verbatimTextOutput("info1"),
                                               br(),
                                               
                                               tags$a(href = "https://www.ecdc.europa.eu/en", "Source: European Center for Disease Control", target = "_blank")
                                        )
                                      )
                                    )),   
                           
                           #tab2
                           
                           tabPanel("Data",
                                    sidebarPanel(
                                      
                                      helpText("Date range input as defined in the previous tab"),
                                      checkboxGroupInput(inputId = "sum_stat", 
                                                  label = strong("Summary statistics for"),
                                                  choices = c("deaths", "cases", "cases_per_100000"),
                                                  selected = "cases_per_100000"),
                                      br(),

                                      checkboxInput(inputId = "show_data", label = "Display data")
                                    ),
                                    
                                     
                                    
                                    mainPanel(
                                      verbatimTextOutput(outputId = "sum_table"),
                                      verbatimTextOutput(outputId = "desc"),
                                      DT::dataTableOutput(outputId = "table")
                                      
                                    )),
                           
                           
                           #Tab 3
                           
                           tabPanel("Worldwide",
                              fluidRow(
                                column(6,
                                    helpText("Total cases and deaths over a one-year period worldwide")
                              )),

                        
                                   
                            
                              fluidRow(
                                column(10,
                                    plotOutput(outputId = "worldcases", height = "250px"),
                                    br(),
                                    plotOutput(outputId = "worlddeaths", height = "250px"),
                                             
                                    tags$a(href = "https://www.ecdc.europa.eu/en", "Source: European Center for Disease Control", target = "_blank")
                                      )
                                    )
                             )
                ))
                                  
                                                     
                                                  
                             
                           
                
#Define server---------------------------------------------------------

server <- function(input, output) {
  
  # Subset data
  selected_country <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    data %>%
      filter(
        country == input$country,
        date > as.POSIXct(input$date[1]) & date < as.POSIXct(input$date[2]
        ))
  })
  
  
  # Create lineplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
    ggplot(data = selected_country(), 
           aes(x=selected_country()$date, y=selected_country()$cases)) + 
      geom_line() +
      labs(title="Trend", x="Date", y="Number of daily infected") +
      scale_x_date(date_labels = "%Y/%m/%d") +
      
      # Display only if smoother is checked
      {if(input$smoother)
        geom_smooth(se = FALSE, span = input$f)
        
      }
  })
  
  output$info <- renderText({
    paste0("Date:", as.POSIXct.Date(input$plot_click$x), "\nNumber of daily infected:", as.integer(input$plot_click$y))
  })
  
  
  
  #Create second lineplot
  
  output$lineplot1 <- renderPlot({
    ggplot(data = selected_country(), 
           aes(x=selected_country()$date, y=selected_country()$deaths)) + 
      geom_line() +
      labs(title="Trend", x="Date", y="Number of daily deceased") +
      scale_x_date(date_labels = "%Y/%m/%d") +
      
      # Display only if smoother is checked
      {if(input$smoother)
        geom_smooth(se = FALSE, span = input$f)
        
      }
  })
  output$info1 <- renderText({
    paste0("Date:", as.POSIXct.Date(input$plot_click1$x, format = "%Y/%m/%d"), "\nNumber of daily deceased:", as.integer(input$plot_click1$y))
  })
  
  
  #Output tab2
  
  #Summary statistics
  output$sum_table <- renderPrint({
    summary(selected_country()[input$sum_stat])
  }) 
  
  #Output data table
  
  output$table <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = selected_country()[ , -c(1, 2, 3, 4, 7, 8, 12)],
                    options = list(pageLength = 5), 
                    rownames = FALSE)
    }
  )
  
  output$desc <- renderText({
    paste0("Mortality rate per 100,000 people in the time-range is:  ", as.integer(myfunction(selected_country()$deaths, selected_country()$popData2019[1])),
           "\nInfection rate per 100,000 people in the time-range is:  ", as.integer(myfunction(selected_country()$cases, selected_country()$popData2019[1]))) 
                                                                                              
  })
  
  #Output tab 3
  
  output$worldcases <- renderPlot({
      continent <- data %>% 
        group_by(continentExp) %>% 
        summarise(deaths = sum(deaths), cases= sum(cases))
        ggplot(continent, aes(x= continentExp, y= cases, fill=continentExp)) +
        geom_bar(stat='identity', position='dodge')+
        labs(title="Covid19 Worldwide", x="Continent") +
        scale_y_continuous(name="Number of Cases", labels = number_format(accuracy = 0.01)) +
        scale_fill_discrete(name="Continents")
    })
      output$worlddeaths <- renderPlot({
        ggplot(continent, aes(x= continentExp, y= deaths, fill=continentExp)) +
          geom_bar(stat='identity', position='dodge')+
          labs(title="Covid19 Worldwide", x="Continent") +
          scale_y_continuous(name="Number of Deaths", labels = number_format(accuracy = 0.01)) +
          scale_fill_discrete(name="Continents")
      })
     
  }
# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)
