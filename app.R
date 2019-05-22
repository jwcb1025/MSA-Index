## app.R ##
library(shinythemes)
library(shinydashboard)
library(plotly)
library(DBI)
library(datetime)
library(dplyr)
#Dashboard Objective:
##Top MSA ranking on homepage (Table Displaying Results of top 10 MSA) (#1 MSA Chart Default Display)
  #User searches MSA/Dropdown Selection:
    ##Query DB based on user selection
      ##Generate a table in memory
        ##Render chart data

#Various Other MSA Analytics
sqlQuery <- function(query){
  
  # creating DB connection object with RMysql package

  # close db connection after function call exits
  on.exit(dbDisconnect(DB))
  
  # send Query to btain result set
  rs <- dbSendQuery(DB, query)
  
  # get elements from result sets and convert to dataframe
  result <- fetch(rs, -1)
  
  # return the dataframe
  return(result)
}

ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "MSA Index"),
  dashboardSidebar(
    selectizeInput("selectLocation", label = "Search Zip Code", choices = NULL),
    sidebarMenu(
      menuItem("Chart", tabName = "Chart", icon = icon("chart-line")),
      menuItem("Table", tabName = "Table", icon = icon("table")),
      menuItem("Download Data", tabName = "Download", icon = icon("download")))),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "Chart",
              fluidPage(
                # selectizeInput("selectCounty", label = "Enter County Name", choices = NULL),
                plotlyOutput("msaPlot", height = 700)
              )),
      tabItem(tabName = "Table",
              fluidPage(
                fluidRow(dataTableOutput("msaTable"), width = "auto"))
             )
    )
  )
)

server <- function(input, output, session) {
  set.seed(122)
  zipquery <- "SELECT *
  FROM zipID;"

  myquery <- "SELECT *
  FROM app_data;"
  
  zipID <- sqlQuery(zipquery)
  msaData <- withProgress(sqlQuery(myquery), message = "Loading Data")
  msaData$date <- as.Date(msaData$date, "%Y-%m-%d")

  #mutate(maxRentPurchasePrice = medianRent75pct * 360)
  
 updateSelectizeInput(session, "selectLocation", choices = zipID$zip, server = TRUE)
  
 msaPlotData <- reactive({
   msaData %>% filter(zip == input$selectLocation)
 })
 
  y1 <- list(
    tickfont = list(color = "blue"),
    side = "left",
   title = ""
  )
  y2 <- list(
    tickfont = list(color = "green"),
    side = "right",
    title = ""
    )
  x1 <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = TRUE,
    showgrid = FALSE
  )
  
  #output$msaTable <- renderDataTable(msaPlotData(), escape = c("county", "year", "medianValue", "medianRent", "incomeMedian"))
  
  output$msaPlot <- renderPlotly({
 # subplot(
    plot_ly(msaPlotData(), x = ~date) %>%
    add_trace(y = ~medianRentEst,name = "Median Rent",  mode = 'lines', yaxis = "y2") %>%
     # add_trace(y = ~medianIncome / 12,name = "Median Income (Monthly)",  mode = 'lines', yaxis = "y2") %>%
     add_trace(y = ~paymentMonthly30,name = "30 Yr Fixed Payment",  mode = 'lines', yaxis = "y2", line = list(color = 'rgb(89, 170, 40)', width = 2)) %>%
  add_trace(y = ~paymentMonthly5yrIntrst,name = "5 Yr Interest Only Payment",  mode = 'lines', yaxis = "y2") %>%
  add_trace(y = ~medianAffordablePayment,name = "Affordable Payment (Baseline)",  mode = 'lines', yaxis = "y2") %>%
  add_trace(y = ~medianRent75pct,name = "Median Rent (75%)",  mode = 'lines', yaxis = "y2") %>%
  add_trace(y = ~maxRentPurchasePrice,name = "Maximum Rental Purchase Price",  mode = 'lines', yaxis = "y1") %>%
  add_trace(y = ~hvi, name = "Median Sale Price", mode = 'lines', yaxis = "y1") %>%
  add_trace(y = ~maxMedianPrice5yrIntrst, name = "Max Median Price (5yr Interest)", mode = 'lines', yaxis = "y1") %>%
    
       layout(
       # title =  input$selectLocation, 
        yaxis = y1, yaxis2 = y2,
        xaxis = x1,
        legend = list(orientation = 'h'),
        margin = 400, xaxis = x1)     
      
 #plot_ly(msaPlotData(), x = ~date) %>%
   # add_trace(y = ~medianIncome,name = "Maximum Rental Purchase Price",  type = 'bar') %>%
  #  add_trace(y = ~hvi, name = "Median Sale Price", type = 'bar', bar = list(color = 'rgb(255, 175, 14)', width = 4)) %>%
# layout(margin = 400, xaxis = x1), 
# nrows=2, shareX = TRUE)
})
}
#add mortgage payment info

shinyApp(ui, server)