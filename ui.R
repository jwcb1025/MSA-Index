#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# Define UI for application that draws a histogram


title <- tags$a(href = 'https://findsellers.com', 
                tags$img(src = "logo-01.png", height = '30', width = '200'))

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = title),
                    dashboardSidebar(
                      sidebarMenu(
                      menuItem("Search", tabName = "Search", icon = icon("search"))),
                      selectizeInput("selectMSA", label = "Metropolitan Statistical Area", choices = NULL),
                      br(),
                      fluidPage(
                        fluidRow(
                          flexdashboard::gaugeOutput("meterREMV")),
                      fluidRow(
                        h3(textOutput("sbarName"))),
                      fluidRow(
                     hr(),
                       HTML("<font size= '3'>
                           Median Cash on Cash Return: 
                           </font>"),
                       textOutput("sbarCashReturn"),
                       hr(),
                      HTML("<font size= '3'>
                           Median Cash Flow: 
                           </font>"),
                     textOutput("sbarCashFlow"),
                     hr(),
                     HTML("<font size= '3'>
                          Median Sale Price: 
                          </font>"),
                     textOutput("sbarSalePrice"),
                     hr(),
                     HTML("<font size= '3'>
                          Median Mortgage Payment: 
                          </font>"),
                     textOutput("sbarPayment"),
                     hr(),
                     HTML("<font size= '3'>
                          Median Rent: 
                          </font>"),
                     textOutput("sbarRent"),
                     hr(),
                     HTML("<font size= '3'>
                          Median Income: 
                          </font>"),
                     textOutput("sbarIncome")
                  
                     ))),
                        #  menuItem("Chart", tabName = "Chart", icon = icon("line-chart"))
                    dashboardBody( 
                      tabsetPanel(id = "mainTab", type = "tabs",
                                               tabPanel("Map",
                                                        fluidPage(
                                                          leafletOutput("rankingMap", height = "450px", width = "100%"), 
                                                          hr(),
                                                          dataTableOutput("searchResults"))),
                                               tabPanel("Chart", 
                                                        fluidPage(
                                                          plotlyOutput("msaPlot", width = "100%", height = "720px" ))
                                                        )
                                               )
                      )
                    )

                                       