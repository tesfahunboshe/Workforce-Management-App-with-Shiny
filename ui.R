library(shiny)
library(shinydashboard)
library(dplyr)
library(shinycssloaders)
library(shinythemes)
library(rhandsontable)
library(bsplus)
library(htmltools)
library(giphyr)
library(rpivotTable)
library(plotly)
library(shinyalert)
library(shinyalert)
library(shinyjs)
library(ggplot2)
# library(supercaliheatmapwidget)


dashboardPage(
  dashboardHeader(dropdownMenuOutput("dropdownmenu"),
                  title = "My WFM App",dropdownMenuOutput("msgOutput")
  ),
  
  
  dashboardSidebar(
    sidebarMenu(id= "tabs", width = 350,
                menuItem("Schedule", icon = icon("calendar"), tabName = "calendar"),
                menuItem("Coverage", icon = icon("signal"), tabName = "coverage"),
                menuItem("Submit Preferences", icon = icon("heart"), tabName = "preferences"),
                menuItem("Team Schedule", tabName = "team_schedule", icon = icon("download")
                )
                
                
    )
    
  ),
  dashboardBody(
    useShinyalert(), 
    useShinyjs(),
    
    tags$head(
      tags$style(
        HTML(".shiny-notification {
                   position:fixed;
                   top: calc(90%);
                   left: calc(0.5%);
                   }
                   "
        )
      )
    ),
    
    fluidPage(
      tabItems(
        tabItem(tabName = "calendar",
                
                box( h2("Filters"),
                     fluidRow(
                       column(6,selectInput("Agent", "Agent", Agent_names, selected = "Agent_1")),
                       column(6,selectInput("Month", "Month", c(1:12), selected = 1))
                       
                     ), 
                     collapsible = TRUE, width = 12, solidHeader = TRUE, status = "primary", title = "Set Filters"
                ),
                
                
                fluidRow(
                  
                  valueBoxOutput("D_1", width = 2),
                  valueBoxOutput("D_2", width = 2),
                  valueBoxOutput("D_3", width = 2),
                  valueBoxOutput("D_4", width = 2),
                  valueBoxOutput("D_5", width = 2),
                  valueBoxOutput("D_6", width = 2),
                  valueBoxOutput("D_7", width = 2),
                  valueBoxOutput("D_8", width = 2),
                  valueBoxOutput("D_9", width = 2),
                  valueBoxOutput("D_10", width = 2),
                  valueBoxOutput("D_11", width = 2),
                  valueBoxOutput("D_12", width = 2),
                  valueBoxOutput("D_13", width = 2),
                  valueBoxOutput("D_14", width = 2),
                  valueBoxOutput("D_15", width = 2),
                  valueBoxOutput("D_16", width = 2),
                  valueBoxOutput("D_17", width = 2),
                  valueBoxOutput("D_18", width = 2),
                  valueBoxOutput("D_19", width = 2),
                  valueBoxOutput("D_20", width = 2),
                  valueBoxOutput("D_21", width = 2),
                  valueBoxOutput("D_22", width = 2),
                  valueBoxOutput("D_23", width = 2),
                  valueBoxOutput("D_24", width = 2),
                  valueBoxOutput("D_25", width = 2),
                  valueBoxOutput("D_26", width = 2),
                  valueBoxOutput("D_27", width = 2),
                  valueBoxOutput("D_28", width = 2),
                  valueBoxOutput("D_29", width = 2),
                  valueBoxOutput("D_30", width = 2),
                  valueBoxOutput("D_31", width = 2)
                )
                
                
                
        ),
        #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------      
        tabItem(tabName = "coverage",
                fluidRow(
                  column(3,box( 
                    selectInput("Market2", "Market", market_names, selected = "Global"),
                    dateInput("startdate", "Start Date:",min = as.Date("2022/01/01"), max = as.Date("2022/12/24"),value = Sys.Date()),
                    # dateInput("startdate", "Start Date:",min = as.Date("2022/01/01"), max = as.Date("2022/12/24"),value = as.Date("2022/01/01")), 
                    collapsible = TRUE, width = 6, solidHeader = TRUE, status = "primary", title = "Set Filters"),),
                  column(3,box(
                    h2(textOutput("efficiency")),
                    collapsible = TRUE, width = 6, solidHeader = TRUE, status = "primary", title = "Overall Efficiency")),
                  column(3,box( 
                    h2(textOutput("forecasted_hours")),
                    collapsible = TRUE, width = 6, solidHeader = TRUE, status = "primary", title = "Required Hours")),
                  column(3,box( 
                    h2(textOutput("scheduled_hours")),
                    collapsible = TRUE, width = 6, solidHeader = TRUE, status = "primary", title = "Scheduled Hours"))
                  
                  
                ),
                
                
                
                box(tabBox(id = "tabset_projects",width = 12,
                           tabPanel("Combined", icon = icon("tasks"),textOutput("Combined"),
                                    fluidRow(
                                      column(6,plotlyOutput('day_1_1')),
                                      column(6,plotlyOutput('day_2_1'))
                                      
                                    ),
                                    fluidRow(
                                      column(4,plotlyOutput('day_3_1')),
                                      column(4,plotlyOutput('day_4_1')),
                                      column(4,plotlyOutput('day_5_1'))
                                      
                                    ),
                                    fluidRow(
                                      column(6,plotlyOutput('day_6_1')),
                                      column(6,plotlyOutput('day_7_1'))
                                      
                                    )
                                    
                           ),
                           
                           tabPanel("Phone", icon = icon("phone-alt"),textOutput("Phone"),
                                    fluidRow(
                                      column(6,plotlyOutput('day_1_2')),
                                      column(6,plotlyOutput('day_2_2'))
                                      
                                    ),
                                    fluidRow(
                                      column(4,plotlyOutput('day_3_2')),
                                      column(4,plotlyOutput('day_4_2')),
                                      column(4,plotlyOutput('day_5_2'))
                                      
                                    ),
                                    fluidRow(
                                      column(6,plotlyOutput('day_6_2')),
                                      column(6,plotlyOutput('day_7_2'))
                                      
                                    )
                                    
                           ),
                           tabPanel("Chat", icon = icon("tasks"),textOutput("Chat"),
                                    
                                    fluidRow(
                                      column(6,plotlyOutput('day_1_4')),
                                      column(6,plotlyOutput('day_2_4'))
                                      
                                    ),
                                    fluidRow(
                                      column(4,plotlyOutput('day_3_4')),
                                      column(4,plotlyOutput('day_4_4')),
                                      column(4,plotlyOutput('day_5_4'))
                                      
                                    ),
                                    fluidRow(
                                      column(6,plotlyOutput('day_6_4')),
                                      column(6,plotlyOutput('day_7_4'))
                                      
                                    )
                           ),
                           tabPanel("Email", icon = icon("tasks"),textOutput("Email"),
                                    fluidRow(
                                      column(6,plotlyOutput('day_1_3')),
                                      column(6,plotlyOutput('day_2_3'))
                                      
                                    ),
                                    fluidRow(
                                      column(4,plotlyOutput('day_3_3')),
                                      column(4,plotlyOutput('day_4_3')),
                                      column(4,plotlyOutput('day_5_3'))
                                      
                                    ),
                                    fluidRow(
                                      column(6,plotlyOutput('day_6_3')),
                                      column(6,plotlyOutput('day_7_3'))
                                      
                                    )
                                    
                                    
                           )
                ),
                title = "Daily Coverage",collapsible = TRUE, width = 12, solidHeader = TRUE, status = "primary")
                
                
        ),
        #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        tabItem(tabName = "preferences",
                fluidPage(
                  shinyjs::useShinyjs(),
                  shinyjs::inlineCSS(appCSS),
                  titlePanel("Please submit when ever your preferences change, your last submission will be considered"),
                  uiOutput("adminPanelContainer"),
                  div(
                    id = "form",
                    
                    textInput("name", labelMandatory("Full Name"), ""),
                    textInput("email", labelMandatory("Email Address"), ""),
                    selectInput("market", "Which Market do you primarily support?",
                                c("DK",  "NO/UK", "SE", "DE/CH","PL","ES","IT","BENE","FR","FI","Other")),
                    textInput("favourite_shift", labelMandatory("Favourite Shift (Please use this format: 17:00-22:00)")),
                    checkboxInput("any_nowork_interval", "I've intervals I can not work", FALSE),
                    textInput("nowork_intervals", labelMandatory("Please list if any"), ""),
                    actionButton("submit", "Submit", class = "btn-primary")
                  )
                ),
                
                div(id = "form",),
                shinyjs::hidden(
                  div(
                    id = "thankyou_msg",
                    h3("Thanks, your response was submitted successfully!"),
                    actionLink("submit_another", "Submit another response")
                  )
                ),  
                
                shinyjs::hidden(
                  span(id = "submit_msg", "Submitting..."),
                  div(id = "error",
                      div(br(), tags$b("Error: "), span(id = "error_msg"))
                  )
                )
                
                
                
        ),     
#..................................................................................................................................................................................................................
        
        tabItem(tabName = "team_schedule",
                
                fluidPage(sidebarLayout(
                  sidebarPanel(    
                    wellPanel(
                      h2("Filters"),
                      selectInput("Market3", "Market", market_names2, selected = "DK"),
                      br(),
                      dateInput("dateselect2", "Date",min = as.Date("2022/01/01"), max = as.Date("2022/12/24"),value = as.Date("2022/01/01")),
                      downloadButton("downloadBtn2", "Download schedule")
                    )        
                    
                  ),
                  
                  mainPanel(
                    
                    DT::dataTableOutput("hot")
                  )
                )
                )
                
        )
        
        
        
        
      )
    )
  )
)
