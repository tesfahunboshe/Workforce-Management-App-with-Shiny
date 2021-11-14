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
                  title = "MIINTO WFM",dropdownMenuOutput("msgOutput")
              ),
  
  
  dashboardSidebar(
        sidebarMenu(id= "tabs", width = 350,
                    menuItem("Schedule", icon = icon("calendar"), tabName = "calendar"),
                    menuItem("Coverage", icon = icon("signal"), tabName = "coverage"),
                    menuItem("Submit Preferences", icon = icon("heart"), tabName = "preferences"),
                    menuItem("Help", icon = icon("hands-helping"), tabName = "help"),
                    menuItem("Download Schedule", tabName = "download_schedule", icon = icon("download")
                             # , 
                             # radioButtons('format', 'Document format', c('PDF', 'Word'),inline = FALSE, selected = 1),
                             # downloadButton("report", "Download", class = "butt"),
                             # tags$head(tags$style(".butt{color: blue !important;}"))
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
                              column(6,selectInput("Hero", "Hero", Agent_names, selected = "Agent_1")),
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
                
                box( h2("Filters"),
                     selectInput("Market2", "Market", market_names, selected = "Global"),
                     dateInput("startdate", "Date range:",min = as.Date("2021/01/01"), max = Sys.Date()+45), 
                     collapsible = TRUE, width = 4, solidHeader = TRUE, status = "primary", title = "Set Filters"),
                
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
                                  )
                           ),
                    title = "Project Details",collapsible = TRUE, width = 12, solidHeader = TRUE, status = "primary")


                 ),


    
  #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------      
      tabItem(tabName = "help",
          box(tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/T1-k7VYwsHg", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
              )
          
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
                  textInput("favourite_shift", labelMandatory("Favourite Shift")),
                  checkboxInput("used_shiny", "I've intervals I can not work", FALSE),
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
 
 tabItem(tabName = "download_schedule",
         
         fluidPage(sidebarLayout(
                      sidebarPanel(    
                              wellPanel(
                                  h2("Filters"),
                                  selectInput("Market3", "Market", market_names, selected = "Global"),
                                  selectInput("Month2", "Month", c(1:12), selected = 1),  
                                  h3("Save"), 
                                  actionButton("save", "Save table")
                                     )        
               
                             ),
             
                   mainPanel(
                     
                     rHandsontableOutput("hot")
                            )
                       )
            )
         
      )
  
  
 
    
      )
    )
  )
)

