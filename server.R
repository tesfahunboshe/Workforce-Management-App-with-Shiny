shinyServer(function(input, output, session){ 
  
  observe({
    if (is.null(input$format) || input$format == "") {
      shinyjs::disable("report")
    } else {
      shinyjs::enable("report")
    }
  })
  
 
  ##------------------------------------------------------------------------------------------------------------------------------------------------------
  ##                                    Section for all the javascripts functions
  ##------------------------------------------------------------------------------------------------------------------------------------------------------
  # Custom renderer function to highlight the entire text in the column with specified color
  color_renderer <- "
  
      function(instance, td) {
      
        Handsontable.renderers.TextRenderer.apply(this, arguments);
        td.style.background = 'AQUA';
        
      }
    "
  
  
  # Custom renderer function to highlight the entire text in the column with specified color
  # Text Renderer
  
  text_renderer <-  "
          function(instance, td, row, col, prop, value, cellProperties) {
          
              if (instance.getData()[row][8] === 'Completed') {
                    td.style.background = 'AQUA';
                  } 
              else if(instance.getData()[row][8] === 'Delayed') {
                    td.style.background = 'RED';
              }
              else{
                  td.style.background = 'YELLOW';
              };
              Handsontable.renderers.TextRenderer.apply(this, arguments);
          }"
  
  text_renderer_tasks <-  "
          function(instance, td, row, col, prop, value, cellProperties) {
          
              if (instance.getData()[row][11] === 'Completed') {
                    td.style.background = 'AQUA';
                  } 
              else if(instance.getData()[row][11] === 'Delayed') {
                    td.style.background = 'RED';
              }
              else{
                  td.style.background = 'YELLOW';
              };
              Handsontable.renderers.TextRenderer.apply(this, arguments);
          }"
  

  ## Input variables
 
  
  dt_schedule <- reactive({ data_schedule })
  dt_coverage <- reactive({ data_coverage })
  dt_requirement <- reactive({ data_requirement })
  

  f_data_sch <- reactive({  # formula for data
    H <- input$Hero
    # Mk <- input$Market
    Mn <- input$Month
    dt_schedule()[dt_schedule()$Agent.name==H,][ , grepl( paste0("X",Mn,"_") , names( dt_schedule() ) ) ]
    

  })
  
  f_data_cov <- reactive({  # formula for data
    date_start <-length(seq(as.Date("2021/01/01"),as.Date(input$startdate),by = "day"))-1
    Phone_start <- date_start
    Phone_end <- date_start+7
    Email_start <- date_start + 365
    Email_end <- date_start + 365 + 7
    Chat_start <- date_start + 365 + 365
    Chat_end <- date_start + 365 + 365 +7
    dt_coverage()[dt_coverage()$Market==input$Market2,][,c(1,Phone_start:Phone_end,Email_start:Email_end,Chat_start:Chat_end)]


  })

  f_data_req <- reactive({  # formula for data
    date_start <-length(seq(as.Date("2021/01/01"),as.Date(input$startdate),by = "day"))-1
    Phone_start <- date_start
    Phone_end <- date_start+7
    Email_start <- date_start + 365
    Email_end <- date_start + 365 + 7
    Chat_start <- date_start + 365 + 365
    Chat_end <- date_start + 365 + 365 +7

    dt_requirement()[dt_requirement()$Market==input$Market2,][,c(1,Phone_start:Phone_end,Email_start:Email_end,Chat_start:Chat_end)]


  })



  ##---------------------------------------------------------------------------------------------------------------------------------------------------
  ##-----------------------------------------Schedule page-----------------------------------------------------------------------
  ##------------------------- code to populate the valueboxes on the home page------------------------------------------------------------------------
  output$D_1 <- renderValueBox({
    valueBox(value = tags$p(1, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,1]),paste0("Email: ", f_data_sch()[,32]),paste0("Chat:  ", f_data_sch()[,63]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_2 <- renderValueBox({
    valueBox(value = tags$p(2, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,2]),paste0("Email: ", f_data_sch()[,33]),paste0("Chat:  ", f_data_sch()[,64]))), icon = icon("stack-overflow"), color = "yellow")
  })

  output$D_3 <- renderValueBox({
    valueBox(value = tags$p(3, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,3]),paste0("Email: ", f_data_sch()[,34]),paste0("Chat:  ", f_data_sch()[,65]))), icon = icon("stack-overflow"), color = "yellow")
  })
  
  output$D_4 <- renderValueBox({
    valueBox(value = tags$p(4, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,4]),paste0("Email: ", f_data_sch()[,35]),paste0("Chat:  ", f_data_sch()[,66]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_5 <- renderValueBox({
    valueBox(value = tags$p(5, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,5]),paste0("Email: ", f_data_sch()[,36]),paste0("Chat:  ", f_data_sch()[,67]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_6 <- renderValueBox({
    valueBox(value = tags$p(6, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,6]),paste0("Email: ", f_data_sch()[,37]),paste0("Chat:  ", f_data_sch()[,68]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_7 <- renderValueBox({
    valueBox(value = tags$p(7, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,7]),paste0("Email: ", f_data_sch()[,38]),paste0("Chat:  ", f_data_sch()[,69]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_8 <- renderValueBox({
    valueBox(value = tags$p(8, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,8]),paste0("Email: ", f_data_sch()[,39]),paste0("Chat:  ", f_data_sch()[,70]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_9 <- renderValueBox({
    valueBox(value = tags$p(9, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,9]),paste0("Email: ", f_data_sch()[,40]),paste0("Chat:  ", f_data_sch()[,71]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_10 <- renderValueBox({
    valueBox(value = tags$p(10, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,10]),paste0("Email: ", f_data_sch()[,41]),paste0("Chat:  ", f_data_sch()[,72]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_11 <- renderValueBox({
    valueBox(value = tags$p(11, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,11]),paste0("Email: ", f_data_sch()[,42]),paste0("Chat:  ", f_data_sch()[,73]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_12 <- renderValueBox({
    valueBox(value = tags$p(12, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,12]),paste0("Email: ", f_data_sch()[,43]),paste0("Chat:  ", f_data_sch()[,74]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_13 <- renderValueBox({
    valueBox(value = tags$p(13, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,13]),paste0("Email: ", f_data_sch()[,44]),paste0("Chat:  ", f_data_sch()[,75]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_14 <- renderValueBox({
    valueBox(value = tags$p(14, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,14]),paste0("Email: ", f_data_sch()[,45]),paste0("Chat:  ", f_data_sch()[,76]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_15 <- renderValueBox({
    valueBox(value = tags$p(15, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,15]),paste0("Email: ", f_data_sch()[,46]),paste0("Chat:  ", f_data_sch()[,77]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_16 <- renderValueBox({
    valueBox(value = tags$p(16, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,16]),paste0("Email: ", f_data_sch()[,47]),paste0("Chat:  ", f_data_sch()[,78]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_17 <- renderValueBox({
    valueBox(value = tags$p(17, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,17]),paste0("Email: ", f_data_sch()[,48]),paste0("Chat:  ", f_data_sch()[,79]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_18 <- renderValueBox({
    valueBox(value = tags$p(18, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,18]),paste0("Email: ", f_data_sch()[,49]),paste0("Chat:  ", f_data_sch()[,80]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_19 <- renderValueBox({
    valueBox(value = tags$p(19, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,19]),paste0("Email: ", f_data_sch()[,50]),paste0("Chat:  ", f_data_sch()[,81]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_20 <- renderValueBox({
    valueBox(value = tags$p(20, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,20]),paste0("Email: ", f_data_sch()[,51]),paste0("Chat:  ", f_data_sch()[,82]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_21 <- renderValueBox({
    valueBox(value = tags$p(21, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,21]),paste0("Email: ", f_data_sch()[,52]),paste0("Chat:  ", f_data_sch()[,83]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_22 <- renderValueBox({
    valueBox(value = tags$p(22, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,22]),paste0("Email: ", f_data_sch()[,53]),paste0("Chat:  ", f_data_sch()[,84]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_23 <- renderValueBox({
    valueBox(value = tags$p(23, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,23]),paste0("Email: ", f_data_sch()[,54]),paste0("Chat:  ", f_data_sch()[,85]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_24 <- renderValueBox({
    valueBox(value = tags$p(24, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,24]),paste0("Email: ", f_data_sch()[,55]),paste0("Chat:  ", f_data_sch()[,86]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_25 <- renderValueBox({
    valueBox(value = tags$p(25, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,25]),paste0("Email: ", f_data_sch()[,56]),paste0("Chat:  ", f_data_sch()[,85]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_26 <- renderValueBox({
    valueBox(value = tags$p(26, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,26]),paste0("Email: ", f_data_sch()[,57]),paste0("Chat:  ", f_data_sch()[,88]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_27 <- renderValueBox({
    valueBox(value = tags$p(27, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,27]),paste0("Email: ", f_data_sch()[,58]),paste0("Chat:  ", f_data_sch()[,89]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_28 <- renderValueBox({
    valueBox(value = tags$p(28, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,28]),paste0("Email: ", f_data_sch()[,59]),paste0("Chat:  ", f_data_sch()[,90]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_29 <- renderValueBox({
    valueBox(value = tags$p(29, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,29]),paste0("Email: ", f_data_sch()[,60]),paste0("Chat:  ", f_data_sch()[,91]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_30 <- renderValueBox({
    valueBox(value = tags$p(30, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,30]),paste0("Email: ", f_data_sch()[,61]),paste0("Chat:  ", f_data_sch()[,92]))), icon = icon("stack-overflow"), color = "yellow")
  })
  output$D_31 <- renderValueBox({
    valueBox(value = tags$p(31, style = "font-size: 150%;"), 
             capture.output(cat(paste0(" Phone: ", f_data_sch()[,31]),paste0("Email: ", f_data_sch()[,62]),paste0("Chat:  ", f_data_sch()[,93]))), icon = icon("stack-overflow"), color = "yellow")
  })
  
  
 

 ##---------------------------------------------------------------------------------------------------------------------------------------------------
 ##-----------------------------------------Coverage----------------------------------------------------------------------------------------

  # plots combined
  output$day_1_1 <- renderPlotly({

    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,2]+f_data_cov()[,7+2]+f_data_cov()[,14+2],
                   shape = "Scheduled"),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,2]+f_data_req()[,7+2]+f_data_req()[,14+2],
                                        shape = "Forecast"),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")
      # + lims(y = c(0, 60))


  })
  output$day_2_1 <- renderPlotly({

    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,3]+f_data_cov()[,7+3]+f_data_cov()[,14+3]),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,3]+f_data_req()[,7+3]+f_data_req()[,14+3]),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")
    # + lims(y = c(0, 60))

  })
  output$day_3_1 <- renderPlotly({


    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,4]+f_data_cov()[,7+4]+f_data_cov()[,14+4]),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,4]+f_data_req()[,7+4]+f_data_req()[,14+4]),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")
    # + lims(y = c(0, 60))

  })
  output$day_4_1 <- renderPlotly({


    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,5]+f_data_cov()[,7+5]+f_data_cov()[,14+5]),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,5]+f_data_req()[,7+5]+f_data_req()[,14+5]),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")
    # + lims(y = c(0, 60))
  })
  output$day_5_1 <- renderPlotly({


    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,6]+f_data_cov()[,7+6]+f_data_cov()[,14+6]),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,6]+f_data_req()[,7+6]+f_data_req()[,14+6]),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")
    # + lims(y = c(0, 60))

  })
  output$day_6_1 <- renderPlotly({


    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,7]+f_data_cov()[,7+7]+f_data_cov()[,14+7]),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,7]+f_data_req()[,7+7]+f_data_req()[,14+7]),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")
    # + lims(y = c(0, 60))

  })
  output$day_7_1 <- renderPlotly({


    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,8]+f_data_cov()[,7+8]+f_data_cov()[,14+8]),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,8]+f_data_req()[,7+8]+f_data_req()[,14+8]),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")
    # + lims(y = c(0, 60))

  })

  # plots phone

  output$day_1_2 <- renderPlotly({
    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,2],
                   shape = "Scheduled"),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,2],
                                        shape = "Forecasted"),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")
    # + lims(y = c(0, 60))

  })
  output$day_2_2 <- renderPlotly({

    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,3]),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,3]),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")

  })
  output$day_3_2 <- renderPlotly({

    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,4]),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,4]),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")

  })
  output$day_4_2 <- renderPlotly({

    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,5]),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,5]),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")

  })
  output$day_5_2 <- renderPlotly({

    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,6]),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,6]),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")

  })
  output$day_6_2 <- renderPlotly({

    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,7]),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,7]),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")

  })
  output$day_7_2 <- renderPlotly({

    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,8]),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,8]),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")

  })

  # plots Email

  output$day_1_3 <- renderPlotly({

    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,7+2],
                   shape = "Scheduled"),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,7+2],
                                        shape = "Forecasted"),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")
    # + lims(y = c(0, 60))

  })
  output$day_2_3 <- renderPlotly({

    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,7+3]),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,7+3]),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")

  })
  output$day_3_3 <- renderPlotly({

    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,7+4]),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,7+4]),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")

  })
  output$day_4_3 <- renderPlotly({

    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,7+5]),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,7+5]),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")

  })
  output$day_5_3 <- renderPlotly({

    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,7+6]),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,7+6]),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")

  })
  output$day_6_3 <- renderPlotly({

    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,7+7]),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,7+7]),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")

  })
  output$day_7_3 <- renderPlotly({

    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,7+8]),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,7+8]),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")

  })

  # plots Chat

  output$day_1_4 <- renderPlotly({

    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,14+2],
                   shape = "Scheduled"),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,14+2],
                                        shape = "Forecasted"),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")
    # + lims(y = c(0, 60))

  })
  output$day_2_4 <- renderPlotly({

    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,14+3]),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,14+3]),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")

  })
  output$day_3_4 <- renderPlotly({

    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,14+4]),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,14+4]),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")

  })
  output$day_4_4 <- renderPlotly({

    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,14+5]),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,14+5]),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")

  })
  output$day_5_4 <- renderPlotly({

    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,14+6]),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,14+6]),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")

  })
  output$day_6_4 <- renderPlotly({

    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,14+7]),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,14+7]),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")

  })
  output$day_7_4 <- renderPlotly({

    ggplot(data = f_data_cov()) +
      geom_col(aes(x = Interval,
                   y = f_data_cov()[,14+8]),color = "#333333", fill = "#62c76b", width = 0.7)+
      # geom_bar(stat = "identity") +
      geom_line(data = f_data_req(),aes(x = Interval,
                                        y = f_data_req()[,14+8]),
                size = 2, color = "darkred")+
      scale_fill_hue() +
      labs(x="Time",y = "FTE")+
      theme_bw() +
      theme(legend.justification = "top")

  })


  

 ##---------------------------------------------------------------------------------------------------------------------------------------------------
 ##-----------------------------------------Submit preferences----------------------------------------------------------------------------------------
       
  
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  }) 
  
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
  })
  
  
  # Save Data
  saveData <- function(data) {
    fileName <- sprintf("%s_%s.csv",
                        humanTime(),
                        digest::digest(data))
    
    write.csv(x = data, file = file.path(responsesDir, fileName),
              row.names = FALSE, quote = TRUE)
    
    # entry <- data %>% as.list() %>% data.frame()
    # sheet_append(SHEET_ID, entry)
    
  }
  
  # action to take when submit button is pressed
  observeEvent(input$submit, {
    saveData(formData())
  })
  
  
  # action to take when submit button is pressed
  observeEvent(input$submit, {
    saveData(formData())
    shinyjs::reset("form")
    shinyjs::hide("form")
    shinyjs::show("thankyou_msg")
  })
  
  observeEvent(input$submit_another, { #Submit another response
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  })  
  
  
  observeEvent(input$submit, {
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    
    tryCatch({
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })
  })
  
  output$responsesTable <- DT::renderDataTable(
    loadData(),
    rownames = FALSE,
    options = list(searching = FALSE, lengthChange = FALSE)
  ) 
 
  output$downloadBtn <- downloadHandler(
    filename = function() { 
      sprintf("responses-form_%s.csv", humanTime())
    },
    content = function(file) {
      write.csv(loadData(), file, row.names = FALSE)
    }
  )
  
  output$adminPanelContainer <- renderUI({
    if (!isAdmin()) return()
    
    wellPanel(
      h2("Previous responses"),
      downloadButton("downloadBtn", "Download responses"), br(), br(),
      DT::dataTableOutput("responsesTable")
    )
  }) 
  
  # check if admin
  # isAdmin <- reactive({
  #   !is.null(session$user) && session$user %in% adminUsers
  # })
  
  isAdmin <- reactive({
    is.null(session$user) || session$user %in% adminUsers
  })
   
 ##---------------------------------------------------------------------------------------------------------------------------------------------------
       
 ##-----------------------------------------Download Schedule-----------------------------------------------------------------------------------------


  
  # DF <- f_data_sch2()
  Mn2 <- month(as.POSIXlt(Sys.Date(), format="%d/%m/%Y"))
  DF <- data_schedule[ , grepl( paste0("X",Mn2,"_") , names( data_schedule ) ) ]
  
  values <- reactiveValues()

  # Handsontable
  observe({
   
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      if (is.null(values[["DF"]]))
        DF <- DF
      else
        DF <- values[["DF"]]
    }
    values[["DF"]] <- DF
  })


  output$hot <- renderRHandsontable({
    DF <- values[["DF"]]
    if (!is.null(DF))
      rhandsontable(DF, useTypes = as.logical(FALSE), stretchH = "all")
  })

  # write.csv(f_data_sch2(), file = "YourSchedule.csv")

#   ## Save
  # observeEvent(input$save, {
  #   finalDF <- isolate(values[["DF"]])
  #   withProgress(message = 'Download in progress',
  #                detail = 'This may take a while...', value = 0, {
  #                  for (i in 1:50) {
  #                    incProgress(1/50)
  #                    Sys.sleep(0.01)
  #                  }
  #                  
  #                  owd <- setwd(tempdir())
  #                  on.exit(setwd(owd))
  #                  write.csv(finalDF, file=file.path(owd, sprintf("%s.csv", "YourSchedule")),row.names = FALSE, quote = TRUE)
  #                })
  #   
  #      })
  
  finalDF <- isolate(values[["DF"]])
  output$downloadBtn2 <- downloadHandler(
    
    filename = function() { 
      sprintf("team_schedule_%s.csv", humanTime())
    },
    content = function(file) {
      write.csv(finalDF, file, row.names = FALSE)
    }
  )

  
      
        
})


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Login manager














  

















