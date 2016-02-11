# Define background calculations for shiny application
shinyServer(function(input, output, session) {
  cat("top of shinyServer\n")
  # Get the URL variable which file to be analyzed 
  ## If no variable is given, create the drop down menu
  output$queryText <- renderText({
    if(session$clientData$url_search == '') {
      query <- input$selectYear
    } else {
      query <- parseQueryString(session$clientData$url_search)
    }
    
    # Return a string with key-value pairs
    paste(query)
  })
  
  # Load the data from database
  nfl.df <- reactive({
    validate(need(input$selectYear, message = FALSE))

    nfl.start <- paste0(as.numeric(input$selectYear), "-01-01")
    nfl.end <- paste0(as.numeric(input$selectYear) + 1, "-01-01")
    nfl.url <- paste0(parseQueryString(session$clientData$url_search), "-01-01")

    if(session$clientData$url_search == '') {
      query <- nfl %>%
        select(Week, Date, Away, Home, PtsA, PtsH, YdsA, YdsH, TOA, TOH) %>%
        filter(Date >= nfl.start,
               Date < nfl.end) %>%
        filter(!is.na(Week)) %>%
        collect()
    } else {
      query <- nfl %>%
        filter(Date >= nfl.url,
               Date < nfl.url + 1) %>%
        collect()
    }
  })
  
  # Data preperation
  ## Get a subset of the data set according to the selected teams
  nfl.df.sub <- reactive({
    nfl.df.sub <- nfl.df() %>%
      filter(Home %in% input$selectTeam | Away %in% input$selectTeam)
  })
  
  ## Get only the away games of the selected teams
  nfl.df.sub.a <- reactive({
    nfl.df.sub.a <- nfl.df.sub() %>%
      filter(Away %in% input$selectTeam) %>%
      select(Date, Away, PtsA, YdsA, TOA)
  })
  
  ## Get only the home games of the selected teams
  nfl.df.sub.h <- reactive({
    nfl.df.sub.h <- nfl.df.sub() %>%
      filter(Home %in% input$selectTeam) %>%
      select(Date, Home, PtsH, YdsH, TOH)
  })
  
  ## Get the combined games of the selected teams
  ### Get and rename home games header to combine later 
  nfl.df.sub.c <- reactive({
    nfl.df.sub.h <- nfl.df.sub.h() %>%
      rename(Team = Home,
             Pts = PtsH,
             Yds = YdsH,
             TO = TOH)
  ### Get and rename home games header to combine later  
  nfl.df.sub.a <- nfl.df.sub.a() %>%
    rename(Team = Away,
           Pts = PtsA,
           Yds = YdsA,
           TO = TOA)
  
  ### Combine all home and away games  
  nfl.df.sub.c <- data.frame(rbind(nfl.df.sub.h,nfl.df.sub.a))
  })
  
  # Analytics
  ## Create the data set summary of the selected teams
  ### Summary for away games
  output$summaryA <- renderTable({
    nfl.df.sub.a <- nfl.df.sub.a() %>%
      group_by(Away) %>%
      summarise(Games = length(unique(Date)), # Number of games
                Min = min(PtsA), # Minimum valus
                Q1 = quantile(PtsA, na.rm = TRUE, names = FALSE)[2], # First quntiel
                Median = round(median(PtsA, na.rm = TRUE),2), # Median
                Q3 = quantile(PtsA, na.rm = TRUE, names = FALSE)[4], # Third quntiel
                Max = max(PtsA), # Maximum value
                Diff = (max(PtsA, na.rm = TRUE) - min(PtsA, na.rm = TRUE)), # Difference between min and max
                Mean = round(mean(PtsA, na.rm = TRUE),1), # Mean
                SD = sd(PtsA, na.rm = FALSE) # Standard Deviation
                )
  })
  
  ### Summary for home games
  output$summaryH <- renderTable({
    nfl.df.sub.h <- nfl.df.sub.h() %>%
      group_by(Home) %>%
      summarise(Games = length(unique(Date)), # Number of games
                Min = min(PtsH), # Minimum valus
                Q1 = quantile(PtsH, na.rm = TRUE, names = FALSE)[2], # First quntiel
                Median = round(median(PtsH, na.rm = TRUE),2), # Median
                Q3 = quantile(PtsH, na.rm = TRUE, names = FALSE)[4], # Third quntiel
                Max = max(PtsH), # Maximum value
                Diff = (max(PtsH, na.rm = TRUE) - min(PtsH, na.rm = TRUE)), # Difference between min and max
                Mean = round(mean(PtsH, na.rm = TRUE),1), # Mean
                SD = sd(PtsH, na.rm = FALSE) # Standard Deviation
    )
  })
  
  ### Summary for combined games
  output$summaryC <- renderTable({
    nfl.df.sub.c <- nfl.df.sub.c() %>%
      group_by(Team) %>%
      summarise(Games = length(unique(Date)), # Number of games
                Min = min(Pts), # Minimum valus
                Q1 = quantile(Pts, na.rm = TRUE, names = FALSE)[2], # First quntiel
                Median = round(median(Pts, na.rm = TRUE),2), # Median
                Q3 = quantile(Pts, na.rm = TRUE, names = FALSE)[4], # Third quntiel
                Max = max(Pts), # Maximum value
                Diff = (max(Pts, na.rm = TRUE) - min(Pts, na.rm = TRUE)), # Difference between min and max
                Mean = round(mean(Pts, na.rm = TRUE),1), # Mean
                SD = sd(Pts, na.rm = FALSE) # Standard Deviation
      )
  })
  
  # Plots
  ## Create a time series plot of the selected data
  output$plot <- renderPlot({
    ### Define variables and meta indicators
    #y.axis.desc <- subset(hdx.desc()$Description, hdx.desc()$Attribute == 'Units') # Get the unit definition
    title.desc <- paste0("Team performance over time in ", as.numeric(input$selectYear)) # Title based on year selection
    
    #### Create plot
    p <- ggplot(nfl.df.sub.c(), aes(x = Date, y = Pts, colour = factor(Team), group = Team)) + # Plot definition
      geom_point(size = 3) + # Add points
      geom_line() + # Add line
      labs(x = NULL, fill = NULL) + # Define axis
      labs(title = title.desc) + # Define title
      
      ##### Design the plot
      guides(colour = guide_legend(title = 'Team', ncol = 3)) +
      theme(plot.title = element_text(size = rel(1.5), vjust=3),
            axis.title = element_text(size = rel(1.2)),
            axis.text.x = element_text(size = rel(1.2), angle = 90, hjust = 1),
            axis.text.y = element_text(size = rel(1.2)),
            legend.position = "bottom",
            legend.text = element_text(size = rel(1.2)),
            legend.title = element_text(size = rel(1.2))
      )
    
    #### Return plot
    print(p)
  })
  
  # Data Tables
  ## Create a data table of the selected contries
  output$table <- renderDataTable({
    
    ### Data set to be displayed
    nfl.df.sub()},
    
    ### Define the functionality of the data table
    options = list(columns.searchable = TRUE, # Is searchable
                   orderClasses = TRUE, # Can set classes
                   lengthMenu = list(c(10, 25, -1), c('10', '25', 'All')), # Define the possible length of the table
                   pageLength = 25) # Set standard lenght
  )
  
  # Create download handler
  output$downloadData <- downloadHandler(
    
    ## This function returns a string which tells the client
    ## browser what name to use when saving the file.
    filename = function() {
      paste(paste('NFL_Data', input$selectYear,sep = "_"), input$filetype, sep = ".")
    },
    
    ## This function should write data to a file given to it by
    ## the argument 'file'.
    content = function(file) {
      ### For Excel we need some different settings
      if(input$filetype == "xls") {
        wb = loadWorkbook(file, create = TRUE)
        sheet = input$selectYear
        createSheet(wb, name = sheet)
        setStyleAction(wb, XLC$"STYLE_ACTION.DATA_FORMAT_ONLY")
        #setDataFormatForType(wb, type = XLC$"DATA_TYPE.NUMERIC", format = "0.0000")
        writeWorksheet(wb, data = as.data.frame(nfl.df.sub()), sheet = sheet, startRow = 1, startCol = 1, header = TRUE)
        saveWorkbook(wb)
      } else {
        ### Settings for text files
        sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
        
        ### Write to a file specified by the 'file' argument
        write.table(nfl.df.sub(), file, sep = sep, row.names = FALSE)
      }
    }
  )
  
  # Menus
  ## Create a menu to select the years to be loaded
  output$selectYear <- renderUI({
    if(session$clientData$url_search == '') {
      selectInput("selectYear", "Choose a year:", nfl.years, selected = "2014")
    }
  })
  
  ## Create a menu to select teams
  output$selectTeam <- renderUI({
    nfl.teams <- as.character(unique(nfl.df()$Home))
    selectInput("selectTeam", "Please select a team", nfl.teams, multiple = TRUE)
  })
  
  ## Create a menu to select visulized variable in graph
  output$selectVar <- renderUI({
    nfl.vars <- as.character(names(nfl.df.sub.c())[c(-1,-2)])
    selectInput("selectVar", "Please select a variable", nfl.vars, selected = "Pts")
  })
})
