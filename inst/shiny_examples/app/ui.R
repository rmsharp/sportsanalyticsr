shinyUI(
  fluidPage (
    titlePanel(HTML("Sports Analytics Explorer <small>(beta version)</small>")),

    fluidRow(
      # Side panel definition
      column(3,
        wellPanel(
          # Short description of the application
          helpText(HTML("This Webapplication gives you the opportunity to explore the NFL data by teams from 1966 to 2014")),
          br(),
               
          # Drop down menu, defined in server.R, to select year to be analyzed. Only shown if no file variable were given in URL
          uiOutput("selectYear"),
               
          # Team selector, defined in server.R, to select the teams that should be analyzed
          helpText("Please select one or more teams that should be analyzed"),
          uiOutput("selectTeam"),
          br(),
               
          # Download handler to download the data from the selected teams
          helpText("You can download the selected subset of the data set. For this choose the desired file extension and press the download button."),
          selectInput("filetype", "File type:",
                      c("CSV" = "csv",
                        "TSV" = "tsv",
                        "Excel" = "xls"
                        )
          ),
          downloadButton('downloadData', 'Download')
        )
      ),
      
      # Main panel definition
      column(9,
        #h3("Loaded File"),
             
        # Display the xls file that is beeing analyzed
        #verbatimTextOutput("queryText"),
        tabsetPanel(
          # Tab with data description of the individual procedures
          tabPanel("Data Description", tableOutput("desc"),

            # Style the description and summary table
            tags$head(
              tags$style(
                type="text/css",
                "#desc table{
                  border: 0px !important;
                }
                #desc th {
                  font-size:1.1em;
                  text-align:left;
                  padding-top:5px;
                  padding-bottom:4px;
                }
                #desc table, th, tr, td {
                  border: 0px !important;
                }
                #desc tr td:first-child {
                  width: 10em;
                  font-weight:bold
                }
                #summary table{
                  border: 0px !important;
                }
                #summary th {
                  font-size:1.1em;
                  text-align:right;
                  padding-top:5px;
                  padding-bottom:4px;
                }
                #summary table, th, tr, td {
                  border: 0px !important;
                }
                #summary tr td:first-child {
                  font-weight:bold
                }"
              )
            )
          ),
                 
          # Tab with the graphical representation of the selected teams
          tabPanel("Graphic",
                   br(),
                   plotOutput("plot")
                   #br(),
                   #helpText("Please select variable to be visualized"),
                   #uiOutput("selectVar")
          ),
                         
          # Tab with basic descriptive statistics of the selected teams
          tabPanel("Statistical Summary",
                   h4("Away Statistics"),
                   tableOutput("summaryA"),
                   h4("Home Statistics"),
                   tableOutput("summaryH"),
                   h4("Overall Statistics"),
                   tableOutput("summaryC"),
                   div("Notice: For the calculations, the NA cells were removed.")
          ),
                         
          # Tab with the data of the selected teams
          tabPanel("Data Table", dataTableOutput("table"),
            tags$head(
              tags$style(
                type="text/css",
                "tfoot {
                  display: table-header-group;
                }
                .dataTables_filter {
                  display: none;
                }"
              )
            )
          )
        )
      )
    ),
    
    # Copyright
    fluidRow(
      HTML("This Webapplication was developed by <a href='http://www.kalisch.biz'>Dr. Dominik Kalisch</a>.
            The source code can be found on github:
            <a xmlns:cc='http://creativecommons.org/ns#'
           href='https://github.com/dkalisch/SportsAnalytics'
           property='cc:attributionName'
           rel='cc:attributionURL'>https://github.com/dkalisch/SportsAnalytics</a>
            <a rel='license' href='http://creativecommons.org/licenses/by-nc-sa/3.0/deed.en'>
              <img alt='Creative Commons License'
              style='border-width:0;
              margin-right:5px;
              margin-left:1px;
              vertical-align:top'
              src='http://i.creativecommons.org/l/by-nc-sa/3.0/80x15.png'/>
            </a>"
           )
    )
  )
)
