#--------- Shiny App: UI ---------####

shiny_ui <- fluidPage(
  titlePanel("CA Analysis Syrian Opposition Statements"),

  #--------- Side panel ---------####
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "axis_x",
        label = "Axis X:",
        min = 1,
        max = 25, # nrow(correspondence_analysis$eig),
        value = 1
      ),
      sliderInput(
        inputId = "axis_y",
        label = "Axis Y:",
        min = 1,
        max = 25, # nrow(correspondence_analysis$eig),
        value = 2
      ),
      checkboxGroupInput(
        "show_in_plots",
        "Show in Plot:",
        inline = "TRUE",
        choiceNames = list(
          "Observations", "Labels", "Interpretations", "Smoothed lines"
        ),
        choiceValues = list(
          "Observations", "Labels", "Interpretations", "Smoothed lines"
        ),
        selected = list(
          "Observations", "Labels", "Smoothed lines"
        )
      ),
      sliderInput(
        inputId = "textsize",
        label = "Text size plots:",
        min = 1,
        max = 100,
        value = 25
      ),
      selectInput(
        inputId = "language",
        label = "Language plots:",
        choices = c("English", "Arabic")
      ),
      selectInput(
        inputId = "selection_criteria",
        label = "Labels plotted on maximum:",
        choices = c(
          "Contribution (axis X and Y)", "Contribution (axis X only)", "Inertia"
        )
      ),
      sliderInput(
        inputId = "number_words",
        label = "Number of words:",
        min = 0,
        max = nrow(correspondence_analysis$row$coord),
        value = 40
      ),
      sliderInput(
        inputId = "number_docs",
        label = "Number of documents:",
        min = 0,
        max = nrow(correspondence_analysis$col$coord),
        value = 20
      ),
      dateRangeInput(
        "selected_dates",
        "Date range:",
        start  = "2011-03-01",
        end = "2020-01-31",
        min = "2010-03-01",
        max = "2020-03-01",
        format = "dd/mm/yyyy",
        separator = " - "
      )
    ),

    #--------- Main panel ---------####
    mainPanel(
        #--------- Main panel: CA ---------####
          tabsetPanel(
            tabPanel(
              "Corpus",
              h3("Overview"),
              h5("Original"),
              tableOutput("corpus_original"),
              h5("Culled"),
              tableOutput("corpus_culled"),
              # hr(),
              # tableOutput("corpus_names"),
              h3("Size documents culled corpus"),
              plotOutput(
                outputId = "histogram_plot"
              )
            ),
            tabPanel(
              "Eigenvalues",
              h3("Eigenvalues"),
              fluidRow(
                column(
                  5, # Output: scatter plot
                  plotOutput(outputId = "scree_plot")
                ),
                column(
                  7,
                  tableOutput("table_eigenvalues")
                )
              )
            ),
            tabPanel(
              "Table Words CA",
              h3(
                textOutput("selected_axis")
              ),
              tableOutput("table_words")
            ),
            tabPanel(
              "Table Docs CA",
              h3(
                textOutput("selected_axis1")
              ),
              tableOutput("table_docs")
            ),
            tabPanel(
              "CA Simultaneous representation",
              plotOutput("scatter_plot")
            )
          )
        ) #,
# 
#         #--------- Main panel: SupCA ---------####
#         tabPanel(
#           "Supplementary CA",
#           br(),
#           tabsetPanel(
#             tabPanel(
#               "Overview Corpora",
#               fluidRow(
#                 column(
#                   5, # Output: scatter plot
#                   h3("Original"),
#                   tableOutput("corpus_sups")
#                 ), ####
#                 column(
#                   5,
#                   h3("Culled"),
#                   tableOutput("corpus_sups_culled")
#                 )
#               )
#             ),
#             tabPanel(
#               "Table Docs SupCA",
#               h3(
#                 textOutput("selected_axis_sups")
#               ),
#               tableOutput("table_suppl_docs")
#             ),
#             tabPanel("SupCa Simultaneous",
#               plotOutput("scatter_plot_suppl"),
#               height = 700
#             ),
#             tabPanel(
#               "SupCa Chrononological",
#               fluidRow(
#                 column(
#                   width = 12,
#                   plotOutput(
#                     "chronological_plot",
#                     height = 700,
#                     click = "chronological_plot_click",
#                     brush = brushOpts(id = "chronological_plot_brush")
#                   )
#                 )
#               ),
#               fluidRow(
#                 column(
#                   width = 6,
#                   h4("Supplementary Statements near click"),
#                   verbatimTextOutput("click_info")
#                 ),
#                 column(
#                   width = 6,
#                   h4("Brushed Supplementary Statements"),
#                   verbatimTextOutput("brush_info")
#                )
#              )
#            )
#          )
#        )
      )
    )
