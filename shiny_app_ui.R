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
      h4("Options CA plot"), # CA or also cluster plot?  
      selectInput(
        inputId = "language",
        label = "Language:",
        choices = c("English", "Arabic")
      ),
      selectInput(
        inputId = "selection_criteria",
        label = "Labels included on basis of:",
        choices = c(
          "Contribution (axis X and Y)", "Contribution (axis X only)", "Inertia"
        )
      ),
      sliderInput(
        inputId = "number_words",
        label = "Number of words plotted:",
        min = 0,
        max = nrow(correspondence_analysis$row$coord),
        value = 40
      ),
      sliderInput(
        inputId = "number_docs",
        label = "Number of documents plotted:",
        min = 0,
        max = nrow(correspondence_analysis$col$coord),
        value = 20
      ),
      sliderInput(
        inputId = "textsize",
        label = "Text size plot:",
        min = 1,
        max = 100,
        value = 25
      )
    ),

    #--------- Main panel ---------####
    mainPanel(
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
    ) 
  )
)
