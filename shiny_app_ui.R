#--------- Shiny App: UI ---------####

shiny_ui <- fluidPage(
  titlePanel(
    h2("Political Islam as Discursive Practice: Reproducing Results", align = "center")
  ),
  
    tabsetPanel(
      #--------- Tab: Correspondence Analysis ---------####
      tabPanel("Introduction",
               br(), 
               includeMarkdown("./introduction.md")
               ),
      tabPanel("Corpus",
               br(), 
               sidebarLayout(
                 sidebarPanel(
                   sliderInput(
                     inputId = "v_min",
                     label = "Threshold for occurence term in corpus:",
                     min = 1,
                     max = 100, # max() -- length hieracrhical index? 
                     value = 20
                     )
                   ),
                 mainPanel(
                   h3("Overview"),
                   h5("Original"),
                   tableOutput("corpus_original"),
                   h5("Culled"),
                   tableOutput("corpus_culled"),
                   h3("Included Documents"),
                   tableOutput("corpus_names"),
                   h3("Size documents culled corpus"),
                   plotOutput(
                     outputId = "histogram_plot"
                     )
                   )
                 )
               ), 
      tabPanel("Correspondence Analysis",
               br(), 
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
                   selectInput(
                     inputId = "language",
                     label = "Language:",
                     choices = c("English", "Arabic")
                     ),
                   selectInput(
                     inputId = "selection_criteria",
                     label = "Labels included in plot on basis of:",
                     choices = c(
                       "Contribution (axis X and Y)", "Contribution (axis X only)", "Inertia"
                       ), 
                     selected = "Contribution (axis X only)"
                     ),
                   sliderInput(
                     inputId = "number_words",
                     label = "Number of words in plot:",
                     min = 0,
                     max = 50, # nrow(correspondence_analysis$row$coord),
                     value = 40
                     ),
                   sliderInput(
                     inputId = "number_docs",
                     label = "Number of documents in plot:",
                     min = 0,
                     max = 50, # nrow(correspondence_analysis$col$coord),
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
                 mainPanel(
                   tabsetPanel(
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
               ), 
      tabPanel("Hierarchical Clustering",
               br(), 
               sidebarLayout(
                 sidebarPanel(
                   selectInput(inputId = "cluster_method", 
                               label = "Select clustering method:", 
                               choices = c("euclidean", "manhattan")
                               ), 
                   sliderInput(inputId = "nb_clust", 
                               label = "Number of clusters:", 
                               min = 1, 
                               max = 6, 
                               value = 3, 
                               step = 1
                               ),
                  sliderInput(
                       inputId = "axis_x_clust",
                       label = "Axis X:",
                       min = 1,
                       max = 25, # nrow(correspondence_analysis$eig),
                       value = 1
                   ),
                   sliderInput(
                     inputId = "axis_y_clust",
                     label = "Axis Y:",
                     min = 1,
                     max = 25, # nrow(correspondence_analysis$eig),
                     value = 2
                   ),
                   selectInput(
                     inputId = "language_clust",
                     label = "Language:",
                     choices = c("English", "Arabic")
                   ),
                   selectInput(
                     inputId = "selection_criteria_clust",
                     label = "Labels included in plot on basis of:",
                     choices = c(
                       "Contribution (axis X and Y)", "Contribution (axis X only)", "Inertia"
                     ), 
                     selected = "Contribution (axis X only)"
                   ),
                   sliderInput(
                     inputId = "number_words_clust",
                     label = "Number of words in plot:",
                     min = 0,
                     max = 50, # nrow(correspondence_analysis$row$coord),
                     value = 40
                   ),
                   sliderInput(
                     inputId = "textsize_clust",
                     label = "Text size plot:",
                     min = 1,
                     max = 100,
                     value = 25
                   ),
                   uiOutput("select_cluster"),
                   uiOutput("select_text_options")
                   ),
                 mainPanel(
                   tabsetPanel( 
                     tabPanel("Dendogram",
                              plotOutput("dendroPlot")
                     ),
                     tabPanel("Descriptions HC Words",
                              h3(
                              textOutput("selected_cluster_words")
                                ),
                              #tableOutput("table.cluster.typical.words"), 
                              tableOutput("table_cluster_words")
                     ),
                     tabPanel("Descriptions HC Texts",
                              h3(
                               textOutput("selected_cluster_docs")
                               ),
                              tableOutput("table_cluster_docs"),
                     ),  
                     tabPanel("Descriptions HC Principle Axes",
                              h3(
                                textOutput("selected_cluster_axes")
                               ),
                              tableOutput("table_cluster_axes")
                     ),  
                     tabPanel("CA cluster plot",
                              plotOutput("clusterCA_Plot")
                     ), 
                     tabPanel("Narrative Selected Text",
                              h3(textOutput("selected_text_output")),
                              tableOutput("selected_text_table"),
                              tableOutput("selected_text_narrative")
                     )
                   )
                 )
               )
      ),
      tabPanel("Words in Context",
               br(), 
               sidebarLayout(
                 sidebarPanel(
                   uiOutput("options_words"),
                   sliderInput(
                     inputId = "range_pre",
                     label = "Range right:",
                     min = 1,
                     max = 25,
                     value = 5
                   ), 
                   sliderInput(
                     inputId = "range_post",
                     label = "Range left:",
                     min = 1,
                     max = 25,
                     value = 5
                   )
                 ), 
                 mainPanel(
                   h3(
                     textOutput("selected_word")
                   ),
                   h3("Original sentences"),
                   tableOutput("words_in_context")
                 )
               )
               )
      )
  )
      



# -------------- End --------- ####
