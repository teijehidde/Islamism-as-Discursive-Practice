# SHINY APP ####
# Define UI for app that draws a histogram  
ui <- fluidPage(
  
  # App title  
  titlePanel("CA Analysis Syrian Opposition Statements"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "axisX", label = "Axis X:", min = 1, max = nrow(CA$eig), value = 1), 
      sliderInput(inputId = "axisY", label = "Axis Y:", min = 1, max = nrow(CA$eig), value = 2), 
      h4("CA plot"),
      checkboxGroupInput("showInPlots", "Show in Plot:", inline = "TRUE", 
                         choiceNames = list("Observations", "Labels", "Interpretations", "Smoothed lines"),
                         choiceValues = list("Observations", "Labels", "Interpretations", "Smoothed lines"), 
                         selected = list("Observations", "Labels", "Smoothed lines")),
      sliderInput(inputId = "textsize", label = "Text size plots:", min = 1, max = 100, value = 25),
      selectInput(inputId = "language", label = "Language plots:", choices = c("English", "Arabic")),
      selectInput(inputId = "selection.criteria", label = "Labels plotted on maximum:", choices = c("Contribution (axis X and Y)", "Contribution (axis X only)", "Inertia")),
      sliderInput(inputId = "numberWords", label = "Number of words:", min = 0, max = nrow(CA$row$coord), value = 40), 
      sliderInput(inputId = "numberDocs", label = "Number of documents:", min = 0, max = nrow(CA$col$coord), value = 20),
      h4("Chronology plot"),
      checkboxGroupInput("SelectedOrgs", "Choose Organizations:", inline = "TRUE", 
                         choiceNames = list("General", "SMB", "AaS", "NF", "JFS", "HTS", "NFL"),
                         choiceValues = list("General","SMB", "AaS", "NF", "JFS", "HTS", "NFL"), 
                         selected = c("SMB", "AaS", "HTS")),
      dateRangeInput("selectedDates", "Date range:", start  = "2011-03-01", end = "2020-01-31", min = "2010-03-01", max = "2020-03-01", format = "dd/mm/yyyy", separator = " - ")
    ),
    mainPanel(
      tabsetPanel(     
        tabPanel("Correspondence Analysis",
                 br(), 
                 tabsetPanel(
                   tabPanel("corpus", 
                            h3("Overview"),
                            
                            h5("Original"),
                            tableOutput("corpus.original"), 
                            h5("Culled"),
                            tableOutput("corpus.culled"), 
                            hr(),
                            tableOutput("corpus.names"), 
                            h3("Size documents culled corpus"),
                            plotOutput(outputId = "histogramPlot")
                   ), 
                   tabPanel("Eigenvalues",
                             h3("Eigenvalues"),
                             fluidRow(column(5, # Output: scatter plot
                                             plotOutput(outputId = "screePlot")),
                                      column(7,
                                             tableOutput("table.eigenvalues")))
                             ),
                    tabPanel("Table Words CA",
                             h3(textOutput("selected_axis")), 
                             tableOutput("table.words")),
                    tabPanel("Table Docs CA",
                             h3(textOutput("selected_axis1")),
                             tableOutput("table.docs")),
                    tabPanel("CA Simultaneous representation",
                             plotOutput("scatterPlot"))
          )),
             tabPanel("Supplementary CA", 
                      br(), 
                      tabsetPanel(  
                        tabPanel("Overview Corpora", 
                        fluidRow(column(5, # Output: scatter plot
                                        h3("Original"),
                                        tableOutput("corpus_sups")), #### 
                                 column(5,
                                        h3("Culled"),
                                        tableOutput("corpus_sups.culled")))),
                        tabPanel("Table Docs SupCA",
                                 h3(textOutput("selected_axis_sups")), 
                                 tableOutput("table.sup.docs")),
                        tabPanel("SupCa Simultaneous",
                                  plotOutput("scatterPlot.sup"), height = 700),
                        tabPanel("SupCa Chrononological",
                                 fluidRow(
                                   column(width = 12,
                                          plotOutput("chronologicalPlot", height = 700,
                                                     click = "chronologicalPlot_click",
                                                     brush = brushOpts(id = "chronologicalPlot_brush")
                                                    )
                                          )
                                   ),
                                 fluidRow(
                                   column(width = 6,
                                          h4("Supplementary Statements near click"),
                                          verbatimTextOutput("click_info")
                                   ),
                                   column(width = 6,
                                          h4("Brushed Supplementary Statements"),
                                          verbatimTextOutput("brush_info")
                                   )))
            
            #                     tableOutput("table.events")),
             ))

            
      ))))

server <- function(input, output, session) {
  
  # Table Overview Original corpus
  output$corpus.original <- renderTable({
    data.frame(
      Documents = length(corpus$Filenames), 
      Number.occurences.T = length(unlist(corpus$stemmed_segments)),  
      Number.words.V = length(TM$V.originalcorpus), 
      Avg.T.per.V = length(unlist(corpus$stemmed_segments)) / length(TM$V.originalcorpus))
  }, striped = TRUE, hover = TRUE, bordered  = TRUE, digits = 0)
  
  # Table Overview Culled corpus
  output$corpus.culled <- renderTable({
    data.frame(
      Documents = length(corpus$Filenames), 
      Number.occurences.T = sum(TM$LexicalTable), 
      Number.words.V = length(TM$V.culledcorpus), 
      Avg.T.per.V = sum(TM$LexicalTable) / length(TM$V.culledcorpus)) 
  }, striped = TRUE, hover = TRUE, bordered  = TRUE, digits = 0)
  
  # Table Overview Names Organizations and label documents 
  output$corpus.names <- renderTable ({
    data.frame(
      No. = corpus$additional_data$organization_metadata$No,
      Name.Ar = corpus$additional_data$organization_metadata$Organization.Arabic, 
      Name.Eng = corpus$additional_data$organization_metadata$Organization.English,
      Charter = corpus$additional_data$document_metadata$Label[match(corpus$additional_data$organization_metadata$Charter, corpus$additional_data$document_metadata$Document)], 
      Self.Descr = corpus$additional_data$document_metadata$Label[match(corpus$additional_data$organization_metadata$Who.Are.we, corpus$additional_data$document_metadata$Document)])
  }, striped = TRUE, hover = TRUE, bordered  = TRUE, digits = 0)
  
  # Histogram length statements ####  
  output$histogramPlot <- renderPlot ({
    hist(colSums(TM$LexicalTable), breaks = 20, xlab = "Total Occurences", ylab = "Number of Documents", main = NULL)
  })
  
  # Screeplot ####
  ScreePlot <- data.frame(
    Variance = CA$eig[, 2],
    Axis = seq(1:nrow(CA$eig))
  )
  
  output$screePlot <- renderPlot({
    ggplot(data = ScreePlot, aes(x = Axis, y = Variance)) +  # intitial plot
      geom_point(size = 2, shape = 1) + geom_line() +
      theme_bw() +
      # scale_shape_discrete(solid=F) +
      guides(size = FALSE, solid = FALSE) +
      theme(panel.grid.minor = element_line(linetype = "solid", size = .1)) +
      theme(panel.grid.major = element_line(linetype = "solid", size = .1)) +
      labs(x = "Axis", y = "Variance") 
    } # ,  width = 4000, height = 4000, res = 600
    )
  
  # Table Eigenvalues ####
  Table.Eigenvalues <- data.frame(CA$eig[] )
  colnames(Table.Eigenvalues) <- c("Eigenvalue", "Variance", "Cumulative")
  rownames(Table.Eigenvalues) <- paste0("Axis ", 1:nrow(Table.Eigenvalues), sep = " ")
  output$table.eigenvalues <- renderTable({ Table.Eigenvalues }, striped = TRUE, hover = TRUE, bordered  = TRUE, rownames = TRUE,  digits = 4)
  
  # Table Words #### 
  output$table.words <- renderTable({ 
    Build.Table.Words(DimX = input$axisX, DimY = input$axisY, DimZ = (input$axisY + 1), 
                      LT = TM$LexicalTable, translation_list = corpus$additional_data$translation_list, 
                      Order = input$selection.criteria)
  }, striped = TRUE, hover = TRUE, bordered  = TRUE, digits = 4) 
  
  # Table Docs #### 
  output$table.docs <- renderTable({  
    Build.Table.Docs(DimX = input$axisX, DimY = input$axisY, 
                     DimZ = (input$axisY + 1), corpus.metadata = corpus$additional_data$document_metadata, 
                     Order = input$selection.criteria)
  }, striped = TRUE, hover = TRUE, bordered  = TRUE, digits = 4)
  
# CA simultaneous plot ####
output$scatterPlot <- renderPlot({ # Define server logic required to draw a scatter plot

# Building dataframes for ggplot 
df.docs <- data.frame(
  label = unlist(lapply(rownames(CA$col$contrib), function(x) corpus$additional_data$document_metadata$Label[corpus$additional_data$document_metadata$Document == x])), 
  x = CA$col$coord[,  input$axisX], 
  y = CA$col$coord[,  input$axisY], 
  type = "Statement")

if(input$language == "English") { 
  df.Words <- data.frame(
    label = corpus$additional_data$translation_list[match(TM$V.culledcorpus[as.numeric(rownames(TM$LexicalTable))], corpus$additional_data$translation_list$Word), 2], 
    x = CA$row$coord[,  input$axisX], 
    y = CA$row$coord[,  input$axisY], 
    type = "Word")
} else {
  df.Words <- data.frame(
    label = TM$V.culledcorpus[as.numeric(rownames(TM$LexicalTable))],
    x = CA$row$coord[,  input$axisX], 
    y = CA$row$coord[,  input$axisY], 
    type = "Word")
} 

df.Annotate <- data.frame(
  Axis.X.Description = paste("Axis ", input$axisX, ", λ = ", round(CA$eig[input$axisX, 1], 2), "; var. = ", round(CA$eig[input$axisX, 2], 2), "%", sep = ""),
  Axis.Y.Description = paste("Axis ", input$axisY, ", λ = ", round(CA$eig[input$axisY, 1], 2), "; var. = ", round(CA$eig[input$axisY, 2], 2), "%", sep = ""),
  Label.X.plus = corpus$additional_data$principal_axes_metadata$Plus.Description[input$axisX],
  Label.X.min  = corpus$additional_data$principal_axes_metadata$Min.Description[input$axisX],
  Label.Y.plus = corpus$additional_data$principal_axes_metadata$Plus.Description[input$axisY],
  Label.Y.min  = corpus$additional_data$principal_axes_metadata$Min.Description[input$axisY]
) 

# culling amount of plotted items 
if(input$selection.criteria == "Contribution (axis X only)") { 
  df.docs.culled <- df.docs[order(CA$col$contrib[,  input$axisX], decreasing = TRUE)[0:input$numberDocs], ]
  df.Words.culled <- df.Words[order(CA$row$contrib[,  input$axisX], decreasing = TRUE)[0:input$numberWords], ]
} 
if(input$selection.criteria == "Contribution (axis X and Y)") { 
  df.docs.culled <- df.docs[order((CA$col$contrib[,  input$axisX] + CA$col$contrib[,  input$axisY]), decreasing = TRUE)[0:input$numberDocs], ]
  df.Words.culled <- df.Words[order((CA$row$contrib[,  input$axisX] + CA$row$contrib[,  input$axisY]), decreasing = TRUE)[0:input$numberWords], ]
} 
if(input$selection.criteria == "Inertia") {
  df.docs.culled <- df.docs[order(CA$call$marge.col, decreasing = TRUE)[0:input$numberDocs], ]
  df.Words.culled <- df.Words[order(CA$call$marge.row, decreasing = TRUE)[0:input$numberWords], ]
}
df.all <- rbind(df.docs, df.Words)
df.all.culled <- rbind(df.docs.culled, df.Words.culled)

# Plotting
scatter <- ggplot(data = df.Words, aes(x = x, y = y)) + 
  geom_hline(yintercept=0, size=.15) + 
  geom_vline(xintercept=0, size=.15) 


if(("Observations" %in% input$showInPlots) == TRUE) { 
  scatter <- scatter + 
    geom_point(data = df.Words, aes(x = x, y = y, colour = type, shape = type), alpha = .25, size = 1.5) + 
    geom_point(data = df.docs, aes(x = x, y = y, colour = type, shape = type), alpha = .25, size = 1.5)
}

if(("Labels" %in% input$showInPlots) == TRUE) { 
  scatter <- scatter + 
    geom_text_repel(data = df.all.culled, aes(x = x, y = y, label = label, colour = type), 
                    size = (input$textsize / 10) , alpha = 1, # colour = "black", 
                    stat = "identity", parse = FALSE, box.padding = unit(0.2, "lines"),
                    point.padding = unit(1e-02, "lines"), segment.color = "white", # #666666
                    segment.size = 0.0, arrow = NULL, force = .2, max.iter = 2000,
                    nudge_x = 0, nudge_y = 0, na.rm = FALSE, show.legend = FALSE, inherit.aes = FALSE) 
} 

if(("Interpretations" %in% input$showInPlots) == TRUE) {
  scatter <- scatter +
    geom_label(data = df.all, aes(x = max(x + .4), y = 0, label = stringr::str_wrap(df.Annotate$Label.X.plus, 10)), fill = "grey80", size = (input$textsize / 10)) +
    geom_label(data = df.all, aes(x = min(x - .4), y = 0, label = stringr::str_wrap(df.Annotate$Label.X.min, 10)), fill = "grey80", size = (input$textsize / 10)) +
    geom_label(data = df.all, aes(x = 0, y = max(y + .4), label = stringr::str_wrap(df.Annotate$Label.X.plus, 10)), fill = "grey80", size = (input$textsize / 10)) +
    geom_label(data = df.all, aes(x = 0, y = min(y - .4), label = stringr::str_wrap(df.Annotate$Label.X.min, 10)), fill = "grey80", size = (input$textsize / 10))
}

# Layout 
scatter <- scatter + 
  theme_bw() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.background = element_blank(), 
    legend.box.background = element_blank(), 
    axis.title.x=element_blank(), 
    axis.text=element_blank(), 
    plot.title = element_text(size=10), 
    axis.ticks =element_blank() 
    # panel.border=element_blank()
    # legend.position="none" # takes out all legends.. 
  ) +
  scale_color_manual(values=c("gray30", "black", "red", "green", "purple", "orange", "brown")) + 
  theme(legend.position=c(.99, .99), # position of legend
        legend.justification=c(1, 1),
        legend.background = element_blank(), 
        legend.title = element_blank())  + 
  labs(x = "", y = "") + 
  ggtitle(paste0("Figure 1: Discursive space of the Syrian conflict, principal plane ", input$axisX, "-", input$axisY, sep = "")) +  
  labs(caption = paste0("The ", as.english(input$numberDocs), " most contributing statements and ", as.english(input$numberWords), " most contributing (translated) words along axis ", input$axisX, " and ", input$axisY, " are plotted."), sep = "") +
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE)

# Drawing scale on map... 
temp.scale <- list()
temp.scale[["x.min"]] <- round(min(df.all$x), 1)  
temp.scale[["x.max"]] <- temp.scale[["x.min"]] + round(((max(df.all$x) - min(df.all$x)) / 10), 1)
temp.scale[["x.middle"]] <- (temp.scale[["x.min"]] + temp.scale[["x.max"]]) / 2
temp.scale[["label"]] <- temp.scale[["x.max"]] - temp.scale[["x.min"]]
temp.scale[["y.lower"]] <- (temp.scale[["x.min"]] + temp.scale[["x.max"]]) / 2
  
scatter <- scatter + 
  geom_segment(data = df.all, aes(x = temp.scale$x.min, y = max(y), 
                                     xend = temp.scale$x.max , yend = max(y)), size = .15 ) + 
  geom_segment(data = df.all, aes(x = temp.scale$x.min, y =    (max(y) + .02), 
                                     xend = temp.scale$x.min, yend = (max(y) - .02)), size = .15) + 
  geom_segment(data = df.all, aes(x = temp.scale$x.max, y =    (max(y) + .02), 
                                     xend = temp.scale$x.max, yend = (max(y) - .02)), size = .15) + 
  annotate("text", x = temp.scale$x.middle, y = max(df.all$y), label = temp.scale$label, size=(input$textsize / 10), alpha = .8,  hjust = .5, vjust = 1.5) + 
  annotate("text", x = temp.scale$x.middle, y = max(df.all$y), label = "scale", size=(input$textsize / 10), alpha = .8,  hjust = .5, vjust = -.75) + 
  # # Description amount variance  
  annotate("text", x = max(df.all$x), y = 0, label = df.Annotate$Axis.X.Description, size=(input$textsize / 10), alpha = .8,  hjust = 1, vjust = -1) +
  annotate("text", x = 0, y = max(df.all$y), label = df.Annotate$Axis.Y.Description, size=(input$textsize / 10), alpha = .8,  hjust = 1, vjust = -1, angle = 90) 

# printing plot
scatter

 }, width = 750, height = 750, res = 100) # 
# }, width = 2500, height = 2500, res = 300) # for publication 

  
# Additional data used in Shiny app #### 
  output$selected_axis <- renderText({ 
    paste("Axis ", input$axisX, ": ", paste("Axis ", input$axisX, ", λ = ", round(CA$eig[input$axisX, 1], 2), "; var. = ", round(CA$eig[input$axisX, 2], 2), "%", sep = ""), sep = "")
  })
  
  output$selected_axis1 <- renderText({ 
    paste("Axis ", input$axisX, ": ", paste("Axis ", input$axisX, ", λ = ", round(CA$eig[input$axisX, 1], 2), "; var. = ", round(CA$eig[input$axisX, 2], 2), "%", sep = ""), sep = "")
  })
    
  output$selected_plane <- renderText({ 
    paste("Plane axes", input$axisX, "-", input$axisY, sep = " ")
  })
  
  output$selected_plane_supCA <- renderText({ 
    paste("Plane axes", input$axisX, "-", input$axisY, sep = " ")
  })
  
  
  output$selected_cluster.words <- renderText({ 
    paste("Cluster ", input$cluster, sep = " ")
  })
  
  output$selected_cluster.docs <- renderText({ 
    paste("Cluster ", input$cluster, sep = " ")
  })
  
  output$selected.narrative.text <- renderText({ 
    paste("Selected text: ", input$narrative, " (Cluster ", input$cluster, ")", sep = "")
  })
  
  output$narrative <- renderTable({
    
    Narrative  <- unlist(corpus$Wordlists)
    Words.cluster <- HC$call$X[HC$call$X$clust == input$cluster, ]
    Document.indices <- TM$indices.docs.start[which(Shiny.df$`1`$Docs$Label == input$narrative)]:TM$indices.docs.end[which(Shiny.df$`1`$Docs$Label == input$narrative)] 
    Word.indices <- unlist(TM$HierachicalIndex[rownames(Words.cluster)]) # list of indices of all words in cluster. 
    
    Narrative[Word.indices[Word.indices %in% Document.indices]] <- 
      lapply(Word.indices[Word.indices %in% Document.indices], function(x) paste0("(+)", Narrative[x], "(+)", sep = ""))
    
    Narrative <- data.frame(Text = paste0(Narrative[Document.indices], collapse = " "))
  }, striped = TRUE, hover = TRUE, bordered  = TRUE, digits = 0)

# Text, Tables and Plots for Supplementary CA Tab #### 
  # Table Overview Sups corpus
  output$corpus_sups <- renderTable({
    data.frame(
      Orgs = names(corpus_sup),
      Docs = unlist(lapply(corpus_sup, function(x) length(x$Documents))),
      occ.T = unlist(lapply(corpus_sup, function(x) length(unlist(x$Wordlists)))),  
      words.V = unlist(lapply(corpus_sup, function(x) length(unique(unlist(x$Wordlists))))),
      Avg.T.per.V = unlist(lapply(TM.sup, function(x) length(x$V.originalcorpus) /  sum(x$LexicalTable))))
  }, striped = TRUE, hover = TRUE, bordered  = TRUE, digits = 0)
  
  # length(unlist(corpus_sup$AaS$Wordlists))
  # Table Overview Culled Sups corpus #### 
  output$corpus_sups.culled <- renderTable({
    data.frame(
      Orgs = names(corpus_sup),
      Docs = unlist(lapply(TM.sup, function(x) length(x$Segmented.HI))),
      occ.T = unlist(lapply(TM.sup, function(x) sum(x$LexicalTable))),  
      words.V = unlist(lapply(TM.sup, function(x) nrow(x$LexicalTable[rowSums(x$LexicalTable) != 0, ]))),
      Avg.T.per.V = unlist(lapply(TM.sup, function(x) 
        sum(x$LexicalTable) / 
        nrow(x$LexicalTable[rowSums(x$LexicalTable) != 0, ]))))
  }, striped = TRUE, hover = TRUE, bordered  = TRUE, digits = 0)
  
  # Table Sup Docs #### 
  output$table.sup.docs <- renderTable({  
    Build.Table.Sup.docs(DimX = input$axisX, corpus.metadata = corpus$additional_data$document_sup_metadata, Orgs = input$SelectedOrgs)
  }, striped = TRUE, hover = TRUE, bordered  = TRUE)
  
  
  output$dateRangetext <- renderText({ 
  paste("Start date: ", input$selectedDates[1], "; end date: ", input$selectedDates[2], sep = "")
})

  output$selected_axis_sups <- renderText({ 
    paste("Axis ", input$axisX, ": ", paste("Axis ", input$axisX, ", λ = ", round(CA$eig[input$axisX, 1], 2), "; var. = ", round(CA$eig[input$axisX, 2], 2), "%", sep = ""), sep = "")
  })
  
# Table Events #### 
Table.Events <- reactive({ 
  data.dates <- corpus$additional_data$event_metadata
  data.dates["Date"] <- as.Date(paste0(data.dates$Event.Day, "/", data.dates$Event.Month, "/", data.dates$Event.Year, sep = ""), "%d/%m/%y")
  data.dates <- data.dates[data.dates$Date %in% seq(input$selectedDates[1], input$selectedDates[2], by="days"), ] 
  data.dates["Date"] <- as.character(data.dates$Date)
  
  data.dates <- data.dates[c("Label", "Date", "Description")]
})

output$table.events <- renderTable({ Table.Events() }, striped = TRUE, hover = TRUE, bordered  = TRUE, digits = 4)

# SupCA Chronological plot #### 
output$chronologicalPlot <- renderPlot({
  
  # Building word data frame (only axis X)
  if(input$language == "English") { 
    df.Words <- data.frame(
      label = corpus$additional_data$translation_list[match(TM$V.culledcorpus[as.numeric(rownames(TM$LexicalTable))], corpus$additional_data$translation_list$Word), 2], 
      x = CA$row$coord[, input$axisX], 
      type = "Word")
  } else {
    df.Words <- data.frame(
      label = TM$V.culledcorpus[as.numeric(rownames(TM$LexicalTable))],
      x = CA$row$coord[, input$axisX], 
      type = "Word")
  } 
  # df.Words <- df.Words[!is.na(df.Words$label), ]
  
  # culling amount of plotted items 
  if(input$selection.criteria == "Contribution (axis X only)")  { df.Words <- df.Words[order(CA$row$contrib[,  input$axisX], decreasing = TRUE)[0:input$numberWords], ]  } 
  if(input$selection.criteria == "Contribution (axis X and Y)") { df.Words <- df.Words[order(CA$row$contrib[,  input$axisX], decreasing = TRUE)[0:input$numberWords], ]} 
  if(input$selection.criteria == "Inertia")                     { df.Words <- df.Words[order(CA$call$marge.row, decreasing = TRUE)[0:input$numberWords], ] }
  
  # creating data frame from supplementary items. 
  df.all <- data.frame(
    Label = paste0(as.character(corpus$additional_data$document_sup_metadata$Org.Abbreviation[match(rownames(CA.sup$col.sup$coord), corpus$additional_data$document_sup_metadata$Document)]),
                   1:nrow(CA.sup$col.sup$coord)),
    Coord. = CA.sup$col.sup$coord[, input$axisX],
    Date.full = as.Date(unlist(lapply(rownames(CA.sup$col.sup$coord), function(x) paste0(corpus$additional_data$document_sup_metadata$Statement.Day[corpus$additional_data$document_sup_metadata$Document  == x], "/",
                                                                                         corpus$additional_data$document_sup_metadata$Statement.Month[corpus$additional_data$document_sup_metadata$Document  == x], "/",
                                                                                         corpus$additional_data$document_sup_metadata$Statement.Year[corpus$additional_data$document_sup_metadata$Document  == x],
                                                                                         sep = ""))), "%d/%m/%y"),
    Org = as.character(corpus$additional_data$document_sup_metadata$Org.Abbreviation[match(rownames(CA.sup$col.sup$coord), corpus$additional_data$document_sup_metadata$Document)])    )
  
  # Making data frames for documents 
  df.docs <- data.frame(
    Label = unlist(lapply(rownames(CA$col$contrib), function(x) corpus$additional_data$document_metadata$Label[corpus$additional_data$document_metadata$Document == x])), 
    Coord. = CA$col$coord[,  input$axisX], 
    Date.full = as.Date(unlist(lapply(rownames(CA$col$contrib), function(x) paste0(corpus$additional_data$document_metadata$Statement.Day[corpus$additional_data$document_metadata$Document == x], "/", 
                                                                                   corpus$additional_data$document_metadata$Statement.Month[corpus$additional_data$document_metadata$Document == x], "/",
                                                                                   corpus$additional_data$document_metadata$Statement.Year[corpus$additional_data$document_metadata$Document == x], 
                                                                                   sep = ""))), "%d/%m/%y"), 
    Org = "General")
  
  df.all <- rbind(df.all, df.docs)
  
  df.Annotate <- data.frame(
    Label.X.plus = corpus$additional_data$principal_axes_metadata$Plus.Description[input$axisX],
    Label.X.min  = corpus$additional_data$principal_axes_metadata$Min.Description[input$axisX]
  )
  
  df.events <- corpus$additional_data$event_metadata
  df.events["Date"] <- as.Date(paste0(df.events$Event.Day, "/", df.events$Event.Month, "/", df.events$Event.Year, sep = ""), "%d/%m/%y")
  df.events <- df.events[df.events$Date %in% seq(input$selectedDates[1], input$selectedDates[2], by="days"), ]
  
  # making selections in supplementary corpus by Org and Date range. NB NOT in primary corpus! 
  df.all <- df.all[df.all$Org %in% input$SelectedOrgs,]
  df.all <- df.all[df.all$Date.full %in% seq(input$selectedDates[1], input$selectedDates[2], by="days"),] 

  # Plotting 
  SupCA.Chrn.Plot <- ggplot(data = df.all)

  if(("Observations" %in% input$showInPlots) == TRUE) {
    SupCA.Chrn.Plot <- SupCA.Chrn.Plot +
      geom_point(data = df.all, aes(x=Date.full, y=Coord., color = Org), alpha = .5) 
    }
  
  if(("Interpretations" %in% input$showInPlots) == TRUE) {
  SupCA.Chrn.Plot <- SupCA.Chrn.Plot +
    geom_label(data = df.Annotate, aes(x = min(input$selectedDates), y = max(df.all$Coord.) -.3, label = stringr::str_wrap(Label.X.plus, 10)), fill = "grey80", size = (input$textsize / 10)) +
    geom_label(data = df.Annotate, aes(x = min(input$selectedDates), y = min(df.all$Coord.) +.3, label = stringr::str_wrap(Label.X.min, 10)), fill = "grey80", size = (input$textsize / 10))
  }

  if(("Smoothed lines" %in% input$showInPlots) == TRUE) {
    SupCA.Chrn.Plot <- SupCA.Chrn.Plot +
      geom_smooth(data = df.all, aes(x=Date.full, y=Coord., color = Org, fill=Org), span = .75, size=1, alpha = .2, level = .9)  
    }
  
  SupCA.Chrn.Plot <- SupCA.Chrn.Plot +
    geom_hline(yintercept=0, size=.3, colour = "grey30") +
    geom_vline(xintercept=df.events$Date, size=.15, colour = "red", alpha = .5) +
    geom_text(data = df.events, aes(x = Date, label = Label), y = (min(df.all$Coord.)), colour = "red")
  
  SupCA.Chrn.Plot <- SupCA.Chrn.Plot + 
  theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x=element_blank(),
      plot.title = element_text(size=10), 
      axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1, size = (input$textsize / 3)),
      plot.caption = element_text(hjust = .5)
      # panel.border=element_blank()
    ) + 
    theme(legend.key=element_rect(fill=alpha('grey70', 0), linetype = 0), # small boxes in legend
          legend.position=c(1, 1), # position of legend
          legend.justification=c(1, 1),
          legend.background = element_rect(fill=alpha('white', 0.75), colour = "white"),
    legend.title = element_blank()
       ) +
    labs(caption = stringr::str_wrap(
      paste("Plot of supplementary statements, smoothing method is loess with a confidence interval of 0.9.", " Events plotted:", 
      paste(corpus$additional_data$event_metadata$Label, corpus$additional_data$event_metadata$Description, sep = " ", collapse = "; ")), 200)) + 
    ggtitle(paste0("Figure 2: Discursive positionalities of supplementary statements across time, principal axis ", input$axisX, ".", sep = " ")) +  
    scale_y_continuous(breaks = df.Words$x, labels = df.Words$label, name = NULL, guide = guide_axis(check.overlap = TRUE)) # n.dodge=3  guide = guide_axis(check.overlap = TRUE) 
  # scale_y_discrete
  #  scale_y_continuouslabels = df.Words$label, breaks = df.Words$x, name = NULL, labels = df.Words$label
  
  # Drawing scale on map... 
  temp.scale <- list()
  temp.scale[["y.max"]] <- round(max(df.all$Coord.), 1) 
  temp.scale[["y.min"]] <- temp.scale[["y.max"]] - .3
  temp.scale[["y.middle"]] <- (temp.scale[["y.min"]] + temp.scale[["y.max"]]) / 2
  temp.scale[["label"]] <- temp.scale[["y.max"]] - temp.scale[["y.min"]]
  temp.scale[["y.lower"]] <- (temp.scale[["y.min"]] + temp.scale[["y.max"]]) / 2
  temp.scale[["su.Date"]] <- (max(df.all$Date.full) - min(df.all$Date.full)) / 150
 
  SupCA.Chrn.Plot <- SupCA.Chrn.Plot + 
    geom_segment(data = df.all, aes(x = min(Date.full) - temp.scale$su.Date, y = temp.scale$y.min, 
                                    xend = min(Date.full) - temp.scale$su.Date, yend = temp.scale$y.max), size = .05 ) +
    geom_segment(data = df.all, aes(x = min(Date.full), y = temp.scale$y.max, 
                                    xend = min(Date.full) - (temp.scale$su.Date * 2), yend = temp.scale$y.max), size = .05) +
    geom_segment(data = df.all, aes(x = min(Date.full), y = temp.scale$y.min, 
                                    xend = min(Date.full) - (temp.scale$su.Date * 2), yend = temp.scale$y.min), size = .05)   + 
    annotate("text", x = min(df.all$Date.full) + (temp.scale$su.Date * 1), y = temp.scale$y.middle, label = temp.scale$label, size=(input$textsize / 7.5), alpha = .8 ) +  # ,  hjust = .5, vjust = 1.5)  
    annotate("text", x = min(df.all$Date.full) - (temp.scale$su.Date * 3), y = temp.scale$y.middle, label = "scale", size=(input$textsize / 7.5), angle = 90, alpha = .8) # ,  hjust = .5, vjust = -.75) + 
    
  SupCA.Chrn.Plot
 }, width = 1100, height = 700, res = 100)
# } , width = 2200, height = 1400, res = 200)

output$click_info <- renderPrint({
  
  # creating data frame from supplementary items. 
  df.all <- data.frame(
    Coord. = CA.sup$col.sup$coord[, input$axisX],
    Date.full = as.Date(unlist(lapply(rownames(CA.sup$col.sup$coord), function(x) paste0(corpus$additional_data$document_sup_metadata$Statement.Day[corpus$additional_data$document_sup_metadata$Document  == x], "/",
                                                                                         corpus$additional_data$document_sup_metadata$Statement.Month[corpus$additional_data$document_sup_metadata$Document  == x], "/",
                                                                                         corpus$additional_data$document_sup_metadata$Statement.Year[corpus$additional_data$document_sup_metadata$Document  == x],
                                                                                         sep = ""))), "%d/%m/%y"),
    Org = as.character(corpus$additional_data$document_sup_metadata$Org.Abbreviation[match(rownames(CA.sup$col.sup$coord), corpus$additional_data$document_sup_metadata$Document)])    )
  
  # Making data frames for documents 
  df.docs <- data.frame(
    Coord. = CA$col$coord[,  input$axisX], 
    Date.full = as.Date(unlist(lapply(rownames(CA$col$contrib), function(x) paste0(corpus$additional_data$document_metadata$Statement.Day[corpus$additional_data$document_metadata$Document == x], "/", 
                                                                                   corpus$additional_data$document_metadata$Statement.Month[corpus$additional_data$document_metadata$Document == x], "/",
                                                                                   corpus$additional_data$document_metadata$Statement.Year[corpus$additional_data$document_metadata$Document == x], 
                                                                                   sep = ""))), "%d/%m/%y"), 
    Org = "General")
  
  df.all <- rbind(df.all, df.docs)
  
  # making selections in supplementary corpus by Org and Date range. NB NOT in primary corpus! 
  df.all <- df.all[df.all$Org %in% input$SelectedOrgs,]
  df.all <- df.all[df.all$Date.full %in% seq(input$selectedDates[1], input$selectedDates[2], by="days"),] 
  
  nearPoints(df.all, input$chronologicalPlot_click, addDist = TRUE)
})

output$brush_info <- renderPrint({
  
  # creating data frame from supplementary items. 
  df.all <- data.frame(
    Coord. = CA.sup$col.sup$coord[, input$axisX],
    Date.full = as.Date(unlist(lapply(rownames(CA.sup$col.sup$coord), function(x) paste0(corpus$additional_data$document_sup_metadata$Statement.Day[corpus$additional_data$document_sup_metadata$Document  == x], "/",
                                                                                         corpus$additional_data$document_sup_metadata$Statement.Month[corpus$additional_data$document_sup_metadata$Document  == x], "/",
                                                                                         corpus$additional_data$document_sup_metadata$Statement.Year[corpus$additional_data$document_sup_metadata$Document  == x],
                                                                                         sep = ""))), "%d/%m/%y"),
    Org = as.character(corpus$additional_data$document_sup_metadata$Org.Abbreviation[match(rownames(CA.sup$col.sup$coord), corpus$additional_data$document_sup_metadata$Document)])    )
  
  # Making data frames for documents 
  df.docs <- data.frame(
    Coord. = CA$col$coord[,  input$axisX], 
    Date.full = as.Date(unlist(lapply(rownames(CA$col$contrib), function(x) paste0(corpus$additional_data$document_metadata$Statement.Day[corpus$additional_data$document_metadata$Document == x], "/", 
                                                                                   corpus$additional_data$document_metadata$Statement.Month[corpus$additional_data$document_metadata$Document == x], "/",
                                                                                   corpus$additional_data$document_metadata$Statement.Year[corpus$additional_data$document_metadata$Document == x], 
                                                                                   sep = ""))), "%d/%m/%y"), 
    Org = "General")
  
  df.all <- rbind(df.all, df.docs)
  
  # making selections in supplementary corpus by Org and Date range. NB NOT in primary corpus! 
  df.all <- df.all[df.all$Org %in% input$SelectedOrgs,]
  df.all <- df.all[df.all$Date.full %in% seq(input$selectedDates[1], input$selectedDates[2], by="days"),] 
  
  brushedPoints(df.all, input$chronologicalPlot_brush)
})

  
# SupCA Simultanious plot ####
output$scatterPlot.sup <- renderPlot({ # Define server logic required to draw a scatter plot
  
  # Building dataframes for ggplot 
  df.docs <- data.frame(
    label = unlist(lapply(rownames(CA$col$contrib), function(x) corpus$additional_data$document_metadata$Label[corpus$additional_data$document_metadata$Document == x])), 
    x = CA$col$coord[,  input$axisX], 
    y = CA$col$coord[,  input$axisY], 
    type = "Statement")
  
  if(input$language == "English") { 
    df.Words <- data.frame(
      label = corpus$additional_data$translation_list[match(TM$V.culledcorpus[as.numeric(rownames(TM$LexicalTable))], corpus$additional_data$translation_list$Word), 2], 
      x = CA$row$coord[,  input$axisX], 
      y = CA$row$coord[,  input$axisY], 
      type = "Word")
  } else {
    df.Words <- data.frame(
      label = TM$V.culledcorpus[as.numeric(rownames(TM$LexicalTable))],
      x = CA$row$coord[,  input$axisX], 
      y = CA$row$coord[,  input$axisY], 
      type = "Word")
  } 
  
    # culling amount of plotted items 
  if(input$selection.criteria == "Contribution (axis X only)") { 
    df.docs.culled <- df.docs[order(CA$col$contrib[,  input$axisX], decreasing = TRUE)[0:input$numberDocs], ]
    df.Words.culled <- df.Words[order(CA$row$contrib[,  input$axisX], decreasing = TRUE)[0:input$numberWords], ]
  } 
  if(input$selection.criteria == "Contribution (axis X and Y)") { 
    df.docs.culled <- df.docs[order((CA$col$contrib[,  input$axisX] + CA$col$contrib[,  input$axisY]), decreasing = TRUE)[0:input$numberDocs], ]
    df.Words.culled <- df.Words[order((CA$row$contrib[,  input$axisX] + CA$row$contrib[,  input$axisY]), decreasing = TRUE)[0:input$numberWords], ]
  } 
  if(input$selection.criteria == "Inertia") {
    df.docs.culled <- df.docs[order(CA$call$marge.col, decreasing = TRUE)[0:input$numberDocs], ]
    df.Words.culled <- df.Words[order(CA$call$marge.row, decreasing = TRUE)[0:input$numberWords], ]
  }
  df.all <- rbind(df.docs, df.Words)
  df.all.culled <- rbind(df.docs.culled, df.Words.culled)
  
  df.sups <- data.frame(
    Label = paste0(as.character(corpus$additional_data$document_sup_metadata$Org.Abbreviation[match(rownames(CA.sup$col.sup$coord), corpus$additional_data$document_sup_metadata$Document)]),
                   1:nrow(CA.sup$col.sup$coord)),
    x = CA.sup$col.sup$coord[, input$axisX],
    y = CA.sup$col.sup$coord[, input$axisY],
    Date.full = as.Date(unlist(lapply(rownames(CA.sup$col.sup$coord), function(x) paste0(corpus$additional_data$document_sup_metadata$Statement.Day[corpus$additional_data$document_sup_metadata$Document  == x], "/",
                                                                                         corpus$additional_data$document_sup_metadata$Statement.Month[corpus$additional_data$document_sup_metadata$Document  == x], "/",
                                                                                         corpus$additional_data$document_sup_metadata$Statement.Year[corpus$additional_data$document_sup_metadata$Document  == x],
                                                                                         sep = ""))), "%d/%m/%y"),
    Org = as.character(corpus$additional_data$document_sup_metadata$Org.Abbreviation[match(rownames(CA.sup$col.sup$coord), corpus$additional_data$document_sup_metadata$Document)])    )
  
  
  # making selections by Org and Date range. 
  df.sups <- df.sups[df.sups$Org %in% input$SelectedOrgs,]
  df.sups <- df.sups[df.sups$Date.full %in% seq(input$selectedDates[1], input$selectedDates[2], by="days"),] 

  df.Annotate <- data.frame(
    Axis.X.Description = paste("Axis ", input$axisX, ", λ = ", round(CA$eig[input$axisX, 1], 2), "; var. = ", round(CA$eig[input$axisX, 2], 2), "%", sep = ""),
    Axis.Y.Description = paste("Axis ", input$axisY, ", λ = ", round(CA$eig[input$axisY, 1], 2), "; var. = ", round(CA$eig[input$axisY, 2], 2), "%", sep = ""),
    Label.X.plus = corpus$additional_data$principal_axes_metadata$Plus.Description[input$axisX],
    Label.X.min  = corpus$additional_data$principal_axes_metadata$Min.Description[input$axisX],
    Label.Y.plus = corpus$additional_data$principal_axes_metadata$Plus.Description[input$axisY],
    Label.Y.min  = corpus$additional_data$principal_axes_metadata$Min.Description[input$axisY]
  ) 
  
  
# Plotting
scatter <- ggplot(data = df.Words, aes(x = x, y = y)) + 
  geom_hline(yintercept=0, size=.15) + 
  geom_vline(xintercept=0, size=.15) 
  
scatter <- scatter + 
  geom_point(data = df.sups, aes(x = x, y = y, colour = Org), alpha = 1, size = 1.5) # shape = 1, 

if(("Observations" %in% input$showInPlots) == TRUE) { 
  scatter <- scatter + 
    geom_point(data = df.Words, aes(x = x, y = y), shape = df.Words$type, colour = "grey", alpha = .5, size = 1.5) + 
    geom_point(data = df.docs, aes(x = x, y = y), shape = df.docs$type, colour = "grey", alpha = .5, size = 1.5)
}

if(("Labels" %in% input$showInPlots) == TRUE) { 
  scatter <- scatter + 
    geom_text_repel(data = df.all.culled, aes(x = x, y = y, label = label), colour = "grey", 
                    size = (input$textsize / 10) , alpha = .5, # colour = "black", 
                    stat = "identity", parse = FALSE, box.padding = unit(0.2, "lines"),
                    point.padding = unit(1e-02, "lines"), segment.color = "white", # #666666
                    segment.size = 0.0, arrow = NULL, force = .2, max.iter = 2000,
                    nudge_x = 0, nudge_y = 0, na.rm = FALSE, show.legend = FALSE, inherit.aes = FALSE) 
} 

if(("Interpretations" %in% input$showInPlots) == TRUE) {
  scatter <- scatter +
    geom_label(data = df.all, aes(x = max(x + .4), y = 0, label = stringr::str_wrap(df.Annotate$Label.X.plus, 10)), fill = "grey80", size = (input$textsize / 10)) +
    geom_label(data = df.all, aes(x = min(x - .4), y = 0, label = stringr::str_wrap(df.Annotate$Label.X.min, 10)), fill = "grey80", size = (input$textsize / 10)) +
    geom_label(data = df.all, aes(x = 0, y = max(y + .4), label = stringr::str_wrap(df.Annotate$Label.X.plus, 10)), fill = "grey80", size = (input$textsize / 10)) +
    geom_label(data = df.all, aes(x = 0, y = min(y - .4), label = stringr::str_wrap(df.Annotate$Label.X.min, 10)), fill = "grey80", size = (input$textsize / 10))
}
    
# # Drawing scale on map... 
temp.scale <- list()
temp.scale[["x.min"]] <- round(min(df.all$x), 1)
temp.scale[["x.max"]] <- temp.scale[["x.min"]] + round(((max(df.all$x) - min(df.all$x)) / 10), 1)
temp.scale[["x.middle"]] <- (temp.scale[["x.min"]] + temp.scale[["x.max"]]) / 2
temp.scale[["label"]] <- temp.scale[["x.max"]] - temp.scale[["x.min"]]
temp.scale[["y.lower"]] <- (temp.scale[["x.min"]] + temp.scale[["x.max"]]) / 2

scatter <- scatter +
  geom_segment(data = df.all, aes(x = temp.scale$x.min, y = max(y),
                                     xend = temp.scale$x.max , yend = max(y)), size = .15, colour = "black" ) +
  geom_segment(data = df.all, aes(x = temp.scale$x.min, y =    (max(y) + .02),
                                     xend = temp.scale$x.min, yend = (max(y) - .02)), size = .15, colour = "black" ) +
  geom_segment(data = df.all, aes(x = temp.scale$x.max, y =    (max(y) + .02),
                                       xend = temp.scale$x.max, yend = (max(y) - .02)), size = .15, colour = "black" ) +
  annotate("text", x = temp.scale$x.middle, y = max(df.all$y), label = temp.scale$label, size=(input$textsize / 10), alpha = .8,  hjust = .5, vjust = 1.5) +
  annotate("text", x = temp.scale$x.middle, y = max(df.all$y), label = "scale", size=(input$textsize / 10), alpha = .8,  hjust = .5, vjust = -.75)

# Layout 
scatter <- scatter +
  theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x=element_blank(),
      axis.text=element_blank(),
      axis.ticks =element_blank(),
      # panel.border=element_blank(),
      # legend.position="none" # takes out all legends..
    ) +
    theme(legend.key=element_rect(fill=alpha('grey70', 0), linetype = 0), # small boxes in legend
          legend.position=c(1, 1), # position of legend
          legend.justification=c(1, 1),
          legend.background = element_rect(fill=alpha('white', 0.75), colour = "white"), 
          legend.title = element_blank()) +
    labs(x = "", y = "") +
    coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE) +
annotate("text", x = max(df.all$x), y = 0, label = df.Annotate$Axis.X.Description, size=(input$textsize / 10), alpha = .8,  hjust = 1, vjust = -.8) +
annotate("text", x = 0, y = max(df.all$y), label = df.Annotate$Axis.Y.Description, size=(input$textsize / 10), alpha = .8,  hjust = .8, vjust = -1, angle = 90)

# density plots 
x.density <- ggplot(data = df.sups, aes(x=x, fill=Org)) + geom_density(alpha=0.4) +
  theme_bw() + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x=element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size=10), 
    axis.text=element_blank(),
    axis.ticks = element_blank(),
    panel.border=element_blank(),
    legend.position="none") + # takes out all legends..
    ggtitle(paste0("Figure 2: Comparative discursive profiles, principal plane ", input$axisX, "-", input$axisY, sep = ""))
     
y.density <- ggplot(df.sups, aes(x=y, fill=Org)) + geom_density(alpha=0.4) + coord_flip() +
    theme_bw() + theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x=element_blank(),
      axis.title.y = element_blank(),
      axis.text=element_blank(),
      axis.ticks = element_blank(),
      panel.border=element_blank(),
      legend.position="none" # takes out all legends..
    )

blanck.plot <- ggplot(data = df.all, aes(x = x, y = y)) +
                      theme_bw() + theme(
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      axis.title.x=element_blank(),
                      axis.title.y = element_blank(),
                      axis.text=element_blank(),
                      axis.ticks = element_blank(),
                      panel.border=element_blank(),
                      legend.position="none" # takes out all legends..
                      )

scatter2 <- ggarrange(x.density, NULL, scatter, y.density,
          ncol = 2, nrow = 2,  align = "hv",
          widths = c(5, 1), heights = c(1, 5),
          common.legend = FALSE)

# scatter2 <- x.density

scatter2
 }, width = 750, height = 750, res = 100)  
#  }, width = 5000, height = 5000, res = 600) # for publication
}

shinyApp(ui, server)
