ShinyServer <- function(input, output, session) {

  # Table Overview Original corpus
  output$corpus_original <- renderTable(
    {
      data.frame(
        documents = length(corpus$file_names),
        number_occurences_t = length(unlist(stemmed_corpus)),
        number_words_v = length(mined_text$v_original_corpus),
        avg_t_per_v = length(unlist(stemmed_corpus)) / length(mined_text$v_original_corpus)
      )
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    digits = 0
  )

  # Table Overview Culled corpus
  output$corpus_culled <- renderTable(
    {
      data.frame(
        documents = length(corpus$file_names),
        number_occurences_t = sum(mined_text$lexical_table),
        number_words_v = length(mined_text$v_culled_corpus),
        avg_t_per_v = sum(mined_text$lexical_table) / length(mined_text$v_culled_corpus)
      )
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    digits = 0
  )

  # [Because I simplified supporting files, this table is a bit more cumborsome to construct. delete? ]
  # Table Overview Names Organizations and label documents
  # output$corpus_names <- renderTable(
  #  {
  #    data.frame(
  #      name_arab = support_files$organization_metadata$org_arabic,
  #      name_eng = support_files$organization_metadata$org_english,
  #      charter = support_files$document_metadata$label[
  #        match(
  #          support_files$organization_metadata$ $Charter, support_files$document_metadata$Document
  #          )
  #        ],
  #      self_descr = support_files$document_metadata$label[
  #        match(
  #          support_files$organization_metadata$Who.Are.we, support_files$document_metadata$Document
  #          )
  #        ]
  #    )
  #  },
  #  striped = TRUE,
  #  hover = TRUE,
  #  bordered = TRUE,
  #  digits = 0
  # )

  # Histogram length statements ####
  output$histogram_plot <- renderPlot({
    hist(
      colSums(
        mined_text$lexical_table
      ),
      breaks = 20,
      xlab = "Total Occurences",
      ylab = "Number of documents",
      main = NULL
    )
  })

  # Screeplot ####
  scree_plot <- data.frame(
    variance = correspondence_analysis$eig[, 2],
    axis = seq(1:nrow(correspondence_analysis$eig))
  )

  output$scree_plot <- renderPlot(
    {
      ggplot(
        data = scree_plot,
        aes(x = axis, y = variance)
      ) +
        geom_point(size = 2, shape = 1) +
        geom_line() +
        theme_bw() +
        guides(size = FALSE, solid = FALSE) +
        theme(
          panel.grid.minor = element_line(
            linetype = "solid",
            size = .1
          )
        ) +
        theme(
          panel.grid.major = element_line(
            linetype = "solid",
            size = .1
          )
        ) +
        labs(x = "Axis", y = "Variance")
    } # ,  width = 4000, height = 4000, res = 600
  )

  # Table Eigenvalues ####
  table_eigenvalues <- data.frame(correspondence_analysis$eig[])
  colnames(table_eigenvalues) <- c("Eigenvalue", "Variance", "Cumulative")
  rownames(table_eigenvalues) <- paste0("Axis ", 1:nrow(table_eigenvalues), sep = " ")
  output$table.eigenvalues <- renderTable(
    {
      table_eigenvalues
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    rownames = TRUE,
    digits = 4
  )

  # Table Words ####
  output$table_words <- renderTable(
    {
      BuildTableWords(
        dim_x = input$axis_x,
        lexical_table = mined_text$lexical_table, 
        translation_list = support_files$translation_list,
        order = input$selection_criteria
      )
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    digits = 4
  )

  # Table Docs ####
  output$table_docs <- renderTable(
    {
      BuildTableDocs(
        dim_x = input$axis_x,
        corpus_metadata = support_files$document_metadata,
        order = input$selection_criteria
      )
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    digits = 4
  )
  
  # correspondence_analysis simultaneous plot ####
  output$scatter_plot <- renderPlot(
    { # Define server logic required to draw a scatter plot

      # Building dataframes for ggplot
      df_docs <- data.frame(
        label = sapply(
          rownames(correspondence_analysis$col$contrib),
          function(x) {
            support_files$document_metadata$label[
              support_files$document_metadata$file_name == x
            ]
          }
        ),
        x = correspondence_analysis$col$coord[, input$axis_x],
        y = correspondence_analysis$col$coord[, input$axis_y],
        type = "statement"
      )

      if (input$language == "English") {
        df_words <- data.frame(
          label = support_files$translation_list[
            match(
              mined_text$v_culled_corpus[
                as.numeric(rownames(mined_text$lexical_table))
              ], support_files$translation_list$word
            ), 2
          ],
          x = correspondence_analysis$row$coord[, input$axis_x],
          y = correspondence_analysis$row$coord[, input$axis_y],
          type = "word"
        )
      } else {
        df_words <- data.frame(
          label = mined_text$v_culled_corpus[
            as.numeric(rownames(mined_text$lexical_table))
          ],
          x = correspondence_analysis$row$coord[, input$axis_x],
          y = correspondence_analysis$row$coord[, input$axis_y],
          type = "word"
        )
      }

      df_annotate <- data.frame(
        axis_x_description = paste(
          "Axis ", input$axis_x,
          ", λ = ", round(correspondence_analysis$eig[input$axis_x, 1], 2),
          "; var. = ", round(correspondence_analysis$eig[input$axis_x, 2], 2), "%",
          sep = ""
        ),
        axis_y_description = paste(
          "Axis ", input$axis_y,
          ", λ = ", round(correspondence_analysis$eig[input$axis_y, 1], 2),
          "; var. = ", round(correspondence_analysis$eig[input$axis_y, 2], 2), "%",
          sep = ""
        ),
        label_x_plus = support_files$principal_axes_metadata$plus_description[input$axis_x],
        label_x_min = support_files$principal_axes_metadata$min_description[input$axis_x],
        label_y_plus = support_files$principal_axes_metadata$plus_description[input$axis_y],
        label_y_min = support_files$principal_axes_metadata$min_description[input$axis_y]
      )

      # culling amount of plotted items
      if (input$selection_criteria == "Contribution (axis X only)") {
        df_docs_culled <- df_docs[
          order(
            correspondence_analysis$col$contrib[, input$axis_x],
            decreasing = TRUE
          )[
            0:input$number_docs
          ],
        ]
        df_words_culled <- df_words[
          order(
            correspondence_analysis$row$contrib[, input$axis_x],
            decreasing = TRUE
          )[
            0:input$number_words
          ],
        ]
      }
      if (input$selection_criteria == "Contribution (axis X and Y)") {
        df_docs_culled <- df_docs[
          order(
            (correspondence_analysis$col$contrib[, input$axis_x] + correspondence_analysis$col$contrib[, input$axis_y]),
            decreasing = TRUE
          )[
            0:input$number_docs
          ],
        ]
        df_words_culled <- df_words[
          order(
            (correspondence_analysis$row$contrib[, input$axis_x] + correspondence_analysis$row$contrib[, input$axis_y]),
            decreasing = TRUE
          )[
            0:input$number_words
          ],
        ]
      }
      if (input$selection_criteria == "Inertia") {
        df_docs_culled <- df_docs[
          order(
            correspondence_analysis$call$marge.col,
            decreasing = TRUE
          )[
            0:input$number_docs
          ],
        ]
        df_words_culled <- df_words[
          order(
            correspondence_analysis$call$marge.row,
            decreasing = TRUE
          )[
            0:input$number_words
          ],
        ]
      }
      df_all <- rbind(df_docs, df_words)
      df_all_culled <- rbind(df_docs_culled, df_words_culled)

      # Plotting
      scatter <- ggplot(data = df_words, aes(x = x, y = y)) +
        geom_hline(yintercept = 0, size = .15) +
        geom_vline(xintercept = 0, size = .15)

      if (("Observations" %in% input$show_in_plots) == TRUE) {
        scatter <- scatter +
          geom_point(
            data = df_words,
            aes(x = x, y = y, colour = type, shape = type),
            alpha = .25,
            size = 1.5
          ) +
          geom_point(
            data = df_docs,
            aes(x = x, y = y, colour = type, shape = type),
            alpha = .25,
            size = 1.5
          )
      }

      if (("Labels" %in% input$show_in_plots) == TRUE) {
        scatter <- scatter +
          geom_text_repel(
            data = df_all_culled,
            aes(x = x, y = y, label = label, colour = type),
            size = (input$textsize / 10),
            alpha = 1, # colour = "black",
            stat = "identity",
            parse = FALSE,
            box.padding = unit(0.2, "lines"),
            point.padding = unit(1e-02, "lines"),
            segment.color = "white", # #666666
            segment.size = 0.0,
            arrow = NULL,
            force = .2,
            max.iter = 2000,
            nudge_x = 0,
            nudge_y = 0,
            na.rm = FALSE,
            show.legend = FALSE,
            inherit.aes = FALSE
          )
      }

      if (("Interpretations" %in% input$show_in_plots) == TRUE) {
        scatter <- scatter +
          geom_label(
            data = df_all,
            aes(x = max(x + .4), y = 0, label = stringr::str_wrap(df_annotate$label_x_plus, 10)),
            fill = "grey80",
            size = (input$textsize / 10)
          ) +
          geom_label(
            data = df_all,
            aes(x = min(x - .4), y = 0, label = stringr::str_wrap(df_annotate$label_x_min, 10)),
            fill = "grey80",
            size = (input$textsize / 10)
          ) +
          geom_label(
            data = df_all,
            aes(x = 0, y = max(y + .4), label = stringr::str_wrap(df_annotate$label_x_plus, 10)),
            fill = "grey80",
            size = (input$textsize / 10)
          ) +
          geom_label(
            data = df_all,
            aes(x = 0, y = min(y - .4), label = stringr::str_wrap(df_annotate$label_x_min, 10)),
            fill = "grey80",
            size = (input$textsize / 10)
          )
      }

      # Layout
      scatter <- scatter +
        theme_bw() +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          axis.title.x = element_blank(),
          axis.text = element_blank(),
          plot.title = element_text(size = 10),
          axis.ticks = element_blank()
        ) +
        scale_color_manual(
          values = c("gray30", "black", "red", "green", "purple", "orange", "brown")
        ) +
        theme(
          legend.position = c(.99, .99), # position of legend
          legend.justification = c(1, 1),
          legend.background = element_blank(),
          legend.title = element_blank()
        ) +
        labs(x = "", y = "") +
        ggtitle(
          paste0("Figure 1: Discursive space of the Syrian conflict, principal plane ",
            input$axis_x,
            "-",
            input$axis_y,
            sep = ""
          )
        ) +
        labs(
          caption = paste0(
            "The ", as.english(input$number_docs),
            " most contributing statements and ",
            as.english(input$number_words),
            " most contributing (translated) words along axis ",
            input$axis_x, " and ", input$axis_y,
            " are plotted."
          ),
          sep = ""
        ) +
        coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE)

      # Drawing scale on map...
      temp_scale <- list()
      temp_scale[["x_min"]] <- round(min(df_all$x), 1)
      temp_scale[["x_max"]] <- temp_scale[["x_min"]] + round(((max(df_all$x) - min(df_all$x)) / 10), 1)
      temp_scale[["x_middle"]] <- (temp_scale[["x_min"]] + temp_scale[["x_max"]]) / 2
      temp_scale[["label"]] <- temp_scale[["x_max"]] - temp_scale[["x_min"]]
      temp_scale[["y_lower"]] <- (temp_scale[["x_min"]] + temp_scale[["x_max"]]) / 2

      scatter <- scatter +
        geom_segment(
          data = df_all, aes(
            x = temp_scale$x_min,
            y = max(y),
            xend = temp_scale$x_max,
            yend = max(y)
          ), size = .15
        ) +
        geom_segment(
          data = df_all, aes(
            x = temp_scale$x_min,
            y = (max(y) + .02),
            xend = temp_scale$x_min,
            yend = (max(y) - .02)
          ), size = .15
        ) +
        geom_segment(
          data = df_all, aes(
            x = temp_scale$x_max,
            y = (max(y) + .02),
            xend = temp_scale$x_max,
            yend = (max(y) - .02)
          ), size = .15
        ) +
        annotate("text",
          x = temp_scale$x_middle,
          y = max(df_all$y),
          label = temp_scale$label,
          size = (input$textsize / 10),
          alpha = .8,
          hjust = .5,
          vjust = 1.5
        ) +
        annotate("text",
          x = temp_scale$x_middle,
          y = max(df_all$y),
          label = "scale",
          size = (input$textsize / 10),
          alpha = .8,
          hjust = .5,
          vjust = -.75
        ) +
        # Description amount variance
        annotate("text",
          x = max(df_all$x),
          y = 0,
          label = df_annotate$axis_x_description,
          size = (input$textsize / 10),
          alpha = .8,
          hjust = 1,
          vjust = -1
        ) +
        annotate("text",
          x = 0,
          y = max(df_all$y),
          label = df_annotate$axis_y_description,
          size = (input$textsize / 10),
          alpha = .8,
          hjust = 1,
          vjust = -1,
          angle = 90
        )

      # printing plot
      scatter
    },
    width = 750,
    height = 750,
    res = 100
  ) #
  # }, width = 2500, height = 2500, res = 300) # for publication


  # Additional data used in Shiny app ####
  output$selected_axis <- renderText({
    paste(
      "Axis ", input$axis_x,
      ", λ = ", round(correspondence_analysis$eig[input$axis_x, 1], 2),
      "; var. = ", round(correspondence_analysis$eig[input$axis_x, 2], 2), "%",
      sep = ""
    )
  })

  output$selected_axis1 <- renderText({
    paste(
      "Axis ", input$axis_x,
      ", λ = ", round(correspondence_analysis$eig[input$axis_x, 1], 2),
      "; var. = ", round(correspondence_analysis$eig[input$axis_x, 2], 2), "%",
      sep = ""
    )
  })

  output$selected_plane <- renderText({
    paste("Plane axes", input$axis_x, "-", input$axis_y, sep = " ")
  })

  output$selected_plane_suppl_correspondence_analysis <- renderText({
    paste("Plane axes", input$axis_x, "-", input$axis_y, sep = " ")
  })


  output$selected_cluster_words <- renderText({
    paste("Cluster ", input$cluster, sep = " ")
  })

  output$selected_cluster_docs <- renderText({
    paste("Cluster ", input$cluster, sep = " ")
  })

  output$selected.narrative_text <- renderText({
    paste("Selected text: ", input$narrative, " (Cluster ", input$cluster, ")", sep = "")
  })

  output$narrative <- renderTable(
    {
      Narrative <- unlist(corpus$Wordlists)
      Words.cluster <- HC$call$X[HC$call$X$clust == input$cluster, ]
      Document.indices <- mined_text$indices.docs.start[which(Shiny.df$`1`$Docs$Label == input$narrative)]:mined_text$indices.docs.end[which(Shiny.df$`1`$Docs$Label == input$narrative)]
      Word.indices <- unlist(mined_text$HierachicalIndex[rownames(Words.cluster)]) # list of indices of all words in cluster.

      Narrative[Word.indices[Word.indices %in% Document.indices]] <-
        lapply(Word.indices[Word.indices %in% Document.indices], function(x) paste0("(+)", Narrative[x], "(+)", sep = ""))

      Narrative <- data.frame(Text = paste0(Narrative[Document.indices], collapse = " "))
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    digits = 0
  )

  # Table Events ####
  Table.Events <- reactive({
    data.dates <- support_files$event_metadata
    data.dates["Date"] <- as.Date(paste0(data.dates$Event.Day, "/", data.dates$Event.Month, "/", data.dates$Event.Year, sep = ""), "%d/%m/%y")
    data.dates <- data.dates[data.dates$Date %in% seq(input$selectedDates[1], input$selectedDates[2], by = "days"), ]
    data.dates["Date"] <- as.character(data.dates$Date)

    data.dates <- data.dates[c("Label", "Date", "Description")]
  })

  output$table.events <- renderTable(
    {
      Table.Events()
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    digits = 4
  )

# -------------- End --------- #### 

# The following btis I will work on later: 
# 
#   # Supcorrespondence_analysis Chronological plot ####
#   output$chronologicalPlot <- renderPlot(
#     {
# 
#       # Building word data frame (only axis X)
#       if (input$language == "English") {
#         df_words <- data.frame(
#           label = support_files$translation_list[match(mined_text$v_culled_corpus[as.numeric(rownames(mined_text$lexical_table))], support_files$translation_list$word), 2],
#           x = correspondence_analysis$row$coord[, input$axis_x],
#           type = "Word"
#         )
#       } else {
#         df_words <- data.frame(
#           label = mined_text$v_culled_corpus[as.numeric(rownames(mined_text$lexical_table))],
#           x = correspondence_analysis$row$coord[, input$axis_x],
#           type = "Word"
#         )
#       }
#       # df_words <- df_words[!is.na(df_words$label), ]
# 
#       # culling amount of plotted items
#       if (input$selection_criteria == "Contribution (axis X only)") {
#         df_words <- df_words[order(correspondence_analysis$row$contrib[, input$axis_x], decreasing = TRUE)[0:input$number_words], ]
#       }
#       if (input$selection_criteria == "Contribution (axis X and Y)") {
#         df_words <- df_words[order(correspondence_analysis$row$contrib[, input$axis_x], decreasing = TRUE)[0:input$number_words], ]
#       }
#       if (input$selection_criteria == "Inertia") {
#         df_words <- df_words[order(correspondence_analysis$call$marge.row, decreasing = TRUE)[0:input$number_words], ]
#       }
# 
#       # creating data frame from supplementary items.
#       df_all <- data.frame(
#         Label = paste0(
#           as.character(support_files$document_sup_metadata$Org.Abbreviation[match(rownames(correspondence_analysis.sup$col.sup$coord), support_files$document_sup_metadata$Document)]),
#           1:nrow(correspondence_analysis.sup$col.sup$coord)
#         ),
#         Coord. = correspondence_analysis.sup$col.sup$coord[, input$axis_x],
#         date_full = as.Date(unlist(lapply(rownames(correspondence_analysis.sup$col.sup$coord), function(x) {
#           paste0(support_files$document_sup_metadata$Statement.Day[support_files$document_sup_metadata$Document == x], "/",
#             support_files$document_sup_metadata$Statement.Month[support_files$document_sup_metadata$Document == x], "/",
#             support_files$document_sup_metadata$Statement.Year[support_files$document_sup_metadata$Document == x],
#             sep = ""
#           )
#         })), "%d/%m/%y"),
#         Org = as.character(support_files$document_sup_metadata$Org.Abbreviation[match(rownames(correspondence_analysis.sup$col.sup$coord), support_files$document_sup_metadata$Document)])
#       )
# 
#       # Making data frames for documents
#       df_docs <- data.frame(
#         Label = unlist(lapply(rownames(correspondence_analysis$col$contrib), function(x) support_files$document_metadata$Label[support_files$document_metadata$Document == x])),
#         Coord. = correspondence_analysis$col$coord[, input$axis_x],
#         date_full = as.Date(unlist(lapply(rownames(correspondence_analysis$col$contrib), function(x) {
#           paste0(support_files$document_metadata$Statement.Day[support_files$document_metadata$Document == x], "/",
#             support_files$document_metadata$Statement.Month[support_files$document_metadata$Document == x], "/",
#             support_files$document_metadata$Statement.Year[support_files$document_metadata$Document == x],
#             sep = ""
#           )
#         })), "%d/%m/%y"),
#         Org = "General"
#       )
# 
#       df_all <- rbind(df_all, df_docs)
# 
#       df_annotate <- data.frame(
#         label_x_plus = support_files$principal_axes_metadata$Plus.Description[input$axis_x],
#         label_x_min  = support_files$principal_axes_metadata$Min.Description[input$axis_x]
#       )
# 
#       df.events <- support_files$event_metadata
#       df.events["Date"] <- as.Date(paste0(df.events$Event.Day, "/", df.events$Event.Month, "/", df.events$Event.Year, sep = ""), "%d/%m/%y")
#       df.events <- df.events[df.events$Date %in% seq(input$selectedDates[1], input$selectedDates[2], by = "days"), ]
# 
#       # making selections in supplementary corpus by Org and Date range. NB NOT in primary corpus!
#       df_all <- df_all[df_all$Org %in% input$SelectedOrgs, ]
#       df_all <- df_all[df_all$date_full %in% seq(input$selectedDates[1], input$selectedDates[2], by = "days"), ]
# 
#       # Plotting
#       Supcorrespondence_analysis.Chrn.Plot <- ggplot(data = df_all)
# 
#       if (("Observations" %in% input$show_in_plots) == TRUE) {
#         Supcorrespondence_analysis.Chrn.Plot <- Supcorrespondence_analysis.Chrn.Plot +
#           geom_point(data = df_all, aes(x = date_full, y = Coord., color = Org), alpha = .5)
#       }
# 
#       if (("Interpretations" %in% input$show_in_plots) == TRUE) {
#         Supcorrespondence_analysis.Chrn.Plot <- Supcorrespondence_analysis.Chrn.Plot +
#           geom_label(data = df_annotate, aes(x = min(input$selectedDates), y = max(df_all$Coord.) - .3, label = stringr::str_wrap(label_x_plus, 10)), fill = "grey80", size = (input$textsize / 10)) +
#           geom_label(data = df_annotate, aes(x = min(input$selectedDates), y = min(df_all$Coord.) + .3, label = stringr::str_wrap(label_x_min, 10)), fill = "grey80", size = (input$textsize / 10))
#       }
# 
#       if (("Smoothed lines" %in% input$show_in_plots) == TRUE) {
#         Supcorrespondence_analysis.Chrn.Plot <- Supcorrespondence_analysis.Chrn.Plot +
#           geom_smooth(data = df_all, aes(x = date_full, y = Coord., color = Org, fill = Org), span = .75, size = 1, alpha = .2, level = .9)
#       }
# 
#       Supcorrespondence_analysis.Chrn.Plot <- Supcorrespondence_analysis.Chrn.Plot +
#         geom_hline(yintercept = 0, size = .3, colour = "grey30") +
#         geom_vline(xintercept = df.events$Date, size = .15, colour = "red", alpha = .5) +
#         geom_text(data = df.events, aes(x = Date, label = Label), y = (min(df_all$Coord.)), colour = "red")
# 
#       Supcorrespondence_analysis.Chrn.Plot <- Supcorrespondence_analysis.Chrn.Plot +
#         theme_bw() +
#         theme(
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           axis.title.x = element_blank(),
#           plot.title = element_text(size = 10),
#           axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1, size = (input$textsize / 3)),
#           plot.caption = element_text(hjust = .5)
#           # panel.border=element_blank()
#         ) +
#         theme(
#           legend.key = element_rect(fill = alpha("grey70", 0), linetype = 0), # small boxes in legend
#           legend.position = c(1, 1), # position of legend
#           legend.justification = c(1, 1),
#           legend.background = element_rect(fill = alpha("white", 0.75), colour = "white"),
#           legend.title = element_blank()
#         ) +
#         labs(caption = stringr::str_wrap(
#           paste(
#             "Plot of supplementary statements, smoothing method is loess with a confidence interval of 0.9.", " Events plotted:",
#             paste(support_files$event_metadata$Label, support_files$event_metadata$Description, sep = " ", collapse = "; ")
#           ), 200
#         )) +
#         ggtitle(paste0("Figure 2: Discursive positionalities of supplementary statements across time, principal axis ", input$axis_x, ".", sep = " ")) +
#         scale_y_continuous(breaks = df_words$x, labels = df_words$label, name = NULL, guide = guide_axis(check.overlap = TRUE)) # n.dodge=3  guide = guide_axis(check.overlap = TRUE)
#       # scale_y_discrete
#       #  scale_y_continuouslabels = df_words$label, breaks = df_words$x, name = NULL, labels = df_words$label
# 
#       # Drawing scale on map...
#       temp_scale <- list()
#       temp_scale[["y.max"]] <- round(max(df_all$Coord.), 1)
#       temp_scale[["y.min"]] <- temp_scale[["y.max"]] - .3
#       temp_scale[["y.middle"]] <- (temp_scale[["y.min"]] + temp_scale[["y.max"]]) / 2
#       temp_scale[["label"]] <- temp_scale[["y.max"]] - temp_scale[["y.min"]]
#       temp_scale[["y.lower"]] <- (temp_scale[["y.min"]] + temp_scale[["y.max"]]) / 2
#       temp_scale[["su.Date"]] <- (max(df_all$date_full) - min(df_all$date_full)) / 150
# 
#       Supcorrespondence_analysis.Chrn.Plot <- Supcorrespondence_analysis.Chrn.Plot +
#         geom_segment(
#           data = df_all, aes(
#           x = min(date_full) - temp_scale$su.Date, y = temp_scale$y.min,
#           xend = min(date_full) - temp_scale$su.Date, yend = temp_scale$y.max
#         ), size = .05) +
#         geom_segment(data = df_all, aes(
#           x = min(date_full), y = temp_scale$y.max,
#           xend = min(date_full) - (temp_scale$su.Date * 2), yend = temp_scale$y.max
#         ), size = .05) +
#         geom_segment(data = df_all, aes(
#           x = min(date_full), y = temp_scale$y.min,
#           xend = min(date_full) - (temp_scale$su.Date * 2), yend = temp_scale$y.min
#         ), size = .05) +
#         annotate("text", x = min(df_all$date_full) + (temp_scale$su.Date * 1), y = temp_scale$y.middle, label = temp_scale$label, size = (input$textsize / 7.5), alpha = .8) + # ,  hjust = .5, vjust = 1.5)
#         annotate("text", x = min(df_all$date_full) - (temp_scale$su.Date * 3), y = temp_scale$y.middle, label = "scale", size = (input$textsize / 7.5), angle = 90, alpha = .8) # ,  hjust = .5, vjust = -.75) +
# 
#       Supcorrespondence_analysis.Chrn.Plot
#     },
#     width = 1100,
#     height = 700,
#     res = 100
#   )
#   # } , width = 2200, height = 1400, res = 200)
# 
#   output$click_info <- renderPrint({
# 
#     # creating data frame from supplementary items.
#     df_all <- data.frame(
#       Coord. = correspondence_analysis.sup$col.sup$coord[, input$axis_x],
#       date_full = as.Date(unlist(lapply(rownames(correspondence_analysis.sup$col.sup$coord), function(x) {
#         paste0(support_files$document_sup_metadata$Statement.Day[support_files$document_sup_metadata$Document == x], "/",
#           support_files$document_sup_metadata$Statement.Month[support_files$document_sup_metadata$Document == x], "/",
#           support_files$document_sup_metadata$Statement.Year[support_files$document_sup_metadata$Document == x],
#           sep = ""
#         )
#       })), "%d/%m/%y"),
#       Org = as.character(support_files$document_sup_metadata$Org.Abbreviation[match(rownames(correspondence_analysis.sup$col.sup$coord), support_files$document_sup_metadata$Document)])
#     )
# 
#     # Making data frames for documents
#     df_docs <- data.frame(
#       Coord. = correspondence_analysis$col$coord[, input$axis_x],
#       date_full = as.Date(unlist(lapply(rownames(correspondence_analysis$col$contrib), function(x) {
#         paste0(support_files$document_metadata$Statement.Day[support_files$document_metadata$Document == x], "/",
#           support_files$document_metadata$Statement.Month[support_files$document_metadata$Document == x], "/",
#           support_files$document_metadata$Statement.Year[support_files$document_metadata$Document == x],
#           sep = ""
#         )
#       })), "%d/%m/%y"),
#       Org = "General"
#     )
# 
#     df_all <- rbind(df_all, df_docs)
# 
#     # making selections in supplementary corpus by Org and Date range. NB NOT in primary corpus!
#     df_all <- df_all[df_all$Org %in% input$SelectedOrgs, ]
#     df_all <- df_all[df_all$date_full %in% seq(input$selectedDates[1], input$selectedDates[2], by = "days"), ]
# 
#     nearPoints(df_all, input$chronologicalPlot_click, addDist = TRUE)
#   })
# 
#   output$brush_info <- renderPrint({
# 
#     # creating data frame from supplementary items.
#     df_all <- data.frame(
#       Coord. = correspondence_analysis.sup$col.sup$coord[, input$axis_x],
#       date_full = as.Date(unlist(lapply(rownames(correspondence_analysis.sup$col.sup$coord), function(x) {
#         paste0(support_files$document_sup_metadata$Statement.Day[support_files$document_sup_metadata$Document == x], "/",
#           support_files$document_sup_metadata$Statement.Month[support_files$document_sup_metadata$Document == x], "/",
#           support_files$document_sup_metadata$Statement.Year[support_files$document_sup_metadata$Document == x],
#           sep = ""
#         )
#       })), "%d/%m/%y"),
#       Org = as.character(support_files$document_sup_metadata$Org.Abbreviation[match(rownames(correspondence_analysis.sup$col.sup$coord), support_files$document_sup_metadata$Document)])
#     )
# 
#     # Making data frames for documents
#     df_docs <- data.frame(
#       Coord. = correspondence_analysis$col$coord[, input$axis_x],
#       date_full = as.Date(unlist(lapply(rownames(correspondence_analysis$col$contrib), function(x) {
#         paste0(support_files$document_metadata$Statement.Day[support_files$document_metadata$Document == x], "/",
#           support_files$document_metadata$Statement.Month[support_files$document_metadata$Document == x], "/",
#           support_files$document_metadata$Statement.Year[support_files$document_metadata$Document == x],
#           sep = ""
#         )
#       })), "%d/%m/%y"),
#       Org = "General"
#     )
# 
#     df_all <- rbind(df_all, df_docs)
# 
#     # making selections in supplementary corpus by Org and Date range. NB NOT in primary corpus!
#     df_all <- df_all[df_all$Org %in% input$SelectedOrgs, ]
#     df_all <- df_all[df_all$date_full %in% seq(input$selectedDates[1], input$selectedDates[2], by = "days"), ]
# 
#     brushedPoints(df_all, input$chronologicalPlot_brush)

  }

# 
