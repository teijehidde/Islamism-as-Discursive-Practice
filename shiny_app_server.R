#--------- Shiny App: Server ---------####

ShinyServer <- function(input, output, session) {

  #--------- data original and culled corpus ---------#### 
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
  
  #--------- results CA analysis ---------#### 
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
    } 
  )

  table_eigenvalues <- data.frame(correspondence_analysis$eig[])
  colnames(table_eigenvalues) <- c("Eigenvalue", "Variance", "Cumulative")
  rownames(table_eigenvalues) <- paste0("Axis ", 1:nrow(table_eigenvalues), sep = " ")
  
  output$table_eigenvalues <- renderTable(
    {
      table_eigenvalues
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    rownames = TRUE,
    digits = 4
  )

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
  
  #--------- scatter plot ---------#### 
  output$scatter_plot <- renderPlot(
    {
      # creating data frames for plot. 
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

      # culling data frames 
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
      
      # creating annotations and scale 
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
      
      temp_scale <- list()
      temp_scale[["x_min"]] <- round(min(df_all$x), 1)
      temp_scale[["x_max"]] <- temp_scale[["x_min"]] + round(((max(df_all$x) - min(df_all$x)) / 10), 1)
      temp_scale[["x_middle"]] <- (temp_scale[["x_min"]] + temp_scale[["x_max"]]) / 2
      temp_scale[["label"]] <- temp_scale[["x_max"]] - temp_scale[["x_min"]]
      temp_scale[["y_lower"]] <- (temp_scale[["x_min"]] + temp_scale[["x_max"]]) / 2
      
      # Begin plotting graph
      scatter <- ggplot(data = df_words, aes(x = x, y = y)) +
        geom_hline(yintercept = 0, size = .15) +
        geom_vline(xintercept = 0, size = .15)

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
          ) +
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
          values = c("gray30", "black")
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
      
      # adding annotations and scale to plot.  
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
    width = 750, height = 750, res = 100
  ) 
  
  #--------- additional information provided in app ---------#### 
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

  # [This is something I can actually add to the app a little later on.] 
  output$narrative <- renderTable(
    {
      Narrative <- unlist(corpus$Wordlists)
      Words.cluster <- HC$call$X[HC$call$X$clust == input$cluster, ]
      Document.indices <- mined_text$indices.docs.start[which(Shiny.df$`1`$Docs$Label == input$narrative)]:mined_text$indices.docs.end[which(Shiny.df$`1`$Docs$Label == input$narrative)]
      Word.indices <- unlist(mined_text$HierachicalIndex[rownames(Words.cluster)]) 

      Narrative[Word.indices[Word.indices %in% Document.indices]] <-
        lapply(Word.indices[Word.indices %in% Document.indices], function(x) paste0("(+)", Narrative[x], "(+)", sep = ""))

      Narrative <- data.frame(Text = paste0(Narrative[Document.indices], collapse = " "))
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    digits = 0
  )
  
}

# -------------- End --------- #### 

