
#--------- Shiny App: Server ---------####

ShinyServer <- function(input, output, session) {
  
  #--------- Tab: Corpus ---------#### 
  culled_lexical_table <- reactive({
    CullingMinedText(
      data = mined_text, 
      culled_words = c(as.character(support_files$tool_words[, 1]), org_names_stemmed),
      v_min = input$v_min
    )
  }) 
  
  output$corpus_original <- renderTable(
    {
      data.frame(
        documents = length(corpus$file_names),
        number_occurences_t = length(unlist(mined_text$t_stemmed_corpus)),
        number_words_v = length(mined_text$v_corpus),
        avg_t_per_v = length(unlist(mined_text$t_stemmed_corpus)) / length(mined_text$v_corpus)
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
        number_occurences_t = sum(culled_lexical_table()),  
        number_words_v = nrow(culled_lexical_table()),
        avg_t_per_v = sum(culled_lexical_table()) / nrow(culled_lexical_table())
      )
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    digits = 0
  )
  
  output$corpus_names <- renderTable(
    {
      data_doc <- support_files$document_metadata
      data_org <- support_files$organization_metadata
      abbr_list <- unique(data_doc$org_abbreviated)
      eng_org_list <- list()
      arab_org_list <- list()
      self_description_list <- list()
      charter_list <- list()
      
      for (i in 1:length(abbr_list)) {
        self_description_list <- append(
          self_description_list, paste0(
            "",
            data_doc[data_doc$org_abbreviated == abbr_list[i] & data_doc$type == "self_description", ]$label
          )
        )
        charter_list <- append(
          charter_list, paste0(
            "",
            data_doc[data_doc$org_abbreviated == abbr_list[i] & data_doc$type == "charter", ]$label
          )
        )
        eng_org_list <- append(
          eng_org_list, paste0(
            "",
            data_org[data_org$org_abbreviated == paste0(abbr_list[i]), ]$org_english
          )
        )
        arab_org_list <- append(
          arab_org_list, paste0(
            "",
            data_org[data_org$org_abbreviated == paste0(abbr_list[i]), ]$org_arabic
          )
        )
      }
      
      return(
        tibble(
          Organization_arabic = arab_org_list,
          Organization_english = eng_org_list,
          Self_description = self_description_list,
          Charter = charter_list
        )
      )
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    digits = 0,
    align = c("rlcc")
  )
  
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
  
  #--------- Tab: Correspondence Analysis ---------####
  cor_an <- reactive(
    {
      CA(
        culled_lexical_table(),
        ncp = NULL,
        row.sup = NULL,
        col.sup = NULL,
        quanti.sup = NULL,
        quali.sup = NULL,
        graph = FALSE,
        row.w = NULL
        )
      }
  )
  
  output$scree_plot <- renderPlot({
      ggplot(
        data = data.frame(
          variance = cor_an()$eig[, 2],
          axis = seq(1:nrow(cor_an()$eig))
          ),
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
    })

  output$table_eigenvalues <- renderTable(
     {
       data.frame(cor_an()$eig[])
     },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    rownames = TRUE,
    digits = 4
  )

  output$selected_axis <- renderText({
   paste(
     "Axis ", input$axis_x,
     ", λ = ", round(cor_an()$eig[input$axis_x, 1], 2),
     "; var. = ", round(cor_an()$eig[input$axis_x, 2], 2), "%",
     sep = ""
     )
  })
   
  output$table_words <- renderTable(
    {
      BuildTableWords(
        data = mined_text,
        ca_analysis = cor_an(),
        dim_x = input$axis_x,
        translation_list = support_files$translation_list,
        order = input$selection_criteria
      )
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    digits = 4
  )

  output$selected_axis1 <- renderText({
   paste(
     "Axis ", input$axis_x,
     ", λ = ", round(cor_an()$eig[input$axis_x, 1], 2),
     "; var. = ", round(cor_an()$eig[input$axis_x, 2], 2), "%",
     sep = ""
   )
  })
   
  output$table_docs <- renderTable(
    {
      BuildTableDocs(
        ca_analysis = cor_an(),
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
  
  output$scatter_plot <- renderPlot(
    {
      # creating data frames for plot.
      df_docs <- data.frame(
        label = sapply(
          rownames(cor_an()$col$contrib),
          function(x) {
            support_files$document_metadata$label[
              support_files$document_metadata$file_name == x
            ]
          }
        ),
        x = cor_an()$col$coord[, input$axis_x],
        y = cor_an()$col$coord[, input$axis_y],
        type = "statement"
      )

      if (input$language == "English") {
        df_words <- data.frame(
          label = support_files$translation_list[
            match(
              mined_text$v_corpus[
                as.numeric(rownames(culled_lexical_table()))
              ], support_files$translation_list$word
            ), 2
          ],
          x = cor_an()$row$coord[, input$axis_x],
          y = cor_an()$row$coord[, input$axis_y],
          type = "word"
        )
      } else {
        df_words <- data.frame(
          label = mined_text$v_corpus[
            as.numeric(rownames(culled_lexical_table()))
          ],
          x = cor_an()$row$coord[, input$axis_x],
          y = cor_an()$row$coord[, input$axis_y],
          type = "word"
        )
      }

      # culling data frames
      if (input$selection_criteria == "Contribution (axis X only)") {
        df_docs_culled <- df_docs[
          order(
            cor_an()$col$contrib[, input$axis_x],
            decreasing = TRUE
          )[
            0:input$number_docs
          ],
        ]
        df_words_culled <- df_words[
          order(
            cor_an()$row$contrib[, input$axis_x],
            decreasing = TRUE
          )[
            0:input$number_words
          ],
        ]
      }
      if (input$selection_criteria == "Contribution (axis X and Y)") {
        df_docs_culled <- df_docs[
          order(
            (cor_an()$col$contrib[, input$axis_x] + cor_an()$col$contrib[, input$axis_y]),
            decreasing = TRUE
          )[
            0:input$number_docs
          ],
        ]
        df_words_culled <- df_words[
          order(
            (cor_an()$row$contrib[, input$axis_x] + cor_an()$row$contrib[, input$axis_y]),
            decreasing = TRUE
          )[
            0:input$number_words
          ],
        ]
      }
      if (input$selection_criteria == "Inertia") {
        df_docs_culled <- df_docs[
          order(
            cor_an()$call$marge.col,
            decreasing = TRUE
          )[
            0:input$number_docs
          ],
        ]
        df_words_culled <- df_words[
          order(
            cor_an()$call$marge.row,
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
          ", λ = ", round(cor_an()$eig[input$axis_x, 1], 2),
          "; var. = ", round(cor_an()$eig[input$axis_x, 2], 2), "%",
          sep = ""
        ),
        axis_y_description = paste(
          "Axis ", input$axis_y,
          ", λ = ", round(cor_an()$eig[input$axis_y, 1], 2),
          "; var. = ", round(cor_an()$eig[input$axis_y, 2], 2), "%",
          sep = ""
        )
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
      # 
      # # adding annotations and scale to plot.
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
  )
  
  #--------- Tab: Hierarchical Clustering ---------####
  hi_clus <- reactive(
    {
      HCPC(
        cor_an(), 
        metric = input$cluster_method, # "manhattan", # metric = "euclidean" and "manhattan". 
        nb.clust = input$nb_clust, 
        order = TRUE, 
        graph = FALSE
      )
    }
  ) 
  
  output$select_cluster <-renderUI(
    {
      max_cluster <- length(hi_clus()$desc.var)
      selectInput(
        inputId = "selected_cluster",
        label = "Select cluster:",
        choices = c(1:max_cluster)
        )
      }
  )
  
  output$select_text_options <-renderUI(
    {
      selectInput(
        inputId = "selected_text",
        label = "Select text:",
        choices = rownames(
          hi_clus()$desc.var[[
            input$selected_cluster
            ]]
          ) 
      )
    }
  )
  
  output$dendroPlot <- renderPlot(
    {
      par(cex=(input$textsize / 35)) # to reduce textsize in plot. 
      plot.HCPC(
        hi_clus(), 
        choice = "tree", 
        tree.barplot = FALSE, 
        title = ""
        )
      par(cex=1) 
  }, height = 700)
   
   output$selected_cluster_words <- renderText({ 
     paste("Cluster ", input$selected_cluster, sep = " ")
   })
   
   output$selected_cluster_docs <- renderText({ 
     paste("Cluster ", input$selected_cluster, sep = " ")
   })
   
   output$selected_cluster_axes <- renderText({ 
     paste("Cluster ", input$selected_cluster, sep = " ")
   })
   
   output$table_cluster_words <- renderTable(
     {
       table <- (hi_clus()$desc.ind$dist[[input$selected_cluster]])
       names(table) <- mined_text$v_corpus[
         as.integer(names(table)) 
         ]
       return (t(table))
     },
     striped = TRUE,
     hover = TRUE,
     bordered = TRUE,
     digits = 6
   )
   
   output$table_cluster_docs <- renderTable(
     {
       hi_clus()$desc.var[[input$selected_cluster]]
     },
     striped = TRUE,
     hover = TRUE,
     bordered = TRUE,
     rownames = TRUE, 
     digits = 6
   )
   
   output$table_cluster_axes <- renderTable(
      {
        hi_clus()$desc.axes[2][1]$quanti[[input$selected_cluster]]
      },
     striped = TRUE,
     hover = TRUE,
     bordered = TRUE,
     rownames = TRUE, 
     digits = 6
   )
   
   output$clusterCA_Plot <- renderPlot(
       {
         if (input$language_clust == "English") {
           df_words <- data.frame(
             label = support_files$translation_list[
               match(
                 mined_text$v_corpus[
                   as.numeric(rownames(culled_lexical_table()))
                 ], support_files$translation_list$word
               ), 2
             ],
             x = cor_an()$row$coord[, input$axis_x_clust],
             y = cor_an()$row$coord[, input$axis_y_clust],
             cluster = hi_clus()$data.clust[
               match(
                 rownames(cor_an()$row$coord), 
                 rownames(hi_clus()$data.clust)
                 ), 
               ]$clust,
             type = "word"
           )
         } else {
           df_words <- data.frame(
             label = mined_text$v_corpus[
               as.numeric(rownames(culled_lexical_table()))
             ],
             x = cor_an()$row$coord[, input$axis_x_clust],
             y = cor_an()$row$coord[, input$axis_y_clust],
             cluster = hi_clus()$data.clust[
               match(
                 rownames(cor_an()$row$coord), 
                 rownames(hi_clus()$data.clust)
               ), 
             ]$clust,
             type = "word"
           )
         }
         
         # culling data frames
         if (input$selection_criteria_clust == "Contribution (axis X only)") {
           df_words_culled <- df_words[
             order(
               cor_an()$row$contrib[, input$axis_x_clust],
               decreasing = TRUE
             )[
               0:input$number_words_clust
             ],
           ]
         }
         if (input$selection_criteria_clust == "Contribution (axis X and Y)") {
           df_words_culled <- df_words[
             order(
               (cor_an()$row$contrib[, input$axis_x_clust] + cor_an()$row$contrib[, input$axis_y_clust]),
               decreasing = TRUE
             )[
               0:input$number_words_clust
             ],
           ]
         }
         if (input$selection_criteria_clust == "Inertia") {
           df_words_culled <- df_words[
             order(
               cor_an()$call$marge.row,
               decreasing = TRUE
             )[
               0:input$number_words_clust
             ],
           ]
         }
         df_all <- df_words
         df_all_culled <- df_words_culled
         
         # creating annotations and scale
         df_annotate <- data.frame(
           axis_x_description = paste(
             "Axis ", input$axis_x_clust,
             ", λ = ", round(cor_an()$eig[input$axis_x_clust, 1], 2),
             "; var. = ", round(cor_an()$eig[input$axis_x_clust, 2], 2), "%",
             sep = ""
           ),
           axis_y_description = paste(
             "Axis ", input$axis_y_clust,
             ", λ = ", round(cor_an()$eig[input$axis_y_clust, 1], 2),
             "; var. = ", round(cor_an()$eig[input$axis_y_clust, 2], 2), "%",
             sep = ""
           )
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
             aes(x = x, y = y, shape = cluster),
             colour = "black",
             alpha = .25,
             size = 1.5
           ) +
           geom_text_repel(
             data = df_all_culled,
             aes(x = x, y = y, label = label),
             colour = "black",
             size = (input$textsize_clust / 10),
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
           theme(
             legend.position = c(.99, .99), # position of legend
             legend.justification = c(1, 1),
             legend.background = element_blank(),
             legend.title = element_blank()
           ) +
           labs(x = "", y = "") +
           ggtitle(
             paste0("Figure 1: Discursive space of the Syrian conflict, principal plane ",
                    input$axis_x_clust,
                    "-",
                    input$axis_y_clust,
                    sep = ""
             )
           ) +
           labs(
             caption = paste0(
               "The ", as.english(input$number_words_clust),
               " most contributing (translated) words along axis ",
               input$axis_x_clust, " and ", input$axis_y_clust,
               " are plotted."
             ),
             sep = ""
           ) +
           coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE)
         # 
         # # adding annotations and scale to plot.
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
                    size = (input$textsize_clust / 10),
                    alpha = .8,
                    hjust = .5,
                    vjust = 1.5
           ) +
           annotate("text",
                    x = temp_scale$x_middle,
                    y = max(df_all$y),
                    label = "scale",
                    size = (input$textsize_clust / 10),
                    alpha = .8,
                    hjust = .5,
                    vjust = -.75
           ) + 
           # Description amount variance
           annotate("text",
                    x = max(df_all$x),
                    y = 0,
                    label = df_annotate$axis_x_description,
                    size = (input$textsize_clust / 10),
                    alpha = .8,
                    hjust = 1,
                    vjust = -1
           ) +
           annotate("text",
                    x = 0,
                    y = max(df_all$y),
                    label = df_annotate$axis_y_description,
                    size = (input$textsize_clust / 10),
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
     )
   
   output$selected_text_output <- renderText(
     { 
       paste("Selected text: ", input$selected_text, " (Cluster ", input$selected_cluster, ")", sep = "")
       }
   )
   
   output$selected_text_table <- renderTable(
     {
       temp <- hi_clus()$desc.var[[input$selected_cluster]]
       t(temp[
          rownames(temp) == input$selected_text,
         ])
   }, striped = TRUE, hover = TRUE, bordered  = TRUE, digits = 4, rownames = FALSE)
   
   output$selected_text_narrative <- renderTable({

     narrative  <- unlist(corpus$word_lists)
     words_cluster <- hi_clus()$call$X [
       hi_clus()$call$X$clust == input$selected_cluster, 
       ]
     document_indices <- mined_text$index$start[
       which(mined_text$index$file_names == input$selected_text)
       ]:
       mined_text$index$end[
         which(mined_text$index$file_names == input$selected_text)
         ]
     word_indices <- unlist(
       mined_text$hierachical_index[
         mined_text$v_corpus[
           as.integer(rownames(words_cluster))
           ] # provides list of indices of all words in cluster.
         ]
       )

     narrative[
       word_indices[
         word_indices %in% document_indices
         ]
       ] <-
       lapply(word_indices[
         word_indices %in% document_indices
         ], function(x) 
         paste0(
           "(+)", narrative[x], "(+)", sep = ""
           )
         )

     narrative <- data.frame(
       Text = paste0(
         narrative[document_indices], collapse = " "
         )
       )
   }, striped = TRUE, hover = TRUE, bordered  = TRUE, digits = 0)
  
   #--------- tab: Words in Context ---------####
   output$options_words <-renderUI({
     options <- mined_text$v_corpus[
       as.integer(rownames(culled_lexical_table()))
     ]
     selectInput(
       inputId = "selected_word_collocates",
       label = "Select word for collocates:",
       choices = options,
       selected = input$selected_word_collocates
     )
   })
   
   output$selected_word <- renderText({
     paste("Word in its sentence context: ", input$selected_word_collocates, sep = " ")
   })
   
   output$words_in_context <- renderTable(
     {
       BuildTableCollocates(
         data = mined_text,
         word = input$selected_word_collocates,
         range_pre = input$range_pre,
         range_post = input$range_post
       )
     },
     striped = TRUE,
     hover = TRUE,
     bordered = TRUE,
     rownames = FALSE,
     align = c("lrcl")
   )
} 


# -------------- End --------- ####