#----------- Functions Analysis -----------####

LoadData <- function(dir) {
  output <- list()
  output[["dir"]] <- dir
  output[["file_names"]] <- list.files(output[["dir"]], full.names = TRUE)
  output[["documents"]] <- lapply(
    output$file_names, function(x) {
      paste0(
        readLines(x),
        collapse = ""
      )
    }
  )
  names(output$documents) <- list.files(
    output[["dir"]],
    full.names = FALSE
  )
  output[["word_lists"]] <- lapply(
    output$documents, function(x) {
      strsplit(
        x,
        split = " "
      )
    }
  )

  return(output)
}

StemmingWords <- function(x,
                          chars,
                          latin = TRUE,
                          digit = TRUE,
                          punctuation = TRUE) {
  x <- x[[1]]

  for (i in 1:nrow(chars)) {
    x <- gsub(chars[i, 1], "", x)
  }

  if (latin == TRUE) {
    x <- gsub("a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z", "", x)
  }
  if (digit == TRUE) {
    x <- gsub("[[:digit:]]", "", x)
  }
  if (punctuation == TRUE) {
    x <- gsub("[[:punct:]]", "", x)
  }

  x <- gsub('"', "", x)

  return(x)
}

MiningText <- function(x, v_min = 0, drop_words = NULL) { # corpus should be list of text factors per part.

  y <- list()

  y[["t_corpus"]] <- unlist(x)
  y[["vi_frequency_words"]] <- table(y$t_corpus)
  y[["v_original_corpus"]] <- unique(y$t_corpus)

  for (i in 1:length(x)) {
    y[["index_end"]][[i]] <- length(unlist(x[1:i]))
  }
  y[["index_start"]] <- c(1, y$index_end + 1)

  y[["v_culled_corpus"]] <- names(
    y$vi_frequency_words[y$vi_frequency_words > v_min]
  )
  y[["v_culled_corpus"]] <- y$v_culled_corpus[!(y$v_culled_corpus %in% drop_words)]
  y[["v_culled_corpus"]] <- y$v_culled_corpus[!(y$v_culled_corpus == "")]

  y[["hierachical_index"]] <- lapply(
    y$v_culled_corpus, function(x) {
      as.vector(
        which(
          y$t_corpus == x
        )
      )
    }
  )
  names(y$hierachical_index) <- y$v_culled_corpus
  y$hierachical_index <- y$hierachical_index[
    order(
      sapply(
        y$hierachical_index, function(x) length(x)
      )
    )
  ]

  y[["segmented_hi"]] <- lapply(
    1:length(x),
    function(z) {
      as.data.frame(
        table(
          match(
            y$t_corpus[y$index_start[z]:y$index_end[z]], y$v_culled_corpus
          )
        )
      )
    }
  )
  names(y$segmented_hi) <- names(x)

  y[["lexical_table"]] <- suppressWarnings(Reduce(
    function(...) full_join(..., by = "Var1"), y$segmented_hi
  ))
  rownames(y$lexical_table) <- y$lexical_table$Var1 # y$v_culled_corpus #[y$lexical_table$Var1] NB! THIS IS A BUG IN R, RTL LANGUAGE MIX UP.
  y$lexical_table <- y$lexical_table[, !(names(y$lexical_table) == "Var1")]
  colnames(y$lexical_table) <- names(x)
  y$lexical_table[is.na(y$lexical_table)] <- 0

  return(y)
}

#----------- Functions Presentation Results  -----------####
BuildTableWords <- function(dim_x, dim_y, dim_z, lexical_table, translation_list, order) {
  table <- data.frame(
    Words = mined_text$v_culled_corpus[
      as.numeric(rownames(mined_text$lexical_table))
      ],
    Rel_Wt = correspondence_analysis$call$marge.row,
    Inertia = correspondence_analysis$row$inertia,
    Coord_X = correspondence_analysis$row$coord[, dim_x], 
    Contr_X = correspondence_analysis$row$contrib[, dim_x], 
    Cos2_X = correspondence_analysis$row$cos2[, dim_x]
  )

  table[["Translation"]] <- support_files$translation_list[
     match(
       mined_text$v_culled_corpus[
         as.numeric(rownames(mined_text$lexical_table))
         ], support_files$translation_list$word
       ), 2
     ]
  
  freq.temp <- data.frame(
     words = mined_text$v_culled_corpus[
       as.numeric(rownames(mined_text$lexical_table))
     ],
     freq = rowSums(mined_text$lexical_table)
   )
   
  table[["Freq"]] <- as.character(
     freq.temp[
       match(table$Words, freq.temp$words
             ), 2
       ]
     )
   
  if (order == "Inertia") {
      table <- table[order(table$Inertia, decreasing = TRUE), ]
  } else {
      table <- table[order(table$Contr_X, decreasing = TRUE), ]
  }
  
  rownames(table) <- NULL
  
  return( 
    table[c("Words", "Translation", "Rel_Wt", "Inertia", "Freq", "Coord_X", "Contr_X", "Cos2_X")]
  )
  
}

BuildTableDocs <- function(dim_x, corpus_metadata, order = "Inertia") {
  table <- data.frame(
    Rel_Wt = correspondence_analysis$call$marge.col,
    Inertia = correspondence_analysis$col$inertia,
    Coord_X = correspondence_analysis$col$coord[, dim_x], 
    Contr_X = correspondence_analysis$col$contrib[, dim_x], 
    Cos2_X = correspondence_analysis$col$cos2[, dim_x],
    File_name = rownames(correspondence_analysis$col$contrib)
  )

  temp <- data.frame(
    file_name = corpus_metadata$file_name,
    label = corpus_metadata$label,
    title = corpus_metadata$title,
    org = corpus_metadata$org_abbreviated
  )

  table[['Label']] <- temp[
    match(
      table$File_name,
      temp$file_name
      ), 2
    ]
  table[['Title']] <- temp[
    match(
      table$File_name,
      temp$file_name
      ), 3
    ]

  if (order == "Inertia") {
    table <- table[order(table$Inertia, decreasing = TRUE), ]
  } else {
    table <- table[order(table$Contr_X, decreasing = TRUE), ]
  }

  rownames(table) <- NULL
  
  return(
    table[c("Label", "Title", "Rel_Wt", "Inertia", "Coord_X", "Contr_X", "Cos2_X")]
  )
}

# [ I might be able to combine this with clicking on CA graph?]
PrintExampleSentences <- function(word, range = 5) {
  temp <- lapply(
    unlist(mined_text$hierachical_index[word]
           ), function(x) mined_text$t_corpus[(x - range):(x + range)]
    )
  names(temp) <- 1:length(
    unlist(mined_text$hierachical_index[word])
    )
  
  return(temp)
}

#----------- End -----------####

