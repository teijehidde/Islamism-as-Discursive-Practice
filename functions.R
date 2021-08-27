#----------- Functions Analysis -----------####

LoadData <- function(dir) {
  output <- list()
  output[["dir"]] <- dir
  output[["file_names"]] <- list.files(output[["dir"]], full.names = TRUE)
  output[["narratives"]] <- lapply(
    output$file_names, function(x) {
      paste0(
        readLines(x),
        collapse = ""
      )
    }
  )
  names(output$narratives) <- list.files(
    output[["dir"]],
    full.names = FALSE
  )
  
  output[["word_lists"]] <- lapply(
    output$narratives, function(x) {
      strsplit(
        x,
        split = " "
      )
    }
  )

  return(output)
}

StemmingWords <- function(x,
                          drop_chars,
                          drop_latin = TRUE,
                          drop_digit = TRUE,
                          drop_punctuation = TRUE) {
  x <- x[[1]]

  for (i in 1:nrow(drop_chars)) {
    x <- gsub(drop_chars[i, 1], "", x)
  }

  if (drop_latin == TRUE) {
    x <- gsub("a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z", "", x)
  }
  if (drop_digit == TRUE) {
    x <- gsub("[[:digit:]]", "", x)
  }
  if (drop_punctuation == TRUE) {
    x <- gsub("[[:punct:]]", "", x)
  }

  x <- gsub('"', "", x)

  return(x)
}

MiningText <- function(corpus, drop_words = NULL, v_min, stemming_fun = StemmingWords, ... ) { # corpus should be list of text factors per part.

  y <- list()
  
  y[["original_texts_list"]] <- lapply(
    corpus$narratives, function(x) {
      strsplit(x, split = " ")
    }
  )
  
  y[["index"]] <- list()
  y$index[["file_names"]] <- names(y$original_texts_list)
  for (i in 1:length(y$original_texts_list)) {
    y$index[["end"]][[i]] <- length(unlist(y$original_texts_list[1:i]))
  }
  y$index[["start"]] <- c(1, y$index$end + 1)
  
  y[["t_stemmed_corpus"]] <- unlist(
    sapply(
      y$original_texts_list, function(x) {
        stemming_fun(
        x,
        drop_chars = support_files$stemmed_characters, 
        drop_punctuation = TRUE
        )
      }
    )
  ) 
  
  y[["vi_frequency_words"]] <- table(y$t_stemmed_corpus)
  y[["v_corpus"]] <- unique(y$t_stemmed_corpus)
  y[["v_culled_corpus"]] <- names(
    y$vi_frequency_words[y$vi_frequency_words > v_min]
  )
  y[["v_culled_corpus"]] <- y$v_culled_corpus[!(y$v_culled_corpus %in% drop_words)]
  y[["v_culled_corpus"]] <- y$v_culled_corpus[!(y$v_culled_corpus == "")]

  y[["hierachical_index"]] <- lapply(
    y$v_culled_corpus, function(x) {
      as.vector(
        which(
          y$t_stemmed_corpus == x
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
     1:length(y$original_texts_list),
     function(z) {
       as.data.frame(
         table(
           match(
             y$t_stemmed_corpus[y$index$start[z]:y$index$end[z]], y$v_culled_corpus
           )
         )
       )
     }
   )
  names(y$segmented_hi) <- y$index$file_names

  y[["lexical_table"]] <- suppressWarnings(Reduce(
    function(...) full_join(..., by = "Var1"), y$segmented_hi
  ))
  rownames(y$lexical_table) <- y$lexical_table$Var1 # y$v_culled_corpus #[y$lexical_table$Var1] NB! THIS IS A BUG IN R, RTL LANGUAGE MIX UP.
  y$lexical_table <- y$lexical_table[, !(names(y$lexical_table) == "Var1")]
  colnames(y$lexical_table) <- y$index$file_names
  y$lexical_table[is.na(y$lexical_table)] <- 0

  return(y)
}


#----------- Functions Presentation Results  -----------####
BuildTableWords <- function(data, dim_x, dim_y, dim_z, lexical_table, translation_list, order) {
  table <- data.frame(
    Words = data$v_culled_corpus[
      as.numeric(rownames(data$lexical_table))
      ],
    Rel_Wt = correspondence_analysis$call$marge.row,
    Inertia = correspondence_analysis$row$inertia,
    Coord_X = correspondence_analysis$row$coord[, dim_x], 
    Contr_X = correspondence_analysis$row$contrib[, dim_x], 
    Cos2_X = correspondence_analysis$row$cos2[, dim_x]
  )

  table[["Translation"]] <- support_files$translation_list[
     match(
       data$v_culled_corpus[
         as.numeric(rownames(data$lexical_table))
         ], support_files$translation_list$word
       ), 2
     ]
  
  freq.temp <- data.frame(
     words = data$v_culled_corpus[
       as.numeric(rownames(data$lexical_table))
     ],
     freq = rowSums(data$lexical_table)
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

# 
BuildTableCollocates <- function(data, word, range, meta_data) {
  
  file_name <- lapply(data$hierachical_index[[word]], function(x) 
    names(data$original_texts_list)[
      length(
        data$index$start[
          data$index$start < x
          ]
        )
      ]
    )
  
  label_text <- lapply(
    file_name, function(x) paste0(
      meta_data[meta_data$file_name == x,]$label
      )
  )
  
  text_pre <- lapply(
    data$hierachical_index[[word]], function(x) 
      paste0(
        unlist(
          data$original_texts_list)[
            (x - range):(x - 1)
            ], collapse = " "
      )
  )
  
  original_word <- lapply(
    data$hierachical_index[[word]], function(x) 
      paste0(
        unlist( 
          data$original_texts_list
          )[x]
      )
  ) 
  
  text_post <- lapply(
    data$hierachical_index[[word]], function(x) 
      paste0(
        unlist( 
          data$original_texts_list)[
            (x + 1):(x + range)
            ], collapse = " "
        )
    )
  
  # note that pre and post are switch because Arabic is an RTL language. 
  result <- tibble( 
    label = label_text, 
    text_post = text_post,
    word = original_word, 
    text_pre = text_pre, 
  )
  
  return (result)
} 

#----------- End -----------####

