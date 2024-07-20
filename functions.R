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

MiningText <- function(
  # corpus should be list of text factors per part.
  corpus, stemming_fun = StemmingWords, ... ) { 

  y <- list()

  y[["original_texts_list"]] <- lapply(
    corpus$narratives, function(x) {
      strsplit(x, split = " ")
    }
  )

  y[["index_documents"]] <- list()
  y$index[["file_names"]] <- names(y$original_texts_list)
  for (i in 1:length(y$original_texts_list)) {
    y$index[["end"]][[i]] <- length(unlist(y$original_texts_list[1:i]))
    if (i == 1) {
      y$index[["start"]][[i]] <- 1
    } else {
      y$index[["start"]][[i]] <- y$index[["end"]][[i - 1]] + 1
    }
    
  }
  # y$index[["start"]] <- c(1, y$index$end[1:length((y$original_texts_list) - 1)]  + 1)
  
  
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
  
  y[["hierachical_index"]] <- as.data.table(y$t_stemmed_corpus)[, list(list(.I)), by = y$t_stemmed_corpus]
  y$hierachical_index <- setattr(y$hierachical_index$V1, 'names', y$hierachical_index$y)

  y[["segmented_hi"]] <- lapply(
     1:length(y$original_texts_list),
     function(z) {
       as.data.frame(
         table(
           match(
             y$t_stemmed_corpus[y$index$start[[z]]:y$index$end[[z]]], y$v_corpus
           )
         )
       )
     }
   )
  names(y$segmented_hi) <- y$index$file_names
  
  # y$t_stemmed_corpus[y$index$start[[1]]]

  y[["lexical_table"]] <- suppressWarnings(Reduce(
    function(...) full_join(..., by = "Var1"), y$segmented_hi
  ))
  rownames(y$lexical_table) <- y$lexical_table$Var1 # y$v_corpus #[y$lexical_table$Var1] NB! THIS IS A BUG IN R, RTL LANGUAGE MIX UP.
  y$lexical_table <- y$lexical_table[, !(names(y$lexical_table) == "Var1")]
  colnames(y$lexical_table) <- y$index$file_names
  y$lexical_table[is.na(y$lexical_table)] <- 0

  return(y)
}

# TESTING #### 
#data = mined_text
#culled_words = c(as.character(support_files$tool_words[, 1]), org_names_stemmed)

#length(culled_words)
#v_min = 20

CullingMinedText <- function(data, culled_words = NULL, v_min) {
  # not implemented yet: culled hierarchical_index, culled segmented_hi. 
  
  y <- list()
  
  y[["v_corpus"]] <- names(
      data$vi_frequency_words[data$vi_frequency_words > v_min]
      )
  y[["v_corpus"]] <- y$v_corpus[!(y$v_corpus %in% culled_words)]
  # y[["v_corpus"]] <- y$v_corpus[!(y$v_corpus == "")]
  
  y[["lexical_table"]] <- data$lexical_table[data$v_corpus %in% y$v_corpus, ]
  # y[["lexical_table"]] <- y$lexical_table[colSums(y$lexical_table) != 0]
  
  return (y$lexical_table )
  }

# # TESTING ####
# data = mined_text
# ca_analysis = cor_an_s
# dim_x = 1
# translation_list = support_files$translation_list
# order = "inertia"

#----------- Functions Presentation Results  -----------####
BuildTableWords <- function(data, ca_analysis, dim_x, translation_list, order) {
  
  table <- data.frame(
    Words = data$v_corpus[
      as.integer(rownames(ca_analysis$call$X))
      ],
    Rel_Wt = ca_analysis$call$marge.row,
    Inertia = ca_analysis$row$inertia,
    Coord_X = ca_analysis$row$coord[, dim_x], 
    Contr_X = ca_analysis$row$contrib[, dim_x], 
    Cos2_X = ca_analysis$row$cos2[, dim_x]
  )

  table[["Translation"]] <- support_files$translation_list[
     match(
       table$Words, support_files$translation_list$word
       ), 2
     ]
  
  freq.temp <- data.frame(
      words = data$v_corpus[
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
    table[c("Words", "Translation", "Freq", "Rel_Wt", "Inertia",  "Coord_X", "Contr_X", "Cos2_X")] # 
  )
  
}

BuildTableDocs <- function(ca_analysis, dim_x, corpus_metadata, order) {
  table <- data.frame(
    Rel_Wt = ca_analysis$call$marge.col,
    Inertia = ca_analysis$col$inertia,
    Coord_X = ca_analysis$col$coord[, dim_x], 
    Contr_X = ca_analysis$col$contrib[, dim_x], 
    Cos2_X = ca_analysis$col$cos2[, dim_x],
    File_name = rownames(ca_analysis$col$contrib)
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

CheckRange <- function(x, pre = NULL, post = NULL) {
  
  # still does not quite work teh way it should... 
  
  if(is.null(pre) == FALSE) {
    range <- max(c(
      x - pre,
      data$index$start[
        length(data$index$start[
          data$index$start <= x
          ])
      ]
      )
    )
  }

  if(is.null(post) == FALSE) {
    range <- min(c(
      x + post,
      data$index$end[
        length(data$index$end[
          data$index$end >! x
        ])
      ]
    )
    ) 
  } 

  return(range)
}

# data = mined_text
# word = "مباد"
# range_pre = 5
# range_post = 5
# index_temp <- data$hierachical_index[[word]]
# x <- 42649
#
BuildTableCollocates <- function(data, word, range_pre, range_post) {
  
  result <- t(
    tibble(
      sapply(data$hierachical_index[[word]], function(x)

        c(
          unlist(
             names(data$original_texts_list)[
                length(
                  data$index$start[
                    data$index$start <= x
                    ]
                  )
                ]
             ),
          paste0(
            unlist(
              data$original_texts_list
              )[
                (x + 1):CheckRange(x, post = range_post)
              ], collapse = " "
          ),
          paste0(
            unlist(
              data$original_texts_list
              )[x]
          ),
          paste0(
            unlist(
              data$original_texts_list
              )[
                CheckRange(x, pre = range_pre):(x - 1)
              ], collapse = " "
          )
        )
      )
    )
  )
  colnames(result) <- c("Filename", " ", "Original word", " ")

  return (result)
}


#test <- BuildTableCollocates(
#  data = mined_text,
#  word = "مباد",
#  range_pre = 5,
#  range_post = 5
#)
#----------- End -----------####

