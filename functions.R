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

# [is it possible to make stemming function modular?]
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

MiningSupplText <- function(x, reference_mined_text, t_part_min = 5) {
  y <- list()

  y[["t_corpus"]] <- unlist(x)
  y[["vi_frequency_words"]] <- table(y$t_corpus)
  y[["V.originalcorpus"]] <- unique(y$t_corpus)

  for (i in 1:length(x)) {
    y[["index_end"]][[i]] <- length(unlist(x[1:i]))
  }
  y[["index_start"]] <- c(1, y$index_end + 1)

  y[["hierachical_index"]] <- lapply(
    reference_mined_text$v_culled_corpus, function(x) {
      as.vector(
        which(
          y$t_corpus == x
        )
      )
    }
  )
  names(y$hierachical_index) <- reference_mined_text$v_culled_corpus
  y$hierachical_index <- y$hierachical_index[
    order(
      match(
        names(
          y$hierachical_index
        ), names(
          reference_mined_text$hierachical_index
        )
      )
    )
  ]

  y[["segmented_hi"]] <- lapply(
    1:length(x), function(z) {
      as.data.frame(
        table(
          match(
            y$t_corpus[
              y$index_start[z]:y$index_end[z]
            ], reference_mined_text$v_culled_corpus
          )
        )
      )
    }
  )
  names(y$segmented_hi) <- names(x)
  y[["segmented_hi"]] <- y$segmented_hi[
    lapply(
      y$segmented_hi, function(x) nrow(x)
    ) > t_part_min
  ]

  y[["lexical_table"]] <- suppressWarnings(Reduce(
    function(...) full_join(..., by = "Var1"), y$segmented_hi
  ))
  rownames(y$lexical_table) <- y$lexical_table$Var1
  y$lexical_table <- y$lexical_table[, !(names(y$lexical_table) == "Var1")]
  colnames(y$lexical_table) <- names(y$segmented_hi)

  # fixing bug with missing words. -- see if it can be taken out.
  # missed.words <-  rownames(reference_mined_text$lexical_table)[ rownames(reference_mined_text$lexical_table) %in%  rownames(y$lexical_table) == FALSE]
  # temp <- data.frame(matrix(0, nrow = length(missed.words), ncol = ncol(y$lexical_table)))
  # rownames(temp) <- missed.words
  # colnames(temp) <- c(names(y$lexical_table))
  # y$lexical_table <- rbind(y$lexical_table, temp)
  ##

  y$lexical_table <- y$lexical_table[, !(names(y$lexical_table) == "Var1")]
  colnames(y$lexical_table) <- c(names(y$segmented_hi))
  y$lexical_table[is.na(y$lexical_table)] <- 0
  y$lexical_table <- y$lexical_table[
    match(
      rownames(
        reference_mined_text$lexical_table
      ), rownames(y$lexical_table)
    ),
  ]
  rownames(y$lexical_table) <- rownames(reference_mined_text$lexical_table)

  return(y)
}

dim_x <- 1

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
  table <- table[c("Words", "Translation", "Rel_Wt", "Inertia", "Freq", "Coord_X", "Contr_X", "Cos2_X")]
  rownames(table) <- NULL

  return(table)
}

# corpus_metadata <- support_files$document_metadata

BuildTableDocs <- function(dim_x, corpus_metadata, order = "Inertia") {
  table <- data.frame(
    Label = "later",
    Title = "later",
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

  table$Label <- temp[
    match(
      table$File_name,
      temp$file_name
      ), 2
    ]
  table$Title <- temp[
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
  table <- table[c("Label", "Title", "Rel_Wt", "Inertia", "Coord_X", "Contr_X", "Cos2_X")]

  return(table)
}

# corpus_sup$AaS$Documents$
BuildTableSupplDocs <- function(dim_x, corpus_metadata, orgs) {
  table <- data.frame(
    Org = "later",
    Date = "later",
    Coord_X = correspondence_analysis.sup$col.sup$coord[, dim_x],
    Cos2_X = correspondence_analysis.sup$col.sup$cos2[, dim_x],
    File_name = rownames(correspondence_analysis.sup$col.sup$coord)
  )

  temp <- data.frame(
    file_name = support_files$document_sup_metadata$file_name,
    org = support_files$document_sup_metadata$org_abbreviated,
    date = as.Date(
      paste0(
        support_files$document_sup_metadata$day, "/",
        support_files$document_sup_metadata$month, "/",
        support_files$document_sup_metadata$Syear,
        sep = ""
        ), "%d/%m/%y"
      )
    )

  table$Org <- temp[
    match(
      table$File_name, 
      temp$file_name
      ), 2
    ]
  table$Date <- as.character(
    temp[
      match(
        table$File_name, 
        temp$file_name
        ), 3
      ]
    )
  table <- table[
    order(
      table$Coord.X, decreasing = FALSE
      ), 
    ]
  rownames(table) <- NULL

  table <- table[c("Filename", "Org", "Coord_X", "Cos2_X", "Date")] # "Label", "Title", "Rel.Wt.", "Inertia",
  table <- table[table$Org %in% Orgs, ]

  return(table)
}

PrintExampleSentences <- function(word, range = 5) {
  temp <- lapply(unlist(mined_text$hierachical_index[word]), function(x) mined_text$t_corpus[(x - range):(x + range)])
  names(temp) <- 1:length(unlist(mined_text$hierachical_index[word]))
  return(temp)
}
