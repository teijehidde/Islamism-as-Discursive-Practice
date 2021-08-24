# FUNCTIONS ####

load_data <- function(Dir) {
  Output <- list()
  Output[["Dir"]] <- paste0(Dir, '//', sep = '')
  Output[["Filenames"]] <- list.files(Output[["Dir"]], full.names = FALSE)
  Output[["Documents"]] <- lapply(Output$Filenames, function(x) paste0(readLines(paste0(Output[["Dir"]], x, sep = "")), collapse = ""))
  names(Output$Documents) <- Output$Filenames
  Output[["Wordlists"]] <- lapply(Output$Documents, function(x) strsplit(x, split = " ")) 
  
  return(Output) 
} 

# [is it possible to make stemming function modular?] 
stem_words <- function(x, Chars, Latin = TRUE, Digit = TRUE, Punctuation = TRUE) {  
  
  x <- x[[1]]
 
  for (i in 1:nrow(Chars)) { x <- gsub(Chars[i, 1], "", x)  }
  
  if(Latin == TRUE) {x <- gsub("a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z", "", x) }
  if(Digit == TRUE) {x <- gsub("[[:digit:]]", "", x) }
  if(Punctuation == TRUE) {x <- gsub("[[:punct:]]", "", x) }
  
  x <- gsub('"', "", x)
  
  return(x)
}

mine_text <- function(x, v_min, dropWords = NULL) { # corpus should be list of text factors per part. 
  
  y <- list() 
  
  y[["T.corpus"]] <- unlist(x) 
  y[["Vi.frequencyWords"]] <- table(y$T.corpus)
  y[["V.originalcorpus"]] <- unique(y$T.corpus)
  
  for(i in 1:(length(x))) { y[["indices.docs.end"]][[i]] <- length(unlist(x[1:i])) } 
  y[["indices.docs.start"]] <- c(1, y$indices.docs.end + 1) 
  
  y[["V.culledcorpus"]] <- names(y$Vi.frequencyWords[y$Vi.frequencyWords > v_min])
  y[["V.culledcorpus"]] <- y$V.culledcorpus[!(y$V.culledcorpus %in% dropWords)]
  y[["V.culledcorpus"]] <- y$V.culledcorpus[!(y$V.culledcorpus == "")]
  y[["HierachicalIndex"]] <- lapply(y$V.culledcorpus, function(x) as.vector(which(y$T.corpus == x)  ))
  names(y$HierachicalIndex) <- y$V.culledcorpus 
  y$HierachicalIndex <- y$HierachicalIndex[order(unlist(lapply(y$HierachicalIndex, function(x) length(x)))) ]
  
  y[["Segmented.HI"]] <- lapply(1:length(x), function(z) as.data.frame(table(match(y$T.corpus[y$indices.docs.start[z]:y$indices.docs.end[z]], y$V.culledcorpus))))
  names(y$Segmented.HI) <- names(x)
  
  y[["LexicalTable"]] <- suppressWarnings(Reduce(function(...) full_join(..., by = "Var1"), y$Segmented.HI))
  rownames(y$LexicalTable) <- y$LexicalTable$Var1 # y$V.culledcorpus #[y$LexicalTable$Var1] NB! THIS IS A BUG IN R, RTL LANGUAGE MIX UP. 
  y$LexicalTable <- y$LexicalTable[, !(names(y$LexicalTable) == "Var1")]
  colnames(y$LexicalTable) <- names(x)
  y$LexicalTable[is.na(y$LexicalTable)] <- 0 
  # rownames(y$LexicalTable) <- y$V.culledcorpus[as.numeric(rownames(y$LexicalTable))]
  
  return(y)
}

# [the following has to be integrated in the preceding function!]
TextMining.Suppl <- function(x, Active.corpus, t_part_min = 5) {
  y <- list() 
  
  y[["T.corpus"]] <- unlist(x) 
  y[["Vi.frequencyWords"]] <- table(y$T.corpus)
  y[["V.originalcorpus"]] <- unique(y$T.corpus)
  
  for(i in 1:(length(x))) { y[["indices.docs.end"]][[i]] <- length(unlist(x[1:i])) } 
  y[["indices.docs.start"]] <- c(1, y$indices.docs.end + 1) 
  
  y[["HierachicalIndex"]] <- lapply(Active.corpus$V.culledcorpus, function(x) as.vector(which(y$T.corpus == x)  ))
  names(y$HierachicalIndex) <- Active.corpus$V.culledcorpus 
  y$HierachicalIndex <- y$HierachicalIndex[order(match(names(y$HierachicalIndex), names(Active.corpus$HierachicalIndex))) ]
  
  y[["Segmented.HI"]] <- lapply(1:length(x), function(z) as.data.frame(table(match(y$T.corpus[y$indices.docs.start[z]:y$indices.docs.end[z]], Active.corpus$V.culledcorpus))))
  names(y$Segmented.HI) <- names(x)
  y[["Segmented.HI"]] <- y$Segmented.HI[lapply(y$Segmented.HI, function(x) nrow(x)) > t_part_min]
  
  y[["LexicalTable"]] <- suppressWarnings(Reduce(function(...) full_join(..., by = "Var1"), y$Segmented.HI))
  rownames(y$LexicalTable) <- y$LexicalTable$Var1 # as.data.frame(Active.corpus$V.culledcorpus)[y$LexicalTable$Var1, ]
  y$LexicalTable <- y$LexicalTable[, !(names(y$LexicalTable) == "Var1")]
  colnames(y$LexicalTable) <- names(y$Segmented.HI)
  
  # fixing bug with missing words. 
  missed.words <-  rownames(Active.corpus$LexicalTable)[ rownames(Active.corpus$LexicalTable) %in%  rownames(y$LexicalTable) == FALSE]
  temp <- data.frame(matrix(0, nrow = length(missed.words), ncol = ncol(y$LexicalTable))) 
  rownames(temp) <- missed.words
  colnames(temp) <- c(names(y$LexicalTable))
  y$LexicalTable <- rbind(y$LexicalTable, temp)
  ## 
  
  y$LexicalTable <- y$LexicalTable[, !(names(y$LexicalTable) == "Var1")]
  colnames(y$LexicalTable) <- c(names(y$Segmented.HI))
  y$LexicalTable[is.na(y$LexicalTable)] <- 0
  y$LexicalTable <- y$LexicalTable[match(rownames(Active.corpus$LexicalTable), rownames(y$LexicalTable)), ]
  rownames(y$LexicalTable) <- rownames(Active.corpus$LexicalTable)
  
  return(y)
}
  
Build.Table.Words <- function(DimX, DimY, DimZ, LT, translation_list, Order) {
  table <- data.frame( 
    Words = TM$V.culledcorpus[as.numeric(rownames(TM$LexicalTable))], 
    Rel.Wt. = CA$call$marge.row, 
    Inertia =  CA$row$inertia, 
    Coord.X = CA$row$coord[, DimX], Contr.X = CA$row$contrib[, DimX], Cos2.X = CA$row$cos2[, DimX]
  )

  table[["Translation"]] <- corpus$additional_data$translation_list[match(TM$V.culledcorpus[as.numeric(rownames(TM$LexicalTable))], corpus$additional_data$translation_list$Word), 2]
  freq.temp <- data.frame(Words = rownames(LT), Freq = rowSums(LT))
  table[["Freq."]] <- as.character(freq.temp[match(table$Words, freq.temp$Words), 2])

  if(Order == "Inertia") { 
    table <- table[ order(table$Inertia, decreasing = TRUE), ]
  } else {
    table <- table[ order(table$Contr.X, decreasing = TRUE), ]
  }
  table <- table[c("Words", "Translation", "Rel.Wt.", "Inertia", "Freq.", "Coord.X", "Contr.X", "Cos2.X")]
  rownames(table) <- NULL
  
  return(table)
}

Build.Table.Docs <- function(DimX, DimY, DimZ, corpus.metadata, Order = "Inertia") {
  table <- data.frame( 
    Label = "later", 
    Title = "later", 
    Rel.Wt. = CA$call$marge.col,
    Inertia = CA$col$inertia,
    Coord.X = CA$col$coord[, DimX], Contr.X = CA$col$contrib[, DimX], Cos2.X = CA$col$cos2[, DimX], 
    Filename = rownames(CA$col$contrib)
  )
  
  temp <- data.frame(Filename = corpus.metadata$Document, 
                     Label = corpus.metadata$Label,
                     Title = corpus.metadata$Title, 
                     Org. = corpus.metadata$Org.Abbreviation)
  
  table$Label <-  temp[match(table$Filename, temp$Filename), 2 ]
  table$Title <- temp[match(table$Filename, temp$Filename), 3 ]
  
  if(Order == "Inertia") { 
     table <- table[ order(table$Inertia, decreasing = TRUE), ]
  } else {
     table <- table[ order(table$Contr.X, decreasing = TRUE), ]
  }
  
  rownames(table) <- NULL
  
  table <- table[c("Label", "Title", "Rel.Wt.", "Inertia", "Coord.X", "Contr.X", "Cos2.X")]
  
  return(table)
}

Build.Table.Words <- function(DimX, DimY, DimZ, LT, translation_list, Order) {
  table <- data.frame( 
    Words = TM$V.culledcorpus[as.numeric(rownames(TM$LexicalTable))], 
    Rel.Wt. = CA$call$marge.row, 
    Inertia =  CA$row$inertia, 
    Coord.X = CA$row$coord[, DimX], Contr.X = CA$row$contrib[, DimX], Cos2.X = CA$row$cos2[, DimX]
  )
  
  table[["Translation"]] <- corpus$additional_data$translation_list[match(TM$V.culledcorpus[as.numeric(rownames(TM$LexicalTable))], corpus$additional_data$translation_list$Word), 2]
  freq.temp <- data.frame(Index = rownames(LT), Freq = rowSums(LT))
  table[["Freq."]] <- as.character(freq.temp[match(rownames(TM$LexicalTable), freq.temp$Index), 2])
  
  if(Order == "Inertia") { 
    table <- table[ order(table$Inertia, decreasing = TRUE), ]
  } else {
    table <- table[ order(table$Contr.X, decreasing = TRUE), ]
  }
  
  table <- table[c("Words", "Translation", "Rel.Wt.", "Inertia", "Freq.", "Coord.X", "Contr.X", "Cos2.X")]
  rownames(table) <- NULL
  
  return(table)
}

# corpus_sup$AaS$Documents$
Build.Table.Sup.docs <- function(DimX, corpus.metadata, Orgs) {
  table <- data.frame( 
    Org = "later", 
    Date = "later", 
    Coord.X = CA.sup$col.sup$coord[, DimX], 
    Cos2.X = CA.sup$col.sup$cos2[, DimX], 
    Filename = rownames(CA.sup$col.sup$coord)
  )
  
  temp <- data.frame(Filename = corpus$additional_data$document_sup_metadata$Document, 
                     Org =  corpus$additional_data$document_sup_metadata$Org.Abbreviation, 
                     Date = as.Date(paste0(corpus$additional_data$document_sup_metadata$Statement.Day, "/",
                                    corpus$additional_data$document_sup_metadata$Statement.Month, "/",
                                    corpus$additional_data$document_sup_metadata$Statement.Year,
                                    sep = ""), "%d/%m/%y"))
  
  table$Org <-  temp[match(table$Filename, temp$Filename), 2 ]
  table$Date <- as.character(temp[match(table$Filename, temp$Filename), 3 ])
  table <- table[ order(table$Coord.X, decreasing = FALSE), ]
  rownames(table) <- NULL
  
  table <- table[c("Filename", "Org", "Coord.X", "Cos2.X", "Date")] # "Label", "Title", "Rel.Wt.", "Inertia", 
  table <- table[table$Org %in% Orgs, ]
  
  return(table)
}

print.example.sentences <- function(word, range = 5) {
  
  temp <- lapply(unlist(TM$HierachicalIndex[word]), function(x) TM$T.corpus[(x-range):(x+range)] )
  names(temp) <- 1:length(unlist(TM$HierachicalIndex[word]))
  return(temp)
} 
