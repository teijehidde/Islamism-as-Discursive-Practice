# FUNCTIONS ####

Upload <- function(Dir) {
  Output <- list()
  Output[["Dir"]] <- Dir
  Output[["Filenames"]] <- list.files(Output[["Dir"]], full.names = FALSE)
  Output[["Documents"]] <- lapply(Output$Filenames, function(x) paste0(readLines(paste0(Output[["Dir"]], x, sep = "")), collapse = ""))
  names(Output$Documents) <- Output$Filenames
  Output[["Wordlists"]] <- lapply(Output$Documents, function(x) strsplit(x, split = " ")) 
  
  return(Output) 
} 

SimpleStemming <- function(x, Chars, Latin = TRUE, Digit = TRUE, Punctuation = TRUE) {  
  
  x <- x[[1]]

  # x <- gsub("الله", "اااا", x)  # The word "God" consists of the definitive article + "him". Meaning it needs to be saved from stemming. 
 
  for (i in 1:nrow(Chars)) { x <- gsub(Chars[i, 1], "", x)  }
  
  if(Latin == TRUE) {x <- gsub("a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z", "", x) }
  if(Digit == TRUE) {x <- gsub("[[:digit:]]", "", x) }
  if(Punctuation == TRUE) {x <- gsub("[[:punct:]]", "", x) }
  
  # x <- gsub("اااا", "الله", x)  
  x <- gsub('"', "", x)
  
  return(x)
}

# 
# x <- Corpus$Stemmed.segments
# Vmin <- 20
# dropWords <- c(as.character(Corpus$Additional.data$Tool.words[, 1]))


TextMining <- function(x, Vmin, dropWords = NULL) { # Corpus should be list of text factors per part. 
  
  y <- list() 
  
  y[["T.Corpus"]] <- unlist(x) 
  y[["Vi.frequencyWords"]] <- table(y$T.Corpus)
  y[["V.originalCorpus"]] <- unique(y$T.Corpus)
  
  for(i in 1:(length(x))) { y[["indices.docs.end"]][[i]] <- length(unlist(x[1:i])) } 
  y[["indices.docs.start"]] <- c(1, y$indices.docs.end + 1) 
  
  y[["V.culledCorpus"]] <- names(y$Vi.frequencyWords[y$Vi.frequencyWords > Vmin])
  y[["V.culledCorpus"]] <- y$V.culledCorpus[!(y$V.culledCorpus %in% dropWords)]
  y[["V.culledCorpus"]] <- y$V.culledCorpus[!(y$V.culledCorpus == "")]
  y[["HierachicalIndex"]] <- lapply(y$V.culledCorpus, function(x) as.vector(which(y$T.Corpus == x)  ))
  names(y$HierachicalIndex) <- y$V.culledCorpus 
  y$HierachicalIndex <- y$HierachicalIndex[order(unlist(lapply(y$HierachicalIndex, function(x) length(x)))) ]
  
  y[["Segmented.HI"]] <- lapply(1:length(x), function(z) as.data.frame(table(match(y$T.Corpus[y$indices.docs.start[z]:y$indices.docs.end[z]], y$V.culledCorpus))))
  names(y$Segmented.HI) <- names(x)
  
  y[["LexicalTable"]] <- suppressWarnings(Reduce(function(...) full_join(..., by = "Var1"), y$Segmented.HI))
  rownames(y$LexicalTable) <- y$LexicalTable$Var1 # y$V.culledCorpus #[y$LexicalTable$Var1] NB! THIS IS A BUG IN R, RTL LANGUAGE MIX UP. 
  y$LexicalTable <- y$LexicalTable[, !(names(y$LexicalTable) == "Var1")]
  colnames(y$LexicalTable) <- names(x)
  y$LexicalTable[is.na(y$LexicalTable)] <- 0 
  # rownames(y$LexicalTable) <- y$V.culledCorpus[as.numeric(rownames(y$LexicalTable))]
  
  return(y)
}

TextMining.Suppl <- function(x, Active.Corpus, T.part.min = 5) {
  y <- list() 
  
  y[["T.Corpus"]] <- unlist(x) 
  y[["Vi.frequencyWords"]] <- table(y$T.Corpus)
  y[["V.originalCorpus"]] <- unique(y$T.Corpus)
  
  for(i in 1:(length(x))) { y[["indices.docs.end"]][[i]] <- length(unlist(x[1:i])) } 
  y[["indices.docs.start"]] <- c(1, y$indices.docs.end + 1) 
  
  y[["HierachicalIndex"]] <- lapply(Active.Corpus$V.culledCorpus, function(x) as.vector(which(y$T.Corpus == x)  ))
  names(y$HierachicalIndex) <- Active.Corpus$V.culledCorpus 
  y$HierachicalIndex <- y$HierachicalIndex[order(match(names(y$HierachicalIndex), names(Active.Corpus$HierachicalIndex))) ]
  
  y[["Segmented.HI"]] <- lapply(1:length(x), function(z) as.data.frame(table(match(y$T.Corpus[y$indices.docs.start[z]:y$indices.docs.end[z]], Active.Corpus$V.culledCorpus))))
  names(y$Segmented.HI) <- names(x)
  y[["Segmented.HI"]] <- y$Segmented.HI[lapply(y$Segmented.HI, function(x) nrow(x)) > T.part.min]
  
  y[["LexicalTable"]] <- suppressWarnings(Reduce(function(...) full_join(..., by = "Var1"), y$Segmented.HI))
  rownames(y$LexicalTable) <- y$LexicalTable$Var1 # as.data.frame(Active.Corpus$V.culledCorpus)[y$LexicalTable$Var1, ]
  y$LexicalTable <- y$LexicalTable[, !(names(y$LexicalTable) == "Var1")]
  colnames(y$LexicalTable) <- names(y$Segmented.HI)
  
  # fixing bug with missing words. 
  missed.words <-  rownames(Active.Corpus$LexicalTable)[ rownames(Active.Corpus$LexicalTable) %in%  rownames(y$LexicalTable) == FALSE]
  temp <- data.frame(matrix(0, nrow = length(missed.words), ncol = ncol(y$LexicalTable))) 
  rownames(temp) <- missed.words
  colnames(temp) <- c(names(y$LexicalTable))
  y$LexicalTable <- rbind(y$LexicalTable, temp)
  ## 
  
  y$LexicalTable <- y$LexicalTable[, !(names(y$LexicalTable) == "Var1")]
  colnames(y$LexicalTable) <- c(names(y$Segmented.HI))
  y$LexicalTable[is.na(y$LexicalTable)] <- 0
  y$LexicalTable <- y$LexicalTable[match(rownames(Active.Corpus$LexicalTable), rownames(y$LexicalTable)), ]
  rownames(y$LexicalTable) <- rownames(Active.Corpus$LexicalTable)
  
  return(y)
}
  
Build.Table.Words <- function(DimX, DimY, DimZ, LT, Translation.list, Order) {
  table <- data.frame( 
    Words = TM$V.culledCorpus[as.numeric(rownames(TM$LexicalTable))], 
    Rel.Wt. = CA$call$marge.row, 
    Inertia =  CA$row$inertia, 
    Coord.X = CA$row$coord[, DimX], Contr.X = CA$row$contrib[, DimX], Cos2.X = CA$row$cos2[, DimX]
  )

  table[["Translation"]] <- Corpus$Additional.data$Translation.list[match(TM$V.culledCorpus[as.numeric(rownames(TM$LexicalTable))], Corpus$Additional.data$Translation.list$Word), 2]
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

Build.Table.Docs <- function(DimX, DimY, DimZ, Corpus.metadata, Order = "Inertia") {
  table <- data.frame( 
    Label = "later", 
    Title = "later", 
    Rel.Wt. = CA$call$marge.col,
    Inertia = CA$col$inertia,
    Coord.X = CA$col$coord[, DimX], Contr.X = CA$col$contrib[, DimX], Cos2.X = CA$col$cos2[, DimX], 
    Filename = rownames(CA$col$contrib)
  )
  
  temp <- data.frame(Filename = Corpus.metadata$Document, 
                     Label = Corpus.metadata$Label,
                     Title = Corpus.metadata$Title, 
                     Org. = Corpus.metadata$Org.Abbreviation)
  
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

Build.Table.Words <- function(DimX, DimY, DimZ, LT, Translation.list, Order) {
  table <- data.frame( 
    Words = TM$V.culledCorpus[as.numeric(rownames(TM$LexicalTable))], 
    Rel.Wt. = CA$call$marge.row, 
    Inertia =  CA$row$inertia, 
    Coord.X = CA$row$coord[, DimX], Contr.X = CA$row$contrib[, DimX], Cos2.X = CA$row$cos2[, DimX]
  )
  
  table[["Translation"]] <- Corpus$Additional.data$Translation.list[match(TM$V.culledCorpus[as.numeric(rownames(TM$LexicalTable))], Corpus$Additional.data$Translation.list$Word), 2]
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

# Corpus.sup$AaS$Documents$
Build.Table.Sup.docs <- function(DimX, Corpus.metadata, Orgs) {
  table <- data.frame( 
    Org = "later", 
    Date = "later", 
    Coord.X = CA.sup$col.sup$coord[, DimX], 
    Cos2.X = CA.sup$col.sup$cos2[, DimX], 
    Filename = rownames(CA.sup$col.sup$coord)
  )
  
  temp <- data.frame(Filename = Corpus$Additional.data$Document.sup.metadata$Document, 
                     Org =  Corpus$Additional.data$Document.sup.metadata$Org.Abbreviation, 
                     Date = as.Date(paste0(Corpus$Additional.data$Document.sup.metadata$Statement.Day, "/",
                                    Corpus$Additional.data$Document.sup.metadata$Statement.Month, "/",
                                    Corpus$Additional.data$Document.sup.metadata$Statement.Year,
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
  
  temp <- lapply(unlist(TM$HierachicalIndex[word]), function(x) TM$T.Corpus[(x-range):(x+range)] )
  names(temp) <- 1:length(unlist(TM$HierachicalIndex[word]))
  return(temp)
} 
