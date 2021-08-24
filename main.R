################ CA ARABIC TEXT ANALYSIS - Suppl Files ##################
### Symbols and terminology following Lebart et al 1998) ### 

# CHECK FOR MISSING PACKAGES #### 
list.of.packages <- c("FactoMineR", "dplyr", "ggplot2","ggpubr",  "shiny", "ggrepel", "ggdendro", "gridExtra", "english")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# CONFIGURATION ####
setwd("/home/teijehidde/Documents/Publications - Academic/200422 The Discursive Life of Syrian Political Islam/Analysis - Shiny/")
invisible(lapply(list.of.packages, require, character.only = TRUE))
Settings <- data.frame(
  Vmin = 20, # minimum occurrences of words to be included in CA. -- 22 (20 is original)
  T.part.min = 7  # minimum occurrences of words in a Sup document to be included in SupCA. 5, 7 are good numbers. 
  )
source("R scripts/functions.R") 

# UPLOAD CORPUS AND METADATA #### 
Corpus <- Upload(paste0(getwd(), "/data/corpus/", sep =""))
Corpus.sup <- lapply(list.files(paste0(getwd(), "/data/suppl corpora/", sep ="")), function(x) Upload(paste0(getwd(), "/data/suppl corpora/", x, "/")))
names(Corpus.sup) <- c("AaS", "HTS", "JFS", "NF", "NFL", "SMB")
Corpus[["Additional.data"]] <- lapply(list.files(paste0(getwd(), "/data/support files/", sep ="")), function(x) read.table(paste0(getwd(), "/data/support files/", x), header=TRUE, sep=",", dec=".", na.strings=c(" ","","NA"), strip.white=FALSE, skip=0))
names(Corpus$Additional.data) <- c("Document.metadata", "Event.metadata", "Organization.metadata", "Principal.Axes.metadata", "Stemmed.characters", "Document.sup.metadata", "Tool.words", "Translation.list")

# Clean up MetaData Documents 
Corpus[["Additional.data"]]$Document.metadata <- Corpus$Additional.data$Document.metadata[(Corpus$Additional.data$Document.metadata$Org.Abbreviation %in% Corpus$Additional.data$Organization.metadata$Abbreviation), ] 
Corpus[["Additional.data"]]$Document.metadata <- Corpus$Additional.data$Document.metadata[ order(Corpus$Additional.data$Document.metadata$Document), ]
rownames(Corpus[["Additional.data"]]$Document.metadata) <- seq(1:nrow(Corpus$Additional.data$Document.metadata))
Corpus[["Additional.data"]]$Document.metadata[["Label"]] <- paste(rownames(Corpus[["Additional.data"]]$Document.metadata), ".", Corpus$Additional.data$Document.metadata$Org.Abbreviation, sep = "")
Corpus[["Additional.data"]][["Org.names"]] <- unlist(strsplit(as.character(Corpus$Additional.data$Organization.metadata$Organization.Arabic), " "))
Corpus[["Additional.data"]][["Org.names.stemmed"]] <- unique(unlist(lapply(Corpus$Additional.data$Org.names, function(x) SimpleStemming(x, Chars = Corpus$Additional.data$Stemmed.characters))))

# WORD STEMMING AND CULLING CORPUS #### 
Corpus[["Stemmed.segments"]] <- lapply(Corpus$Wordlists, function(x) SimpleStemming(x, Chars = Corpus$Additional.data$Stemmed.characters, Punctuation = TRUE)) 
Stemmed.segments.sup <- lapply(Corpus.sup, function(y) lapply(y[["Wordlists"]], function(x) SimpleStemming(x, Chars = Corpus$Additional.data$Stemmed.characters)))
TM <- TextMining(Corpus$Stemmed.segments, Vmin = Settings$Vmin, dropWords = c(as.character(Corpus$Additional.data$Tool.words[, 1]), Corpus$Additional.data$Org.names.stemmed))
TM.sup <- lapply(Stemmed.segments.sup, function(x) TextMining.Suppl(x, Active.Corpus = TM, T.part.min = Settings$T.part.min))
LexicalTable.full <- cbind(TM$LexicalTable, TM.sup$AaS$LexicalTable, TM.sup$HTS$LexicalTable, TM.sup$JFS$LexicalTable, TM.sup$NF$LexicalTable, TM.sup$NFL$LexicalTable, TM.sup$SMB$LexicalTable)

# CORRESPONDENCE ANALYSIS and SUPPLEMENTARY CORRESPONDENCE ANALYSIS ####
CA <- CA(TM$LexicalTable, ncp = NULL, row.sup = NULL, col.sup = NULL, quanti.sup = NULL, quali.sup = NULL, graph = FALSE, row.w = NULL)
CA.sup <- CA(LexicalTable.full, ncp = NULL, row.sup = NULL, col.sup = (ncol(TM$LexicalTable)):ncol(LexicalTable.full), quanti.sup = NULL, quali.sup = NULL, graph = FALSE, row.w = NULL)

# SHINY - PRESENTATION OUTCOME DATA #### 
source("R scripts/app.R")
shinyApp(ui, server)

#----------- End -----------#
