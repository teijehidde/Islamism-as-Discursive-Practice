#
# Here comes a generic explanation of the shiny app and its functionality.  
#

#----------- loading (and if necessary installing) packages -----------# 
# [simplify?]
list_of_packages <- c("FactoMineR", "dplyr", "ggplot2","ggpubr",  "shiny", "ggrepel", "ggdendro", "gridExtra", "english")
invisible(lapply(list_of_packages, require, character.only = TRUE))

#----------- Config -----------#
setwd("./")
# [if settings are not neccesary, take out.]
settings <- data.frame(
  v_min = 20, # minimum occurrences of words to be included in CA. -- 22 (20 is original)
  t_part_min = 7  # minimum occurrences of words in a Sup document to be included in SupCA. 5, 7 are good numbers. 
  )
source("functions.R") 

#----------- Load corpus, Supplementary corpus and Meta Data -----------#
corpus <- load_data(paste0(getwd(), "/data/corpus/", sep =""))

suppl_corpus_folders <- list.files(
  paste0(getwd(), "/data/suppl_corpora", sep =""), 
  full.names = TRUE)

suppl_corpus <- lapply(
  suppl_corpus_folders, function(x) load_data(x)
  )
names(suppl_corpus) <- c("AaS", "HTS", "JFS", "NF", "NFL", "SMB")

support_files_list <- list.files(
  paste0(getwd(), "/data/support_files/", sep =""),
  full.names = TRUE)

support_files <- lapply(
  support_files_list, function(x) read.table(
    x, header=TRUE, sep=",", dec=".", na.strings=c(" ","","NA"), strip.white=FALSE, skip=0)
  )
names(support_files) <- c("document_metadata", "event_metadata", "organization_metadata", "principal_axes_metadata", "stemmed_characters", "document_sup_metadata", "tool_words", "translation_list")

#### CONTINUE HERE ### (renaming files & cleaning up data frames) 
file.rename(support_files$document_metadata$document_old, support_files$document_metadata$document_new)) 

View(support_files$document_metadata)

file.rename(list.files(pattern="water_*.img"), paste0("water_", 1:700))

# # Clean up MetaData Documents # NB -- this needs to be resolved. 
# corpus[["additional_data"]]$document_metadata <- corpus$additional_data$document_metadata[(corpus$additional_data$document_metadata$Org.Abbreviation %in% corpus$additional_data$organization_metadata$Abbreviation), ] 
# corpus[["additional_data"]]$document_metadata <- corpus$additional_data$document_metadata[ order(corpus$additional_data$document_metadata$Document), ]
# rownames(corpus[["additional_data"]]$document_metadata) <- seq(1:nrow(corpus$additional_data$document_metadata))
# corpus[["additional_data"]]$document_metadata[["Label"]] <- paste(rownames(corpus[["additional_data"]]$document_metadata), ".", corpus$additional_data$document_metadata$Org.Abbreviation, sep = "")

# corpus[["additional_data"]][["Org.names"]] <- unlist(strsplit(as.character(corpus$additional_data$organization_metadata$Organization.Arabic), " "))
# corpus[["additional_data"]][["Org.names.stemmed"]] <- unique(unlist(lapply(corpus$additional_data$Org.names, function(x) SimpleStemming(x, Chars = corpus$additional_data$stemmed_characters))))
# 
# #----------- Word stemming and text mining -----------#
# corpus[["stemmed_segments"]] <- lapply(corpus$Wordlists, function(x) SimpleStemming(x, Chars = corpus$additional_data$stemmed_characters, Punctuation = TRUE)) 
# stemmed_segments.sup <- lapply(suppl_corpus, function(y) lapply(y[["Wordlists"]], function(x) SimpleStemming(x, Chars = corpus$additional_data$stemmed_characters)))
# TM <- TextMining(corpus$stemmed_segments, v_min = settings$v_min, dropWords = c(as.character(corpus$additional_data$tool_words[, 1]), corpus$additional_data$Org.names.stemmed))
# TM.sup <- lapply(stemmed_segments.sup, function(x) TextMining.Suppl(x, Active.corpus = TM, t_part_min = settings$t_part_min))
# LexicalTable.full <- cbind(TM$LexicalTable, TM.sup$AaS$LexicalTable, TM.sup$HTS$LexicalTable, TM.sup$JFS$LexicalTable, TM.sup$NF$LexicalTable, TM.sup$NFL$LexicalTable, TM.sup$SMB$LexicalTable)
# 
# #----------- (Supplementary) Correspondence Analysis -----------#
# CA <- CA(TM$LexicalTable, ncp = NULL, row.sup = NULL, col.sup = NULL, quanti.sup = NULL, quali.sup = NULL, graph = FALSE, row.w = NULL)
# CA.sup <- CA(LexicalTable.full, ncp = NULL, row.sup = NULL, col.sup = (ncol(TM$LexicalTable)):ncol(LexicalTable.full), quanti.sup = NULL, quali.sup = NULL, graph = FALSE, row.w = NULL)
# 
# #----------- Presentation Data in Shiny app -----------#
# source("R scripts/app.R")
# shinyApp(ui, server)

#----------- End -----------#
