### 
# This repository provides the data and script necessary to reproduce the results of the article *Political Islam as Discursive Practice*.
# It also includes a shiny app as an accessible approach to try out the methods used in the article.  
### 

#----------- Config -----------####
list_of_packages <- c("FactoMineR", "dplyr", "ggplot2", "ggpubr", "shiny", "ggrepel", "ggdendro", "gridExtra", "english", "data.table", "here")
new_packages <- list_of_packages[
  !(list_of_packages %in% installed.packages()[,"Package"])
  ]
if(length(new_packages)) install.packages(new_packages)
invisible(lapply(list_of_packages, require, character.only = TRUE))
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("functions.R")

#----------- Loading corpora and meta data -----------####

corpus <- LoadData("./corpus")
support_files_list <- list.files("./support_files", full.names = TRUE)
support_files <- lapply(
  support_files_list, function(x) {
    read.table(
      x,
      header = TRUE,
      sep = ",",
      dec = ".",
      na.strings = c(" ", "", "NA"),
      strip.white = FALSE,
      skip = 0
    )
  }
)


names(support_files) <- gsub(
  ".csv", "", list.files(
    "./support_files",
    full.names = FALSE
  )
)

#----------- Word stemming and text mining -----------####
mined_text <- MiningText(
  corpus, 
  stemming_fun = StemmingWords,
  drop_chars = support_files$stemmed_characters,
  drop_latin = TRUE,
  drop_digit = TRUE,
  drop_punctuation = TRUE
)

org_names_stemmed <- unlist(  
  strsplit(
    as.character(
      support_files$organization_metadata$org_arabic
      ), " "
    )
) 
org_names_stemmed <- unique(
  sapply(
    org_names_stemmed, function(x) StemmingWords(
      x, drop_chars = support_files$stemmed_characters
      )
    )
  )

# NOT RUN {
# culled_lexical_table_s <- CullingMinedText(
#    data = mined_text,
#    culled_words = c(as.character(support_files$tool_words[, 1]), org_names_stemmed),
#    v_min = 20
#  )
# 
# #----------- Analysis -----------####
# cor_an_s <- CA(
#    culled_lexical_table_s,
#    ncp = NULL,
#    row.sup = NULL,
#    col.sup = NULL,
#    quanti.sup = NULL,
#    quali.sup = NULL,
#    graph = FALSE,
#    row.w = NULL
#  )
# #
# 
# hi_clus_s <-  HCPC(
#    cor_an_s,
#    metric = "manhattan", # "manhattan", # metric = "euclidean" and "manhattan".
#    nb.clust = 3,
#    order = TRUE,
#    graph = FALSE
# )
# } NOT RUN

#----------- Dynamic Data Analysis in Shiny app -----------####
source("shiny_app_ui.R")
source("shiny_app_server.R")
shinyApp(shiny_ui, ShinyServer)

#----------- End -----------#
