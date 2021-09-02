
# install.packages("quanteda.textmodels") 

#----------- Config -----------####
list_of_packages <- c("FactoMineR", "dplyr", "ggplot2", "ggpubr", "shiny", "ggrepel", "ggdendro", "gridExtra", "english", "data.table")
invisible(lapply(list_of_packages, require, character.only = TRUE))
setwd("/home/teijehidde/Documents/Git Blog and Coding/Islamism as Discursive Practice (Correspondence Analysis and Text Analysis)")
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

#----------- Analysis -----------####
# include CA 
# include HC 

#----------- Dynamic Data Analysis in Shiny app -----------####
source("shiny_app_ui.R")
source("shiny_app_server.R")
shinyApp(shiny_ui, ShinyServer)

#----------- End -----------#
