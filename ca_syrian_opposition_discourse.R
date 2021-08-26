#
# Here comes a generic explanation of the shiny app and its functionality.
#
# NB, note to self regarding style:
# I use BigCamelCase for functions, camel_casing for objects.
# I avoid right hand assignments. attempts to use <- as much as possible.
# I try to use explicit return in functions as much as possible.
# These exceptions are in line with google approach (see: https://google.github.io/styleguide/Rguide.html) and close to coding in Python.
# For other bits, everything according to tidyverse approach. See: https://style.tidyverse.org/syntax.html
#

#----------- Config -----------####
list_of_packages <- c("FactoMineR", "dplyr", "ggplot2", "ggpubr", "shiny", "ggrepel", "ggdendro", "gridExtra", "english")
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
  drop_punctuation = TRUE,
  drop_words = c(as.character(support_files$tool_words[, 1])),
  v_min = 20
)

#----------- (Supplementary) Correspondence Analysis -----------####
correspondence_analysis <- CA(
  mined_text$lexical_table,
  ncp = NULL,
  row.sup = NULL,
  col.sup = NULL,
  quanti.sup = NULL,
  quali.sup = NULL,
  graph = FALSE,
  row.w = NULL
)

#----------- Presentation Data in Shiny app -----------####
source("shiny_app_server.R")
source("shiny_app_ui.R")
shinyApp(shiny_ui, ShinyServer)

#----------- End -----------#
