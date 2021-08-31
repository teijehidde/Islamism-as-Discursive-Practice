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
# NB 2: 
# I am rewriting this app as showcase of different methods for complexity reduction on an (Arabic) corpus. 
# This also means that culling of corpus needs to be dynamic (as different methods will work better at different TDM densities..)! 
# Methods I would like to apply: 
# 1: Collocations - Cosin? /  
# 2: Clustering - K-means 
# 3: CA (possibly with clustering added in the end) - scaling 
# 4: Linguistic networks of some sort? 
# 5: T-SNE - iterative scaling 
# install.packages("quanteda.textmodels") 

#----------- Config -----------####
# install.packages("data.table")
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

#----------- Dynamic Data Analysis in Shiny app -----------####
source("shiny_app_ui.R")
source("shiny_app_server.R")
shinyApp(shiny_ui, ShinyServer)

#----------- End -----------#
