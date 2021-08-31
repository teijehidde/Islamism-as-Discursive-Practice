

# y[["v_culled_corpus"]] <- names(
#   y$vi_frequency_words[y$vi_frequency_words > v_min]
# )
# y[["v_culled_corpus"]] <- y$v_culled_corpus[!(y$v_culled_corpus %in% drop_words)]
# y[["v_culled_corpus"]] <- y$v_culled_corpus[!(y$v_culled_corpus == "")]

corpus_test <- c('la', 'yo',  'yo',  'la',  'tadiya', 'la', 'la', 'boe', 'tadiya', 'yo', 'tadiya', 'tadiya',  'yo')
corpus_test_unique <- unique(corpus_test) 
corpus_test_factor <- factor(corpus_test, levels = corpus_test_unique)
gg <- split(g, af)
test <- match(corpus_test_unique, corpus_test)

vec <- c("D","B","B","C","C")

dt <- as.data.table(vec)[, list(list(.I)), by = vec]
setattr(dt$V1, 'words', dt$vec)
dt$V1




start_time <- Sys.time()
sleep_for_a_minute()
end_time <- Sys.time()

