# ============================================================
# NLP untuk Membaca Dokumen Besar - R
# Packages: tidytext, tm, quanteda, readr, dplyr, ggplot2
# ============================================================

# Install packages jika belum ada
packages <- c("tidytext", "tm", "quanteda", "quanteda.textstats",
              "readr", "dplyr", "ggplot2", "stringr", "topicmodels",
              "wordcloud", "RColorBrewer", "sentimentr")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}
invisible(lapply(packages, install_if_missing))

# ============================================================
# 1. BACA DOKUMEN BESAR SECARA EFISIEN (CHUNK BY CHUNK)
# ============================================================

read_large_file_chunks <- function(filepath, chunk_size = 1000) {
  con <- file(filepath, "r")
  all_text <- character(0)
  repeat {
    chunk <- readLines(con, n = chunk_size, warn = FALSE)
    if (length(chunk) == 0) break
    all_text <- c(all_text, chunk)
  }
  close(con)
  return(paste(all_text, collapse = " "))
}

# Contoh penggunaan:
# text_data <- read_large_file_chunks("dokumen_besar.txt")

# Untuk file PDF:
read_pdf_large <- function(filepath) {
  if (!require("pdftools", quietly = TRUE)) install.packages("pdftools")
  library(pdftools)
  pages <- pdf_text(filepath)
  return(paste(pages, collapse = " "))
}

# ============================================================
# 2. PREPROCESSING TEKS
# ============================================================

preprocess_text <- function(text) {
  text <- tolower(text)                          # lowercase
  text <- str_remove_all(text, "[[:punct:]]")    # hapus tanda baca
  text <- str_remove_all(text, "[[:digit:]]")    # hapus angka
  text <- str_squish(text)                       # hapus spasi berlebih
  return(text)
}

# ============================================================
# 3. TOKENISASI & HAPUS STOP WORDS
# ============================================================

tokenize_and_clean <- function(text, language = "english") {
  df <- data.frame(text = text, stringsAsFactors = FALSE)

  tokens <- df %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words, by = "word") %>%   # hapus stop words (English)
    filter(nchar(word) > 2)                  # hapus kata < 3 karakter

  return(tokens)
}

# ============================================================
# 4. FREKUENSI KATA (WORD FREQUENCY)
# ============================================================

word_frequency <- function(tokens, top_n = 20) {
  freq <- tokens %>%
    count(word, sort = TRUE) %>%
    head(top_n)

  ggplot(freq, aes(x = reorder(word, n), y = n)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(title = "Top Kata Terbanyak", x = "Kata", y = "Frekuensi") +
    theme_minimal()
}

# ============================================================
# 5. WORD CLOUD
# ============================================================

generate_wordcloud <- function(tokens, max_words = 100) {
  freq <- tokens %>% count(word, sort = TRUE)
  wordcloud(words = freq$word,
            freq  = freq$n,
            max.words = max_words,
            colors = brewer.pal(8, "Dark2"),
            random.order = FALSE)
}

# ============================================================
# 6. ANALISIS SENTIMEN
# ============================================================

sentiment_analysis <- function(tokens) {
  sentiments <- tokens %>%
    inner_join(get_sentiments("bing"), by = "word") %>%
    count(sentiment) %>%
    mutate(percent = n / sum(n) * 100)

  print(sentiments)

  ggplot(sentiments, aes(x = sentiment, y = n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    scale_fill_manual(values = c("negative" = "tomato", "positive" = "seagreen")) +
    labs(title = "Analisis Sentimen", x = "Sentimen", y = "Jumlah Kata") +
    theme_minimal()
}

# ============================================================
# 7. TOPIC MODELING (LDA)
# ============================================================

topic_modeling <- function(tokens, n_topics = 5) {
  dtm <- tokens %>%
    count(word) %>%
    cast_dtm(document = 1, term = word, value = n)

  lda_model <- LDA(dtm, k = n_topics, control = list(seed = 42))

  topics <- tidy(lda_model, matrix = "beta") %>%
    group_by(topic) %>%
    slice_max(beta, n = 10) %>%
    ungroup()

  ggplot(topics, aes(x = reorder_within(term, beta, topic),
                     y = beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~topic, scales = "free") +
    coord_flip() +
    scale_x_reordered() +
    labs(title = "Topic Modeling (LDA)", x = "Term", y = "Beta") +
    theme_minimal()
}

# ============================================================
# 8. PIPELINE LENGKAP - JALANKAN SEMUA SEKALIGUS
# ============================================================

run_nlp_pipeline <- function(filepath, file_type = "txt", top_words = 20, n_topics = 5) {
  cat(">> Membaca file...\n")
  if (file_type == "pdf") {
    raw_text <- read_pdf_large(filepath)
  } else {
    raw_text <- read_large_file_chunks(filepath)
  }

  cat(">> Preprocessing...\n")
  clean_text <- preprocess_text(raw_text)

  cat(">> Tokenisasi & hapus stop words...\n")
  tokens <- tokenize_and_clean(clean_text)

  cat(">> Word frequency plot...\n")
  print(word_frequency(tokens, top_n = top_words))

  cat(">> Word cloud...\n")
  generate_wordcloud(tokens)

  cat(">> Analisis sentimen...\n")
  print(sentiment_analysis(tokens))

  cat(">> Topic modeling...\n")
  print(topic_modeling(tokens, n_topics = n_topics))

  cat(">> Selesai!\n")
  return(tokens)
}

# ============================================================
# CONTOH PENGGUNAAN
# ============================================================

# Untuk file TXT:
# tokens <- run_nlp_pipeline("dokumen_besar.txt", file_type = "txt")

# Untuk file PDF:
# tokens <- run_nlp_pipeline("laporan.pdf", file_type = "pdf")
