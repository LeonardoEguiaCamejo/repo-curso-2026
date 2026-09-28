##### Este articulo habla del comportamiento de la Generacion Z y como impacta en distintos ambitos de hoy en dia #####

#install.packages("tidytext")
#install.packages("textdata")

library(readr)
library(tidytext)
library(textdata)
library(dplyr)

datos <- read.csv("C:/Users/legui/OneDrive/Documents/Curso-E520/DATA-T9-mckinsey-mind-the-gap-articles-20251020.csv")

## conteo de palabras
palabras_frecuentes <- datos_tidy |> 
  anti_join(stop_words, by = "word") |> 
  count(word, sort = TRUE) |>
  as_tibble()
palabras_frecuentes

## filtrado de palabras positivas y negativas
positive <- get_sentiments("bing") |>
  filter(sentiment == "positive")
positive

negative <- get_sentiments("bing") |>
  filter(sentiment == "negative")
negative

## cantidad de palabras positivas y negativas
sentimiento_palabras <- datos_tidy |> 
  inner_join(bing, by = "word")
sentimiento_palabras |> 
  count(sentiment)

## conteo de palabras positivas y negativas con 2 diccionarios distintos
afinn <- get_sentiments("afinn")

palabras_afinn <- datos |> 
  unnest_tokens(output = word, input = article_text) |> 
  inner_join(afinn, by = "word") |> 
  count(word, value, sort = TRUE)|>
  as_tibble()
palabras_afinn

palabras_bing <- datos |> 
  unnest_tokens(output = word, input = article_text) |> 
  inner_join(bing, by = "word") |> 
  count(word, sentiment, sort = TRUE) |> 
  ungroup()
head(palabras_bing, 10)

## bigrams mas comunes
bigrams <- datos |> 
  unnest_tokens(output = bigram, input = article_text, token = "ngrams", n = 2)

bigrams_limpios <- bigrams |> 
  separate(bigram, c("word1", "word2"), sep = " ") |> 
  filter(!word1 %in% stop_words$word) |> 
  filter(!word2 %in% stop_words$word) |> 
  count(word1, word2, sort = TRUE)
head(bigrams_limpios, 10)

## trigrams mas comunes
trigrams <- datos |> 
  unnest_tokens(output = trigram, input = article_text, token = "ngrams", n = 3) |> 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") |> 
  count(word1, word2, word3, sort = TRUE)
head(trigrams, 10)

# le quitamos los conectores para mejor analisis
trigrams_filtrado <- datos |> 
  unnest_tokens(output = trigram, input = article_text, token = "ngrams", n = 3) |> 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") |> 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) |> 
  count(word1, word2, word3, sort = TRUE)
head(trigrams, 10)
