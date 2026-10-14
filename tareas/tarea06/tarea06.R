# install.packages(c("edgar", "tidytext", "dplyr", "tidyr", "stringr", "ggplot2", "XML"))

library(edgar)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(ggplot2)


# ---- 1. Parametros -----------------------------------------------------------
useragent <- "Leonardo Eguia leguiacamejo@gmail.com"
anio_presentacion <- 2025

empresas <- tibble::tribble(
  ~cik,     ~empresa,         ~ticker,
  913059,   "BBVA Argentina", "BBAR",
  1347426,  "Banco Macro",    "BMA",
  1469395,  "Pampa Energia",  "PAM",
  904851,   "YPF",            "YPF"
)

palabras_clave <- c("inflation", "devaluation", "exchange controls",
                    "default", "restructuring", "imf", "capital controls",
                    "price controls", "country risk", "embi")

dir.create("resultados", showWarnings = FALSE)


# ---- 2. Descarga de los 20-F con edgar ---------------------------------------
descarga <- getFilings(cik.no       = empresas$cik,
                       form.type    = "20-F",
                       filing.year  = anio_presentacion,
                       quarter      = c(1, 2, 3, 4),
                       downl.permit = "y",
                       useragent    = useragent)
print(descarga)


# ---- 3. Lectura y limpieza del texto -----------------------------------------
leer_20f <- function(archivo) {
  lineas <- readLines(archivo, warn = FALSE, encoding = "UTF-8")
  
  inicios <- grep("<DOCUMENT>", lineas, ignore.case = TRUE)
  finales <- grep("</DOCUMENT>", lineas, ignore.case = TRUE)
  tipos   <- sapply(inicios, function(i) lineas[i + 1])
  k <- which(grepl("<TYPE>20-F", tipos, ignore.case = TRUE))[1]
  if (is.na(k)) k <- 1
  doc <- paste(lineas[inicios[k]:finales[k]], collapse = "\n")
  
  html <- XML::htmlParse(doc, asText = TRUE, encoding = "UTF-8")
  basura <- XML::getNodeSet(html, "//script | //style | //*[local-name()='header']")
  if (length(basura) > 0) XML::removeNodes(basura)
  texto <- paste(XML::xpathSApply(html, "//body//text()", XML::xmlValue),
                 collapse = " ")
  XML::free(html)
  
  texto %>%
    str_replace_all("[\u00a0\\s]+", " ") %>%   # espacios duros y saltos
    str_to_lower() %>%
    str_squish()
}

carpeta <- "edgar_Filings/Form 20-F"
archivos <- tibble(archivo = list.files(carpeta, pattern = "\\.txt$",
                                        recursive = TRUE, full.names = TRUE)) %>%
  mutate(nombre = basename(archivo),
         cik    = as.numeric(str_extract(nombre, "^\\d+")),
         fecha  = as.Date(str_extract(nombre, "\\d{4}-\\d{2}-\\d{2}"))) %>%
  filter(cik %in% empresas$cik,
         format(fecha, "%Y") == as.character(anio_presentacion)) %>%
  group_by(cik) %>% slice_max(fecha, n = 1, with_ties = FALSE) %>% ungroup() %>%
  inner_join(empresas, by = "cik")

if (nrow(archivos) == 0) {
  stop("No se encontraron 20-F descargados. Revisar el useragent y la conexion.")
}
print(archivos %>% select(empresa, fecha, nombre))

textos <- archivos %>%
  mutate(texto = vapply(archivo, leer_20f, character(1), USE.NAMES = FALSE)) %>%
  select(empresa, ticker, fecha, texto)

# Tamaño de cada reporte
textos %>%
  mutate(palabras = str_count(texto, "\\S+")) %>%
  select(empresa, fecha, palabras) %>%
  print()


# ---- 4. Incidencia de las palabras clave -------------------------------------
incidencia <- textos %>%
  mutate(total_palabras = str_count(texto, "\\S+")) %>%
  crossing(termino = palabras_clave) %>%
  mutate(n = str_count(texto, regex(paste0("\\b", termino, "\\b"))),
         por_10mil = 10000 * n / total_palabras) %>%
  select(empresa, termino, n, por_10mil)

tabla_incidencia <- incidencia %>%
  select(empresa, termino, n) %>%
  pivot_wider(names_from = empresa, values_from = n)
print(tabla_incidencia)
write.csv(tabla_incidencia, "resultados/incidencia_palabras_clave.csv",
          row.names = FALSE)

g_incidencia <- ggplot(incidencia,
                       aes(x = reorder(termino, por_10mil), y = por_10mil,
                           fill = empresa)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = paste("Incidencia de terminos clave - 20-F presentados en",
                     anio_presentacion),
       x = NULL, y = "Apariciones cada 10.000 palabras", fill = NULL) +
  theme_minimal()
print(g_incidencia)

# ---- 5. Tokenizacion ---------------------------------------------------------
palabras <- textos %>%
  select(empresa, texto) %>%
  unnest_tokens(word, texto) %>%
  filter(!str_detect(word, "\\d"),
         nchar(word) > 1) %>%
  anti_join(stop_words, by = "word")


# ---- 6. Palabras distintivas por empresa (tf-idf) ----------------------------
excluir <- c("bbva", "macro", "pampa", "ypf", "bma", "bbar", "pam")

tfidf <- palabras %>%
  filter(!word %in% excluir) %>%
  count(empresa, word, sort = TRUE) %>%
  bind_tf_idf(word, empresa, n) %>%
  filter(tf_idf > 0) %>%   # descarta palabras presentes en las 4 empresas
  arrange(desc(tf_idf))

top_tfidf <- tfidf %>%
  group_by(empresa) %>%
  slice_max(tf_idf, n = 15, with_ties = FALSE) %>%
  ungroup()
print(top_tfidf, n = 30)

g_tfidf <- ggplot(top_tfidf,
                  aes(x = tf_idf, y = reorder_within(word, tf_idf, empresa),
                      fill = empresa)) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  facet_wrap(~ empresa, scales = "free") +
  labs(title = "Palabras distintivas por empresa (tf-idf)",
       x = "tf-idf", y = NULL) +
  theme_minimal()
print(g_tfidf)

# ---- 7. Sentimiento con Loughran-McDonald ------------------------------------
load(system.file("data/LMMasterDictionary.rda", package = "edgar"))

loughran <- LMMasterDictionary %>%
  select(word, negative, positive, uncertainty, litigious,
         constraining, superfluous) %>%
  pivot_longer(-word, names_to = "sentiment", values_to = "marca") %>%
  filter(marca != 0) %>%
  select(-marca) %>%
  mutate(word = str_to_lower(word))

sentimientos <- palabras %>%
  inner_join(loughran, by = "word", relationship = "many-to-many")

tono <- sentimientos %>%
  count(empresa, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(tono_neto = (positive - negative) / (positive + negative)) %>%
  arrange(desc(tono_neto))
print(tono)

prop_sent <- tono %>%
  select(-tono_neto) %>%
  pivot_longer(-empresa, names_to = "sentiment", values_to = "n") %>%
  group_by(empresa) %>%
  mutate(proporcion = n / sum(n)) %>%
  ungroup()

g_sent <- ggplot(prop_sent,
                 aes(x = sentiment, y = proporcion, fill = empresa)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Sentimiento Loughran-McDonald por empresa",
       x = NULL, y = "% de palabras con sentimiento", fill = NULL) +
  theme_minimal()
print(g_sent)


# 7b. Principales palabras de cada sentimiento (todas las empresas juntas)
top_sent <- sentimientos %>%
  count(sentiment, word, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10, with_ties = FALSE) %>%
  ungroup()
print(top_sent, n = 30)

g_top_sent <- ggplot(top_sent,
                     aes(x = n, y = reorder_within(word, n, sentiment),
                         fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  facet_wrap(~ sentiment, scales = "free") +
  labs(title = "Palabras que mas aportan a cada sentimiento",
       x = "Frecuencia", y = NULL) +
  theme_minimal()
print(g_top_sent)

# 7c. Principales palabras negativas y positivas por empresa
top_pos_neg <- sentimientos %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(empresa, sentiment, word, sort = TRUE) %>%
  group_by(empresa, sentiment) %>%
  slice_max(n, n = 5, with_ties = FALSE) %>%
  ungroup()
print(top_pos_neg, n = 40)

