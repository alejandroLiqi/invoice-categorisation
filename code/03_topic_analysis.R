

# : -----------------------------------------------------------------------


# A. SETUP =================================================================================================

## Load packages: -----------------------------------------------------------------------
box::use(data.table[...])
box::use(tm[...])
box::use(topicmodels[...])
box::use(wordcloud[...])
box::use(RColorBrewer[brewer.pal])
box::use(ggplot2[...])
box::use(plotly[ggplotly])

set.seed(1234)


## Select data: -----------------------------------------------------------------------

lid_company = 'LID_50_dimensionesanitariasrl'

dts = readRDS(file.path('data', 'output', lid_company, paste0(lid_company, '_tagged.RDS')))
setDT(dts)



# B. DATA PREP =================================================================================================

### Filter relevant fields
dts = dts[type == 'Fattura']

# dts_y = dts[, .(invoice_id, sender_name, sender_vat, sender_ateco, sender_ateco_desc, sender_supplychain, category, amount_net, hexiles, vat_perc, amount_perc_y, prob, tags)]

setkey(dts, 'invoice_id')
setorderv(dts, cols = c('sender_supplychain', 'sender_vat', 'sender_name', 'invoice_id'))



# C. WORD CLOUD =================================================================================================

dts[, category := fifelse(is.na(category), 'NOCAT', category)]

vec_categories = unique(dts$category)
# vec_categories = vec_categories[14]
# vec_categories
# vec_categories = 'SPESE_RD'

topic_list_result = list()

for (i in 1:length(vec_categories)) {
    
    cats = vec_categories[i]
    # cats = vec_categories
    
## Select the group
dtw = dts[category == cats]

## Create a Corpus -------------------------------
corpus = Corpus(VectorSource(dtw$tags))

# Preprocess the text (convert to lowercase, remove punctuation, etc.)
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removeWords, stopwords("italian"))

myStopwords = c(setdiff(stopwords('italian'), c("r")),'articoli', 'documento', 'documenti', "riga", 'righe', "informazione", 'informazioni', 'spese', 'spesa', 'vendita', 'vendite', 'sconto', 'sconti', 'trasporto', 'trasporti', 'condizioni', 'condizione', 'aggiuntiva', 'aggiuntive', 'aggiuntivi', 'aggiuntivo'  )
corpus = tm_map(corpus, removeWords, myStopwords)


## Create a Document-Term Matrix (DTM) -------------------------------
dtm = DocumentTermMatrix(corpus)
word_freq_matrix = as.matrix(dtm)

# Calculate word frequencies
word_freqs = colSums(word_freq_matrix)

# Create a data frame with word frequencies
word_freq_df = data.table(word = names(word_freqs), freq = word_freqs)
setorder(word_freq_df, -freq)

## Create Wordcloud prelim -------------------------------
# wc = wordcloud(words = head(word_freq_df$word, 400), freq = word_freq_df$freq, min.freq = 10, colors = RColorBrewer::brewer.pal(12, "Dark2"))
# gc()


# D. TOPIC MODELLING =================================================================================================

# Create an LDA model
non_empty_rows = rowSums(word_freq_matrix > 0) > 0
dtm_filtered = word_freq_matrix[non_empty_rows, ]

lda_model = LDA(dtm_filtered, k = 8)  # You can adjust 'k' to specify the number of topics to discover

# Get the top words for each topic
topics = terms(lda_model, 15)  # Change '10' to the number of top words you want to display per topic

# Usable data.table
dt_topics = as.data.table(topics)

if(nrow(dt_topics) > 0) {
    
names(dt_topics) = paste0('topic_', 1:ncol(dt_topics))

dt_topics = melt(dt_topics, 
     id.vars = NULL, measure.vars = names(dt_topics),
     variable.name = 'topic', value.name = 'word')


## Puting things together -------------------------------

dt_analysis = merge(dt_topics, word_freq_df, all.x = TRUE, by = 'word') |> setcolorder(c('topic', 'word', 'freq'))


# Function to associate words with topics
associate_words_with_topics = function(tag) {
    # Split the 'tag' column into individual words
    words = unlist(strsplit(tag, ", "))
    topic = list()
    
    # Iterate through each word and determine its topic
    for (i in 1:length(words)) {
        parola = words[i]
        topic[i] = as.character(paste(dt_analysis[word %in% parola]$topic, collapse = ','))
        
    }
    
    # Prepare list
    topics_list = unlist(topic)
    topics_list = unlist(strsplit(topics_list, ","))
    topics_list = unique(topics_list[!topics_list %chin% c('')])
    
    topics_list = paste(topics_list, collapse = ',')
    
    return(topics_list)
}

# Apply the word-topic association function to your 'tags' column
dtw$topics = lapply(dtw$tags, associate_words_with_topics)

all_topics = as.character(unique(dt_topics$topic))

# Loop through each unique topic and create a new column
for (topic in all_topics) {
    dtw[, (topic) := as.integer(grepl(topic, topics))]
}
# Calculate the average invoice amount for each word within each topic

avg_topic = data.table(topic = character(0), avg_invoice = double(0))

for (topic in all_topics) {
    
    avg_topic = rbind(avg_topic, data.table(topic = topic, avg_invoice = mean(dtw[get(topic) == 1]$amount_net, na.rm = TRUE)))

}

## FINAL VIEWS -------------------------------

dt_analysis = merge(dt_analysis, avg_topic, all.x = TRUE, by = 'topic')
dt_analysis[, word := if (.N > 1) paste(word, sub("topic_", "", topic), sep = "_") else word, by = word]

dt_analysis[, sizex := freq * avg_invoice / mean(freq * avg_invoice)]

dtw[, topic_desc := list(dt_topics)]

dtws = dtw[, .(invoice_id, topics, topic_desc)]

topic_list_result[cats] = list(dtws)

}

dtws = data.table(invoice_id = NA_character_, topics = NA_character_, topic_desc = NA_character_)

}


dt_topics = rbindlist(topic_list_result, idcol = 'category')

dts = merge(dts, dt_topics, all.x = TRUE, by = c('invoice_id', 'category'))



