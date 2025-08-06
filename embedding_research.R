library(tidyverse)
source("lit_review_functions_final.R")
setwd("~/Capstone/R-code")

#### reading in the data ####
metadata<-read.csv("CORD_19_Abstracts_H1_2020.csv")
metadata$publish_time<-as.Date(meta_data$publish_time)

#when were the studies published?

table(month(metadata$publish_time))

early_studies<-metadata%>%
  filter(publish_time<='2020-03-31')%>%
  filter(doi != "")

sentence_df<-split_sentences(early_studies$abstract, id_col=early_studies$doi, block_size = 1)


word_df<-split_words(sentence_df$sentence,sentence_df$doc, sentence_df$line)

#dropping line since it would dulicate, and I'm looking at the abstract level
word_df<-word_df%>%
  select(-line)

tf_idf_matrix<-calc_baseline_tf_idf(word_data_frame = word_df, word_col = "word", line_col = "doc")

hist(tf_idf_matrix$tf_idf)

#dropping docs with very few words

cutoff<-summary(tf_idf_matrix$total)[2]/2

tf_idf_matrix<-tf_idf_matrix[tf_idf_matrix$total> cutoff,]

# how many distinct docs and how many distinct words?

total_cells<-length(unique(tf_idf_matrix$word)) * length(unique(tf_idf_matrix$line))
total_cells

# high dimensional embedding

hd_matrix<-make_tf_idf_matrix(tf_idf_matrix,word_col = 'word', line_col = 'line')


# map to low dim

ld_matrix<-map_to_low_dim(hd_matrix, variance_threshold = 0.8)

eigenvalues<-ld_matrix$eigenvalues

loading_scores<-ld_matrix$loading_scores

low_dim_embed<-ld_matrix$pc_df

write.csv(eigenvalues, "eigenvalues_H1_2020.csv")
write.csv(loading_scores, "loading_scores_H1_2020.csv")
write.csv(low_dim_embed, "low_dim_embed_H1_2020.csv")
