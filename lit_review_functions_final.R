##################### functions library ##########################

#### UI Functions ####


pdfs_to_text_csv<-function(pdf_folder='pdf_docs', file_out='study_text.csv'){
  library(pdftools)
  suppressWarnings(library(tidyverse))
  
  pdf_folder<-'pdf_docs'
  
  docs<-list.files(path=pdf_folder)
  
  clean_df<-data.frame(matrix(nrow=length(docs), ncol=2))
  colnames(clean_df)<-c("Doc","Text")
  
  
  for (i in 1:length(docs)){
    dtr<-docs[i]
    loc<-paste0(pdf_folder,"/",dtr)
    txt<-pdf_text(loc)
    doc_text<-NULL
    for (k in 1:length(txt)){
      page<-txt[k]%>%
        str_split("\\n")%>%
        unlist()
      left_side<-NULL
      right_side<-NULL
      for (j in 1:length(page)){
        split_line<-page[j]%>%str_split("\\s{2,}")%>%unlist()
        left_side<-append(left_side, split_line[1])
        right_side<-append(right_side, split_line[2])
      }
      page_name<-paste0("Doc_",i,"_page_",k)
      page_lines<-append(left_side, right_side)
      page_text<-paste(page_lines, collapse=" ")
      doc_text<-append(doc_text, page_text)
    }
    clean_df[i,1]<-dtr
    clean_df[i,2]<-paste(doc_text, collapse=" ")%>%
      str_replace_all("\\s+NA\\s+","")%>%
      str_replace_all("REFERENCES[:]?\\s+.*","")%>%
      str_replace_all("References[:]?\\s+.*","")
    
  }
  write.csv(clean_df, file = file_out, row.names = FALSE)
}

create_vector_db<-function(path_to_file = 'study_text.csv', text_col_name = 'Text', doc_col_name = 'Doc', abstraction_level = 0.2, block_length = 10){
  
  #setup
  suppressWarnings(library(tidyverse))
  suppressWarnings(library(tidytext))
  options(dplyr.summarise.inform = FALSE)
  
  ###### testing block ####  
  #path_to_file = 'study_text.csv'
  #text_col_name = "Text"
  #doc_col_name = "Doc"
  #abstraction_level = 0.2
  #block_length=10

  

  #read in the data
  full_data<-read.csv(path_to_file)
  stop_words<-read.csv("custom_stop_words.csv")
  source('lit_review_functions_final.R')
  
  names(full_data)[names(full_data)== text_col_name]<-'text_col'
  names(full_data)[names(full_data)==doc_col_name]<-'doc_col'
  text_col = full_data$text_col
  doc_col = full_data$doc_col
  
  
  #make a dataframe of split-out sentences
  sentence_df<-split_sentences(input_text = text_col, id_col=doc_col, block_size=block_length)
  
  #make a dataframe of split-out words
  word_df<-split_words(sentence_col = sentence_df$sentence, doc_id_col = sentence_df$doc, line_id_col = sentence_df$block)
  
  
  #calculate tf-idf scores
  abstract_tf_idf<-calc_baseline_tf_idf(word_data_frame = word_df, word_col = "word", line_col="block")
  
  #turn the list of scores into a large matrix
  tfidf_matrix<-make_tf_idf_matrix(existing_tf_idf = abstract_tf_idf,word_col="word", line_col="block")
  
  #embed in low dim
  embedding<-map_to_low_dim(existing_tf_mat = tfidf_matrix, variance_threshold = abstraction_level)
  
  study_db<-list(sentence_df=sentence_df, word_df= word_df,abstract_tf_idf = abstract_tf_idf, tfidf_matrix=tfidf_matrix, embedding = embedding)
  print("Database Ready!")
  return(study_db)
}


ask_query<-function(existing_vector_db){
  source('lit_review_functions_final.R')
  
  #testing block
  #query_string<-"COVID-19" 
  #existing_vector_db<-study_db
  
  #ask query
  query_string<-readline("What do you want to know?")
  existing_vector_db<-existing_vector_db
  sentence_df<-existing_vector_db$sentence_df  
  word_df<-existing_vector_db$word_df
  abstract_tf_idf<-existing_vector_db$abstract_tf_idf
  tfidf_matrix<-existing_vector_db$tfidf_matrix
  embedding<-existing_vector_db$embedding
  
  


   #add query to the database
    query_embed<-add_new_strings_2(new_string = query_string, new_string_id = "Q", existing_vector_db = existing_vector_db)

    
    #get results
    qc1_results<-query_returns_2(existing_vector_db=existing_vector_db, center_query_df=query_embed)
    
    
    return(qc1_results)
    
}


################### helper functions ##############

#### split sentences ####

split_sentences<-function(input_text, id_col, block_size){
  ###testing block
  #input_text<-text_col
  #id_col<-doc_col
  
  #sent_patt<-"(?<=\\s[a-z]{2,})[.?!]\\s+(?=[A-Z])"
  sent_patt<-"[.?!:]\\s+(?=[A-Z])"
  noise_patt<-"[.?!:][0-9]+[,-]?[0-9]*"
  
  new_df<-data.frame()
  
  for (i in 1:length(input_text)){
    doc_id<-id_col[i]
    abs_text<-input_text[i]
    
    
    
    doc_df<-abs_text%>%
      str_replace_all(noise_patt, ". ")%>%
      str_split(sent_patt)%>%
      unlist()%>%
      as.data.frame()%>%
      mutate(doc = doc_id)
    
    new_df<-rbind(new_df, doc_df)
  }
  
  new_df$line<-paste0("line_",seq_along(1:nrow(new_df)))
  names(new_df)[names(new_df)== "."]<-"sentence"
  new_df$row_nr<-seq_along(1:nrow(new_df))
  new_df$block<-paste0("block_",floor(new_df$row_nr/block_size))
  
  return(new_df)
}

#### split words ####

split_words<-function(sentence_col, doc_id_col, line_id_col){
  
  ##testing block
  #sentence_col = sentence_df$sentence
  #doc_id_col = sentence_df$doc
  #line_id_col = sentence_df$line
  #i=1
  
  stop_words<-read.csv('custom_stop_words.csv')
  word_patt<-"\\s+"
  punct_patt<-"[^\\w\\s]+"
  
  word_df<-data.frame()
  
  for (i in 1:length(sentence_col)){
    doc_id<-doc_id_col[i]
    sentence_id<-line_id_col[i]
    sent_text<-sentence_col[i]
    
    temp_df<-sent_text%>%
      str_to_lower()%>%
      str_replace_all(punct_patt, "")%>%
      str_split(word_patt)%>%
      unlist()%>%
      as.data.frame()%>%
      mutate(doc = doc_id,
             line = sentence_id)
    word_df<-rbind(word_df, temp_df)
    
  }
  
  names(word_df)[names(word_df)== "."]<-"word"
  
  word_df$word<-na_if(word_df$word,"")
  
  word_df<-word_df%>%anti_join(stop_words, by = 'word')%>%drop_na()
  

  
  return(word_df)
}

#### term document / inverse document frequency calc####

calc_baseline_tf_idf<-function(word_data_frame, word_col, line_col){
  ##testing block
  #word_data_frame<-word_df
  #word_col<-"word"
  #line_col<-"block"
  
  
  
  names(word_data_frame)[names(word_data_frame)== word_col]<-"word"
  names(word_data_frame)[names(word_data_frame)==line_col]<-"line"
  
  
  tf_df<-word_data_frame%>%
    group_by(line, word)%>%
    summarize(n())
  
  
  names(tf_df)[names(tf_df)=="n()"]<-"count" # # of times that word occurs in a block
  
  tf_total<-tf_df%>%
    group_by(line)%>%
    summarize(sum(count))
  
  names(tf_total)[names(tf_total)=="sum(count)"]<-"total" # # of times word is used overall
  
  idf_df<-word_data_frame%>%
    group_by(word)%>%
    summarize(n_distinct(line))
  
  names(idf_df)[names(idf_df)=="n_distinct(line)"]<-"line_count" # # of blocks/lines the word is used in
  
  tf_idf_df<-tf_df%>%
    left_join(tf_total, by = "line")%>%
    left_join(idf_df, by = "word")
  
  tf_idf_df$tf_idf<-log((tf_idf_df$count/tf_idf_df$total)*(1/tf_idf_df$line_count))
  
  tf_idf_df<-tf_idf_df[(tf_idf_df$line_count>1 & tf_idf_df$total>1),]
  
  return(tf_idf_df)
}

#### transform into wide dataframe ####

make_tf_idf_matrix<-function(existing_tf_idf, word_col, line_col){
  
  ### testing block
  #existing_tf_idf<-abstract_tf_idf
  #word_col<-"word"
  #line_col<-"line"
  
  names(existing_tf_idf)[names(existing_tf_idf)== word_col]<-"word"
  names(existing_tf_idf)[names(existing_tf_idf)==line_col]<-"line"
  
  
  vocab<-existing_tf_idf$word%>%unique()
  col_length<-length(vocab)
  doc_list<-existing_tf_idf$line%>%unique()
  row_length<-length(doc_list)
  
  
  tf_mat<-matrix(data=NA, nrow=row_length, ncol=col_length)
  
  rownames(tf_mat)<-doc_list
  colnames(tf_mat)<-vocab
  
  for (i in 1:nrow(existing_tf_idf)){
    r<-existing_tf_idf[i,1]%>%unlist()
    c<-existing_tf_idf[i,2]%>%unlist()
    v<-existing_tf_idf[i,6]%>%unlist()
    tf_mat[r,c]<-v
  }
  
  
  tf_mat[is.na(tf_mat)]<-0
  
  return(tf_mat)
}


#### Add new strings ####

add_new_strings_2<-function(new_string, new_string_id, existing_vector_db){
  #### testing block#
  #new_string<-"Describe the natural reservoir of giardia"
  #new_string_id<-'Q'
  #existing_vector_db<-study_db
  
  existing_tf_idf<-existing_vector_db$abstract_tf_idf
  existing_tf_mat<-existing_vector_db$tfidf_matrix
  existing_embedding<-existing_vector_db$embedding
  
  word_patt<-"\\s+"
  punct_patt<-"[^\\w\\s]+"
  
  query_df<-new_string%>%
    str_to_lower()%>%
    str_replace_all(punct_patt,"")%>%
    str_split(word_patt)%>%
    unlist()%>%
    as.data.frame()
  
  colnames(query_df)<-"word"
  query_marker=rep(new_string_id, times=nrow(query_df))
  
  query_tf<-query_df%>%
    mutate(query=query_marker)%>%
    group_by(word)%>%
    summarize(n())
  
  query_tf$total<-sum(query_tf$`n()`)
  names(query_tf)[names(query_tf)=="n()"]<-'count'
  
  temp_idf_df<-existing_tf_idf%>%
    ungroup()%>%
    select(word, line_count)
  
  query_tf_idf<-query_tf%>%
    inner_join(temp_idf_df, by="word")
  
  error_message<-"query not in source data"
  if(nrow(query_tf_idf) == 0){
    return(error_message)
  } else {
    
    
    query_tf_idf$tf_idf<-log((query_tf_idf$count/query_tf_idf$total)*(1/query_tf_idf$line_count))
    
    query_list<-query_tf_idf$word%>%unique()
    query_length<-length(query_list)
    
    col_length<-ncol(existing_tf_mat)
    
    query_mat<-matrix(data=NA, nrow = 1, ncol=col_length) #same vocab list as above
    
    colnames(query_mat)<-colnames(existing_tf_mat)
    rownames(query_mat)<-new_string_id
    
    for (i in 1:nrow(query_tf_idf)){
      c<-query_tf_idf[i,1]%>%unlist()
      v<-query_tf_idf[i,5]%>%unlist()
      query_mat[1,c]<-v
    }
    
    query_mat[is.na(query_mat)]<-0
    
    embed_rows<-nrow(existing_embedding$loading_scores)
    embed_cols<-ncol(existing_embedding$loading_scores)
    
    load_mat<-data.matrix(existing_embedding$loading_scores)
    
    rotated_q<-query_mat %*% load_mat
    
    query_mat<-rotated_q[,1:ncol(existing_embedding$pc_df)]
    
    return(query_mat)
  }
}

#### Singular Value Decomposition ########

map_to_low_dim<-function(existing_tf_mat, variance_threshold){
  ## testing block
  #existing_tf_mat<-tfidf_matrix
  #variance_threshold<-0.5
  
  
  SVD_out<-svd(existing_tf_mat)
  
  #calculate % of total variance for each PC
  total_variance<-sum(SVD_out$d^2)
  eigenvalues<-as.data.frame(SVD_out$d^2)%>%mutate(cum_var=cumsum(`SVD_out$d^2`))%>%
    mutate(percent_total=cum_var/total_variance)
  
  pc_list<-paste0("PC",seq_along(1:nrow(eigenvalues)))
  
  plot(eigenvalues$percent_total, ylab="% of variance explained")
  
  ncomp<-min(which(eigenvalues$percent_total>=variance_threshold))
  
  loading_scores<-as.data.frame(SVD_out$v)
  colnames(loading_scores)<-pc_list
  rownames(loading_scores)<-colnames(existing_tf_mat)
  
  pc_df<-as.data.frame(SVD_out$u %*% diag(SVD_out$d))
  rownames(pc_df)<-rownames(existing_tf_mat)
  colnames(pc_df)<-pc_list
  
  short_pc_df<-pc_df[,1:ncomp]
  
  low_dim_output<-list("eigenvalues" = eigenvalues, "loading_scores"=loading_scores, "pc_df"=short_pc_df)
  
  return(low_dim_output)
  
}

#### cosine similarity ####

cosine_sim<-function(vector_1, vector_2){
  v1N<-sum(vector_1^2)
  v2N<-sum(vector_2^2)
  dt_prod<-vector_1 %*% vector_2
  
  return(dt_prod/(sqrt(v1N) * sqrt(v2N)))
}



#### query returns #####

query_returns_2<-function(existing_vector_db, center_query_df){
  
  #test block
  #existing_vector_db<-study_db
  #center_query_df<-center1_embed
  
  existing_embedding<-existing_vector_db$embedding
  qc_loc_df<-center_query_df
  existing_sentence_df<-existing_vector_db$sentence_df
  
  other_vectors<-existing_embedding$pc_df

  
  dist_matrix<-matrix(nrow = nrow(other_vectors), ncol=1)
  
  
  for (i in 1:nrow(other_vectors)){
      center_str<-as.numeric(center_query_df)
    
      line_tmp<-as.numeric(other_vectors[i,])
    
      cosine_similarity<-cosine_sim(center_str, line_tmp)
    
      dist_matrix[i,1]<-cosine_similarity
    
    }

  
  q_df<-as.data.frame(dist_matrix)
  colnames(q_df)<-c("center_embed")
  rownames(q_df)<-rownames(other_vectors)
  q_df[is.na(q_df)]<-0
  score_df<-q_df[order(-q_df$center_embed), ,drop=FALSE]
  top_scores<-row.names(score_df)[1:5]
  
  high_score_lines<-as.character(existing_sentence_df$sentence[which(existing_sentence_df$block %in% top_scores)])
  match_out<-paste(high_score_lines, collapse = " ")
  
  return_list<-list(match_out, score_df[1:5,1])
  
  return(return_list)
}



#### update stop words ####

update_stop_words<-function(new_stop_word){
  stop_words<-read.csv("custom_stop_words.csv")
  custom_words<-data.frame(word = c(new_stop_word),
                           lexicon = c('custom'))
  
  stop_words<-rbind(stop_words,custom_words)
  write.csv(stop_words, file='custom_stop_words.csv',row.names = FALSE)
  
}


