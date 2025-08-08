library(tidyverse)
# Sample text data
text <- paste(readLines("walden.txt"), collapse = " ")

# Preprocess the text
text <- tolower(text)  # Convert to lowercase
text <- str_replace_all(text, "[[:punct:]]", "")  # Remove punctuation
words <- unlist(str_split(text, "\\s+"))  # Split into words

# Define n-gram size
n <- 3  # For bigrams

stack_trans<-matrix(nrow = (length(words)-3), ncol = 4) #for trigram
colnames(stack_trans)<- c("w1","w2","w3", "w4")

for (i in 1:(length(words)-3)){
  stack_trans[i,1]<-words[i]
  stack_trans[i,2]<-words[i+1]
  stack_trans[i,3]<-words[i+2]
  stack_trans[i,4]<-words[i+3]
}


group_stack<-stack_trans%>%
  as.data.frame()%>%
  group_by(w1, w2, w3,w4)%>%
  summarize(n())%>%
  rename(count = 'n()')

start<-"Pond"

start_vec<-start%>%
  tolower()%>%
  str_replace_all("[[:punct:]]", "")%>%
  str_split("\\s+")%>%
  unlist()

out_length<-100

c1 <- start_vec[1]
c2 <- start_vec[2]
c3<- start_vec[3]

if(is.na(c2)&is.na(c3)){
  out_vec<-paste(c1)} else if (is.na(c3)){
  out_vec<-paste(c1,c2)
  } else {
  out_vec<-paste(c1,c2,c3)
  }

for (i in 1:(out_length-2)){
 
  if (is.na(c1)){
    break
  } else if(is.na(c2)&is.na(c3)){
    stack_tmp<-group_stack[group_stack$w1 == c1,]
    prob_vec<-stack_tmp$count/sum(stack_tmp$count)
    word_out_1<-sample(stack_tmp$w2,1,FALSE,prob=prob_vec)
    
    stack2<-group_stack[group_stack$w1==c1 & group_stack$w2==word_out_1,]
    prob_vec_2<-stack2$count/sum(stack2$count)
    word_out_2<-sample(stack2$w3,1,FALSE,prob=prob_vec_2)
    
    stack3<-group_stack[group_stack$w1==c1 & group_stack$w2==word_out_1 & group_stack$w3 == word_out_2,]
    prob_vec_3<-stack3$count/sum(stack3$count)
    word_out_3<-sample(stack3$w4,1,FALSE,prob=prob_vec_3)
    
    out_vec<-paste(out_vec, word_out_1, word_out_2, word_out_3)
    c1<-word_out_1
    c2<-word_out_2
    c3<-word_out_3
    
    
  }else if(is.na(c3)){
    stack_tmp<-group_stack[group_stack$w1 == c1 & group_stack$w2 == c2,]
    prob_vec<-stack_tmp$count/sum(stack_tmp$count)
    word_out_1<-sample(stack_tmp$w3,1,FALSE,prob=prob_vec)
    
    stack2<-group_stack[group_stack$w1==c1 & group_stack$w2==c2 & group_stack$w3 == word_out_1,]
    prob_vec_2<-stack2$count/sum(stack2$count)
    word_out_2<-sample(stack2$w4,1,FALSE,prob=prob_vec_2)
    
    out_vec<-paste(out_vec, word_out_1, word_out_2)
    c1<-c2
    c2<-word_out_1
    c3<-word_out_2
    
  } else {
  stack_tmp<-group_stack[group_stack$w1 == c1 & group_stack$w2 == c2 & group_stack$w3 == c3,]
  prob_vec<-stack_tmp$count/sum(stack_tmp$count)
  word_out<-sample(stack_tmp$w4,1,FALSE,prob=prob_vec)
  out_vec<-paste(out_vec, word_out)
  c1<-c2
  c2<-c3
  c3<-word_out
  }
}


print(out_vec)


