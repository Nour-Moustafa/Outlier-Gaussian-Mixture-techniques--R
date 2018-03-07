library(readr)
library(urltools)
library(caret)
library(caTools)
library(e1071)

data <- read_csv("E:/My-papers/Cyber-Simulation/Phishing-Simulation-Modeling/data1.csv")
View(data)
data <- data[,2:4]

#####################################
Clean_String <- function(string){
  # Lowercase
  temp <- tolower(string)
  #' Remove everything that is not a number or letter (may want to keep more 
  #' stuff in your actual analyses). 
  temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
  # Shrink down to just one white space
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  # Split it
  temp <- stringr::str_split(temp, " ")[[1]]
  # Get rid of trailing "" if necessary
  indexes <- which(temp == "")
  if(length(indexes) > 0){
    temp <- temp[-indexes]
  } 
  return(temp)
}
##################################
#' function to clean text
Clean_Text_Block <- function(text){
  if(length(text) <= 1){
    # Check to see if there is any text at all with another conditional
    if(length(text) == 0){
      cat("There was no text in this document! \n")
      to_return <- list(num_tokens = 0, unique_tokens = 0, text = "")
    }else{
      # If there is , and only only one line of text then tokenize it
      clean_text <- Clean_String(text)
      num_tok <- length(clean_text)
      num_uniq <- length(unique(clean_text))
      to_return <- list(num_tokens = num_tok, unique_tokens = num_uniq, text = clean_text)
    }
  }else{
    # Get rid of blank lines
    indexes <- which(text == "")
    if(length(indexes) > 0){
      text <- text[-indexes]
    }  
    # Loop through the lines in the text and use the append() function to 
    clean_text <- Clean_String(text[1])
    for(i in 2:length(text)){
      # add them to a vector 
      clean_text <- append(clean_text,Clean_String(text[i]))
    }
    # Calculate the number of tokens and unique tokens and return them in a 
    # named list object.
    num_tok <- length(clean_text)
    num_uniq <- length(unique(clean_text))
    to_return <- list(num_tokens = num_tok, unique_tokens = num_uniq, text = clean_text)
  }
  return(to_return)
}

#############################

### gettring words and their count 

d_clean= apply(data[,1],1,Clean_Text_Block)
len_words=sapply(d_clean, '[[', 1)
words= sapply(d_clean, '[[', 3)
max.length <- max(sapply(words, length))
## Add NA values to list elements
words <- lapply(words, function(v) { c(v, rep(NA, max.length-length(v)))})
## Rbind
words= do.call(rbind, words)
#words[is.na(words)] <- 0
clean_words_counts=  cbind(words[,2:ncol(words)]) 

rm(words)
rm(d_clean)
#rm(len_words)
rm(max.length)

######################################################
# getting scheme, domain, path

library(urltools)
dt <- matrix(0,nrow(data),1)
for (i in 1:nrow(data[,1]))
{
  dt[i,1]=as.character(data[i,1])
}

parsed_dt <- url_parse(dt)
#parsed_dt[is.na(parsed_dt)] <- 0
clean_parsed_dt <- cbind(parsed_dt[,1],parsed_dt[,2],parsed_dt[,4])

rm(dt)
rm(parsed_dt)
rm(i)

#############################################
# final string attributes

final_dt <- cbind (clean_parsed_dt, clean_words_counts)

rm(clean_parsed_dt)
rm(clean_words_counts)
#############################################
# lenght of data
final_dt[is.na(final_dt)] <- 0
len_final_dt=sapply(final_dt,nchar)
dim(len_final_dt) <- c(nrow(final_dt),ncol(final_dt))

final_dt_lb <- cbind (final_dt, data[,2])
#write.csv(final_dt_lb, file = "pre_data.csv")
### convert data to numeric
num_dt <- as.factor(final_dt)
levels(num_dt) <- 1:length(levels(num_dt))
num_dt <- as.numeric(num_dt)
dim(num_dt) <- c(nrow(final_dt),ncol(final_dt))
final_num_dt <- cbind(num_dt[,2:ncol(num_dt)],len_words)
final_num_dt[is.na(final_num_dt)] <- 0

# 14 numeric +noofwords+len of 14 attributes
all_num_len <- cbind(final_num_dt, len_final_dt)

rm(num_dt)
rm(len_words)


### data with label 

#original_data <- cbind (all_num_len, data[,2])


#dt_lb <- cbind (final_num_dt, data[,2],data[,3])
original_data <- cbind (final_num_dt, data[,2])


head(original_data)
