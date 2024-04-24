# load the packages----
library(stringr)
library(tidyverse)
library(readxl)
library(writexl)

# Leo Mandarin n----

Leo_Mandarin_n <- readLines("CLAN Output_even months.N.txt", encoding = "UTF-8")

# Find the beginning line of each instance
indices <- which(grepl("\\*\\*\\* File", Leo_Mandarin_n))

# Initialize empty data frame to store results
results <- data.frame(
  filename = character(),
  speaker_line = character(),
  timepoint = character(),
  line_number = character(),
  keyword = character(),
  word = character(),
  word_pinyin = character(),
  partofspeech = character(),
  morpheme_pinyin = character(),
  gloss = character(),
  phoneme_pinyin = character(),
  speaker = character(),
  word_simplified_character = character(),
  stringsAsFactors = FALSE
)

# Function to extract morpheme containing the target sound
# match the following pattern
# [n] means beginning with n
# [^\\d]* means any character that is NOT a digit
# [\\d] means any digit
extract_morpheme <- function(string) {
  str_extract(string, "[n][^\\d]*\\d")
}


# Function to extract the first or first two letters based on the second letter
extract_phon <- function(string) {
  ifelse(substr(string, 2, 2) == "h", substr(string, 1, 2), substr(string, 1, 1))
}

# Function to extract Chinese word
extract_chinese_word <- function(keyword_line, speaker_line, keyword) {
  # Split lines into words
  keyword_words <- strsplit(keyword_line, "\\s")[[1]] # do not change the s
  speaker_words <- strsplit(speaker_line, "\\s")[[1]] # do not change the s
  
  # Trim whitespace from keyword
  keyword <- trimws(keyword)
  
  # Find the position of the keyword in the keyword line
  keyword_position <- grep(keyword, keyword_words, fixed = TRUE)
  
  # Return the corresponding word in the speaker line
  if (length(keyword_position) > 0) {
    return(speaker_words[keyword_position])
  } else {
    return("NA")
  }
}

# Loop over indices
for (index in indices) {
  file_line <- Leo_Mandarin_n[index]
  speaker_line <- Leo_Mandarin_n[index+1]
  keyword_line <- Leo_Mandarin_n[index+2]
  # Split the line into components
  components <- strsplit(file_line, "\"")[[1]]
  filename <- components[2]
  timepoint <- str_extract(file_line, "\\d{6}")
  line_number <- str_extract(components[3], "\\d+")
  keywords <- strsplit(gsub(".*Keywords?: ", "", components[3]), ", ")
  
  for (keyword in keywords[[1]]) {
    
    partofspeech <- strsplit(keyword, "\\|")[[1]][1]
    
    word <- strsplit(keyword, "\\=")[[1]][1]
    
    morpheme_pinyin <- extract_morpheme(word)
    gloss <- strsplit(keyword, "\\=")[[1]][2]
    
    phoneme_pinyin <- extract_phon(morpheme_pinyin)
    speaker <- str_extract(Leo_Mandarin_n[index+1], "(?<=\\*)\\w+")
    
    word_pinyin <- gsub("\\+.*?\\|", "", word)
    word_pinyin <- gsub(".+?\\|", "", word_pinyin)
    word_pinyin <- gsub("(\\d)(\\D)", "\\1 \\2", word_pinyin)
    
    word_simplified_character <- extract_chinese_word(keyword_line, speaker_line, keyword)
    
    # Append results to data frame
    results <- rbind(results, data.frame(
      filename = filename,
      speaker_line = speaker_line,
      timepoint = timepoint,
      line_number = line_number,
      keyword = trimws(keyword),
      word = trimws(word),
      word_pinyin = word_pinyin,
      partofspeech = partofspeech,
      morpheme_pinyin = morpheme_pinyin,
      gloss = trimws(gloss),
      phoneme_pinyin = phoneme_pinyin,
      speaker = speaker,
      word_simplified_character = word_simplified_character,
      stringsAsFactors = FALSE
    ))
  }
}

# Print results
View(results)

# export the data and do the following:
# debug

Leo_Mandarin_n_dict <- results %>% 
  group_by(word, gloss, word_simplified_character) %>% 
  summarise(n()) %>% 
  arrange(desc(`n()`), nchar(word_simplified_character)) %>%
  slice(1)

# write_xlsx(Leo_Mandarin_n_dict, "Leo_Mandarin_n_dict.xlsx")

Leo_Mandarin_n_dict_clean <- read_xlsx("Leo_Mandarin_n_dict_clean.xlsx")

Leo_Mandarin_n <- results %>%
  left_join(Leo_Mandarin_n_dict_clean, by = c("word", "gloss"))%>%
  mutate(word_simplified_character = word_simplified_character.y) %>% 
  select(-word_simplified_character.x, -`n()`, -word_simplified_character.y)

Leo_Mandarin_n <- Leo_Mandarin_n %>% 
  mutate(morpheme_pinyin = gsub(".*\\|", "", morpheme_pinyin))


# Function to find the position of the morpheme in the word
find_morpheme_position <- function(jyutping, morpheme) {
  split_jyutping <- strsplit(jyutping, " ")[[1]]
  
  # If the length of split_jyutping is 1, return 1
  if(length(split_jyutping) == 1) {
    return(1)
  } else {
    # If the length of split_jyutping is more than 1, return the position of the morpheme
    return(match(morpheme, split_jyutping))
  }
}

# Apply the function to find the position
Leo_Mandarin_n$morpheme_position <- mapply(find_morpheme_position, Leo_Mandarin_n$word_pinyin, Leo_Mandarin_n$morpheme_pinyin)

# Function to extract the corresponding character based on the position
extract_character <- function(characters, position) {
  split_characters <- strsplit(characters, "")
  return(split_characters[[1]][position])
}

# Apply the function to extract the corresponding character

Leo_Mandarin_n$morpheme_simplified_character <- mapply(extract_character, Leo_Mandarin_n$word_simplified_character, Leo_Mandarin_n$morpheme_position)


# add the language factor
Leo_Mandarin_n$language <- "Mandarin"


# write_xlsx(Leo_Mandarin_n, "Leo_Mandarin_n_done.xlsx")
