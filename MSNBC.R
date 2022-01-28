################################################################################
############################## MSNBC YEAR 2006 #################################
################################################################################
# all files 
MSNBC_06 = readtext("C:/Users/Michelle/Desktop/ContentAnalysis/Data/ByChannel/MSNBC/MSNBC_2006*.docx")

## Create columns for date, year, and day for year datafile
MSNBC_06 <- MSNBC_06 %>%
  add_column(date = substr(MSNBC_06$text, 1, 10)) %>%
  add_column(year = substr(MSNBC_06$text, 7, 10)) %>%
  add_column(day = substr(MSNBC_06$text, 11, 14)) 
# View(MSNBC_06)

## Tokenize
MSNBC_06_tk <- MSNBC_06 %>% unnest_tokens("word", text)
# View(MSNBC_06_tk)

## Remove whitespaces
MSNBC_06_tk$word <- gsub("\\s+","", MSNBC_06_tk$word)

## Stemming
MSNBC_06_tk <- MSNBC_06_tk %>%
  mutate_at("word", funs(wordStem((.), language="en")))

## Remove stop words
data("stop_words")
MSNBC_06_tk <- MSNBC_06_tk %>%
  anti_join(stop_words)

## Remove numbers
MSNBC_06_tk <- MSNBC_06_tk[-grep("\\b\\d+\\b", MSNBC_06_tk$word),]


###############################################################################
## Checking against high competence dictionary
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highComp_match = str_detect(MSNBC_06_tk$word, pattern = paste(highComp_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(highComp_match, na.rm = TRUE)
# View(highComp_match)

MSNBC06_hiComp = sum(highComp_match, na.rm = TRUE)

# Count total number of words in 2006
ttlwrd_msnbc_2006 = length(MSNBC_06_tk$word)

# Calculate percentage of HC words
perHC_msnbc_2006 = (MSNBC06_hiComp/ttlwrd_msnbc_2006)*100

# str_which detects which words matched in tokenized column
# highComp_whr = str_which(MSNBC_06_tk$word, paste(highComp_dict, collapse = "|"))
# View(highComp_whr)

# str_extract pulls out which words in tokenized file matched
# highComp_ext = str_extract(MSNBC_06_tk$word, paste(highComp_dict, collapse = "|"))
# View(highComp_ext)

# Return items that matched
highComp_sub = str_subset(MSNBC_06_tk$word, pattern = paste(highComp_dict, collapse = "|"))
highComp_sub

# create a table to count freq of words and put that into another dataframe
hiComp.frq = table(highComp_sub)
hiComp.no = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq))
names(hiComp.no)[1] = "word"
names(hiComp.no)[2] = "n"
# View(hiComp.frq)

hiComp.no$word = factor(hiComp.no$word,
                        levels = hiComp.no$word[order(hiComp.no$word, 
                                                      decreasing = FALSE)])

# # plot frequency of words
# ggplot(hiComp.no, aes(x = reorder(word, -n), y = n, fill = word))+
#   geom_bar(stat="identity")+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   ylab("Frequency of High Competence Words")+
#   xlab("")+
#   ggtitle("MSNBC 2006")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   guides(fill=FALSE)


# transform n to percentage over total words
hiComp.per = table(highComp_sub)
hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq))
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"

hiComppercent = (hiComp.per$n)/ttlwrd_msnbc_2006
rm(hiComp.per)

hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq), hiComppercent)
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"
names(hiComp.per)[3] = "percentage"

# plot frequency of words in percentage
ggplot(hiComp.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2006 - High Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)


################################################################################
## Checking against low competence dictionary
# str_detect detects patterns in tokenized file from low comp dictionary
# paste takes the dictionary and combines them into one cell
lowComp_match = str_detect(MSNBC_06_tk$word, pattern = paste(lowComp_dict, collapse = "|"))


# Count number of TRUE (in other words, number of matches)
sum(lowComp_match, na.rm = TRUE)

MSNBC06_loComp = sum(lowComp_match, na.rm = TRUE)

# Calculate percentage of HC words
perLC_msnbc_2006 = (MSNBC06_loComp/ttlwrd_msnbc_2006)*100

# Return items that matched
lowComp_sub = str_subset(MSNBC_06_tk$word, pattern = paste(lowComp_dict, collapse = "|"))
lowComp_sub
# View(lowComp_sub)

# create dataframe to count frequency of words
loComp.frq = table(lowComp_sub)
loComp.no = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq))
names(loComp.no)[1] = "word"
names(loComp.no)[2] = "n"

loComp.no$word = factor(loComp.no$word,
                        levels = loComp.no$word[order(loComp.no$word, 
                                                      decreasing = FALSE)])

# plot frequency of words
# ggplot(loComp.no, aes(x= reorder(word, -n), y=n, fill=word))+
#   geom_bar(stat="identity")+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   ylab("Frequency of Low Competence Words")+
#   xlab("")+
#   ggtitle("MSNBC 2006")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   guides(fill=FALSE)

# transform n to percentage over total words
loComp.per = table(lowComp_sub)
loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq))
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"

loComppercent = (loComp.per$n)/ttlwrd_msnbc_2006
rm(loComp.per)

loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq), loComppercent)
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"
names(loComp.per)[3] = "percentage"

# plot frequency of words in percentage
ggplot(loComp.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2006 - Low Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highMoral_match = str_detect(MSNBC_06_tk$word, pattern = paste(highMoral_dict, collapse = "|"))
# View(highMoral_match)

# Count number of TRUE (in other words, number of matches)
sum(highMoral_match, na.rm = TRUE)

MSNBC06_hiMoral = sum(highMoral_match, na.rm = TRUE)

# Calculate percentage of HC words
perHM_msnbc_2006 = (MSNBC06_hiMoral/ttlwrd_msnbc_2006)*100

# Return items that matched
highMoral_sub = str_subset(MSNBC_06_tk$word, pattern = paste(highMoral_dict, collapse = "|"))
highMoral_sub
# View(highMoral_sub)

# create a table to count freq of words and put that into another dataframe
hiMoral.frq = table(highMoral_sub)
hiMoral.no = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq))
names(hiMoral.no)[1] = "word"
names(hiMoral.no)[2] = "n"

hiMoral.no$word = factor(hiMoral.no$word,
                         levels = hiMoral.no$word[order(hiMoral.no$word, 
                                                        decreasing = FALSE)])

# plot frequency of words
# ggplot(hiMoral.no, aes(x= reorder(word, -n), y=n, fill=word))+
#   geom_bar(stat="identity")+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   ylab("Frequency of High Morality Words")+
#   xlab("")+
#   ggtitle("MSNBC 2006")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   guides(fill=FALSE)

# transform n to percentage over total words
hiMoral.per = table(highMoral_sub)
hiMoral.per = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq))
names(hiMoral.per)[1] = "word"
names(hiMoral.per)[2] = "n"

hiMoralpercent = (hiMoral.per$n)/ttlwrd_msnbc_2006
rm(hiMoral.per)

hiMoral.per = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq), hiMoralpercent)
names(hiMoral.per)[1] = "word"
names(hiMoral.per)[2] = "n"
names(hiMoral.per)[3] = "percentage"

# plot frequency of words in percentage
ggplot(hiMoral.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2006 - High Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
lowMoral_match = str_detect(MSNBC_06_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowMoral_match, na.rm = TRUE)

MSNBC06_loMoral = sum(lowMoral_match, na.rm = TRUE)

# Calculate percentage of LM words
perLM_msnbc_2006 = (MSNBC06_loMoral/ttlwrd_msnbc_2006)*100

# Return items that matched
lowMoral_sub = str_subset(MSNBC_06_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))
lowMoral_sub

# create a table to count freq of words and put that into another dataframe
loMoral.frq = table(lowMoral_sub)
loMoral.no = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq))
names(loMoral.no)[1] = "word"
names(loMoral.no)[2] = "n"

loMoral.no$word = factor(loMoral.no$word,
                         levels = loMoral.no$word[order(loMoral.no$word, 
                                                        decreasing = FALSE)])

# plot frequency of words
ggplot(loMoral.no, aes(x= reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency of High Morality Words")+
  xlab("")+
  ggtitle("MSNBC 2006")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
loMoral.per = table(lowMoral_sub)
loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq))
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"

loMoralpercent = (loMoral.per$n)/ttlwrd_msnbc_2006
rm(loMoral.per)

loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq), loMoralpercent)
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"
names(loMoral.per)[3] = "percentage"

# plot frequency of words in percentage
ggplot(loMoral.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2006 - Low Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)


## Adding 2006 values to master datafile
MSNBC06_rw <- c(2006, "MSNBC", perHC_msnbc_2006, perLC_msnbc_2006, 
                    perHM_msnbc_2006, perLM_msnbc_2006)

master <- data.frame(2006, "MSNBC", perHC_msnbc_2006, perLC_msnbc_2006, 
                     perHM_msnbc_2006, perLM_msnbc_2006)
colnames(master) <- c("Year", "Source", "HiComp", "LowComp", "HiMoral", "LowMoral")

msnbc.df <- data.frame(2006, perHC_msnbc_2006, perLC_msnbc_2006, 
                       perHM_msnbc_2006, perLM_msnbc_2006)
colnames(msnbc.df) <- c("Year", "HiComp", "LowComp", "HiMoral", "LowMoral")


################################################################################
############################## MSNBC YEAR 2007 #################################
################################################################################
# all files
MSNBC_07 = readtext("C:/Users/Michelle/Desktop/test/MSNBC_2007_*.docx")

## Create columns for date, year, and day for year datafile
MSNBC_07 <- MSNBC_07 %>%
  add_column(date = substr(MSNBC_07$text, 1, 10)) %>%
  add_column(year = substr(MSNBC_07$text, 7, 10)) %>%
  add_column(day = substr(MSNBC_07$text, 11, 14)) 
# View(MSNBC_07)

## Tokenize
MSNBC_07_tk <- MSNBC_07 %>% unnest_tokens("word", text)
# View(MSNBC_07_tk)

## Remove whitespaces
MSNBC_07_tk$word <- gsub("\\s+","", MSNBC_07_tk$word)

## Stemming
MSNBC_07_tk <- MSNBC_07_tk %>%
  mutate_at("word", funs(wordStem((.), language="en")))

## Remove stop words
data("stop_words")
MSNBC_07_tk <- MSNBC_07_tk %>%
  anti_join(stop_words)

## Remove numbers
MSNBC_07_tk <- MSNBC_07_tk[-grep("\\b\\d+\\b", MSNBC_07_tk$word),]

###############################################################################
## Checking against high competence dictionary
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highComp_match = str_detect(MSNBC_07_tk$word, pattern = paste(highComp_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(highComp_match, na.rm = TRUE)
MSNBC07_hiComp = sum(highComp_match, na.rm = TRUE)

# View(highComp_match)

# Count total number of words in 2007
ttlwrd_msnbc_2007 = length(MSNBC_07_tk$word)

# Calculate percentage of HC words
perHC_msnbc_2007 = (MSNBC07_hiComp/ttlwrd_msnbc_2007)*100

# Return items that matched
highComp_sub = str_subset(MSNBC_07_tk$word, pattern = paste(highComp_dict, collapse = "|"))
highComp_sub

# create a table to count freq of words and put that into another dataframe
hiComp.frq = table(highComp_sub)
hiComp.no = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq))
names(hiComp.no)[1] = "word"
names(hiComp.no)[2] = "n"
# View(hiComp.frq)

hiComp.no$word = factor(hiComp.no$word,
                        levels = hiComp.no$word[order(hiComp.no$word, 
                                                      decreasing = FALSE)])

# plot frequency of words
# ggplot(hiComp.no, aes(x= reorder(word, -n), y=n, fill=word))+
#   geom_bar(stat="identity")+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   ylab("Frequency of High Competence Words")+
#   xlab("")+
#   ggtitle("MSNBC 2007")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   guides(fill=FALSE)

# transform n to percentage over total words
hiComp.per = table(highComp_sub)
hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq))
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"

hiComppercent = (hiComp.per$n)/ttlwrd_msnbc_2007
rm(hiComp.per)

hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq), hiComppercent)
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"
names(hiComp.per)[3] = "percentage"

# plot frequency of words in percentage
ggplot(hiComp.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2007 - High Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

################################################################################
## Checking against low competence dictionary
# str_detect detects patterns in tokenized file from low comp dictionary
# paste takes the dictionary and combines them into one cell
lowComp_match = str_detect(MSNBC_07_tk$word, pattern = paste(lowComp_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowComp_match, na.rm = TRUE)
MSNBC07_loComp = sum(lowComp_match, na.rm = TRUE)

# Calculate percentage of LC words
perLC_msnbc_2007 = (MSNBC07_loComp/ttlwrd_msnbc_2007)*100

# Return items that matched
lowComp_sub = str_subset(MSNBC_07_tk$word, pattern = paste(lowComp_dict, collapse = "|"))
lowComp_sub
# View(lowComp_sub)

# create dataframe to count frequency of words
loComp.frq = table(lowComp_sub)
loComp.no = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq))
names(loComp.no)[1] = "word"
names(loComp.no)[2] = "n"

loComp.no$word = factor(loComp.no$word,
                        levels = loComp.no$word[order(loComp.no$word, 
                                                      decreasing = FALSE)])

# plot frequency of words
# ggplot(loComp.no, aes(x= reorder(word, -n), y=n, fill=word))+
#   geom_bar(stat="identity")+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   ylab("Frequency of Low Competence Words")+
#   xlab("")+
#   ggtitle("MSNBC 2007")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   guides(fill=FALSE)

# transform n to percentage over total words
loComp.per = table(lowComp_sub)
loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq))
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"

loComppercent = (loComp.per$n)/ttlwrd_msnbc_2007
rm(loComp.per)

loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq), loComppercent)
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"
names(loComp.per)[3] = "percentage"

# plot frequency of words in percentage
ggplot(loComp.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2007 - Low Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highMoral_match = str_detect(MSNBC_07_tk$word, 
                             pattern = paste(highMoral_dict, collapse = "|"))
# View(highMoral_match)

# Count number of TRUE (in other words, number of matches)
sum(highMoral_match, na.rm = TRUE)
MSNBC07_hiMoral = sum(highMoral_match, na.rm = TRUE)

# Calculate percentage of LC words
perHM_msnbc_2007 = (MSNBC07_hiMoral/ttlwrd_msnbc_2007)*100

# Return items that matched
highMoral_sub = str_subset(MSNBC_07_tk$word, pattern = paste(highMoral_dict, collapse = "|"))
highMoral_sub
# View(highMoral_sub)

# create a table to count freq of words and put that into another dataframe
hiMoral.frq = table(highMoral_sub)
hiMoral.no = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq))
names(hiMoral.no)[1] = "word"
names(hiMoral.no)[2] = "n"

hiMoral.no$word = factor(hiMoral.no$word,
                         levels = hiMoral.no$word[order(hiMoral.no$word, 
                                                        decreasing = TRUE)])

# plot frequency of words
# ggplot(hiMoral.no, aes(x= reorder(word, -n), y=n, fill=word))+
#   geom_bar(stat="identity")+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   ylab("Frequency of High Morality Words")+
#   xlab("")+
#   ggtitle("MSNBC 2007")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   guides(fill=FALSE)

# transform n to percentage over total words
hiMoral.per = table(highMoral_sub)
hiMoral.per = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq))
names(hiMoral.per)[1] = "word"
names(hiMoral.per)[2] = "n"

hiMoralpercent = (hiMoral.per$n)/ttlwrd_msnbc_2007
rm(hiMoral.per)

hiMoral.per = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq), hiMoralpercent)
names(hiMoral.per)[1] = "word"
names(hiMoral.per)[2] = "n"
names(hiMoral.per)[3] = "percentage"

# plot frequency of words in percentage
ggplot(hiMoral.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2007 - High Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
lowMoral_match = str_detect(MSNBC_07_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowMoral_match, na.rm = TRUE)
MSNBC07_loMoral = sum(lowMoral_match, na.rm = TRUE)

# Calculate percentage of LC words
perLM_msnbc_2007 = (MSNBC07_loMoral/ttlwrd_msnbc_2007)*100

# Return items that matched
lowMoral_sub = str_subset(MSNBC_07_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))
lowMoral_sub

# create a table to count freq of words and put that into another dataframe
loMoral.frq = table(lowMoral_sub)
loMoral.no = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq))
names(loMoral.no)[1] = "word"
names(loMoral.no)[2] = "n"

loMoral.no$word = factor(loMoral.no$word,
                         levels = loMoral.no$word[order(loMoral.no$word, 
                                                        decreasing = TRUE)])

# plot frequency of words
ggplot(loMoral.no, aes(x= reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency of Low Morality Words")+
  xlab("")+
  ggtitle("MSNBC 2007")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
loMoral.per = table(lowMoral_sub)
loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq))
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"

loMoralpercent = (loMoral.per$n)/ttlwrd_msnbc_2007
rm(loMoral.per)

loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq), loMoralpercent)
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"
names(loMoral.per)[3] = "percentage"

# plot frequency of words in percentage
ggplot(loMoral.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2007 - Low Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

## Adding 2007 values to master datafile
MSNBC07_rw <- c(2007, "MSNBC", perHC_msnbc_2007, perLC_msnbc_2007, 
                perHM_msnbc_2007, perLM_msnbc_2007)

master <- rbind(master, MSNBC07_rw)

MSNBC07 <- c(2007, perHC_msnbc_2007, perLC_msnbc_2007, 
             perHM_msnbc_2007, perLM_msnbc_2007)

msnbc.df <- rbind(msnbc.df, MSNBC07)

################################################################################
############################## MSNBC YEAR 2008 #################################
################################################################################
# all files
MSNBC_08 = readtext("C:/Users/Michelle/Desktop/test/MSNBC_2008_*.docx")

## Create columns for date, year, and day for year datafile
MSNBC_08 <- MSNBC_08 %>%
  add_column(date = substr(MSNBC_08$text, 1, 10)) %>%
  add_column(year = substr(MSNBC_08$text, 7, 10)) %>%
  add_column(day = substr(MSNBC_08$text, 11, 14)) 
# View(MSNBC_08)

## Tokenize
MSNBC_08_tk <- MSNBC_08 %>% unnest_tokens("word", text)
# View(MSNBC_08_tk)

## Remove whitespaces
MSNBC_08_tk$word <- gsub("\\s+","", MSNBC_08_tk$word)

## Stemming
MSNBC_08_tk <- MSNBC_08_tk %>%
  mutate_at("word", funs(wordStem((.), language="en")))

## Remove stop words
data("stop_words")
MSNBC_08_tk <- MSNBC_08_tk %>%
  anti_join(stop_words)

## Remove numbers
MSNBC_08_tk <- MSNBC_08_tk[-grep("\\b\\d+\\b", MSNBC_08_tk$word),]

###############################################################################
## Checking against high competence dictionary
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highComp_match = str_detect(MSNBC_08_tk$word, 
                            pattern = paste(highComp_dict, 
                                            collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(highComp_match, na.rm = TRUE)
MSNBC08_hiComp = sum(highComp_match, na.rm = TRUE)

# Count total number of words in 2008
ttlwrd_msnbc_2008 = length(MSNBC_08_tk$word)

# Calculate percentage of HC words
perHC_msnbc_2008 = (MSNBC08_hiComp/ttlwrd_msnbc_2008)*100

# View(highComp_match)

# str_which detects which words matched in tokenized column
# highComp_whr = str_which(MSNBC_06_tk$word, paste(highComp_dict, collapse = "|"))
# View(highComp_whr)

# str_extract pulls out which words in tokenized file matched
# highComp_ext = str_extract(MSNBC_06_tk$word, paste(highComp_dict, collapse = "|"))
# View(highComp_ext)

# Return items that matched
highComp_sub = str_subset(MSNBC_08_tk$word, 
                          pattern = paste(highComp_dict, collapse = "|"))
highComp_sub

# create a table to count freq of words and put that into another dataframe
hiComp.frq = table(highComp_sub)
hiComp.no = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq))
names(hiComp.no)[1] = "word"
names(hiComp.no)[2] = "n"
# View(hiComp.frq)

hiComp.no$word = factor(hiComp.no$word,
                        levels = hiComp.no$word[order(hiComp.no$word, 
                                                      decreasing = FALSE)])

# plot frequency of words
# ggplot(hiComp.no, aes(x= reorder(word, -n), y=n, fill=word))+
#   geom_bar(stat="identity")+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   ylab("Frequency of High Competence Words")+
#   xlab("")+
#   ggtitle("MSNBC 2008")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   guides(fill=FALSE)

# transform n to percentage over total words
hiComp.per = table(highComp_sub)
hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq))
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"

hiComppercent = (hiComp.per$n)/ttlwrd_msnbc_2008
rm(hiComp.per)

hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq), hiComppercent)
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"
names(hiComp.per)[3] = "percentage"

# plot frequency of words in percentage
fc_msnbc_hc <- ggplot(hiComp.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2008 - High Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

################################################################################
## Checking against low competence dictionary
# str_detect detects patterns in tokenized file from low comp dictionary
# paste takes the dictionary and combines them into one cell
lowComp_match = str_detect(MSNBC_08_tk$word, pattern = paste(lowComp_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowComp_match, na.rm = TRUE)
MSNBC08_loComp = sum(lowComp_match, na.rm = TRUE)

# Calculate percentage of LC words
perLC_msnbc_2008 = (MSNBC08_loComp/ttlwrd_msnbc_2008)*100

# Return items that matched
lowComp_sub = str_subset(MSNBC_08_tk$word, 
                         pattern = paste(lowComp_dict, collapse = "|"))
lowComp_sub
# View(lowComp_sub)

# create dataframe to count frequency of words
loComp.frq = table(lowComp_sub)
loComp.no = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq))
names(loComp.no)[1] = "word"
names(loComp.no)[2] = "n"

loComp.no$word = factor(loComp.no$word,
                        levels = loComp.no$word[order(loComp.no$word, 
                                                      decreasing = FALSE)])

# Calculate percentage of LC words
perLC_msnbc_2008 = (MSNBC08_loComp/ttlwrd_msnbc_2008)*100

# plot frequency of words
# ggplot(loComp.no, aes(x= reorder(word, -n), y=n, fill=word))+
#   geom_bar(stat="identity")+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   ylab("Frequency of Low Competence Words")+
#   xlab("")+
#   ggtitle("MSNBC 2008")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   guides(fill=FALSE)

# transform n to percentage over total words
loComp.per = table(lowComp_sub)
loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq))
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"

loComppercent = (loComp.per$n)/ttlwrd_msnbc_2008
rm(loComp.per)

loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq), loComppercent)
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"
names(loComp.per)[3] = "percentage"

# plot frequency of words in percentage
fc_msnbc_lc <- ggplot(loComp.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2008 - Low Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highMoral_match = str_detect(MSNBC_08_tk$word, 
                             pattern = paste(highMoral_dict, 
                                             collapse = "|"))
# View(highMoral_match)

# Count number of TRUE (in other words, number of matches)
sum(highMoral_match, na.rm = TRUE)
MSNBC08_hiMoral = sum(highMoral_match, na.rm = TRUE)

# Calculate percentage of HM word
perHM_msnbc_2008 = (MSNBC08_hiMoral/ttlwrd_msnbc_2008)*100

# Return items that matched
highMoral_sub = str_subset(MSNBC_08_tk$word, pattern = paste(highMoral_dict, collapse = "|"))
highMoral_sub
# View(highMoral_sub)

# create a table to count freq of words and put that into another dataframe
hiMoral.frq = table(highMoral_sub)
hiMoral.no = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq))
names(hiMoral.no)[1] = "word"
names(hiMoral.no)[2] = "n"

hiMoral.no$word = factor(hiMoral.no$word,
                         levels = hiMoral.no$word[order(hiMoral.no$word, 
                                                        decreasing = FALSE)])

# plot frequency of words
# ggplot(hiMoral.no, aes(x= reorder(word, -n), y=n, fill=word))+
#   geom_bar(stat="identity")+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   ylab("Frequency of High Morality Words")+
#   xlab("")+
#   ggtitle("MSNBC 2008")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   guides(fill=FALSE)

# transform n to percentage over total words
hiMoral.per = table(highMoral_sub)
hiMoral.per = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq))
names(hiMoral.per)[1] = "word"
names(hiMoral.per)[2] = "n"

hiMoralpercent = (hiMoral.per$n)/ttlwrd_msnbc_2008
rm(hiMoral.per)

hiMoral.per = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq), hiMoralpercent)
names(hiMoral.per)[1] = "word"
names(hiMoral.per)[2] = "n"
names(hiMoral.per)[3] = "percentage"

# plot frequency of words in percentage
fc_msnbc_hm <- ggplot(hiMoral.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2008 - High Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
lowMoral_match = str_detect(MSNBC_08_tk$word, 
                            pattern = paste(lowMoral_dict, 
                                            collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowMoral_match, na.rm = TRUE)
MSNBC08_loMoral = sum(lowMoral_match, na.rm = TRUE)

# Calculate percentage of LM words
perLM_msnbc_2008 = (MSNBC08_loMoral/ttlwrd_msnbc_2008)*100

# str_which detects which words matched in tokenized column
# lowMoral_whr = str_which(MSNBC_06_tk$word, paste(lowMoral_dict, collapse = "|"))
# View(lowMoral_whr)

# str_extract pulls out which words in tokenized file matched
# lowMoral_ext = str_extract(MSNBC_06_tk$word, paste(lowMoral_dict, collapse = "|"))
# View(lowMoral_ext)

# Calculate percentage of HM words
perHM_msnbc_2008 = (MSNBC08_hiMoral/ttlwrd_msnbc_2008)*100

# Return items that matched
lowMoral_sub = str_subset(MSNBC_08_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))
lowMoral_sub

# create a table to count freq of words and put that into another dataframe
loMoral.frq = table(lowMoral_sub)
loMoral.no = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq))
names(loMoral.no)[1] = "word"
names(loMoral.no)[2] = "n"

loMoral.no$word = factor(loMoral.no$word,
                         levels = loMoral.no$word[order(loMoral.no$word, 
                                                        decreasing = FALSE)])

# plot frequency of words
# ggplot(loMoral.no, aes(x= reorder(word, -n), y=n, fill=word))+
#   geom_bar(stat="identity")+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   ylab("Frequency of Low Morality Words")+
#   xlab("")+
#   ggtitle("MSNBC 2008")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   guides(fill=FALSE)

# transform n to percentage over total words
loMoral.per = table(lowMoral_sub)
loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq))
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"

loMoralpercent = (loMoral.per$n)/ttlwrd_msnbc_2008
rm(loMoral.per)

loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq), loMoralpercent)
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"
names(loMoral.per)[3] = "percentage"

# plot frequency of words in percentage
fc_msnbc_lm <- ggplot(loMoral.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2008 - Low Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

## Adding 2008 values to master datafile
MSNBC08_rw <- c(2008, "MSNBC", perHC_msnbc_2008, perLC_msnbc_2008, 
                perHM_msnbc_2008, perLM_msnbc_2008)

master <- rbind(master, MSNBC08_rw)

MSNBC08 <- c(2008, perHC_msnbc_2008, perLC_msnbc_2008, 
             perHM_msnbc_2008, perLM_msnbc_2008)

msnbc.df <- rbind(msnbc.df, MSNBC08)

################################################################################
############################## MSNBC YEAR 2009 #################################
################################################################################
# all files
MSNBC_09 = readtext("C:/Users/Michelle/Desktop/test/MSNBC_2009_*.docx")

## Create columns for date, year, and day for year datafile
MSNBC_09 <- MSNBC_09 %>%
  add_column(date = substr(MSNBC_09$text, 1, 10)) %>%
  add_column(year = substr(MSNBC_09$text, 7, 10)) %>%
  add_column(day = substr(MSNBC_09$text, 11, 14)) 
# View(MSNBC_09)

## Tokenize
MSNBC_09_tk <- MSNBC_09 %>% unnest_tokens("word", text)
# View(MSNBC_08_tk)

## Remove whitespaces
MSNBC_09_tk$word <- gsub("\\s+","", MSNBC_09_tk$word)

## Stemming
MSNBC_09_tk <- MSNBC_09_tk %>%
  mutate_at("word", funs(wordStem((.), language="en")))

## Remove stop words
data("stop_words")
MSNBC_09_tk <- MSNBC_09_tk %>%
  anti_join(stop_words)

## Remove numbers
MSNBC_09_tk <- MSNBC_09_tk[-grep("\\b\\d+\\b", MSNBC_09_tk$word),]

###############################################################################
## Checking against high competence dictionary
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highComp_match = str_detect(MSNBC_09_tk$word, 
                            pattern = paste(highComp_dict, 
                                            collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(highComp_match, na.rm = TRUE)
MSNBC09_hiComp = sum(highComp_match, na.rm = TRUE)

# View(highComp_match)

# Count total number of words in 2009
ttlwrd_msnbc_2009 = length(MSNBC_09_tk$word)

# Calculate percentage of HC words
perHC_msnbc_2009 = (MSNBC09_hiComp/ttlwrd_msnbc_2009)*100

# str_which detects which words matched in tokenized column
# highComp_whr = str_which(MSNBC_06_tk$word, paste(highComp_dict, collapse = "|"))
# View(highComp_whr)

# str_extract pulls out which words in tokenized file matched
# highComp_ext = str_extract(MSNBC_06_tk$word, paste(highComp_dict, collapse = "|"))
# View(highComp_ext)

# Return items that matched
highComp_sub = str_subset(MSNBC_09_tk$word, pattern = paste(highComp_dict, collapse = "|"))
highComp_sub

# create a table to count freq of words and put that into another dataframe
hiComp.frq = table(highComp_sub)
hiComp.no = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq))
names(hiComp.no)[1] = "word"
names(hiComp.no)[2] = "n"
# View(hiComp.frq)

hiComp.no$word = factor(hiComp.no$word,
                        levels = hiComp.no$word[order(hiComp.no$word, 
                                                      decreasing = FALSE)])

# plot frequency of words
# ggplot(hiComp.no, aes(x= reorder(word, -n), y=n, fill=word))+
#   geom_bar(stat="identity")+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   ylab("Frequency of High Competence Words")+
#   xlab("")+
#   ggtitle("MSNBC 2009")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   guides(fill=FALSE)

# transform n to percentage over total words
hiComp.per = table(highComp_sub)
hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq))
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"

hiComppercent = (hiComp.per$n)/ttlwrd_msnbc_2009
rm(hiComp.per)

hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq), hiComppercent)
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"
names(hiComp.per)[3] = "percentage"

# plot frequency of words in percentage
ggplot(hiComp.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2009 - High Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)


################################################################################
## Checking against low competence dictionary
# str_detect detects patterns in tokenized file from low comp dictionary
# paste takes the dictionary and combines them into one cell
lowComp_match = str_detect(MSNBC_09_tk$word, pattern = paste(lowComp_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowComp_match, na.rm = TRUE)
MSNBC09_loComp = sum(lowComp_match, na.rm = TRUE)


# Calculate percentage of HC words
perLC_msnbc_2009 = (MSNBC09_loComp/ttlwrd_msnbc_2009)*100

# Return items that matched
lowComp_sub = str_subset(MSNBC_09_tk$word, pattern = paste(lowComp_dict, collapse = "|"))
lowComp_sub
# View(lowComp_sub)

# create dataframe to count frequency of words
loComp.frq = table(lowComp_sub)
loComp.no = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq))
names(loComp.no)[1] = "word"
names(loComp.no)[2] = "n"

loComp.no$word = factor(loComp.no$word,
                        levels = loComp.no$word[order(loComp.no$word, 
                                                      decreasing = FALSE)])

# plot frequency of words
# ggplot(loComp.no, aes(x= reorder(word, -n), y=n, fill=word))+
#   geom_bar(stat="identity")+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   ylab("Frequency of Low Competence Words")+
#   xlab("")+
#   ggtitle("MSNBC 2009")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   guides(fill=FALSE)

# transform n to percentage over total words
loComp.per = table(lowComp_sub)
loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq))
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"

loComppercent = (loComp.per$n)/ttlwrd_msnbc_2009
rm(loComp.per)

loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq), loComppercent)
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"
names(loComp.per)[3] = "percentage"

# plot frequency of words in percentage
ggplot(loComp.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2009 - Low Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highMoral_match = str_detect(MSNBC_09_tk$word, pattern = paste(highMoral_dict, collapse = "|"))
# View(highMoral_match)

# Count number of TRUE (in other words, number of matches)
sum(highMoral_match, na.rm = TRUE)
MSNBC09_hiMoral = sum(highMoral_match, na.rm = TRUE)

# Calculate percentage of HC words
perHM_msnbc_2009 = (MSNBC09_hiMoral/ttlwrd_msnbc_2009)*100

# Return items that matched
highMoral_sub = str_subset(MSNBC_09_tk$word, pattern = paste(highMoral_dict, collapse = "|"))
highMoral_sub
# View(highMoral_sub)

# create a table to count freq of words and put that into another dataframe
hiMoral.frq = table(highMoral_sub)
hiMoral.no = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq))
names(hiMoral.no)[1] = "word"
names(hiMoral.no)[2] = "n"

hiMoral.no$word = factor(hiMoral.no$word,
                         levels = hiMoral.no$word[order(hiMoral.no$word, 
                                                        decreasing = FALSE)])

# plot frequency of words
# ggplot(hiMoral.no, aes(x= reorder(word, -n), y=n, fill=word))+
#   geom_bar(stat="identity")+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   ylab("Frequency of High Morality Words")+
#   xlab("")+
#   ggtitle("MSNBC 2009")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   guides(fill=FALSE)

# transform n to percentage over total words
hiMoral.per = table(highMoral_sub)
hiMoral.per = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq))
names(hiMoral.per)[1] = "word"
names(hiMoral.per)[2] = "n"

hiMoralpercent = (hiMoral.per$n)/ttlwrd_msnbc_2009
rm(hiMoral.per)

hiMoral.per = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq), hiMoralpercent)
names(hiMoral.per)[1] = "word"
names(hiMoral.per)[2] = "n"
names(hiMoral.per)[3] = "percentage"

# plot frequency of words in percentage
ggplot(hiMoral.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2009 - High Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
lowMoral_match = str_detect(MSNBC_09_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowMoral_match, na.rm = TRUE)
MSNBC09_loMoral = sum(lowMoral_match, na.rm = TRUE)

# str_which detects which words matched in tokenized column
# lowMoral_whr = str_which(MSNBC_06_tk$word, paste(lowMoral_dict, collapse = "|"))
# View(lowMoral_whr)

# str_extract pulls out which words in tokenized file matched
# lowMoral_ext = str_extract(MSNBC_06_tk$word, paste(lowMoral_dict, collapse = "|"))
# View(lowMoral_ext)

# Calculate percentage of LM words
perLM_msnbc_2009 = (MSNBC09_loMoral/ttlwrd_msnbc_2009)*100

# Return items that matched
lowMoral_sub = str_subset(MSNBC_09_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))
lowMoral_sub

# create a table to count freq of words and put that into another dataframe
loMoral.frq = table(lowMoral_sub)
loMoral.no = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq))
names(loMoral.no)[1] = "word"
names(loMoral.no)[2] = "n"

loMoral.no$word = factor(loMoral.no$word,
                         levels = loMoral.no$word[order(loMoral.no$word, 
                                                        decreasing = FALSE)])

# # plot frequency of words
# ggplot(loMoral.no, aes(x= reorder(word, -n), y=n, fill=word))+
#   geom_bar(stat="identity")+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   ylab("Frequency of Low Morality Words")+
#   xlab("")+
#   ggtitle("MSNBC 2009")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   guides(fill=FALSE)

# transform n to percentage over total words
loMoral.per = table(lowMoral_sub)
loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq))
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"

loMoralpercent = (loMoral.per$n)/ttlwrd_msnbc_2009
rm(loMoral.per)

loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq), loMoralpercent)
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"
names(loMoral.per)[3] = "percentage"

# plot frequency of words in percentage
ggplot(loMoral.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2009 - Low Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

## Adding 2009 values to master datafile
MSNBC09_rw <- c(2009, "MSNBC", perHC_msnbc_2009, perLC_msnbc_2009, 
                perHM_msnbc_2009, perLM_msnbc_2009)

master <- rbind(master, MSNBC09_rw)

MSNBC09 <- c(2009, perHC_msnbc_2009, perLC_msnbc_2009, 
             perHM_msnbc_2009, perLM_msnbc_2009)

msnbc.df <- rbind(msnbc.df, MSNBC09)


################################################################################
############################## MSNBC YEAR 2010 #################################
################################################################################
# all files
MSNBC_10 = readtext("C:/Users/Michelle/Desktop/test/MSNBC_2010_*.docx")

## Create columns for date, year, and day for year datafile
MSNBC_10 <- MSNBC_10 %>%
  add_column(date = substr(MSNBC_10$text, 1, 10)) %>%
  add_column(year = substr(MSNBC_10$text, 7, 10)) %>%
  add_column(day = substr(MSNBC_10$text, 11, 14)) 
# View(MSNBC_10)

## Tokenize
MSNBC_10_tk <- MSNBC_10 %>% unnest_tokens("word", text)
# View(MSNBC_08_tk)

## Remove whitespaces
MSNBC_10_tk$word <- gsub("\\s+","", MSNBC_10_tk$word)

## Stemming
MSNBC_10_tk <- MSNBC_10_tk %>%
  mutate_at("word", funs(wordStem((.), language="en")))

## Remove stop words
data("stop_words")
MSNBC_10_tk <- MSNBC_10_tk %>%
  anti_join(stop_words)

## Remove numbers
MSNBC_10_tk <- MSNBC_10_tk[-grep("\\b\\d+\\b", MSNBC_10_tk$word),]

###############################################################################
## Checking against high competence dictionary
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highComp_match = str_detect(MSNBC_10_tk$word, pattern = paste(highComp_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(highComp_match, na.rm = TRUE)
MSNBC10_hiComp = sum(highComp_match, na.rm = TRUE)

# Count total number of words in 2010
ttlwrd_msnbc_2010 = length(MSNBC_10_tk$word)

# Calculate percentage of HC words
perHC_msnbc_2010 = (MSNBC10_hiComp/ttlwrd_msnbc_2010)*100

# Return items that matched
highComp_sub = str_subset(MSNBC_10_tk$word, pattern = paste(highComp_dict, collapse = "|"))
highComp_sub

# create a table to count freq of words and put that into another dataframe
hiComp.frq = table(highComp_sub)
hiComp.no = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq))
names(hiComp.no)[1] = "word"
names(hiComp.no)[2] = "n"
# View(hiComp.frq)

hiComp.no$word = factor(hiComp.no$word,
                        levels = hiComp.no$word[order(hiComp.no$word, 
                                                      decreasing = FALSE)])

# plot frequency of words
# ggplot(hiComp.no, aes(x= reorder(word, -n), y=n, fill=word))+
#   geom_bar(stat="identity")+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   ylab("Frequency of High Competence Words")+
#   xlab("")+
#   ggtitle("MSNBC 2010")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   guides(fill=FALSE)

# transform n to percentage over total words
hiComp.per = table(highComp_sub)
hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq))
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"

hiComppercent = (hiComp.per$n)/ttlwrd_msnbc_2010
rm(hiComp.per)

hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq), hiComppercent)
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"
names(hiComp.per)[3] = "percentage"

# plot frequency of words in percentage
ggplot(hiComp.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2010 - High Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

################################################################################
## Checking against low competence dictionary
# str_detect detects patterns in tokenized file from low comp dictionary
# paste takes the dictionary and combines them into one cell
lowComp_match = str_detect(MSNBC_10_tk$word, 
                           pattern = paste(lowComp_dict, 
                                           collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowComp_match, na.rm = TRUE)
MSNBC10_loComp = sum(lowComp_match, na.rm = TRUE)

# Calculate percentage of LC words
perLC_msnbc_2010 = (MSNBC10_loComp/ttlwrd_msnbc_2010)*100

# Return items that matched
lowComp_sub = str_subset(MSNBC_10_tk$word, pattern = paste(lowComp_dict, collapse = "|"))
lowComp_sub
# View(lowComp_sub)

# create dataframe to count frequency of words
loComp.frq = table(lowComp_sub)
loComp.no = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq))
names(loComp.no)[1] = "word"
names(loComp.no)[2] = "n"

loComp.no$word = factor(loComp.no$word,
                        levels = loComp.no$word[order(loComp.no$word, 
                                                      decreasing = FALSE)])

# plot frequency of words
# ggplot(loComp.no, aes(x= reorder(word, -n), y=n, fill=word))+
#   geom_bar(stat="identity")+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   ylab("Frequency of Low Competence Words")+
#   xlab("")+
#   ggtitle("MSNBC 2010")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   guides(fill=FALSE)

# transform n to percentage over total words
loComp.per = table(lowComp_sub)
loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq))
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"

loComppercent = (loComp.per$n)/ttlwrd_msnbc_2010
rm(loComp.per)

loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq), loComppercent)
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"
names(loComp.per)[3] = "percentage"

# plot frequency of words in percentage
ggplot(loComp.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2010 - Low Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highMoral_match = str_detect(MSNBC_10_tk$word, pattern = paste(highMoral_dict, collapse = "|"))
# View(highMoral_match)

# Count number of TRUE (in other words, number of matches)
sum(highMoral_match, na.rm = TRUE)
MSNBC10_hiMoral = sum(highMoral_match, na.rm = TRUE)

# Calculate percentage of HC words
perHM_msnbc_2010 = (MSNBC10_hiMoral/ttlwrd_msnbc_2010)*100

# Return items that matched
highMoral_sub = str_subset(MSNBC_10_tk$word, pattern = paste(highMoral_dict, collapse = "|"))
highMoral_sub
# View(highMoral_sub)

# create a table to count freq of words and put that into another dataframe
hiMoral.frq = table(highMoral_sub)
hiMoral.no = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq))
names(hiMoral.no)[1] = "word"
names(hiMoral.no)[2] = "n"

hiMoral.no$word = factor(hiMoral.no$word,
                         levels = hiMoral.no$word[order(hiMoral.no$word, 
                                                        decreasing = FALSE)])



# plot frequency of words
ggplot(hiMoral.no, aes(x= reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency of High Morality Words")+
  xlab("")+
  ggtitle("MSNBC 2010")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
hiMoral.per = table(highMoral_sub)
hiMoral.per = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq))
names(hiMoral.per)[1] = "word"
names(hiMoral.per)[2] = "n"

hiMoralpercent = (hiMoral.per$n)/ttlwrd_msnbc_2010
rm(hiMoral.per)

hiMoral.per = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq), hiMoralpercent)
names(hiMoral.per)[1] = "word"
names(hiMoral.per)[2] = "n"
names(hiMoral.per)[3] = "percentage"

# plot frequency of words in percentage
ggplot(hiMoral.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2010 - High Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
lowMoral_match = str_detect(MSNBC_10_tk$word, 
                            pattern = paste(lowMoral_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowMoral_match, na.rm = TRUE)
MSNBC10_loMoral = sum(lowMoral_match, na.rm = TRUE)

# str_which detects which words matched in tokenized column
# lowMoral_whr = str_which(MSNBC_06_tk$word, paste(lowMoral_dict, collapse = "|"))
# View(lowMoral_whr)

# str_extract pulls out which words in tokenized file matched
# lowMoral_ext = str_extract(MSNBC_06_tk$word, paste(lowMoral_dict, collapse = "|"))
# View(lowMoral_ext)

# Calculate percentage of HC words
perLM_msnbc_2010 = (MSNBC10_loMoral/ttlwrd_msnbc_2010)*100

# Return items that matched
lowMoral_sub = str_subset(MSNBC_10_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))
lowMoral_sub

# create a table to count freq of words and put that into another dataframe
loMoral.frq = table(lowMoral_sub)
loMoral.no = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq))
names(loMoral.no)[1] = "word"
names(loMoral.no)[2] = "n"

loMoral.no$word = factor(loMoral.no$word,
                         levels = loMoral.no$word[order(loMoral.no$word, 
                                                        decreasing = FALSE)])

# plot frequency of words
# ggplot(loMoral.no, aes(x= reorder(word, -n), y=n, fill=word))+
#   geom_bar(stat="identity")+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   ylab("Frequency of Low Morality Words")+
#   xlab("")+
#   ggtitle("MSNBC 2010")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   guides(fill=FALSE)

# transform n to percentage over total words
loMoral.per = table(lowMoral_sub)
loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq))
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"

loMoralpercent = (loMoral.per$n)/ttlwrd_msnbc_2010
rm(loMoral.per)

loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq), loMoralpercent)
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"
names(loMoral.per)[3] = "percentage"

# plot frequency of words in percentage
ggplot(loMoral.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2010 - Low Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

## Adding 2010 values to master datafile
MSNBC10_rw <- c(2010, "MSNBC", perHC_msnbc_2010, perLC_msnbc_2010, 
                perHM_msnbc_2010, perLM_msnbc_2010)

master <- rbind(master, MSNBC10_rw)

MSNBC10 <- c(2010, perHC_msnbc_2010, perLC_msnbc_2010, 
             perHM_msnbc_2010, perLM_msnbc_2010)

msnbc.df <- rbind(msnbc.df, MSNBC10)

################################################################################
############################## MSNBC YEAR 2011 #################################
################################################################################
# all files
MSNBC_11 = readtext("C:/Users/Michelle/Desktop/test/MSNBC_2011_*.docx")

## Create columns for date, year, and day for year datafile
MSNBC_11 <- MSNBC_11 %>%
  add_column(date = substr(MSNBC_11$text, 1, 10)) %>%
  add_column(year = substr(MSNBC_11$text, 7, 10)) %>%
  add_column(day = substr(MSNBC_11$text, 11, 14)) 
# View(MSNBC_11)

## Tokenize
MSNBC_11_tk <- MSNBC_11 %>% unnest_tokens("word", text)
# View(MSNBC_11_tk)

## Remove whitespaces
MSNBC_11_tk$word <- gsub("\\s+","", MSNBC_11_tk$word)

## Stemming
MSNBC_11_tk <- MSNBC_11_tk %>%
  mutate_at("word", funs(wordStem((.), language="en")))

## Remove stop words
data("stop_words")
MSNBC_11_tk <- MSNBC_11_tk %>%
  anti_join(stop_words)

## Remove numbers
MSNBC_11_tk <- MSNBC_11_tk[-grep("\\b\\d+\\b", MSNBC_11_tk$word),]

###############################################################################
## Checking against high competence dictionary
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highComp_match = str_detect(MSNBC_11_tk$word, pattern = paste(highComp_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(highComp_match, na.rm = TRUE)
MSNBC11_hiComp = sum(highComp_match, na.rm = TRUE)

# Count total number of words in 2011
ttlwrd_msnbc_2011 = length(MSNBC_11_tk$word)

# Calculate percentage of HC words
perHC_msnbc_2011 = (MSNBC11_hiComp/ttlwrd_msnbc_2011)*100

# Return items that matched
highComp_sub = str_subset(MSNBC_11_tk$word, pattern = paste(highComp_dict, collapse = "|"))
highComp_sub

# create a table to count freq of words and put that into another dataframe
hiComp.frq = table(highComp_sub)
hiComp.no = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq))
names(hiComp.no)[1] = "word"
names(hiComp.no)[2] = "n"
# View(hiComp.frq)

hiComp.no$word = factor(hiComp.no$word,
                        levels = hiComp.no$word[order(hiComp.no$word, 
                                                      decreasing = FALSE)])

# plot frequency of words
# ggplot(hiComp.no, aes(x= reorder(word, -n), y=n, fill=word))+
#   geom_bar(stat="identity")+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   ylab("Frequency of High Competence Words")+
#   xlab("")+
#   ggtitle("MSNBC 2011")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   guides(fill=FALSE)

# transform n to percentage over total words
hiComp.per = table(highComp_sub)
hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq))
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"

hiComppercent = (hiComp.per$n)/ttlwrd_msnbc_2011
rm(hiComp.per)

hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq), hiComppercent)
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"
names(hiComp.per)[3] = "percentage"

# plot frequency of words in percentage
ows_msnbc_hc <- ggplot(hiComp.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2011 - High Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)


################################################################################
## Checking against low competence dictionary
# str_detect detects patterns in tokenized file from low comp dictionary
# paste takes the dictionary and combines them into one cell
lowComp_match = str_detect(MSNBC_11_tk$word, pattern = paste(lowComp_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowComp_match, na.rm = TRUE)
MSNBC11_loComp = sum(lowComp_match, na.rm = TRUE)


# Calculate percentage of LC words
perLC_msnbc_2011 = (MSNBC11_loComp/ttlwrd_msnbc_2011)*100

# Return items that matched
lowComp_sub = str_subset(MSNBC_11_tk$word, pattern = paste(lowComp_dict, collapse = "|"))
lowComp_sub
# View(lowComp_sub)

# create dataframe to count frequency of words
loComp.frq = table(lowComp_sub)
loComp.no = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq))
names(loComp.no)[1] = "word"
names(loComp.no)[2] = "n"

loComp.no$word = factor(loComp.no$word,
                        levels = loComp.no$word[order(loComp.no$word, 
                                                      decreasing = FALSE)])

# plot frequency of words
ggplot(loComp.no, aes(x= reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency of Low Competence Words")+
  xlab("")+
  ggtitle("MSNBC 2011")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
loComp.per = table(lowComp_sub)
loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq))
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"

loComppercent = (loComp.per$n)/ttlwrd_msnbc_2011
rm(loComp.per)

loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq), loComppercent)
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"
names(loComp.per)[3] = "percentage"

# plot frequency of words in percentage
ows_msnbc_lc <- ggplot(loComp.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2011 - Low Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highMoral_match = str_detect(MSNBC_11_tk$word, pattern = paste(highMoral_dict, collapse = "|"))
# View(highMoral_match)

# Count number of TRUE (in other words, number of matches)
sum(highMoral_match, na.rm = TRUE)
MSNBC11_hiMoral = sum(highMoral_match, na.rm = TRUE)


# Calculate percentage of HM words
perHM_msnbc_2011 = (MSNBC11_hiMoral/ttlwrd_msnbc_2011)*100

# Return items that matched
highMoral_sub = str_subset(MSNBC_11_tk$word, pattern = paste(highMoral_dict, collapse = "|"))
highMoral_sub
# View(highMoral_sub)

# create a table to count freq of words and put that into another dataframe
hiMoral.frq = table(highMoral_sub)
hiMoral.no = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq))
names(hiMoral.no)[1] = "word"
names(hiMoral.no)[2] = "n"

hiMoral.no$word = factor(hiMoral.no$word,
                         levels = hiMoral.no$word[order(hiMoral.no$word, 
                                                        decreasing = FALSE)])

# plot frequency of words
# ggplot(hiMoral.no, aes(x= reorder(word, -n), y=n, fill=word))+
#   geom_bar(stat="identity")+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   ylab("Frequency of High Morality Words")+
#   xlab("")+
#   ggtitle("MSNBC 2011")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   guides(fill=FALSE)

# transform n to percentage over total words
hiMoral.per = table(highMoral_sub)
hiMoral.per = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq))
names(hiMoral.per)[1] = "word"
names(hiMoral.per)[2] = "n"

hiMoralpercent = (hiMoral.per$n)/ttlwrd_msnbc_2011
rm(hiMoral.per)

hiMoral.per = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq), hiMoralpercent)
names(hiMoral.per)[1] = "word"
names(hiMoral.per)[2] = "n"
names(hiMoral.per)[3] = "percentage"

# plot frequency of words in percentage
ows_msnbc_hm <- ggplot(hiMoral.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2011 - High Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
lowMoral_match = str_detect(MSNBC_11_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowMoral_match, na.rm = TRUE)
MSNBC11_loMoral = sum(lowMoral_match, na.rm = TRUE)

# Calculate percentage of HC words
perLM_msnbc_2011 = (MSNBC11_loMoral/ttlwrd_msnbc_2011)*100

# Return items that matched
lowMoral_sub = str_subset(MSNBC_11_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))
lowMoral_sub

# create a table to count freq of words and put that into another dataframe
loMoral.frq = table(lowMoral_sub)
loMoral.no = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq))
names(loMoral.no)[1] = "word"
names(loMoral.no)[2] = "n"

loMoral.no$word = factor(loMoral.no$word,
                         levels = loMoral.no$word[order(loMoral.no$word, 
                                                        decreasing = FALSE)])

# plot frequency of words
ggplot(loMoral.no, aes(x= reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency of Low Morality Words")+
  xlab("")+
  ggtitle("MSNBC 2011")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
loMoral.per = table(lowMoral_sub)
loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq))
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"

loMoralpercent = (loMoral.per$n)/ttlwrd_msnbc_2011
rm(loMoral.per)

loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq), loMoralpercent)
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"
names(loMoral.per)[3] = "percentage"

# plot frequency of words in percentage
ows_msnbc_lm <- ggplot(loMoral.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2011 - Low Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

## Adding 2011 values to master datafile
MSNBC11_rw <- c(2011, "MSNBC", perHC_msnbc_2011, perLC_msnbc_2011, 
                perHM_msnbc_2011, perLM_msnbc_2011)

master <- rbind(master, MSNBC11_rw)

MSNBC11 <- c(2011, perHC_msnbc_2011, perLC_msnbc_2011, 
             perHM_msnbc_2011, perLM_msnbc_2011)

msnbc.df <- rbind(msnbc.df, MSNBC11)


################################################################################
############################## MSNBC YEAR 2012 #################################
################################################################################
# all files
MSNBC_12 = readtext("C:/Users/Michelle/Desktop/test/MSNBC_2012_*.docx")

## Create columns for date, year, and day for year datafile
MSNBC_12 <- MSNBC_12 %>%
  add_column(date = substr(MSNBC_12$text, 1, 10)) %>%
  add_column(year = substr(MSNBC_12$text, 7, 10)) %>%
  add_column(day = substr(MSNBC_12$text, 11, 14)) 
# View(MSNBC_12)

## Tokenize
MSNBC_12_tk <- MSNBC_12 %>% unnest_tokens("word", text)
# View(MSNBC_12_tk)

## Remove whitespaces
MSNBC_12_tk$word <- gsub("\\s+","", MSNBC_12_tk$word)

## Stemming
MSNBC_12_tk <- MSNBC_12_tk %>%
  mutate_at("word", funs(wordStem((.), language="en")))

## Remove stop words
data("stop_words")
MSNBC_12_tk <- MSNBC_12_tk %>%
  anti_join(stop_words)

## Remove numbers
MSNBC_12_tk <- MSNBC_12_tk[-grep("\\b\\d+\\b", MSNBC_12_tk$word),]

###############################################################################
## Checking against high competence dictionary
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highComp_match = str_detect(MSNBC_12_tk$word, pattern = paste(highComp_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(highComp_match, na.rm = TRUE)
MSNBC12_hiComp = sum(highComp_match, na.rm = TRUE)

# Count total number of words in 2012
ttlwrd_msnbc_2012 = length(MSNBC_12_tk$word)

# Calculate percentage of HC words
perHC_msnbc_2012 = (MSNBC12_hiComp/ttlwrd_msnbc_2012)*100

# Return items that matched
highComp_sub = str_subset(MSNBC_12_tk$word, pattern = paste(highComp_dict, collapse = "|"))
highComp_sub

# create a table to count freq of words and put that into another dataframe
hiComp.frq = table(highComp_sub)
hiComp.no = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq))
names(hiComp.no)[1] = "word"
names(hiComp.no)[2] = "n"
# View(hiComp.frq)

hiComp.no$word = factor(hiComp.no$word,
                        levels = hiComp.no$word[order(hiComp.no$word, 
                                                      decreasing = FALSE)])

# plot frequency of words
# ggplot(hiComp.no, aes(x= reorder(word, -n), y=n, fill=word))+
#   geom_bar(stat="identity")+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   ylab("Frequency of High Competence Words")+
#   xlab("")+
#   ggtitle("MSNBC 2012")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   guides(fill=FALSE)

# transform n to percentage over total words
hiComp.per = table(highComp_sub)
hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq))
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"

hiComppercent = (hiComp.per$n)/ttlwrd_msnbc_2012
rm(hiComp.per)

hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq), hiComppercent)
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"
names(hiComp.per)[3] = "percentage"

# plot frequency of words in percentage
ggplot(hiComp.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2012 - High Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

################################################################################
## Checking against low competence dictionary
# str_detect detects patterns in tokenized file from low comp dictionary
# paste takes the dictionary and combines them into one cell
lowComp_match = str_detect(MSNBC_12_tk$word, pattern = paste(lowComp_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowComp_match, na.rm = TRUE)
MSNBC12_loComp = sum(lowComp_match, na.rm = TRUE)


# Calculate percentage of LC words
perLC_msnbc_2012 = (MSNBC12_loComp/ttlwrd_msnbc_2012)*100

# Return items that matched
lowComp_sub = str_subset(MSNBC_12_tk$word, pattern = paste(lowComp_dict, collapse = "|"))
lowComp_sub
# View(lowComp_sub)

# create dataframe to count frequency of words
loComp.frq = table(lowComp_sub)
loComp.no = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq))
names(loComp.no)[1] = "word"
names(loComp.no)[2] = "n"

loComp.no$word = factor(loComp.no$word,
                        levels = loComp.no$word[order(loComp.no$word, 
                                                      decreasing = FALSE)])

# plot frequency of words
ggplot(loComp.no, aes(x= reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency of Low Competence Words")+
  xlab("")+
  ggtitle("MSNBC 2012")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
loComp.per = table(lowComp_sub)
loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq))
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"

loComppercent = (loComp.per$n)/ttlwrd_msnbc_2012
rm(loComp.per)

loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq), loComppercent)
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"
names(loComp.per)[3] = "percentage"

# plot frequency of words in percentage
ggplot(loComp.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2012 - Low Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highMoral_match = str_detect(MSNBC_12_tk$word, pattern = paste(highMoral_dict, collapse = "|"))
# View(highMoral_match)

# Count number of TRUE (in other words, number of matches)
sum(highMoral_match, na.rm = TRUE)
MSNBC12_hiMoral = sum(highMoral_match, na.rm = TRUE)


# Calculate percentage of HM words
perHM_msnbc_2012 = (MSNBC12_hiMoral/ttlwrd_msnbc_2012)*100

# Return items that matched
highMoral_sub = str_subset(MSNBC_12_tk$word, pattern = paste(highMoral_dict, collapse = "|"))
highMoral_sub
# View(highMoral_sub)

# create a table to count freq of words and put that into another dataframe
hiMoral.frq = table(highMoral_sub)
hiMoral.no = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq))
names(hiMoral.no)[1] = "word"
names(hiMoral.no)[2] = "n"

hiMoral.no$word = factor(hiMoral.no$word,
                         levels = hiMoral.no$word[order(hiMoral.no$word, 
                                                        decreasing = FALSE)])

# plot frequency of words
# ggplot(hiMoral.no, aes(x= reorder(word, -n), y=n, fill=word))+
#   geom_bar(stat="identity")+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   ylab("Frequency of High Morality Words")+
#   xlab("")+
#   ggtitle("MSNBC 2012")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   guides(fill=FALSE)

# transform n to percentage over total words
hiMoral.per = table(highMoral_sub)
hiMoral.per = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq))
names(hiMoral.per)[1] = "word"
names(hiMoral.per)[2] = "n"

hiMoralpercent = (hiMoral.per$n)/ttlwrd_msnbc_2012
rm(hiMoral.per)

hiMoral.per = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq), hiMoralpercent)
names(hiMoral.per)[1] = "word"
names(hiMoral.per)[2] = "n"
names(hiMoral.per)[3] = "percentage"

# plot frequency of words in percentage
ggplot(hiMoral.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2012 - High Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
lowMoral_match = str_detect(MSNBC_12_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowMoral_match, na.rm = TRUE)
MSNBC12_loMoral = sum(lowMoral_match, na.rm = TRUE)

# Calculate percentage of LM words
perLM_msnbc_2012 = (MSNBC12_loMoral/ttlwrd_msnbc_2012)*100

# Return items that matched
lowMoral_sub = str_subset(MSNBC_12_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))
lowMoral_sub

# create a table to count freq of words and put that into another dataframe
loMoral.frq = table(lowMoral_sub)
loMoral.no = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq))
names(loMoral.no)[1] = "word"
names(loMoral.no)[2] = "n"

loMoral.no$word = factor(loMoral.no$word,
                         levels = loMoral.no$word[order(loMoral.no$word, 
                                                        decreasing = FALSE)])

# plot frequency of words
ggplot(loMoral.no, aes(x= reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency of Low Morality Words")+
  xlab("")+
  ggtitle("MSNBC 2012")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
loMoral.per = table(lowMoral_sub)
loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq))
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"

loMoralpercent = (loMoral.per$n)/ttlwrd_msnbc_2012
rm(loMoral.per)

loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq), loMoralpercent)
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"
names(loMoral.per)[3] = "percentage"

# plot frequency of words in percentage
ggplot(loMoral.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2012 - Low Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

## Adding 2012 values to master datafile
MSNBC12_rw <- c(2012, "MSNBC", perHC_msnbc_2012, perLC_msnbc_2012, 
                perHM_msnbc_2012, perLM_msnbc_2012)

master <- rbind(master, MSNBC12_rw)

MSNBC12 <- c(2012, perHC_msnbc_2012, perLC_msnbc_2012, 
             perHM_msnbc_2012, perLM_msnbc_2012)

msnbc.df <- rbind(msnbc.df, MSNBC12)

################################################################################
############################## MSNBC YEAR 2013 #################################
################################################################################
# all files
MSNBC_13 = readtext("C:/Users/Michelle/Desktop/test/MSNBC_2013_*.docx")

## Create columns for date, year, and day for year datafile
MSNBC_13 <- MSNBC_13 %>%
  add_column(date = substr(MSNBC_13$text, 1, 10)) %>%
  add_column(year = substr(MSNBC_13$text, 7, 10)) %>%
  add_column(day = substr(MSNBC_13$text, 11, 14)) 
# View(MSNBC_13)

## Tokenize
MSNBC_13_tk <- MSNBC_13 %>% unnest_tokens("word", text)
# View(MSNBC_13_tk)

## Remove whitespaces
MSNBC_13_tk$word <- gsub("\\s+","", MSNBC_13_tk$word)

## Stemming
MSNBC_13_tk <- MSNBC_13_tk %>%
  mutate_at("word", funs(wordStem((.), language="en")))

## Remove stop words
data("stop_words")
MSNBC_13_tk <- MSNBC_13_tk %>%
  anti_join(stop_words)

## Remove numbers
MSNBC_13_tk <- MSNBC_13_tk[-grep("\\b\\d+\\b", MSNBC_13_tk$word),]

###############################################################################
## Checking against high competence dictionary
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highComp_match = str_detect(MSNBC_13_tk$word, pattern = paste(highComp_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(highComp_match, na.rm = TRUE)
MSNBC13_hiComp = sum(highComp_match, na.rm = TRUE)

# Count total number of words in 2013
ttlwrd_msnbc_2013 = length(MSNBC_13_tk$word)

# Calculate percentage of HC words
perHC_msnbc_2013 = (MSNBC13_hiComp/ttlwrd_msnbc_2013)*100

# Return items that matched
highComp_sub = str_subset(MSNBC_13_tk$word, pattern = paste(highComp_dict, collapse = "|"))
highComp_sub

# create a table to count freq of words and put that into another dataframe
hiComp.frq = table(highComp_sub)
hiComp.no = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq))
names(hiComp.no)[1] = "word"
names(hiComp.no)[2] = "n"
# View(hiComp.frq)

hiComp.no$word = factor(hiComp.no$word,
                        levels = hiComp.no$word[order(hiComp.no$word, 
                                                      decreasing = FALSE)])

# plot frequency of words
ggplot(hiComp.no, aes(x= reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency of High Competence Words")+
  xlab("")+
  ggtitle("MSNBC 2013")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
hiComp.per = table(highComp_sub)
hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq))
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"

hiComppercent = (hiComp.per$n)/ttlwrd_msnbc_2013
rm(hiComp.per)

hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq), hiComppercent)
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"
names(hiComp.per)[3] = "percentage"

# plot frequency of words in percentage
ggplot(hiComp.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2013 - High Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

################################################################################
## Checking against low competence dictionary
# str_detect detects patterns in tokenized file from low comp dictionary
# paste takes the dictionary and combines them into one cell
lowComp_match = str_detect(MSNBC_13_tk$word, pattern = paste(lowComp_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowComp_match, na.rm = TRUE)
MSNBC13_loComp = sum(lowComp_match, na.rm = TRUE)

# Calculate percentage of LC words
perLC_msnbc_2013 = (MSNBC13_loComp/ttlwrd_msnbc_2013)*100

# Return items that matched
lowComp_sub = str_subset(MSNBC_13_tk$word, pattern = paste(lowComp_dict, collapse = "|"))
lowComp_sub
# View(lowComp_sub)

# create dataframe to count frequency of words
loComp.frq = table(lowComp_sub)
loComp.no = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq))
names(loComp.no)[1] = "word"
names(loComp.no)[2] = "n"

loComp.no$word = factor(loComp.no$word,
                        levels = loComp.no$word[order(loComp.no$word, 
                                                      decreasing = FALSE)])

# plot frequency of words
ggplot(loComp.no, aes(x= reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency of Low Competence Words")+
  xlab("")+
  ggtitle("MSNBC 2013")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
loComp.per = table(lowComp_sub)
loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq))
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"

loComppercent = (loComp.per$n)/ttlwrd_msnbc_2013
rm(loComp.per)

loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq), loComppercent)
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"
names(loComp.per)[3] = "percentage"

# plot frequency of words in percentage
ggplot(loComp.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2013 - Low Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highMoral_match = str_detect(MSNBC_13_tk$word, pattern = paste(highMoral_dict, collapse = "|"))
# View(highMoral_match)

# Count number of TRUE (in other words, number of matches)
sum(highMoral_match, na.rm = TRUE)
MSNBC13_hiMoral = sum(highMoral_match, na.rm = TRUE)

# Calculate percentage of HM words
perHM_msnbc_2013 = (MSNBC13_hiMoral/ttlwrd_msnbc_2013)*100

# Return items that matched
highMoral_sub = str_subset(MSNBC_13_tk$word, pattern = paste(highMoral_dict, collapse = "|"))
highMoral_sub
# View(highMoral_sub)

# create a table to count freq of words and put that into another dataframe
hiMoral.frq = table(highMoral_sub)
hiMoral.no = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq))
names(hiMoral.no)[1] = "word"
names(hiMoral.no)[2] = "n"

hiMoral.no$word = factor(hiMoral.no$word,
                         levels = hiMoral.no$word[order(hiMoral.no$word, 
                                                        decreasing = FALSE)])

# plot frequency of words
ggplot(hiMoral.no, aes(x= reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency of High Morality Words")+
  xlab("")+
  ggtitle("MSNBC 2013")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
hiMoral.per = table(highMoral_sub)
hiMoral.per = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq))
names(hiMoral.per)[1] = "word"
names(hiMoral.per)[2] = "n"

hiMoralpercent = (hiMoral.per$n)/ttlwrd_msnbc_2013
rm(hiMoral.per)

hiMoral.per = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq), hiMoralpercent)
names(hiMoral.per)[1] = "word"
names(hiMoral.per)[2] = "n"
names(hiMoral.per)[3] = "percentage"

# plot frequency of words in percentage
ggplot(hiMoral.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2013 - High Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
lowMoral_match = str_detect(MSNBC_13_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowMoral_match, na.rm = TRUE)
MSNBC13_loMoral = sum(lowMoral_match, na.rm = TRUE)

# Calculate percentage of LM words
perLM_msnbc_2013 = (MSNBC13_loMoral/ttlwrd_msnbc_2013)*100

# Return items that matched
lowMoral_sub = str_subset(MSNBC_13_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))
lowMoral_sub

# create a table to count freq of words and put that into another dataframe
loMoral.frq = table(lowMoral_sub)
loMoral.no = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq))
names(loMoral.no)[1] = "word"
names(loMoral.no)[2] = "n"

loMoral.no$word = factor(loMoral.no$word,
                         levels = loMoral.no$word[order(loMoral.no$word, 
                                                        decreasing = FALSE)])

# plot frequency of words
ggplot(loMoral.no, aes(x= reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency of Low Morality Words")+
  xlab("")+
  ggtitle("MSNBC 2013")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
loMoral.per = table(lowMoral_sub)
loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq))
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"

loMoralpercent = (loMoral.per$n)/ttlwrd_msnbc_2013
rm(loMoral.per)

loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq), loMoralpercent)
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"
names(loMoral.per)[3] = "percentage"

# plot frequency of words in percentage
ggplot(loMoral.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("MSNBC 2013 - Low Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

## Adding 2013 values to master datafile
MSNBC13_rw <- c(2013, "MSNBC", perHC_msnbc_2013, perLC_msnbc_2013, 
                perHM_msnbc_2013, perLM_msnbc_2013)

master <- rbind(master, MSNBC13_rw)

MSNBC13 <- c(2013, perHC_msnbc_2013, perLC_msnbc_2013, 
             perHM_msnbc_2013, perLM_msnbc_2013)

msnbc.df <- rbind(msnbc.df, MSNBC13)


################################################################################
################################ MSNBC ALL #####################################
################################################################################
# all files 
MSNBC_all = readtext("C:/Users/Michelle/Desktop/test/MSNBC_*.docx")

## Create columns for date, year, and day for year datafile
MSNBC_all <- MSNBC_all %>%
  add_column(date = substr(MSNBC_all$text, 1, 10)) %>%
  add_column(year = substr(MSNBC_all$text, 7, 10)) %>%
  add_column(day = substr(MSNBC_all$text, 11, 14)) 
# View(MSNBC_06)

## Tokenize
MSNBC_all_tk <- MSNBC_all %>% unnest_tokens("word", text)
# View(MSNBC_06_tk)

## Remove whitespaces
MSNBC_all_tk$word <- gsub("\\s+","", MSNBC_all_tk$word)

## Stemming
MSNBC_all_tk <- MSNBC_all_tk %>%
  mutate_at("word", funs(wordStem((.), language="en")))

## Remove stop words
data("stop_words")
MSNBC_all_tk <- MSNBC_all_tk %>%
  anti_join(stop_words)

## Remove numbers
MSNBC_all_tk <- MSNBC_all_tk[-grep("\\b\\d+\\b", MSNBC_all_tk$word),]


###############################################################################
## Checking against high competence dictionary
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highComp_all_match = str_detect(MSNBC_all_tk$word, 
                                pattern = paste(highComp_dict, 
                                                collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(highComp_all_match, na.rm = TRUE)
# View(highComp_match)

MSNBC_all_hiComp = sum(highComp_all_match, na.rm = TRUE)

# Count total number of words 
ttlwrd_msnbc_all = length(MSNBC_all_tk$word)

# Calculate percentage of HC words
perHC_msnbc_all = (MSNBC_all_hiComp/ttlwrd_msnbc_all)*100

# str_which detects which words matched in tokenized column
# highComp_whr = str_which(MSNBC_06_tk$word, paste(highComp_dict, collapse = "|"))
# View(highComp_whr)

# str_extract pulls out which words in tokenized file matched
# highComp_ext = str_extract(MSNBC_06_tk$word, paste(highComp_dict, collapse = "|"))
# View(highComp_ext)

# Return items that matched
highComp_all_sub = str_subset(MSNBC_all_tk$word, pattern = paste(highComp_dict, collapse = "|"))
highComp_all_sub

# create a table to count freq of words and put that into another dataframe
hiComp_all.frq = table(highComp_all_sub)
hiComp_all.no = cbind.data.frame(names(hiComp_all.frq), as.integer(hiComp_all.frq))
names(hiComp_all.no)[1] = "word"
names(hiComp_all.no)[2] = "n"

## Graphing top 20 high competence words
hiComp_all.no = hiComp_all.no[with(hiComp_all.no, order(n, decreasing = TRUE)),]
top_20_hc = hiComp_all.no[1:20,]

# plot frequency of words
msnbc_hc <- ggplot(top_20_hc, aes(x= reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Count")+
  xlab("")+
  ggtitle("Top 20 High Competence Words for MSNBC")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
hiComp.all.per = table(highComp_all_sub)
hiComp.all.per = cbind.data.frame(names(hiComp_all.frq), as.integer(hiComp_all.frq))
names(hiComp.all.per)[1] = "word"
names(hiComp.all.per)[2] = "n"

hiComppercent.all = (hiComp.all.per$n)/ttlwrd_msnbc_all
rm(hiComp.all.per)

hiComp.all.per = cbind.data.frame(names(hiComp_all.frq), as.integer(hiComp_all.frq), hiComppercent.all)
names(hiComp.all.per)[1] = "word"
names(hiComp.all.per)[2] = "n"
names(hiComp.all.per)[3] = "percentage"

# plot frequency of words in percentage
perHC_MSNBC <- ggplot(hiComp.all.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("")+
  xlab("")+
  ggtitle("MSNBC")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

################################################################################
## Checking against low competence dictionary
# str_detect detects patterns in tokenized file from low comp dictionary
# paste takes the dictionary and combines them into one cell
lowComp_all_match = str_detect(MSNBC_all_tk$word, 
                               pattern = paste(lowComp_dict, 
                                               collapse = "|"))


# Count number of TRUE (in other words, number of matches)
sum(lowComp_all_match, na.rm = TRUE)

MSNBC_all_loComp = sum(lowComp_all_match, na.rm = TRUE)

# Calculate percentage of HC words
perLC_msnbc_all = (MSNBC_all_loComp/ttlwrd_msnbc_all)*100

# Return items that matched
lowComp_all_sub = str_subset(MSNBC_all_tk$word, pattern = paste(lowComp_dict, collapse = "|"))
lowComp_all_sub
# View(lowComp_sub)

# create dataframe to count frequency of words
loComp_all.frq = table(lowComp_all_sub)
loComp_all.no = cbind.data.frame(names(loComp_all.frq), as.integer(loComp_all.frq))
names(loComp_all.no)[1] = "word"
names(loComp_all.no)[2] = "n"

## Graphing top 20 high competence words
loComp_all.no = loComp_all.no[with(loComp_all.no, order(n, decreasing = TRUE)),]
top_20_lc = loComp_all.no[1:20,]

# plot frequency of words
msnbc_lc <- ggplot(top_20_lc, aes(x= reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Count")+
  xlab("")+
  ggtitle("Top 20 Low Competence Words for MSNBC")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
loComp.all.per = table(lowComp_all_sub)
loComp.all.per = cbind.data.frame(names(loComp_all.frq), as.integer(loComp_all.frq))
names(loComp.all.per)[1] = "word"
names(loComp.all.per)[2] = "n"

loComppercent.all = (loComp.all.per$n)/ttlwrd_msnbc_all
rm(loComp.all.per)

loComp.all.per = cbind.data.frame(names(loComp_all.frq), 
                                  as.integer(loComp_all.frq), 
                                  loComppercent.all)
names(loComp.all.per)[1] = "word"
names(loComp.all.per)[2] = "n"
names(loComp.all.per)[3] = "percentage"

# plot frequency of words in percentage
perLC_MSNBC <- ggplot(loComp.all.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("")+
  xlab("")+
  ggtitle("MSNBC")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highMoral_all_match = str_detect(MSNBC_all_tk$word, pattern = paste(highMoral_dict, collapse = "|"))
# View(highMoral_match)

# Count number of TRUE (in other words, number of matches)
sum(highMoral_all_match, na.rm = TRUE)

MSNBC_all_hiMoral = sum(highMoral_all_match, na.rm = TRUE)

# Calculate percentage of HM words
perHM_msnbc_all = (MSNBC_all_hiMoral/ttlwrd_msnbc_all)*100

# Return items that matched
highMoral_all_sub = str_subset(MSNBC_all_tk$word, pattern = paste(highMoral_dict, collapse = "|"))
highMoral_all_sub
# View(highMoral_sub)

# create a table to count freq of words and put that into another dataframe
hiMoral_all.frq = table(highMoral_all_sub)
hiMoral_all.no = cbind.data.frame(names(hiMoral_all.frq), as.integer(hiMoral_all.frq))
names(hiMoral_all.no)[1] = "word"
names(hiMoral_all.no)[2] = "n"

## Graphing top 20 high competence words
hiMoral_all.no = hiMoral_all.no[with(hiMoral_all.no, order(n, decreasing = TRUE)),]
top_20_hm = hiMoral_all.no[1:20,]

# plot frequency of words
msnbc_hm <- ggplot(top_20_hm, aes(x= reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Count")+
  xlab("")+
  ggtitle("Top 20 High Morality Words for MSNBC")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
hiMoral.all.per = table(highMoral_all_sub)
hiMoral.all.per = cbind.data.frame(names(hiMoral_all.frq), 
                                   as.integer(hiMoral_all.frq))
names(hiMoral.all.per)[1] = "word"
names(hiMoral.all.per)[2] = "n"

hiMoralpercent.all = (hiMoral.all.per$n)/ttlwrd_msnbc_all
rm(hiMoral.all.per)

hiMoral.all.per = cbind.data.frame(names(hiMoral_all.frq), 
                                  as.integer(hiMoral_all.frq), 
                                  hiMoralpercent.all)
names(hiMoral.all.per)[1] = "word"
names(hiMoral.all.per)[2] = "n"
names(hiMoral.all.per)[3] = "percentage"

# plot frequency of words in percentage
perHM_MSNBC <- ggplot(hiMoral.all.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("")+
  xlab("")+
  ggtitle("MSNBC")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
lowMoral_all_match = str_detect(MSNBC_all_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowMoral_all_match, na.rm = TRUE)

MSNBC_all_loMoral = sum(lowMoral_all_match, na.rm = TRUE)

# Calculate percentage of LM words
perLM_msnbc_all = (MSNBC_all_loMoral/ttlwrd_msnbc_all)*100

# Return items that matched
lowMoral_all_sub = str_subset(MSNBC_all_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))
lowMoral_all_sub

# create a table to count freq of words and put that into another dataframe
loMoral_all.frq = table(lowMoral_all_sub)
loMoral_all.no = cbind.data.frame(names(loMoral_all.frq), as.integer(loMoral_all.frq))
names(loMoral_all.no)[1] = "word"
names(loMoral_all.no)[2] = "n"

## Graphing top 20 high competence words
loMoral_all.no = loMoral_all.no[with(loMoral_all.no, order(n, decreasing = TRUE)),]
top_20_lm = loMoral_all.no[1:20,]

# plot frequency of words
msnbc_lm <- ggplot(top_20_lm, aes(x= reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Count")+
  xlab("")+
  ggtitle("Top 20 Low Morality Words for MSNBC")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
loMoral.all.per = table(lowMoral_all_sub)
loMoral.all.per = cbind.data.frame(names(loMoral_all.frq), 
                                   as.integer(loMoral_all.frq))
names(loMoral.all.per)[1] = "word"
names(loMoral.all.per)[2] = "n"

loMoralpercent.all = (loMoral.all.per$n)/ttlwrd_msnbc_all
rm(loMoral.all.per)

loMoral.all.per = cbind.data.frame(names(loMoral_all.frq), 
                                   as.integer(loMoral_all.frq), 
                                   loMoralpercent.all)
names(loMoral.all.per)[1] = "word"
names(loMoral.all.per)[2] = "n"
names(loMoral.all.per)[3] = "percentage"

# plot frequency of words in percentage
perLM_MSNBC <- ggplot(loMoral.all.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("")+
  xlab("")+
  ggtitle("MSNBC")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

msnbc_top <- ggarrange(perHC_MSNBC, perLC_MSNBC, perHM_MSNBC, perLM_MSNBC,
                  ncol = 2, nrow = 2,
                  common.legend = TRUE, legend = "bottom")
msnbc_top

## Adding values to datafile
Ideo_comp <- data.frame("MSNBC", perHC_msnbc_all, perLC_msnbc_all, 
                perHM_msnbc_all, perLM_msnbc_all)

colnames(Ideo_comp) <- c("Source", "HiComp", 
                        "LowComp", "HiMoral", "LowMoral")

MSNBC_DF <- data.frame(perHC_msnbc_all, perLC_msnbc_all, 
                       perHM_msnbc_all, perLM_msnbc_all)
colnames(MSNBC_DF) <- c("HiComp", "LowComp", "HiMoral", "LowMoral")

write.csv(Ideo_comp, "C:\\Users\\Michelle\\Desktop\\Ideo_comp.csv", row.names = TRUE)
