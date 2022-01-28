################################################################################
############################## FOX YEAR 2006 ###################################
################################################################################
# all files 
FOX_06 = readtext("C:/Users/Michelle/Desktop/test/Fox_2006_*.docx")

## Create columns for date, year, and day for year datafile
FOX_06 <- FOX_06 %>%
  add_column(date = substr(FOX_06$text, 1, 10)) %>%
  add_column(year = substr(FOX_06$text, 7, 10)) %>%
  add_column(day = substr(FOX_06$text, 11, 14)) 
# View(FOX_06)

## Tokenize
FOX_06_tk <- FOX_06 %>% unnest_tokens("word", text)
#View(FOX_06_tk)

## Remove whitespaces
FOX_06_tk$word <- gsub("\\s+","", FOX_06_tk$word)

## Stemming
FOX_06_tk <- FOX_06_tk %>%
  mutate_at("word", funs(wordStem((.), language="en")))

## Remove stop words
data("stop_words")
FOX_06_tk <- FOX_06_tk %>%
  anti_join(stop_words)

## Remove numbers
FOX_06_tk <- FOX_06_tk[-grep("\\b\\d+\\b", FOX_06_tk$word),]

###############################################################################
## Checking against high competence dictionary
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highComp_match = str_detect(FOX_06_tk$word, pattern = paste(highComp_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(highComp_match, na.rm = TRUE)
# View(highComp_match)

FOX06_hiComp = sum(highComp_match, na.rm = TRUE)

# Count total number of words in 2006
ttlwrd_fox_2006 = length(FOX_06_tk$word)

# Calculate percentage of HC words
perHC_fox_2006 = (FOX06_hiComp/ttlwrd_fox_2006)*100

# Return items that matched
highComp_sub = str_subset(FOX_06_tk$word, pattern = paste(highComp_dict, collapse = "|"))
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
  ggtitle("Fox 2006")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
hiComp.per = table(highComp_sub)
hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq))
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"

hiComppercent = (hiComp.per$n)/ttlwrd_fox_2006
rm(hiComp.per)

hiComp.per = cbind.data.frame(names(hiComp.frq), 
                              as.integer(hiComp.frq), hiComppercent)
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
  ggtitle("Fox News 2006 - High Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

################################################################################
## Checking against low competence dictionary
# str_detect detects patterns in tokenized file from low comp dictionary
# paste takes the dictionary and combines them into one cell
lowComp_match = str_detect(FOX_06_tk$word, pattern = paste(lowComp_dict, collapse = "|"))


# Count number of TRUE (in other words, number of matches)
sum(lowComp_match, na.rm = TRUE)

FOX06_loComp = sum(lowComp_match, na.rm = TRUE)

# Calculate percentage of LC words
perLC_fox_2006 = (FOX06_loComp/ttlwrd_fox_2006)*100

# Return items that matched
lowComp_sub = str_subset(FOX_06_tk$word, pattern = paste(lowComp_dict, collapse = "|"))
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
#   ggtitle("Fox 2006")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   guides(fill=FALSE)

# transform n to percentage over total words
loComp.per = table(lowComp_sub)
loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq))
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"

loComppercent = (loComp.per$n)/ttlwrd_fox_2006
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
  ggtitle("Fox News 2006 - Low Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highMoral_match = str_detect(FOX_06_tk$word, 
                             pattern = paste(highMoral_dict, 
                                             collapse = "|"))
# View(highMoral_match)

# Count number of TRUE (in other words, number of matches)
# sum(FOX_match, na.rm = TRUE)

FOX06_hiMoral = sum(highMoral_match, na.rm = TRUE)

# Calculate percentage of HM words
perHM_fox_2006 = (FOX06_hiMoral/ttlwrd_fox_2006)*100

# Return items that matched
highMoral_sub = str_subset(FOX_06_tk$word, 
                           pattern = paste(highMoral_dict, 
                                           collapse = "|"))
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
  ggtitle("Fox 2006")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
hiMoral.per = table(highMoral_sub)
hiMoral.per = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq))
names(hiMoral.per)[1] = "word"
names(hiMoral.per)[2] = "n"

hiMoralpercent = (hiMoral.per$n)/ttlwrd_fox_2006
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
  ggtitle("Fox News 2006 - High Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
lowMoral_match = str_detect(FOX_06_tk$word, 
                            pattern = paste(lowMoral_dict, 
                                            collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowMoral_match, na.rm = TRUE)

FOX06_loMoral = sum(lowMoral_match, na.rm = TRUE)

# Calculate percentage of LM words
perLM_fox_2006 = (FOX06_loMoral/ttlwrd_fox_2006)*100

# Return items that matched
lowMoral_sub = str_subset(FOX_06_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))
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
  ggtitle("Fox 2006")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
loMoral.per = table(lowMoral_sub)
loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq))
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"

loMoralpercent = (loMoral.per$n)/ttlwrd_fox_2006
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
  ggtitle("Fox News 2006 - Low Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

## Adding 2006 values to master datafile
FOX06_rw <- c(2006, "FOX", perHC_fox_2006, perLC_fox_2006, 
              perHM_fox_2006, perLM_fox_2006)

master <- rbind(master, FOX06_rw)

fox.df <- data.frame(2006, perHC_fox_2006, perLC_fox_2006, 
                     perHM_fox_2006, perLM_fox_2006)
colnames(fox.df) <- c("Year", "HiComp", "LowComp", "HiMoral", "LowMoral")

################################################################################
############################## FOX YEAR 2007 ###################################
################################################################################
# all files 
FOX_07 = readtext("C:/Users/Michelle/Desktop/test/Fox_2007_*.docx")

## Create columns for date, year, and day for year datafile
FOX_07 <- FOX_07 %>%
  add_column(date = substr(FOX_07$text, 1, 10)) %>%
  add_column(year = substr(FOX_07$text, 7, 10)) %>%
  add_column(day = substr(FOX_07$text, 11, 14)) 
# View(FOX_07)

## Tokenize
FOX_07_tk <- FOX_07 %>% unnest_tokens("word", text)
# View(FOX_06_tk)

## Remove whitespaces
FOX_07_tk$word <- gsub("\\s+","", FOX_07_tk$word)

## Stemming
FOX_07_tk <- FOX_07_tk %>%
  mutate_at("word", funs(wordStem((.), language="en")))

## Remove stop words
data("stop_words")
FOX_07_tk <- FOX_07_tk %>%
  anti_join(stop_words)

## Remove numbers
FOX_07_tk <- FOX_07_tk[-grep("\\b\\d+\\b", FOX_07_tk$word),]

###############################################################################
## Checking against high competence dictionary
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highComp_match = str_detect(FOX_07_tk$word, 
                            pattern = paste(highComp_dict, 
                                            collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(highComp_match, na.rm = TRUE)
# View(highComp_match)

FOX07_hiComp = sum(highComp_match, na.rm = TRUE)

# Count total number of words in 2007
ttlwrd_fox_2007 = length(FOX_07_tk$word)

# Calculate percentage of HC words
perHC_fox_2007 = (FOX07_hiComp/ttlwrd_fox_2007)*100

# Return items that matched
highComp_sub = str_subset(FOX_07_tk$word, pattern = paste(highComp_dict, collapse = "|"))
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
  ggtitle("Fox 2007")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
hiComp.per = table(highComp_sub)
hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq))
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"

hiComppercent = (hiComp.per$n)/ttlwrd_fox_2007
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
  ggtitle("Fox News 2007 - High Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

################################################################################
## Checking against low competence dictionary
# str_detect detects patterns in tokenized file from low comp dictionary
# paste takes the dictionary and combines them into one cell
lowComp_match = str_detect(FOX_07_tk$word, 
                           pattern = paste(lowComp_dict, 
                                           collapse = "|"))


# Count number of TRUE (in other words, number of matches)
sum(lowComp_match, na.rm = TRUE)

FOX07_loComp = sum(lowComp_match, na.rm = TRUE)

# Calculate percentage of LC words
perLC_fox_2007 = (FOX07_loComp/ttlwrd_fox_2007)*100

# Return items that matched
lowComp_sub = str_subset(FOX_07_tk$word, pattern = paste(lowComp_dict, collapse = "|"))
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
  ggtitle("Fox 2007")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
loComp.per = table(lowComp_sub)
loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq))
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"

loComppercent = (loComp.per$n)/ttlwrd_fox_2007
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
  ggtitle("Fox News 2007 - Low Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highMoral_match = str_detect(FOX_07_tk$word, 
                             pattern = paste(highMoral_dict, collapse = "|"))
# View(highMoral_match)

# Count number of TRUE (in other words, number of matches)
sum(highMoral_match, na.rm = TRUE)

FOX07_hiMoral = sum(highMoral_match, na.rm = TRUE)

# Calculate percentage of HM words
perHM_fox_2007 = (FOX07_hiMoral/ttlwrd_fox_2007)*100

# Return items that matched
highMoral_sub = str_subset(FOX_07_tk$word, pattern = paste(highMoral_dict, collapse = "|"))
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
  ggtitle("Fox 2007")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
hiMoral.per = table(highMoral_sub)
hiMoral.per = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq))
names(hiMoral.per)[1] = "word"
names(hiMoral.per)[2] = "n"

hiMoralpercent = (hiMoral.per$n)/ttlwrd_fox_2007
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
  ggtitle("Fox News 2007 - High Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
lowMoral_match = str_detect(FOX_07_tk$word, 
                            pattern = paste(lowMoral_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowMoral_match, na.rm = TRUE)

FOX07_loMoral = sum(lowMoral_match, na.rm = TRUE)

# Calculate percentage of LM words
perLM_fox_2007 = (FOX07_loMoral/ttlwrd_fox_2007)*100

# Return items that matched
lowMoral_sub = str_subset(FOX_07_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))
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
  ggtitle("Fox 2007")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
loMoral.per = table(lowMoral_sub)
loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq))
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"

loMoralpercent = (loMoral.per$n)/ttlwrd_fox_2007
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
  ggtitle("Fox News 2007 - Low Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

## Adding 2007 values to master datafile
FOX07_rw <- c(2007, "FOX", perHC_fox_2007, perLC_fox_2007, 
              perHM_fox_2007, perLM_fox_2007)

master <- rbind(master, FOX07_rw)

FOX07 <- c(2007, perHC_fox_2007, perLC_fox_2007, 
           perHM_fox_2007, perLM_fox_2007)

fox.df <- rbind(fox.df, FOX07)

################################################################################
############################## FOX YEAR 2008 ###################################
################################################################################
# all files 
FOX_08 = readtext("C:/Users/Michelle/Desktop/test/Fox_2008_*.docx")

## Create columns for date, year, and day for year datafile
FOX_08 <- FOX_08 %>%
  add_column(date = substr(FOX_08$text, 1, 10)) %>%
  add_column(year = substr(FOX_08$text, 7, 10)) %>%
  add_column(day = substr(FOX_08$text, 11, 14)) 
# View(FOX_08)

## Tokenize
FOX_08_tk <- FOX_08 %>% unnest_tokens("word", text)
# View(FOX_06_tk)

## Remove whitespaces
FOX_08_tk$word <- gsub("\\s+","", FOX_08_tk$word)

## Stemming
FOX_08_tk <- FOX_08_tk %>%
  mutate_at("word", funs(wordStem((.), language="en")))

## Remove stop words
data("stop_words")
FOX_08_tk <- FOX_08_tk %>%
  anti_join(stop_words)

## Remove numbers
FOX_08_tk <- FOX_08_tk[-grep("\\b\\d+\\b", FOX_08_tk$word),]

###############################################################################
## Checking against high competence dictionary
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highComp_match = str_detect(FOX_08_tk$word, 
                            pattern = paste(highComp_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(highComp_match, na.rm = TRUE)
# View(highComp_match)

FOX08_hiComp = sum(highComp_match, na.rm = TRUE)

# Count total number of words in 2008
ttlwrd_fox_2008 = length(FOX_08_tk$word)

# Calculate percentage of HC words
perHC_fox_2008 = (FOX08_hiComp/ttlwrd_fox_2008)*100

# Return items that matched
highComp_sub = str_subset(FOX_08_tk$word, pattern = paste(highComp_dict, collapse = "|"))
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
  ggtitle("Fox 2008")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
hiComp.per = table(highComp_sub)
hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq))
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"

hiComppercent = (hiComp.per$n)/ttlwrd_fox_2008
rm(hiComp.per)

hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq), hiComppercent)
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"
names(hiComp.per)[3] = "percentage"

# plot frequency of words in percentage
fc_fox_hc <- ggplot(hiComp.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("Fox News 2008 - High Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

################################################################################
## Checking against low competence dictionary
# str_detect detects patterns in tokenized file from low comp dictionary
# paste takes the dictionary and combines them into one cell
lowComp_match = str_detect(FOX_08_tk$word, 
                           pattern = paste(lowComp_dict, collapse = "|"))


# Count number of TRUE (in other words, number of matches)
sum(lowComp_match, na.rm = TRUE)

FOX08_loComp = sum(lowComp_match, na.rm = TRUE)

# Calculate percentage of lC words
perLC_fox_2008 = (FOX08_loComp/ttlwrd_fox_2008)*100

# Return items that matched
lowComp_sub = str_subset(FOX_08_tk$word, pattern = paste(lowComp_dict, collapse = "|"))
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
  ggtitle("Fox 2008")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
loComp.per = table(lowComp_sub)
loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq))
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"

loComppercent = (loComp.per$n)/ttlwrd_fox_2008
rm(loComp.per)

loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq), loComppercent)
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"
names(loComp.per)[3] = "percentage"

# plot frequency of words in percentage
fc_fox_lc <- ggplot(loComp.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("Fox News 2008 - Low Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)


###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highMoral_match = str_detect(FOX_08_tk$word, 
                             pattern = paste(highMoral_dict, collapse = "|"))
# View(highMoral_match)

# Count number of TRUE (in other words, number of matches)
sum(highMoral_match, na.rm = TRUE)

FOX08_hiMoral = sum(highMoral_match, na.rm = TRUE)

# Calculate percentage of HM words
perHM_fox_2008 = (FOX08_hiMoral/ttlwrd_fox_2008)*100


# Return items that matched
highMoral_sub = str_subset(FOX_08_tk$word, 
                           pattern = paste(highMoral_dict, collapse = "|"))
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
  ggtitle("Fox 2008")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
hiMoral.per = table(highMoral_sub)
hiMoral.per = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq))
names(hiMoral.per)[1] = "word"
names(hiMoral.per)[2] = "n"

hiMoralpercent = (hiMoral.per$n)/ttlwrd_fox_2008
rm(hiMoral.per)

hiMoral.per = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq), hiMoralpercent)
names(hiMoral.per)[1] = "word"
names(hiMoral.per)[2] = "n"
names(hiMoral.per)[3] = "percentage"

# plot frequency of words in percentage
fc_fox_hm <- ggplot(hiMoral.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("Fox News 2008 - High Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)


###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
lowMoral_match = str_detect(FOX_08_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowMoral_match, na.rm = TRUE)

FOX08_loMoral = sum(lowMoral_match, na.rm = TRUE)

# Calculate percentage of LM words
perLM_fox_2008 = (FOX08_loMoral/ttlwrd_fox_2008)*100

# Return items that matched
lowMoral_sub = str_subset(FOX_08_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))
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
  ggtitle("Fox 2008")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
loMoral.per = table(lowMoral_sub)
loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq))
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"

loMoralpercent = (loMoral.per$n)/ttlwrd_fox_2008
rm(loMoral.per)

loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq), loMoralpercent)
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"
names(loMoral.per)[3] = "percentage"

# plot frequency of words in percentage
fc_fox_lm <- ggplot(loMoral.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("Fox News 2008 - Low Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

## Adding 2008 values to master datafile
FOX08_rw <- c(2008, "FOX", perHC_fox_2008, perLC_fox_2008, 
              perHM_fox_2008, perLM_fox_2008)

master <- rbind(master, FOX08_rw)

FOX08 <- c(2008, perHC_fox_2008, perLC_fox_2008, 
           perHM_fox_2008, perLM_fox_2008)

fox.df <- rbind(fox.df, FOX08)

################################################################################
############################## FOX YEAR 2009 ###################################
################################################################################
# all files 
FOX_09 = readtext("C:/Users/Michelle/Desktop/test/Fox_2009_*.docx")

## Create columns for date, year, and day for year datafile
FOX_09 <- FOX_09 %>%
  add_column(date = substr(FOX_09$text, 1, 10)) %>%
  add_column(year = substr(FOX_09$text, 7, 10)) %>%
  add_column(day = substr(FOX_09$text, 11, 14)) 
# View(FOX_09)

## Tokenize
FOX_09_tk <- FOX_09 %>% unnest_tokens("word", text)
# View(FOX_09_tk)

## Remove whitespaces
FOX_09_tk$word <- gsub("\\s+","", FOX_09_tk$word)

## Stemming
FOX_09_tk <- FOX_09_tk %>%
  mutate_at("word", funs(wordStem((.), language="en")))

## Remove stop words
data("stop_words")
FOX_09_tk <- FOX_09_tk %>%
  anti_join(stop_words)

## Remove numbers
FOX_09_tk <- FOX_09_tk[-grep("\\b\\d+\\b", FOX_09_tk$word),]

###############################################################################
## Checking against high competence dictionary
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highComp_match = str_detect(FOX_09_tk$word, 
                            pattern = paste(highComp_dict, 
                                            collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(highComp_match, na.rm = TRUE)
# View(highComp_match)

FOX09_hiComp = sum(highComp_match, na.rm = TRUE)

# Count total number of words in 2009
ttlwrd_fox_2009 = length(FOX_09_tk$word)

# Calculate percentage of HC words
perHC_fox_2009 = (FOX09_hiComp/ttlwrd_fox_2009)*100

# Return items that matched
highComp_sub = str_subset(FOX_09_tk$word, pattern = paste(highComp_dict, collapse = "|"))
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
  ggtitle("Fox 2009")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
hiComp.per = table(highComp_sub)
hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq))
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"

hiComppercent = (hiComp.per$n)/ttlwrd_fox_2009
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
  ggtitle("Fox News 2009 - High Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

################################################################################
## Checking against low competence dictionary
# str_detect detects patterns in tokenized file from low comp dictionary
# paste takes the dictionary and combines them into one cell
lowComp_match = str_detect(FOX_09_tk$word, 
                           pattern = paste(lowComp_dict, 
                                           collapse = "|"))


# Count number of TRUE (in other words, number of matches)
sum(lowComp_match, na.rm = TRUE)

FOX09_loComp = sum(lowComp_match, na.rm = TRUE)

# Calculate percentage of HC words
perLC_fox_2009 = (FOX09_loComp/ttlwrd_fox_2009)*100

# Return items that matched
lowComp_sub = str_subset(FOX_09_tk$word, pattern = paste(lowComp_dict, collapse = "|"))
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
  ggtitle("Fox 2009")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
loComp.per = table(lowComp_sub)
loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq))
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"

loComppercent = (loComp.per$n)/ttlwrd_fox_2009
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
  ggtitle("Fox News 2009 - Low Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highMoral_match = str_detect(FOX_09_tk$word, 
                             pattern = paste(highMoral_dict, collapse = "|"))
# View(highMoral_match)

# Count number of TRUE (in other words, number of matches)
sum(highMoral_match, na.rm = TRUE)

FOX09_hiMoral = sum(highMoral_match, na.rm = TRUE)

# Calculate percentage of HM words
perHM_fox_2009 = (FOX09_hiMoral/ttlwrd_fox_2009)*100

# Return items that matched
highMoral_sub = str_subset(FOX_09_tk$word, pattern = paste(highMoral_dict, collapse = "|"))
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
  ggtitle("Fox 2009")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
hiMoral.per = table(highMoral_sub)
hiMoral.per = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq))
names(hiMoral.per)[1] = "word"
names(hiMoral.per)[2] = "n"

hiMoralpercent = (hiMoral.per$n)/ttlwrd_fox_2009
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
  ggtitle("Fox News 2009 - High Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
lowMoral_match = str_detect(FOX_09_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowMoral_match, na.rm = TRUE)

FOX09_loMoral = sum(lowMoral_match, na.rm = TRUE)

# Calculate percentage of LM words
perLM_fox_2009 = (FOX09_loMoral/ttlwrd_fox_2009)*100

# Return items that matched
lowMoral_sub = str_subset(FOX_09_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))
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
  ggtitle("Fox 2009")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
loMoral.per = table(lowMoral_sub)
loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq))
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"

loMoralpercent = (loMoral.per$n)/ttlwrd_fox_2009
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
  ggtitle("Fox News 2009 - Low Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

## Adding 2009 values to master datafile
FOX09_rw <- c(2009, "FOX", perHC_fox_2009, perLC_fox_2009, 
              perHM_fox_2009, perLM_fox_2009)

master <- rbind(master, FOX09_rw)

FOX09 <- c(2009, perHC_fox_2009, perLC_fox_2009, 
           perHM_fox_2009, perLM_fox_2009)

fox.df <- rbind(fox.df, FOX09)

################################################################################
############################## FOX YEAR 2010 ###################################
################################################################################
# all files 
FOX_10 = readtext("C:/Users/Michelle/Desktop/test/Fox_2010_*.docx")

## Create columns for date, year, and day for year datafile
FOX_10 <- FOX_10 %>%
  add_column(date = substr(FOX_10$text, 1, 10)) %>%
  add_column(year = substr(FOX_10$text, 7, 10)) %>%
  add_column(day = substr(FOX_10$text, 11, 14)) 
# View(FOX_10)

## Tokenize
FOX_10_tk <- FOX_10 %>% unnest_tokens("word", text)
# View(FOX_10_tk)

## Remove whitespaces
FOX_10_tk$word <- gsub("\\s+","", FOX_10_tk$word)

## Stemming
FOX_10_tk <- FOX_10_tk %>%
  mutate_at("word", funs(wordStem((.), language="en")))

## Remove stop words
data("stop_words")
FOX_10_tk <- FOX_10_tk %>%
  anti_join(stop_words)

## Remove numbers
FOX_10_tk <- FOX_10_tk[-grep("\\b\\d+\\b", FOX_10_tk$word),]

###############################################################################
## Checking against high competence dictionary
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highComp_match = str_detect(FOX_10_tk$word, pattern = paste(highComp_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(highComp_match, na.rm = TRUE)
# View(highComp_match)

FOX10_hiComp = sum(highComp_match, na.rm = TRUE)

# Count total number of words in 2010
ttlwrd_fox_2010 = length(FOX_10_tk$word)

# Calculate percentage of HC words
perHC_fox_2010 = (FOX10_hiComp/ttlwrd_fox_2010)*100

# Return items that matched
highComp_sub = str_subset(FOX_10_tk$word, pattern = paste(highComp_dict, collapse = "|"))
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
  ggtitle("Fox 2010")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
hiComp.per = table(highComp_sub)
hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq))
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"

hiComppercent = (hiComp.per$n)/ttlwrd_fox_2010
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
  ggtitle("Fox News 2010 - High Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

################################################################################
## Checking against low competence dictionary
# str_detect detects patterns in tokenized file from low comp dictionary
# paste takes the dictionary and combines them into one cell
lowComp_match = str_detect(FOX_10_tk$word, pattern = paste(lowComp_dict, collapse = "|"))


# Count number of TRUE (in other words, number of matches)
sum(lowComp_match, na.rm = TRUE)

FOX10_loComp = sum(lowComp_match, na.rm = TRUE)

# Calculate percentage of LC words
perLC_fox_2010 = (FOX10_loComp/ttlwrd_fox_2010)*100

# Return items that matched
lowComp_sub = str_subset(FOX_10_tk$word, pattern = paste(lowComp_dict, collapse = "|"))
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
  ggtitle("Fox 2010")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
loComp.per = table(lowComp_sub)
loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq))
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"

loComppercent = (loComp.per$n)/ttlwrd_fox_2010
rm(loComp.per)

loComp.per = cbind.data.frame(names(loComp.frq), 
                              as.integer(loComp.frq), loComppercent)
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
  ggtitle("Fox News 2010 - Low Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highMoral_match = str_detect(FOX_10_tk$word, pattern = paste(highMoral_dict, collapse = "|"))
# View(highMoral_match)

# Count number of TRUE (in other words, number of matches)
sum(highMoral_match, na.rm = TRUE)

FOX10_hiMoral = sum(highMoral_match, na.rm = TRUE)

# Calculate percentage of HM words
perHM_fox_2010 = (FOX10_hiMoral/ttlwrd_fox_2010)*100

# Return items that matched
highMoral_sub = str_subset(FOX_10_tk$word, pattern = paste(highMoral_dict, collapse = "|"))
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
  ggtitle("Fox 2010")+
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
  ggtitle("Fox News 2010 - High Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)


###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
lowMoral_match = str_detect(FOX_10_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowMoral_match, na.rm = TRUE)

FOX10_loMoral = sum(lowMoral_match, na.rm = TRUE)

# Calculate percentage of LM words
perLM_fox_2010 = (FOX10_loMoral/ttlwrd_fox_2010)*100

# Return items that matched
lowMoral_sub = str_subset(FOX_10_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))
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
  ggtitle("Fox 2010")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
loMoral.per = table(lowMoral_sub)
loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq))
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"

loMoralpercent = (loMoral.per$n)/ttlwrd_fox_2010
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
  ggtitle("Fox News 2010 - Low Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

## Adding 2010 values to master datafile
FOX10_rw <- c(2010, "FOX", perHC_fox_2010, perLC_fox_2010,
              perHM_fox_2010, perLM_fox_2010)

master <- rbind(master, FOX10_rw)

FOX10 <- c(2010, perHC_fox_2010, perLC_fox_2010,
           perHM_fox_2010, perLM_fox_2010)

fox.df <- rbind(fox.df, FOX10)

################################################################################
############################## FOX YEAR 2011 ###################################
################################################################################
# all files 
FOX_11 = readtext("C:/Users/Michelle/Desktop/test/Fox_2011_*.docx")

## Create columns for date, year, and day for year datafile
FOX_11 <- FOX_11 %>%
  add_column(date = substr(FOX_11$text, 1, 10)) %>%
  add_column(year = substr(FOX_11$text, 7, 10)) %>%
  add_column(day = substr(FOX_11$text, 11, 14)) 
# View(FOX_11)

## Tokenize
FOX_11_tk <- FOX_11 %>% unnest_tokens("word", text)
# View(FOX_11_tk)

## Remove whitespaces
FOX_11_tk$word <- gsub("\\s+","", FOX_11_tk$word)

## Stemming
FOX_11_tk <- FOX_11_tk %>%
  mutate_at("word", funs(wordStem((.), language="en")))

## Remove stop words
data("stop_words")
FOX_11_tk <- FOX_11_tk %>%
  anti_join(stop_words)

## Remove numbers
FOX_11_tk <- FOX_11_tk[-grep("\\b\\d+\\b", FOX_11_tk$word),]

###############################################################################
## Checking against high competence dictionary
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highComp_match = str_detect(FOX_11_tk$word, 
                            pattern = paste(highComp_dict, 
                                            collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(highComp_match, na.rm = TRUE)
# View(highComp_match)

FOX11_hiComp = sum(highComp_match, na.rm = TRUE)

# Count total number of words in 2011
ttlwrd_fox_2011 = length(FOX_11_tk$word)

# Calculate percentage of HC words
perHC_fox_2011 = (FOX11_hiComp/ttlwrd_fox_2011)*100

# Return items that matched
highComp_sub = str_subset(FOX_11_tk$word, pattern = paste(highComp_dict, collapse = "|"))
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
  ggtitle("Fox 2011")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
hiComp.per = table(highComp_sub)
hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq))
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"

hiComppercent = (hiComp.per$n)/ttlwrd_fox_2011
rm(hiComp.per)

hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq), hiComppercent)
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"
names(hiComp.per)[3] = "percentage"

# plot frequency of words in percentage
ows_fox_hc <- ggplot(hiComp.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("Fox News 2011 - High Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

################################################################################
## Checking against low competence dictionary
# str_detect detects patterns in tokenized file from low comp dictionary
# paste takes the dictionary and combines them into one cell
lowComp_match = str_detect(FOX_11_tk$word, 
                           pattern = paste(lowComp_dict, collapse = "|"))


# Count number of TRUE (in other words, number of matches)
sum(lowComp_match, na.rm = TRUE)

FOX11_loComp = sum(lowComp_match, na.rm = TRUE)

# Calculate percentage of LC words
perLC_fox_2011 = (FOX11_loComp/ttlwrd_fox_2011)*100

# Return items that matched
lowComp_sub = str_subset(FOX_11_tk$word, pattern = paste(lowComp_dict, collapse = "|"))
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
  ggtitle("Fox 2011")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
loComp.per = table(lowComp_sub)
loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq))
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"

loComppercent = (loComp.per$n)/ttlwrd_fox_2011
rm(loComp.per)

loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq), loComppercent)
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"
names(loComp.per)[3] = "percentage"

# plot frequency of words in percentage
ows_fox_lc <- ggplot(loComp.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("Fox News 2011 - Low Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highMoral_match = str_detect(FOX_11_tk$word, 
                             pattern = paste(highMoral_dict, collapse = "|"))
# View(highMoral_match)

# Count number of TRUE (in other words, number of matches)
sum(FOX_match, na.rm = TRUE)

FOX11_hiMoral = sum(highMoral_match, na.rm = TRUE)

# Calculate percentage of HM words
perHM_fox_2011 = (FOX11_hiMoral/ttlwrd_fox_2011)*100

# Return items that matched
highMoral_sub = str_subset(FOX_11_tk$word, pattern = paste(highMoral_dict, collapse = "|"))
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
  ggtitle("Fox 2011")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

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
ows_fox_hm <- ggplot(hiMoral.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("Fox News 2011 - High Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
lowMoral_match = str_detect(FOX_11_tk$word, 
                            pattern = paste(lowMoral_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowMoral_match, na.rm = TRUE)

FOX11_loMoral = sum(lowMoral_match, na.rm = TRUE)

# Calculate percentage of LM words
perLM_fox_2011 = (FOX11_loMoral/ttlwrd_fox_2011)*100

# Return items that matched
lowMoral_sub = str_subset(FOX_11_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))
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
  ggtitle("Fox 2011")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
loMoral.per = table(lowMoral_sub)
loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq))
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"

loMoralpercent = (loMoral.per$n)/ttlwrd_fox_2011
rm(loMoral.per)

loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq), loMoralpercent)
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"
names(loMoral.per)[3] = "percentage"

# plot frequency of words in percentage
ows_fox_lm <- ggplot(loMoral.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("Fox News 2011 - Low Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

## Adding 2011 values to master datafile
FOX11_rw <- c(2011, "FOX", perHC_fox_2011, perLC_fox_2011,
              perHM_fox_2011, perLM_fox_2011)

master <- rbind(master, FOX11_rw)

FOX11 <- c(2011, perHC_fox_2011, perLC_fox_2011,
           perHM_fox_2011, perLM_fox_2011)

fox.df <- rbind(fox.df, FOX11)

################################################################################
############################## FOX YEAR 2012 ###################################
################################################################################
# all files 
FOX_12 = readtext("C:/Users/Michelle/Desktop/test/Fox_2012_*.docx")

## Create columns for date, year, and day for year datafile
FOX_12 <- FOX_12 %>%
  add_column(date = substr(FOX_12$text, 1, 10)) %>%
  add_column(year = substr(FOX_12$text, 7, 10)) %>%
  add_column(day = substr(FOX_12$text, 11, 14)) 
# View(FOX_12)

## Tokenize
FOX_12_tk <- FOX_12 %>% unnest_tokens("word", text)
# View(FOX_12_tk)

## Remove whitespaces
FOX_12_tk$word <- gsub("\\s+","", FOX_12_tk$word)

## Stemming
FOX_12_tk <- FOX_12_tk %>%
  mutate_at("word", funs(wordStem((.), language="en")))

## Remove stop words
data("stop_words")
FOX_12_tk <- FOX_12_tk %>%
  anti_join(stop_words)

## Remove numbers
FOX_12_tk <- FOX_12_tk[-grep("\\b\\d+\\b", FOX_12_tk$word),]

###############################################################################
## Checking against high competence dictionary
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highComp_match = str_detect(FOX_12_tk$word, pattern = paste(highComp_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(highComp_match, na.rm = TRUE)
# View(highComp_match)

FOX12_hiComp = sum(highComp_match, na.rm = TRUE)

# Count total number of words in 2012
ttlwrd_fox_2012 = length(FOX_12_tk$word)

# Calculate percentage of HC words
perHC_fox_2012 = (FOX12_hiComp/ttlwrd_fox_2012)*100

# Return items that matched
highComp_sub = str_subset(FOX_12_tk$word, pattern = paste(highComp_dict, collapse = "|"))
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
  ggtitle("Fox 2012")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
hiComp.per = table(highComp_sub)
hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq))
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"

hiComppercent = (hiComp.per$n)/ttlwrd_fox_2012
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
  ggtitle("Fox News 2012 - High Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

################################################################################
## Checking against low competence dictionary
# str_detect detects patterns in tokenized file from low comp dictionary
# paste takes the dictionary and combines them into one cell
lowComp_match = str_detect(FOX_12_tk$word, pattern = paste(lowComp_dict, collapse = "|"))


# Count number of TRUE (in other words, number of matches)
sum(lowComp_match, na.rm = TRUE)

FOX12_loComp = sum(lowComp_match, na.rm = TRUE)

# Calculate percentage of LC words
perLC_fox_2012 = (FOX12_loComp/ttlwrd_fox_2012)*100

# Return items that matched
lowComp_sub = str_subset(FOX_12_tk$word, pattern = paste(lowComp_dict, collapse = "|"))
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
  ggtitle("Fox 2012")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)


# transform n to percentage over total words
loComp.per = table(lowComp_sub)
loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq))
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"

loComppercent = (loComp.per$n)/ttlwrd_fox_2012
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
  ggtitle("Fox News 2012 - Low Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highMoral_match = str_detect(FOX_12_tk$word, pattern = paste(highMoral_dict, collapse = "|"))
# View(highMoral_match)

# Count number of TRUE (in other words, number of matches)
sum(FOX_match, na.rm = TRUE)

FOX12_hiMoral = sum(highMoral_match, na.rm = TRUE)

# Calculate percentage of HM words
perHM_fox_2012 = (FOX12_hiMoral/ttlwrd_fox_2012)*100

# Return items that matched
highMoral_sub = str_subset(FOX_12_tk$word, pattern = paste(highMoral_dict, collapse = "|"))
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
  ggtitle("Fox 2012")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
hiMoral.per = table(highMoral_sub)
hiMoral.per = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq))
names(hiMoral.per)[1] = "word"
names(hiMoral.per)[2] = "n"

hiMoralpercent = (hiMoral.per$n)/ttlwrd_fox_2012
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
lowMoral_match = str_detect(FOX_12_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowMoral_match, na.rm = TRUE)

FOX12_loMoral = sum(lowMoral_match, na.rm = TRUE)

# Calculate percentage of LM words
perLM_fox_2012 = (FOX12_loMoral/ttlwrd_fox_2012)*100

# Return items that matched
lowMoral_sub = str_subset(FOX_12_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))
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
  ggtitle("Fox 2012")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
loMoral.per = table(lowMoral_sub)
loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq))
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"

loMoralpercent = (loMoral.per$n)/ttlwrd_fox_2012
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
  ggtitle("Fox News 2012 - Low Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)


## Adding 2012 values to master datafile
FOX12_rw <- c(2012, "FOX", perHC_fox_2012, perLC_fox_2012,
              perHM_fox_2012, perLM_fox_2012)

master <- rbind(master, FOX12_rw)

FOX12 <- c(2012, perHC_fox_2012, perLC_fox_2012,
           perHM_fox_2012, perLM_fox_2012)

fox.df <- rbind(fox.df, FOX12)

################################################################################
############################## FOX YEAR 2013 ###################################
################################################################################
# all files 
FOX_13 = readtext("C:/Users/Michelle/Desktop/test/Fox_2013_*.docx")

## Create columns for date, year, and day for year datafile
FOX_13 <- FOX_13 %>%
  add_column(date = substr(FOX_13$text, 1, 10)) %>%
  add_column(year = substr(FOX_13$text, 7, 10)) %>%
  add_column(day = substr(FOX_13$text, 11, 14)) 
# View(FOX_13)

## Tokenize
FOX_13_tk <- FOX_13 %>% unnest_tokens("word", text)
# View(FOX_13_tk)

## Remove whitespaces
FOX_13_tk$word <- gsub("\\s+","", FOX_13_tk$word)

## Stemming
FOX_13_tk <- FOX_13_tk %>%
  mutate_at("word", funs(wordStem((.), language="en")))

## Remove stop words
data("stop_words")
FOX_13_tk <- FOX_13_tk %>%
  anti_join(stop_words)

## Remove numbers
FOX_13_tk <- FOX_13_tk[-grep("\\b\\d+\\b", FOX_13_tk$word),]

###############################################################################
## Checking against high competence dictionary
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highComp_match = str_detect(FOX_13_tk$word, pattern = paste(highComp_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(highComp_match, na.rm = TRUE)
# View(highComp_match)

FOX13_hiComp = sum(highComp_match, na.rm = TRUE)

# Count total number of words in 2013
ttlwrd_fox_2013 = length(FOX_13_tk$word)

# Calculate percentage of HC words
perHC_fox_2013 = (FOX13_hiComp/ttlwrd_fox_2013)*100

# Return items that matched
highComp_sub = str_subset(FOX_13_tk$word, pattern = paste(highComp_dict, collapse = "|"))
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
  ggtitle("Fox 2013")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
hiComp.per = table(highComp_sub)
hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq))
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"

hiComppercent = (hiComp.per$n)/ttlwrd_fox_2013
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
  ggtitle("Fox News 2013 - High Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

################################################################################
## Checking against low competence dictionary
# str_detect detects patterns in tokenized file from low comp dictionary
# paste takes the dictionary and combines them into one cell
lowComp_match = str_detect(FOX_13_tk$word, pattern = paste(lowComp_dict, collapse = "|"))


# Count number of TRUE (in other words, number of matches)
sum(lowComp_match, na.rm = TRUE)

FOX13_loComp = sum(lowComp_match, na.rm = TRUE)

# Calculate percentage of LC words
perLC_fox_2013 = (FOX13_loComp/ttlwrd_fox_2013)*100

# Return items that matched
lowComp_sub = str_subset(FOX_13_tk$word, pattern = paste(lowComp_dict, collapse = "|"))
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
  ggtitle("Fox 2013")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
loComp.per = table(lowComp_sub)
loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq))
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"

loComppercent = (loComp.per$n)/ttlwrd_fox_2013
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
  ggtitle("Fox News 2013 - Low Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highMoral_match = str_detect(FOX_13_tk$word, pattern = paste(highMoral_dict, collapse = "|"))
# View(highMoral_match)

# Count number of TRUE (in other words, number of matches)
sum(highMoral_match, na.rm = TRUE)

FOX13_hiMoral = sum(highMoral_match, na.rm = TRUE)

# Calculate percentage of HM words
perHM_fox_2013 = (FOX13_hiMoral/ttlwrd_fox_2013)*100

# Return items that matched
highMoral_sub = str_subset(FOX_13_tk$word, pattern = paste(highMoral_dict, collapse = "|"))
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
  ggtitle("Fox 2013")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
hiMoral.per = table(highMoral_sub)
hiMoral.per = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq))
names(hiMoral.per)[1] = "word"
names(hiMoral.per)[2] = "n"

hiMoralpercent = (hiMoral.per$n)/ttlwrd_fox_2013
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
  ggtitle("Fox News 2013 - High Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
lowMoral_match = str_detect(FOX_13_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowMoral_match, na.rm = TRUE)

FOX13_loMoral = sum(lowMoral_match, na.rm = TRUE)

# Calculate percentage of LM words
perLM_fox_2013 = (FOX13_loMoral/ttlwrd_fox_2013)*100

# Return items that matched
lowMoral_sub = str_subset(FOX_13_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))
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
ggplot(hiMoral.no, aes(x= reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequency of High Morality Words")+
  xlab("")+
  ggtitle("Fox 2013")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
loMoral.per = table(lowMoral_sub)
loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq))
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"

loMoralpercent = (loMoral.per$n)/ttlwrd_fox_2013
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
  ggtitle("Fox News 2013 - Low Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

## Adding 2013 values to master datafile
FOX13_rw <- c(2013, "FOX", perHC_fox_2013, perLC_fox_2013, 
              perHM_fox_2013, perLM_fox_2013)

master <- rbind(master, FOX13_rw)

FOX13 <- c(2013, perHC_fox_2013, perLC_fox_2013, 
           perHM_fox_2013, perLM_fox_2013)

fox.df <- rbind(fox.df, FOX13)

################################################################################
################################## FOX ALL #####################################
################################################################################
# all files 
Fox_all = readtext("C:/Users/Michelle/Desktop/test/Fox_*.docx")

## Create columns for date, year, and day for year datafile
Fox_all <- Fox_all %>%
  add_column(date = substr(Fox_all$text, 1, 10)) %>%
  add_column(year = substr(Fox_all$text, 7, 10)) %>%
  add_column(day = substr(Fox_all$text, 11, 14)) 
# View(MSNBC_06)

## Tokenize
Fox_all_tk <- Fox_all %>% unnest_tokens("word", text)
# View(MSNBC_06_tk)

## Remove whitespaces
Fox_all_tk$word <- gsub("\\s+","", Fox_all_tk$word)

## Stemming
Fox_all_tk <- Fox_all_tk %>%
  mutate_at("word", funs(wordStem((.), language="en")))

## Remove stop words
data("stop_words")
Fox_all_tk <- Fox_all_tk %>%
  anti_join(stop_words)

## Remove numbers
Fox_all_tk <- Fox_all_tk[-grep("\\b\\d+\\b", Fox_all_tk$word),]


###############################################################################
## Checking against high competence dictionary
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highComp_all_Fox_match = str_detect(Fox_all_tk$word, pattern = paste(highComp_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(highComp_all_Fox_match, na.rm = TRUE)
# View(highComp_match)

Fox_all_hiComp = sum(highComp_all_Fox_match, na.rm = TRUE)

# Count total number of words in all of Fox
ttlwrd_fox_all = length(Fox_all_tk$word)

# Calculate percentage of HC words
perHC_fox_all = (Fox_all_hiComp/ttlwrd_fox_all)*100

# str_which detects which words matched in tokenized column
# highComp_whr = str_which(MSNBC_06_tk$word, paste(highComp_dict, collapse = "|"))
# View(highComp_whr)

# str_extract pulls out which words in tokenized file matched
# highComp_ext = str_extract(MSNBC_06_tk$word, paste(highComp_dict, collapse = "|"))
# View(highComp_ext)

# Return items that matched
highComp_all_Fox_sub = str_subset(Fox_all_tk$word, 
                                  pattern = paste(highComp_dict, 
                                                  collapse = "|"))
highComp_all_Fox_sub

# create a table to count freq of words and put that into another dataframe
hiComp_all_Fox.frq = table(highComp_all_Fox_sub)
hiComp_all_Fox.no = cbind.data.frame(names(hiComp_all_Fox.frq), as.integer(hiComp_all_Fox.frq))
names(hiComp_all_Fox.no)[1] = "word"
names(hiComp_all_Fox.no)[2] = "n"


## Graphing top 20 high competence words
hiComp_all_Fox.no = hiComp_all_Fox.no[with(hiComp_all_Fox.no, order(n, decreasing = TRUE)),]
top_20_Fox_hc = hiComp_all_Fox.no[1:20,]

# plot frequency of words
fox_hc <- ggplot(top_20_Fox_hc, aes(x= reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Count")+
  xlab("")+
  ggtitle("Top 20 High Competence Words for Fox")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
hiComp.all.per = table(highComp_all_Fox_sub)
hiComp.all.per = cbind.data.frame(names(hiComp_all_Fox.frq), 
                                  as.integer(hiComp_all_Fox.frq))
names(hiComp.all.per)[1] = "word"
names(hiComp.all.per)[2] = "n"

hiComppercent.all = (hiComp.all.per$n)/ttlwrd_fox_all
rm(hiComp.all.per)

hiComp.all.per = cbind.data.frame(names(hiComp_all_Fox.frq), 
                                  as.integer(hiComp_all_Fox.frq), 
                                  hiComppercent.all)
names(hiComp.all.per)[1] = "word"
names(hiComp.all.per)[2] = "n"
names(hiComp.all.per)[3] = "percentage"

# plot frequency of words in percentage
perHC_fox <- ggplot(hiComp.all.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("")+
  xlab("")+
  ggtitle("Fox News")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)


################################################################################
## Checking against low competence dictionary
# str_detect detects patterns in tokenized file from low comp dictionary
# paste takes the dictionary and combines them into one cell
lowComp_all_Fox_match = str_detect(Fox_all_tk$word, pattern = paste(lowComp_dict, collapse = "|"))


# Count number of TRUE (in other words, number of matches)
sum(lowComp_all_Fox_match, na.rm = TRUE)

Fox_all_loComp = sum(lowComp_all_Fox_match, na.rm = TRUE)

# Calculate percentage of LC words
perLC_fox_all = (Fox_all_loComp/ttlwrd_fox_all)*100

# Return items that matched
lowComp_all_Fox_sub = str_subset(Fox_all_tk$word, pattern = paste(lowComp_dict, collapse = "|"))
lowComp_all_Fox_sub
# View(lowComp_sub)

# create dataframe to count frequency of words
loComp_all_Fox.frq = table(lowComp_all_Fox_sub)
loComp_all_Fox.no = cbind.data.frame(names(loComp_all_Fox.frq), as.integer(loComp_all_Fox.frq))
names(loComp_all_Fox.no)[1] = "word"
names(loComp_all_Fox.no)[2] = "n"

## Graphing top 20 high competence words
loComp_all_Fox.no = loComp_all_Fox.no[with(loComp_all_Fox.no, order(n, decreasing = TRUE)),]
top_20_Fox_lc = loComp_all_Fox.no[1:20,]

# plot frequency of words
fox_lc <- ggplot(top_20_Fox_lc, aes(x= reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Count")+
  xlab("")+
  ggtitle("Top 20 Low Competence Words for Fox")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
loComp.all.per = table(lowComp_all_Fox_sub)
loComp.all.per = cbind.data.frame(names(loComp_all_Fox.frq), 
                                  as.integer(loComp_all_Fox.frq))
names(loComp.all.per)[1] = "word"
names(loComp.all.per)[2] = "n"

loComppercent.all = (loComp.all.per$n)/ttlwrd_fox_all
rm(loComp.all.per)

loComp.all.per = cbind.data.frame(names(loComp_all_Fox.frq), 
                                  as.integer(loComp_all_Fox.frq), 
                                  loComppercent.all)
names(loComp.all.per)[1] = "word"
names(loComp.all.per)[2] = "n"
names(loComp.all.per)[3] = "percentage"

# plot frequency of words in percentage
perLC_fox <- ggplot(loComp.all.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("")+
  xlab("")+
  ggtitle("Fox News")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highMoral_all_Fox_match = str_detect(Fox_all_tk$word, 
                                     pattern = paste(highMoral_dict, 
                                                     collapse = "|"))
# View(highMoral_match)

# Count number of TRUE (in other words, number of matches)
sum(highMoral_all_Fox_match, na.rm = TRUE)

Fox_all_hiMoral = sum(highMoral_all_Fox_match, na.rm = TRUE)

# Calculate percentage of HM words
perHM_fox_all = (Fox_all_hiMoral/ttlwrd_fox_all)*100

# Return items that matched
highMoral_all_Fox_sub = str_subset(Fox_all_tk$word, pattern = paste(highMoral_dict, collapse = "|"))
highMoral_all_Fox_sub
# View(highMoral_sub)

# create a table to count freq of words and put that into another dataframe
hiMoral_all_Fox.frq = table(highMoral_all_Fox_sub)
hiMoral_all_Fox.no = cbind.data.frame(names(hiMoral_all_Fox.frq), as.integer(hiMoral_all_Fox.frq))
names(hiMoral_all_Fox.no)[1] = "word"
names(hiMoral_all_Fox.no)[2] = "n"

## Graphing top 20 high competence words
hiMoral_all_Fox.no = hiMoral_all_Fox.no[with(hiMoral_all_Fox.no, 
                                             order(n, decreasing = TRUE)),]
top_20_Fox_hm = hiMoral_all_Fox.no[1:20,]

# plot frequency of words
fox_hm <- ggplot(top_20_Fox_hm, aes(x= reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Count")+
  xlab("")+
  ggtitle("Top 20 High Morality Words for Fox")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
hiMoral.all.per = table(highMoral_all_Fox_sub)
hiMoral.all.per = cbind.data.frame(names(hiMoral_all_Fox.frq), 
                                   as.integer(hiMoral_all_Fox.frq))
names(hiMoral.all.per)[1] = "word"
names(hiMoral.all.per)[2] = "n"

hiMoralpercent.all = (hiMoral.all.per$n)/ttlwrd_fox_all
rm(hiMoral.all.per)

hiMoral.all.per = cbind.data.frame(names(hiMoral_all_Fox.frq), 
                                   as.integer(hiMoral_all_Fox.frq), 
                                   hiMoralpercent.all)
names(hiMoral.all.per)[1] = "word"
names(hiMoral.all.per)[2] = "n"
names(hiMoral.all.per)[3] = "percentage"

# plot frequency of words in percentage
perHM_fox <- ggplot(hiMoral.all.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("")+
  xlab("")+
  ggtitle("Fox News")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
lowMoral_all_Fox_match = str_detect(Fox_all_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowMoral_all_Fox_match, na.rm = TRUE)

Fox_all_loMoral = sum(lowMoral_all_Fox_match, na.rm = TRUE)

# Calculate percentage of LM words
perLM_fox_all = (Fox_all_loMoral/ttlwrd_fox_all)*100

# Return items that matched
lowMoral_all_Fox_sub = str_subset(Fox_all_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))
lowMoral_all_Fox_sub

# create a table to count freq of words and put that into another dataframe
loMoral_all_Fox.frq = table(lowMoral_all_Fox_sub)
loMoral_all_Fox.no = cbind.data.frame(names(loMoral_all_Fox.frq), as.integer(loMoral_all_Fox.frq))
names(loMoral_all_Fox.no)[1] = "word"
names(loMoral_all_Fox.no)[2] = "n"

## Graphing top 20 high competence words
loMoral_all_Fox.no = loMoral_all_Fox.no[with(loMoral_all_Fox.no, order(n, decreasing = TRUE)),]
top_20_Fox_lm = loMoral_all_Fox.no[1:20,]

# plot frequency of words
fox_lm <- ggplot(top_20_Fox_lm, aes(x= reorder(word, -n), y=n, fill=word))+
  scale_color_brewer(palette = "YlOrRd")+
  geom_bar(stat="identity")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Count")+
  xlab("")+
  ggtitle("Top 20 Low Morality Words for Fox")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

# transform n to percentage over total words
loMoral.all.per = table(lowMoral_all_Fox_sub)
loMoral.all.per = cbind.data.frame(names(loMoral_all_Fox.frq), 
                                   as.integer(loMoral_all_Fox.frq))
names(loMoral.all.per)[1] = "word"
names(loMoral.all.per)[2] = "n"

loMoralpercent.all = (loMoral.all.per$n)/ttlwrd_fox_all
rm(loMoral.all.per)

loMoral.all.per = cbind.data.frame(names(loMoral_all_Fox.frq), 
                                   as.integer(loMoral_all_Fox.frq), 
                                   loMoralpercent.all)
names(loMoral.all.per)[1] = "word"
names(loMoral.all.per)[2] = "n"
names(loMoral.all.per)[3] = "percentage"

# plot frequency of words in percentage
perLM_fox <- ggplot(loMoral.all.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("")+
  xlab("")+
  ggtitle("Fox News")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)



## Adding values to datafile
FoxNews_DF <- c("Fox", perHC_fox_all, perLC_fox_all, 
                        perHM_fox_all, perLM_fox_all)

Ideo_comp <- rbind(Ideo_comp, FoxNews_DF)



write.csv(Ideo_comp, "C:\\Users\\Michelle\\Desktop\\Ideo_comp.csv", row.names = TRUE)

################################################################
fox_top <- ggarrange(perHC_fox, perLC_fox, perHM_fox, perLM_fox,
                       ncol = 2, nrow = 2,
                       common.legend = TRUE, legend = "bottom")
fox_top

hc_compare <- ggarrange(perHC_fox, perHC_MSNBC,
                        common.legend = TRUE, legend = "bottom")
title <- expression(atop(bold("High Competence"), scriptstyle("Percentage of Word Count")))
annotate_figure(hc_compare,
                top=text_grob(title))

lc_compare <- ggarrange(perLC_fox, perLC_MSNBC,
                        common.legend = TRUE, legend = "bottom")
title <- expression(atop(bold("Low Competence"), scriptstyle("Percentage of Word Count")))
annotate_figure(lc_compare,
                top=text_grob(title))

hm_compare <- ggarrange(perHM_fox, perHM_MSNBC,
                        common.legend = TRUE, legend = "bottom")
title <- expression(atop(bold("High Morality"), scriptstyle("Percentage of Word Count")))
annotate_figure(hm_compare,
                top=text_grob(title))

lm_compare <- ggarrange(perLM_fox, perLM_MSNBC,
                        common.legend = TRUE, legend = "bottom")
title <- expression(atop(bold("Low Morality"), scriptstyle("Percentage of Word Count")))
annotate_figure(lm_compare,
                top=text_grob(title))

###############################################################################
############################## OTHER GRAPHS ###################################
###############################################################################
## Convert master into dataframe
master.df = as.data.frame(master)
master.df <- master.df %>%
  add_column(index = 1:16)

master.df$LowComp = as.numeric(as.character(master.df$LowComp))
master.df$HiComp = as.numeric(as.character(master.df$HiComp))
master.df$LowMoral = as.numeric(as.character(master.df$LowMoral))
master.df$HiMoral = as.numeric(as.character(master.df$HiMoral))

###############################################################################  
## ggplots HIGH COMPETENCY
HiCompg <- ggplot(data = master.df, aes(y = HiComp, x = Year)) +
  geom_line(aes(group = Source, color = Source), size = 1) +
  ylab("Percentage") + xlab("Year") +
  ggtitle("High Competency") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  theme(axis.text=element_text(size = 14))

## ggplots LOW COMPETENCY
LoCompg <- ggplot(data = master.df, aes(y = LowComp, x = Year)) +
  geom_line(aes(group = Source, color = Source), size = 1) +
  ylab("Percentage") + xlab("Year") +
  ggtitle("Low Competency") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  theme(axis.text=element_text(size= 14))
  
## ggplots HIGH MORALITY
HiMoralg <- ggplot(data = master.df, aes(y = HiMoral, x = Year)) +
  geom_line(aes(group = Source, color = Source), size = 1) +
  ylab("Percentage") + xlab("Year") +
  ggtitle("High Morality") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  theme(axis.text=element_text(size = 14))

## ggplots LOW MORALITY
LoMoralg <- ggplot(data = master.df, aes(y = LowMoral, x = Year)) +
  geom_line(aes(group = Source, color = Source), size = 1) +
  ylab("Percentage") + xlab("Year") +
  ggtitle("Low Morality") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  theme(axis.text=element_text(size = 14))

fig4 <- ggarrange(HiCompg, LoCompg, HiMoralg, LoMoralg,
                    ncol = 2, nrow = 2,
                    common.legend = TRUE, legend = "bottom")
fig4

############################################################################### 
both_hc <- ggarrange(msnbc_hc, fox_hc,
                     ncol = 2, nrow = 1,
                     common.legend = TRUE, legend = "bottom")
both_hc

both_lc <- ggarrange(msnbc_lc, fox_lc, 
                     ncol = 2, nrow = 1,
                     common.legend = TRUE, legend = "bottom")
both_lc

both_hm <- ggarrange(msnbc_hm, fox_hm, 
                     ncol = 2, nrow = 1,
                     common.legend = TRUE, legend = "bottom")
both_hm

both_lm <- ggarrange(msnbc_lm, fox_lm, 
                     ncol = 2, nrow = 1,
                     common.legend = TRUE, legend = "bottom")
both_lm

write.csv(master, "C:\\Users\\Michelle\\Desktop\\Fox_MSNBC.csv", row.names = TRUE)


  