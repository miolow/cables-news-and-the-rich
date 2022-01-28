################################################################################
############################### ALL YEAR 2007 #################################
################################################################################
# all files 
all_07 = readtext("C:/Users/Michelle/Desktop/ContentAnalysis/Data/ByYear/2007/*.docx")

## Create columns for date, year, and day for year datafile
all_07 <- all_07 %>%
  add_column(date = substr(all_07$text, 1, 10)) %>%
  add_column(year = substr(all_07$text, 7, 10)) %>%
  add_column(day = substr(all_07$text, 11, 14)) 


## Tokenize
all_07_tk <- all_07 %>% unnest_tokens("word", text)

## Remove whitespaces
all_07_tk$word <- gsub("\\s+","", all_07_tk$word)

## Stemming
all_07_tk <- all_07_tk %>%
  mutate_at("word", funs(wordStem((.), language="en")))

## Remove stop words
data("stop_words")
all_07_tk <- all_07_tk %>%
  anti_join(stop_words)

## Remove numbers
all_07_tk <- all_07_tk[-grep("\\b\\d+\\b", all_07_tk$word),]


###############################################################################
## Checking against high competence dictionary
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highComp_match = str_detect(all_07_tk$word, 
                            pattern = paste(highComp_dict, 
                                            collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(highComp_match, na.rm = TRUE)
# View(highComp_match)

all07_hiComp = sum(highComp_match, na.rm = TRUE)

# Count total number of words in 2007
ttlwrd_all_2007 = length(all_07_tk$word)

# Calculate percentage of HC words
perHC_all_2007 = (all07_hiComp/ttlwrd_all_2007)*100

# Return items that matched
highComp_sub = str_subset(all_07_tk$word, pattern = paste(highComp_dict, collapse = "|"))
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


# transform n to percentage over total words
hiComp.per = table(highComp_sub)
hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq))
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"

hiComppercent = (hiComp.per$n)/ttlwrd_all_2007
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
  ggtitle("Fox News & MSNBC 2007 - High Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

################################################################################
## Checking against low competence dictionary
# str_detect detects patterns in tokenized file from low comp dictionary
# paste takes the dictionary and combines them into one cell
lowComp_match = str_detect(all_07_tk$word, 
                           pattern = paste(lowComp_dict, collapse = "|"))


# Count number of TRUE (in other words, number of matches)
sum(lowComp_match, na.rm = TRUE)

all07_loComp = sum(lowComp_match, na.rm = TRUE)

# Calculate percentage of HC words
perLC_all_2007 = (all07_loComp/ttlwrd_all_2007)*100

# Return items that matched
lowComp_sub = str_subset(all_07_tk$word, 
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

# # plot frequency of words
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

loComppercent = (loComp.per$n)/ttlwrd_all_2007
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
  ggtitle("Fox News & MSNBC 2007 - Low Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highMoral_match = str_detect(all_07_tk$word, 
                             pattern = paste(highMoral_dict, 
                                             collapse = "|"))
# View(highMoral_match)

# Count number of TRUE (in other words, number of matches)
sum(highMoral_match, na.rm = TRUE)

all07_hiMoral = sum(highMoral_match, na.rm = TRUE)

# Calculate percentage of HC words
perHM_all_2007 = (all07_hiMoral/ttlwrd_all_2007)*100

# Return items that matched
highMoral_sub = str_subset(all_07_tk$word, 
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

# # plot frequency of words
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

hiMoralpercent = (hiMoral.per$n)/ttlwrd_all_2007
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
  ggtitle("Fox News & MSNBC 2007 - High Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
lowMoral_match = str_detect(all_07_tk$word, 
                            pattern = paste(lowMoral_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowMoral_match, na.rm = TRUE)

all07_loMoral = sum(lowMoral_match, na.rm = TRUE)

# Calculate percentage of LM words
perLM_all_2007 = (all07_loMoral/ttlwrd_all_2007)*100

# Return items that matched
lowMoral_sub = str_subset(all_07_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))
lowMoral_sub

# create a table to count freq of words and put that into another dataframe
loMoral.frq = table(lowMoral_sub)
loMoral.no = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq))
names(loMoral.no)[1] = "word"
names(loMoral.no)[2] = "n"

loMoral.no$word = factor(loMoral.no$word,
                         levels = loMoral.no$word[order(loMoral.no$word, 
                                                        decreasing = FALSE)])


# transform n to percentage over total words
loMoral.per = table(lowMoral_sub)
loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq))
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"

loMoralpercent = (loMoral.per$n)/ttlwrd_all_2007
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
  ggtitle("Fox News & MSNBC 2007 - Low Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)


## Adding 2007 values to master datafile
all07_rw <- c(2007, perHC_all_2007, perLC_all_2007, 
                perHM_all_2007, perLM_all_2007)

masterALL <- rbind(masterALL, all07_rw)

all07 <- c(2007, perHC_all_2007, perLC_all_2007, 
             perHM_all_2007, perLM_all_2007)

all.df <- rbind(all.df, all07)

## Saving top words
highComp_07 <- highComp_sub
lowComp_07 <- lowComp_sub 
highMoral_07 <- highMoral_sub 
lowMoral_07 <- lowMoral_sub
