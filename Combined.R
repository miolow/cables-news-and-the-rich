################################################################################
# read all text files
# all files 
fullfiles = readtext("C:/Users/Michelle/Desktop/test/*.docx")

## Create columns for date, year, and day for year datafile
fullfiles <- fullfiles %>%
  add_column(date = substr(fullfiles$text, 1, 10)) %>%
  add_column(year = substr(fullfiles$text, 7, 10)) %>%
  add_column(day = substr(fullfiles$text, 11, 14))

# View(fullfiles)


## Tokenize
fullfiles_tk <- fullfiles %>% unnest_tokens("word", text)
View(fullfiles_tk)

## Remove whitespaces
fullfiles_tk$word <- gsub("\\s+","", fullfiles_tk$word)

## Stemming
fullfiles_tk <- fullfiles_tk %>%
  mutate_at("word", funs(wordStem((.), language="en")))

## Remove stop words
data("stop_words")
fullfiles_tk <- fullfiles_tk %>%
  anti_join(stop_words)

## Remove numbers
fullfiles_tk <- fullfiles_tk[-grep("\\b\\d+\\b", fullfiles_tk$word),]


###############################################################################
## Checking against high competence dictionary
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highComp_match = str_detect(fullfiles_tk$word, 
                            pattern = paste(highComp_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(highComp_match, na.rm = TRUE)
# View(highComp_match)

fullfiles_hiComp = sum(highComp_match, na.rm = TRUE)

# Count total number of words
ttlwrd_fullfiles = length(fullfiles_tk$word)

# Calculate percentage of HC words
perHC_fullfiles = (fullfiles_hiComp/ttlwrd_fullfiles)*100

# Return items that matched
highComp_sub = str_subset(fullfiles_tk$word, pattern = paste(highComp_dict, collapse = "|"))
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

hiComppercent = (hiComp.per$n)/ttlwrd_fullfiles
rm(hiComp.per)

hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq), hiComppercent)
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"
names(hiComp.per)[3] = "percentage"

View(hiComp.per)

# plot frequency of words in percentage
f1<- ggplot(hiComp.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("High Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

f1
################################################################################
## Checking against low competence dictionary
# str_detect detects patterns in tokenized file from low comp dictionary
# paste takes the dictionary and combines them into one cell
lowComp_match = str_detect(fullfiles_tk$word, pattern = paste(lowComp_dict, collapse = "|"))


# Count number of TRUE (in other words, number of matches)
sum(lowComp_match, na.rm = TRUE)

fullfiles_loComp = sum(lowComp_match, na.rm = TRUE)

# Calculate percentage of HC words
perLC_fullfiles = (fullfiles_loComp/ttlwrd_fullfiles)*100

# Return items that matched
lowComp_sub = str_subset(fullfiles_tk$word, pattern = paste(lowComp_dict, collapse = "|"))
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

# transform n to percentage over total words
loComp.per = table(lowComp_sub)
loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq))
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"

loComppercent = (loComp.per$n)/ttlwrd_fullfiles
rm(loComp.per)

loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq), loComppercent)
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"
names(loComp.per)[3] = "percentage"

# plot frequency of words in percentage
f2 <- ggplot(loComp.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("Low Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highMoral_match = str_detect(fullfiles_tk$word, 
                             pattern = paste(highMoral_dict, 
                                             collapse = "|"))
# View(highMoral_match)

# Count number of TRUE (in other words, number of matches)
sum(highMoral_match, na.rm = TRUE)

fullfiles_hiMoral = sum(highMoral_match, na.rm = TRUE)

# Calculate percentage of HC words
perHM_fullfiles = (fullfiles_hiMoral/ttlwrd_fullfiles)*100

# Return items that matched
highMoral_sub = str_subset(fullfiles_tk$word, 
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

# transform n to percentage over total words
hiMoral.per = table(highMoral_sub)
hiMoral.per = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq))
names(hiMoral.per)[1] = "word"
names(hiMoral.per)[2] = "n"

hiMoralpercent = (hiMoral.per$n)/ttlwrd_fullfiles
rm(hiMoral.per)

hiMoral.per = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq), hiMoralpercent)
names(hiMoral.per)[1] = "word"
names(hiMoral.per)[2] = "n"
names(hiMoral.per)[3] = "percentage"

# plot frequency of words in percentage
f3 <- ggplot(hiMoral.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("High Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
lowMoral_match = str_detect(fullfiles_tk$word, 
                            pattern = paste(lowMoral_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowMoral_match, na.rm = TRUE)

fullfiles_loMoral = sum(lowMoral_match, na.rm = TRUE)

# Calculate percentage of LM words
perLM_fullfiles = (fullfiles_loMoral/ttlwrd_fullfiles)*100

# Return items that matched
lowMoral_sub = str_subset(fullfiles_tk$word, pattern = paste(lowMoral_dict, collapse = "|"))
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

loMoralpercent = (loMoral.per$n)/ttlwrd_fullfiles
rm(loMoral.per)

loMoral.per = cbind.data.frame(names(loMoral.frq), as.integer(loMoral.frq), loMoralpercent)
names(loMoral.per)[1] = "word"
names(loMoral.per)[2] = "n"
names(loMoral.per)[3] = "percentage"

# plot frequency of words in percentage
f4 <- ggplot(loMoral.per, aes(x = reorder(word, -percentage), y = percentage, fill = word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Frequency in Percentage")+
  xlab("")+
  ggtitle("Low Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

## Adding values to master datafile
fullfiles_rw <- c(perHC_fullfiles, perLC_fullfiles, 
                  perHM_fullfiles, perLM_fullfiles)

masterFULL <- data.frame(perHC_fullfiles, perLC_fullfiles, 
                     perHM_fullfiles, perLM_fullfiles)

colnames(masterFULL) <- c("HiComp", "LowComp", "HiMoral", "LowMoral")

full.df <- data.frame(perHC_fullfiles, perLC_fullfiles, 
                      perHM_fullfiles, perLM_fullfiles)

colnames(full.df) <- c("HiComp", "LowComp", "HiMoral", "LowMoral")

write.csv(full.df, "C:\\Users\\Michelle\\Desktop\\fulldata.csv", row.names = TRUE)

## COMBINE GRAPH
fullgraph_c <- ggarrange(f1, f2,
                       ncol = 2, nrow = 1,
                       common.legend = TRUE, legend = "bottom")
fullgraph_c

fullgraph_m <- ggarrange(f3, f4,
                         ncol = 2, nrow = 1,
                         common.legend = TRUE, legend = "bottom")
fullgraph_m

