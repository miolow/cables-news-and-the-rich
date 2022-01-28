################################################################################
############################### ALL YEAR 2013 #################################
################################################################################
# all files 
all_13 = readtext("C:/Users/Michelle/Desktop/ContentAnalysis/Data/ByYear/2013/*.docx")

## Create columns for date, year, and day for year datafile
all_13 <- all_13 %>%
  add_column(date = substr(all_13$text, 1, 10)) %>%
  add_column(year = substr(all_13$text, 7, 10)) %>%
  add_column(day = substr(all_13$text, 11, 14)) 


## Tokenize
all_13_tk <- all_13 %>% unnest_tokens("word", text)

## Remove whitespaces
all_13_tk$word <- gsub("\\s+","", all_13_tk$word)

## Stemming
all_13_tk <- all_13_tk %>%
  mutate_at("word", funs(wordStem((.), language="en")))

## Remove stop words
data("stop_words")
all_13_tk <- all_13_tk %>%
  anti_join(stop_words)

## Remove numbers
all_13_tk <- all_13_tk[-grep("\\b\\d+\\b", all_13_tk$word),]


###############################################################################
## Checking against high competence dictionary
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highComp_match = str_detect(all_13_tk$word, 
                            pattern = paste(highComp_dict, 
                                            collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(highComp_match, na.rm = TRUE)
# View(highComp_match)

all13_hiComp = sum(highComp_match, na.rm = TRUE)

# Count total number of words in 2013
ttlwrd_all_2013 = length(all_13_tk$word)

# Calculate percentage of HC words
perHC_all_2013 = (all13_hiComp/ttlwrd_all_2013)*100

# Return items that matched
highComp_sub = str_subset(all_13_tk$word, 
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


# transform n to percentage over total words
hiComp.per = table(highComp_sub)
hiComp.per = cbind.data.frame(names(hiComp.frq), as.integer(hiComp.frq))
names(hiComp.per)[1] = "word"
names(hiComp.per)[2] = "n"

hiComppercent = (hiComp.per$n)/ttlwrd_all_2013
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
  ggtitle("Fox News & MSNBC 2013 - High Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

################################################################################
## Checking against low competence dictionary
# str_detect detects patterns in tokenized file from low comp dictionary
# paste takes the dictionary and combines them into one cell
lowComp_match = str_detect(all_13_tk$word, 
                           pattern = paste(lowComp_dict, collapse = "|"))


# Count number of TRUE (in other words, number of matches)
sum(lowComp_match, na.rm = TRUE)

all13_loComp = sum(lowComp_match, na.rm = TRUE)

# Calculate percentage of HC words
perLC_all_2013 = (all13_loComp/ttlwrd_all_2013)*100

# Return items that matched
lowComp_sub = str_subset(all_13_tk$word, 
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

# transform n to percentage over total words
loComp.per = table(lowComp_sub)
loComp.per = cbind.data.frame(names(loComp.frq), as.integer(loComp.frq))
names(loComp.per)[1] = "word"
names(loComp.per)[2] = "n"

loComppercent = (loComp.per$n)/ttlwrd_all_2013
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
  ggtitle("Fox News & MSNBC 2013 - Low Competence")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
highMoral_match = str_detect(all_13_tk$word, 
                             pattern = paste(highMoral_dict, 
                                             collapse = "|"))
# View(highMoral_match)

# Count number of TRUE (in other words, number of matches)
sum(highMoral_match, na.rm = TRUE)

all13_hiMoral = sum(highMoral_match, na.rm = TRUE)

# Calculate percentage of HC words
perHM_all_2013 = (all13_hiMoral/ttlwrd_all_2013)*100

# Return items that matched
highMoral_sub = str_subset(all_13_tk$word, 
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


# transform n to percentage over total words
hiMoral.per = table(highMoral_sub)
hiMoral.per = cbind.data.frame(names(hiMoral.frq), as.integer(hiMoral.frq))
names(hiMoral.per)[1] = "word"
names(hiMoral.per)[2] = "n"

hiMoralpercent = (hiMoral.per$n)/ttlwrd_all_2013
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
  ggtitle("Fox News & MSNBC 2013 - High Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)

###############################################################################
# str_detect detects patterns in tokenized file from high comp dictionary
# paste takes the dictionary and combines them into one cell
lowMoral_match = str_detect(all_13_tk$word, 
                            pattern = paste(lowMoral_dict, collapse = "|"))

# Count number of TRUE (in other words, number of matches)
sum(lowMoral_match, na.rm = TRUE)

all13_loMoral = sum(lowMoral_match, na.rm = TRUE)

# Calculate percentage of LM words
perLM_all_2013 = (all11_loMoral/ttlwrd_all_2013)*100

# Return items that matched
lowMoral_sub = str_subset(all_13_tk$word, 
                          pattern = paste(lowMoral_dict, collapse = "|"))
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

loMoralpercent = (loMoral.per$n)/ttlwrd_all_2013
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
  ggtitle("Fox News & MSNBC 2013 - Low Morality")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)


## Adding 2013 values to master datafile
all13_rw <- c(2013, perHC_all_2013, perLC_all_2013, 
                perHM_all_2013, perLM_all_2013)

masterALL <- rbind(masterALL, all13_rw)

all13 <- c(2013, perHC_all_2013, perLC_all_2013, 
           perHM_all_2013, perLM_all_2013)

all.df <- rbind(all.df, all13)

## Saving top words
highComp_13 <- highComp_sub
lowComp_13 <- lowComp_sub 
highMoral_13 <- highMoral_sub 
lowMoral_13 <- lowMoral_sub


write.csv(all.df, "C:\\Users\\Michelle\\Desktop\\alldata.csv", row.names = TRUE)

###############################################################################  
## ggplots HIGH COMPETENCY
HiComp_all <- ggplot(data = masterALL, aes(y = HiComp, x = Year)) +
  geom_line(size = 1) + 
  lims(y = c(0, 2)) + 
  scale_x_continuous(breaks = c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013)) +
  ylab("Percentage") + xlab("Year") +
  ggtitle("High Competency") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  theme(axis.text=element_text(size = 12))

## ggplots LOW COMPETENCY
LoComp_all <- ggplot(data = masterALL, aes(y = LowComp, x = Year)) +
  geom_line(size = 1) + 
  lims(y = c(0, 2)) + 
  scale_x_continuous(breaks = c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013)) +
  ylab("Percentage") + xlab("Year") +
  ggtitle("Low Competency") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  theme(axis.text=element_text(size= 12))

## ggplots HIGH MORALITY
HiMoral_all <- ggplot(data = masterALL, aes(y = HiMoral, x = Year)) +
  geom_line(size = 1) + 
  lims(y = c(0, 2)) +
  scale_x_continuous(breaks = c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013)) +
  ylab("Percentage") + xlab("Year") +
  ggtitle("High Morality") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  theme(axis.text=element_text(size = 12))

## ggplots LOW MORALITY
LoMoral_all <- ggplot(data = masterALL, aes(y = LowMoral, x = Year)) +
  geom_line(size = 1) + 
  lims(y = c(0, 2)) +
  scale_x_continuous(breaks = c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013)) +
  ylab("Percentage") + xlab("Year") +
  ggtitle("Low Morality") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  theme(axis.text=element_text(size = 12))

fig_all <- ggarrange(HiComp_all, LoComp_all, HiMoral_all, LoMoral_all,
                  ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")
fig_all

HiComp_all <- ggplot(data = masterALL, aes(y = HiComp, x = Year)) +
  geom_line(size = 1) + 
  lims(y = c(0, 2)) + 
  scale_x_continuous(breaks = c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013)) +
  ylab("Percentage") + xlab("Year") +
  ggtitle("High Competency") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  theme(axis.text=element_text(size = 12))