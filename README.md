# Cable News and The Rich
A text analysis project which looks at how American ideological news networks like Fox News and MSNBC portray the rich over time (2006-2013).

## Introduction
In this project, I looked utilized dictionary analysis to examine how ideological news networks portray the rich. I am particularly interested in two things:
1. How has the conservative Fox News and the liberal MSNBC portray the rich? Are there differences in the way the two news networks discuss about the rich? 
2. Whether portrayals of the rich change as relevant news events about the rich take place. For example, how has the 2008 Financial Crisis and 2011 Occupy Wall Street movement changed the way cable news depict the rich? 

### Why study these portrayals?
Given the rising popularity of discussions on taxing the rich and Democratic political candidates who champion issues pertaining to raising taxes on the rich, it would be interesting to see how the rich, the group that is implicated in this discourse, is discussed in the media, especially in media.

## Methods
I assembled a dataset of news transcripts from Fox News and MSNBC. The time frame of analysis is January 1, 2006 to December 31, 2013. This time frame was selected because it contained two major events related to economic inequality, specifically the 2008 Financial Crisis and the 2011 Occupy Wall Street Movement. The selected time frame enables me to look at the two years prior to and after each of these major events, thereby allowing me to get a gauge of how the rich were discussed before and after these events.

### Data Collection and Management
To retrieve the relevant news transcripts, I utilized the following search terms, (“rich people”) or (billionaire!) or (millionaire!) or (“high income”) or (“wealthy” andnot w/1 countr! Or nation!). This search term enabled me to pull out articles that discussed rich individuals and not rich nations or countries. 

Because of the overwhelming number of retrieved hits, I implemented the reconstructed week method (Riff, Lacy, and Fico 2006) and constructed a more manageable sample of six weeks of coverage per year for both Fox News and MSNBC. Riff et al. (2006) recommended the utilization of constructed weeks to reduce the amount of work required for content analysis and demonstrated this method’s reliability. The intuition behind this method is simple. Instead of sifting through each hit over long time periods, a researcher could construct a given number of weeks each year. Riff and colleagues (2006) recommended constructing at least 6 weeks to provide a representative sample of articles. Taking their recommendation, I proceeded to reconstruct the sample by randomly selecting six Mondays, Tuesdays, Wednesdays, Thursdays, and Fridays for each year. I excluded the weekends as the popular shows on both networks were only featured on weekdays. In the end, the sample contained 30 transcripts per year for each network, totaling to 240 transcripts for each network across eight years. 
Each transcript was then processed. Paragraphs that did not mention rich individuals were removed. This was done so as these news shows typically included other types of news within the same segment. Dropping these unrelated paragraphs helped minimize noise in the analysis. Additionally, the news transcripts were all stripped off of capitalization, punctuation, numbers, and common stop words such as, the, as, and it. Additionally, any paragraphs in the news transcripts.

### Dictionary Construction 
I created four dictionaries to assess low/high competence and morality in order to conduct dictionary-based analysis. These dictionaries will allow for me to operationalize the number of times each news network described the rich as high or low in competence and morality. I began the dictionary creation phase with the items that appeared in the Fiske et al. (2002) Stereotype Content Model. This is a well established social psychology model that examines people's perceptions of social groups. It has two dimensions: (1) perceived morality and (2) erceived competence. 

- For the high competence dictionary, I started off with their items of competent, confident, independent, competitive, and intelligent. Next, I went through each adjective and looked up its synonyms in the thesaurus and included those synonyms. For instance, for the term competent, I included its synonyms such as able, capable, fit, and good. This dictionary ended up with a total of 145 terms.

- For low competence, I looked up the antonyms to the Fiske et al. (2002) items and then proceeded to look for its synonyms. This resulted in a total of 92 terms. 

- For high morality, I utilized Fiske et al.’s (2002) items, tolerant, warm, good-natured, and sincere, and then expanded off this list by looking at their synonyms. This dictionary ended up with 128 terms. 

- For low morality, I looked up the antonyms of those words and then expanded off the list by looking up their synonyms. This dictionary ended up with 247 terms. For all the terms included in these four dictionaries, please refer to the appendix for more details. 

To count the occurrence of high/low competence/morality words, each dictionary is parsed through the news transcripts. Dictionary words that appeared in the news transcripts were accounted for. The count is then standardized by taking the number of occurrences of dictionary divided by the total number of words that appeared in each transcript. For both MSNBC and Fox News’ coverage, all eight years of coverage portrayed the rich as high competence more so than low competence. 

## General Portrayal of the Rich

| Low Competence | High Competence | Low Morality | High Morality |
| :-------------: | :-------------: | :-------------: | :-------------: |
| 0.28 | 1.63 | 0.91 | 0.67 |



- Generally speaking, the cable news media portrayed the rich as high in competence (1.62% mention in high competence terms) rather than in low competence (0.28%). It also portrays the rich as low in morality (0.91%) than high morality (0.67%). 
- In other words, there is a tendency to view the rich as capable and highly talented but unscrupulous and unprincipled. This is consistent with findings from the social psychology literature (see Fiske et al. 2002).  

## Ideological Differences in the Portrayal of the Rich

| Source | Low Competence | High Competence | Low Morality | High Morality |
| :-------------: | :-------------: | :-------------: | :-------------: | :-------------: |
| Fox News | 0.27 | 1.60 | 0.94 | 0.76 |
| MSNBC | 0.29 | 1.65 | 0.88 | 0.57 |

- There is little difference in Fox News and MSNBC's portrayal of the rich when it comes to high competence. Across all eight years, both news network had similar percentages in their mention of high competence words when it comes to describing the rich, 1.60% for Fox News and 1.65% for MSNBC. 
- For low competence mentions, Fox News (0.27%) used higher percentage of low competence (0.29%) description than MSNBC.
- In terms of high morality, the two news network differed. Fox News (0.76%) used more high morality terms than MSNBC (0.57%). 
- For low morality, the two networks did not differed much. Fox News (0.94%) reported higher levels of low morality terms than MSNBC (0.88%). 
- In general, Fox News and MSNBC's depiction of the rich differed only in terms of low competence, high morality, and low morality. 

## The Changing Portrayal of the Rich Across Time
Over the period between 2006 to 2013, two major political events, namely, the 2008 Financial Crisis and the 2011 Occupy Wall Street movement had taken place. In each of these events, the rich were implicated in one way or another. 

### 2008 Financial Crisis
For this event, the rich and their careless and unethical business practices were frequently singled out for causing the economic collapse. I tracked portrayals of the rich before and after the crisis.

![Wrap text](https://github.com/miolow/cables-news-and-the-rich/blob/main/2008FC_Comp.png)

- Prior to the Financial Crisis and in 2006 and 2007, high competence depiction was at 1.35% and 1.72% respectively. It remained at 1.72% during 2008, the year of the Financial Crisis, dipped to 1.34% in 2009, and increased to 1.87% in 2010.
-  As for low competence portrayals, it was at a low of 0.20% and 0.12% in 2006 and 2007. It increased to 0.22% in 2008, surged to 0.60% in 2009, and eventually dipped back down to 0.27%. 
-  This overall decreasing trend for high competence mentions and increasing trend for low competence mentions in 2009 (a year after the Financial Crisis) matches with the ongoing discourse of the time, whereby the rich, particularly the executives of Wall Street were condemned for their failed business practices and decisions. 
-  After 2009 and in 2010, we can see the effects of the 2008 Financial Crisis discourse trailing off as the use of high competence terms increased again to a high while the use of low competence terms decreased to nearly where it was back in 2006. 
-  Turning to morality, we see that high morality portrayals were at 0.46% in 2006 and 0.64% in 2007, these mentions dipped down to 0.49% in 2008, and moved back up to 0.95% in 2009 and to 0.62% in 2010. 
-  As for low morality mentions, it was at 1.31% in 2006 and 1.23% in 2007, decreased to 0.62% in 2008, and back up to 0.79% in 2009 and 0.73% in 2010. 
-  The increase in high morality descriptors and decrease in low morality descriptors after the 2008 FC is surprising, given that one of the main themes recurring in the discourse of the Financial Crisis is the corrupt and greedy business practices of the executives of Wall Street. 

### 2011 Occupy Wall Street Movement
Participants of this movement protested against extreme wealth inequality and the exploitations and discriminatory practices of the rich. 
- High competence mentions was at 1.34% in 2009 and 1.87% in 2010. This fell in 2011, during the year of OWS, to 1.61% and then increased again to 1.74% in 2012. 
- Low competence mention was at 0.20% in 2009 and 0.27% in 2010. This decreased to 0.22% in the year of OWS (2011) and increased to 0.26% in 2012 and 0.33% in 2013. 
- These numbers tell us that the OWS Movement’s rhetoric about the rich’s catastrophic failure in business practices had little long term effect on the media’s portrayal of the rich. High competence depictions did not only not decrease but increased over time. 
- In 2009 and 2010, high morality mention was at 0.46% and 0.62% respectively. This surprisingly increased to 0.91% in 2011, the year of the OWS movement. But after the peak of the movement, high morality mention dipped to 0.76% in 2012 and to 0.53% in 2013. 
- As for low morality portrayals, it was at a high of 1.31% in 2009. It dipped to 0.73% in 2010 and 0.75% in 2011. A year after OWS, low morality depictions decreased further to 0.58% in 2012 and increased a little to 0.64% in 2013. 
- These figures demonstrated that the media presented its most critical and negative coverage of the rich in 2009 and 2010, the years after the 2008 Financial Crisis. This negative coverage of the rich waned in 2011, 2012, and 2013, despite OWS’ rhetoric about how the rich is unprincipled, greedy, and immoral. Taken together, this suggests that OWS seem to exert little effect on swaying the media to present a more negative and critical coverage of the rich.  
