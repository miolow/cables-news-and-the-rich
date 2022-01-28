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

#### Competence
<p align="center">
  <img width="850" height="550" src="https://github.com/miolow/cables-news-and-the-rich/blob/main/2008FC_Comp.png">
</p>

- Prior to the Financial Crisis and in 2006 and 2007, high competence depiction was at 1.35% and 1.72% respectively. It remained at 1.72% during 2008, the year of the Financial Crisis, dipped to 1.34% in 2009, and increased to 1.87% in 2010.
- As for low competence portrayals, it was at a low of 0.20% and 0.12% in 2006 and 2007. It increased to 0.22% in 2008, surged to 0.60% in 2009, and eventually dipped back down to 0.27%. 
- This overall decreasing trend for high competence mentions and increasing trend for low competence mentions in 2009 (a year after the Financial Crisis) matches with the ongoing discourse of the time, whereby the rich, particularly the executives of Wall Street were condemned for their failed business practices and decisions. 
- After 2009 and in 2010, we can see the effects of the 2008 Financial Crisis discourse trailing off as the use of high competence terms increased again to a high while the use of low competence terms decreased to nearly where it was back in 2006. 

#### Morality
<p align="center">
  <img width="850" height="550" src="https://github.com/miolow/cables-news-and-the-rich/blob/main/2008FC_Moral.png">
</p>

- Turning to morality, we see that high morality portrayals were at 0.46% in 2006 and 0.64% in 2007, these mentions dipped down to 0.49% in 2008, and moved back up to 0.95% in 2009 and to 0.62% in 2010. 
- As for low morality mentions, it was at 1.31% in 2006 and 1.23% in 2007, decreased to 0.62% in 2008, and back up to 0.79% in 2009 and 0.73% in 2010. 
- The increase in high morality descriptors and decrease in low morality descriptors after the 2008 FC is surprising, given that one of the main themes recurring in the discourse of the Financial Crisis is the corrupt and greedy business practices of the executives of Wall Street. 

### 2011 Occupy Wall Street Movement
#### Competence
<p align="center">
  <img width="850" height="550" src="https://github.com/miolow/cables-news-and-the-rich/blob/main/2011OWS_Compare.png">
</p>

- High competence mentions was at 1.34% in 2009 and 1.87% in 2010. This fell in 2011, during the year of OWS, to 1.61% and then increased again to 1.74% in 2012. 
- Low competence mention was at 0.20% in 2009 and 0.27% in 2010. This decreased to 0.22% in the year of OWS (2011) and increased to 0.26% in 2012 and 0.33% in 2013. 
- These numbers tell us that the OWS Movement’s rhetoric about the rich’s catastrophic failure in business practices had little long term effect on the media’s portrayal of the rich. High competence depictions did not only not decrease but increased over time. 

#### Morality
<p align="center">
  <img width="850" height="550" src="https://github.com/miolow/cables-news-and-the-rich/blob/main/2011OWS_Moral.png">
</p>

- In 2009 and 2010, high morality mention was at 0.46% and 0.62% respectively. This surprisingly increased to 0.91% in 2011, the year of the OWS movement. But after the peak of the movement, high morality mention dipped to 0.76% in 2012 and to 0.53% in 2013. 
- As for low morality portrayals, it was at a high of 1.31% in 2009. It dipped to 0.73% in 2010 and 0.75% in 2011. A year after OWS, low morality depictions decreased further to 0.58% in 2012 and increased a little to 0.64% in 2013. 
- These figures demonstrated that the media presented its most critical and negative coverage of the rich in 2009 and 2010, the years after the 2008 Financial Crisis. This negative coverage of the rich waned in 2011, 2012, and 2013, despite OWS’ rhetoric about how the rich is unprincipled, greedy, and immoral. Taken together, this suggests that OWS seem to exert little effect on swaying the media to present a more negative and critical coverage of the rich.  


## Ideological Differences in the Portrayal of the Rich During the 2008 Financial Crisis
### Competence
<p align="center">
  <img width="850" height="550" src="https://github.com/miolow/cables-news-and-the-rich/blob/main/fc_comp_news.png">
</p>

- Prior to the Financial Crisis, in 2006 and 2007: 
  - MSNBC had more high competence depictions than Fox News (1.55% versus 1.16% in 2006; 1.84% versus 1.58% in 2007). 
  - MSNBC had less low competence depictions than Fox News in 2006 (0.18% versus 0.22%) but more low competence depictions in 2007 (0.15% versus 0.09%).
  - In other words, prior to the Financial Crisis, MSNBC had a more favorable portrayal of the rich in terms of competency than Fox News. 

- During the year of the Financial Crisis: 
  - Fox News experienced an increase in their use of high competence descriptors (from 1.58% to 1.84% in 2008) while MSNBC decreased theirs (from 1.84% to 1.55% in 2008), resulting in Fox News presenting more high competence portrayal of the rich than MSNBC. 
  - As for low competence, Fox News’ use of terms in this category increased (from 0.09% to 0.28%) while MSNBC experienced no change at all (0.15%). 
  - This revealed that compared to MSNBC, Fox News maintained a more positive coverage of the rich despite the Financial Crisis. 

- After the Financial Crisis:
  - Fox News decreased its high competence portrayals in 2009 (from 1.84% to 1.26%) and then an increase in 2010 (from 1.26% to 1.76%). 
  - MSNBC also experienced a decrease in high competence portrayals in 2009 (from 1.55% to 1.45%) and eventually an increase in 2010 (1.45% to 2.00%). 
  - Both news networks also increased their use of low competence descriptors in 2009; Fox News increased its use of low competence portrayals from 0.28% to 0.42%; MSNBC increased low competence portrayals from 0.15% to 0.81%. 
  - These low competence portrayals began to drop in numbers the year after. 
  - In 2010, from 0.42% to 0.29% for Fox News and 0.81% to 0.26% for MSNBC. 
  - These numbers suggest that in light of the 2008 FC, while both networks became more critical in their depiction of the rich, MSNBC rolled out a more critical and negative coverage of the rich as indicated by their substantial increased use of low competence terms compared to Fox News (0.66% versus 0.33%). 
  - However, this negative coverage of the rich did not last long. For both news networks, low competence portrayal eventually moved back to what it was prior to the 2008 Financial Crisis while high competence portrayal increased to a rate that was higher than the pre-Financial Crisis depiction.

### Morality
<p align="center">
  <img width="850" height="550" src="https://github.com/miolow/cables-news-and-the-rich/blob/main/fc_moral_news.png">
</p>

- Prior to the 2008 Financial Crisis:
  - MSNBC had a more negative coverage of the rich than Fox News, this is indicated by their lower high morality depictions (0.38% versus 0.53%) and higher low morality depictions (1.53% versus 1.09%) in 2006.
  - This difference in coverage on the morality of the rich minimized slightly in 2007, not so much in terms of high morality depictions but more so for low morality portrayals.
  - Fox News’ ended up with higher low morality portrayals than MSNBC (1.73% versus 0.77%). MSNBC continued to have lower high morality portrayals than Fox News (0.49% versus 0.80%).

- During the year of the 2008 Financial Crisis:
  - Both news networks decreased their high morality portrayals. Fox News decreased from 0.80% to 0.60% while MSNBC decreased theirs from 0.49% to 0.25%. 
  - As for low morality portrayals, Fox News reduced their low portrayal depictions substantially, from 1.73% to 0.77%. Similarly, MSNBC reduced theirs from 0.77% to 0.46%. 
  - Comparing across news networks, it appears that during the year of the Financial Crisis, MSNBC produced a harsher morality-based presentation of the rich than Fox News.

- After the 2008 Financial Crisis:
  - High morality portrayals surprisingly increased for both networks. 
  - Fox News’ high morality depictions increased from 0.60% to 1.03%. Similarly, MSNBC increased their high morality depictions from 0.25% to 0.86%. 
  - Low morality depictions decreased for Fox News, from 0.74% to 0.63% while it increased for MSNBC from 0.46% to 0.99%. 
  - These figures indicate that after the Financial Crisis, MSNBC continued to maintain a harsher morality-based coverage of the rich than Fox News. 

## Ideological Differences in the Portrayal of the Rich During the 2008 Financial Crisis
### Competence
<p align="center">
  <img width="850" height="550" src="https://github.com/miolow/cables-news-and-the-rich/blob/main/ows_comp_news.png">
</p>

- Prior to the 2011 Occupy Wall Street Movement:
  - Fox News’ high competence mention of the rich was at 1.26% in 2009 and 1.72% in 2010. 
  - As for MSNBC, high competence mention started off at 1.45% in 2009 and 2% in 2010. 
  - MSNBC had a higher amount of high competence mention than Fox News prior to OWS. 
  - As for low competence portrayals, MSNBC had a higher amount of low competence portrayals in 2009 than Fox News (0.81% versus 0.42%). But this difference in low competence depictions closed in 2010, with 0.29% for Fox News and 0.26% for MSNBC. 
 
 - During the 2011 Occupy Wall Street Movement:
   - High competence mentions decreased for both news networks, from 1.72% to 1.52% for Fox News and from 2.00% to 1.73% for MSNBC. 
   - Low competence mentions also decreased for both news networks, from 0.29% to 0.28% for Fox News and from 0.26% to 0.14% for MSNBC. 
   - At this time point, it appears that MSNBC had a more positive coverage of the rich with its higher amounts of high competence portrayal at 1.73% (versus 1.52% for Fox News) and lower amounts of low competence portrayal at 0.14% (versus 0.28% for Fox News). 

- After the 2011 Occupy Wall Street Movement:
  - Generally, Fox News appeared to increase both of its high and low competence depictions while MSNBC decreased both forms of depictions. 
  - When comparing across both networks, Fox News had higher amounts of high competence portrayals than MSNBC in both 2012 (2.06% versus 1.53%) and 2013 (1.80% versus 1.57%). 
  - For low competence depictions, Fox News had higher amounts of low competence portrayals than MSNBC in 2012 (0.36% versus 0.19%) but not in 2013 (0.23% versus 0.44%). By 2013, Fox News ended up with a more positive and less critical coverage of the rich’s competency than MSNBC. 

### Morality
<p align="center">
  <img width="850" height="550" src="https://github.com/miolow/cables-news-and-the-rich/blob/main/ows_moral_news.png">
</p>

- Prior to the 2011 Occupy Wall Street Movement:
  - Fox News presented a more positive coverage on the rich’s ethical conduct. 
  - Fox News utilized a higher amount of high morality descriptors than MSNBC (1.03% versus 0.86% in 2009; 0.67% versus 0.58% in 2010). 
  - Fox News also utilized a lower amount of low morality descriptors than MSNBC (0.63% versus 0.99% in 2009; 0.70% versus 0.75% in 2010). All of which indicated that Fox News had a more positive morality portrayal of the rich prior to OWS.

- During the 2011 Occupy Wall Street Movement:
  - Fox News increased its positive depiction of the rich, both in terms of increasing high morality portrayals (0.67% to 1.05%) and decreasing low morality portrayals (0.70% versus 0.51%). 
  - Contrarily, MSNBC presented a more negative depiction of the rich by increasing low morality portrayals (0.75% to 1.11%). Its high morality portrayal increased slightly from 0.58% to 0.69%. 

- After the 2011 Occupy Wall Street Movement:
  - MSNBC ended up with a more positive coverage of the rich’s morality. MSNBC used a higher amount of high morality descriptors than Fox News (0.82% versus 0.66%), and a lower amount of low morality descriptors (0.71% versus 1.24%). 
  - This difference in coverage of the rich minimized in 2013. MSNBC then ended up with lower levels of high morality descriptors than Fox News (0.39% versus 0.67%) and slightly lower levels of low morality descriptors (0.83% versus 0.92%). 
