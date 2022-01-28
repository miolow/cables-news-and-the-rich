###############################################################################  
### GRAPHING THE 2011 OWS ###
masterCopy <- master
ows_news <- subset(masterCopy, Year != 2006 & Year != 2007 & Year != 2008)

## ggplots HIGH COMPETENCY
ows_news$HiComp <- as.numeric(ows_news$HiComp)

HiCompg <- ggplot(data = ows_news, aes(y = HiComp, x = Year)) +
  geom_line(aes(group = Source, color = Source), size = 1) +
  ylab("Percentage") + xlab("Year") + 
  scale_y_continuous(limits = c(0,2.1)) +
  geom_vline(xintercept = "2011", color = "purple", linetype = "dotted") +
  ggtitle("High Competency") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  theme(axis.text=element_text(size = 12))

## ggplots LOW COMPETENCY
ows_news$LowComp <- as.numeric(ows_news$LowComp)

LoCompg <- ggplot(data = ows_news, aes(y = LowComp, x = Year)) +
  geom_line(aes(group = Source, color = Source), size = 1) +
  ylab("Percentage") + xlab("Year") +
  scale_y_continuous(limits = c(0,2)) +
  geom_vline(xintercept = "2011", color = "purple", linetype = "dotted") +
  ggtitle("Low Competency") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  theme(axis.text=element_text(size= 12))

## ggplots HIGH MORALITY
ows_news$HiMoral <- as.numeric(ows_news$HiMoral)

HiMoralg <- ggplot(data = ows_news, aes(y = HiMoral, x = Year)) +
  geom_line(aes(group = Source, color = Source), size = 1) +
  ylab("Percentage") + xlab("Year") +
  scale_y_continuous(limits = c(0,2)) +
  geom_vline(xintercept = "2011", color = "purple", linetype = "dotted") +
  ggtitle("High Morality") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  theme(axis.text=element_text(size = 12))

## ggplots LOW MORALITY
ows_news$LowMoral <- as.numeric(ows_news$LowMoral)

LoMoralg <- ggplot(data = ows_news, aes(y = LowMoral, x = Year)) +
  geom_line(aes(group = Source, color = Source), size = 1) +
  ylab("Percentage") + xlab("Year") +
  scale_y_continuous(limits = c(0,2)) +
  geom_vline(xintercept = "2011", color = "purple", linetype = "dotted") +
  ggtitle("Low Morality") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  theme(axis.text=element_text(size = 12))

ows_news_comp <- ggarrange(HiCompg, LoCompg,
                          ncol = 2, nrow = 1,
                          common.legend = TRUE, legend = "bottom")
ows_news_comp

ows_news_moral <- ggarrange(HiMoralg, LoMoralg,
                           ncol = 2, nrow = 1,
                           common.legend = TRUE, legend = "bottom")
ows_news_moral