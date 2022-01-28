###############################################################################  
### GRAPHING THE 2008 FINANCIAL CRISIS ###
masterCopy <- master
fc_news <- subset(masterCopy, Year != 2011 & Year != 2012 & Year != 2013)

## ggplots HIGH COMPETENCY
fc_news$HiComp <- as.numeric(fc_news$HiComp)

HiCompg <- ggplot(data = fc_news, aes(y = HiComp, x = Year)) +
  geom_line(aes(group = Source, color = Source), size = 1) +
  ylab("Percentage") + xlab("Year") + 
  scale_y_continuous(limits = c(0,2)) +
  geom_vline(xintercept = "2008", color = "orange", linetype = "dotted") +
  ggtitle("High Competency") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  theme(axis.text=element_text(size = 12))

## ggplots LOW COMPETENCY
fc_news$LowComp <- as.numeric(fc_news$LowComp)

LoCompg <- ggplot(data = fc_news, aes(y = LowComp, x = Year)) +
  geom_line(aes(group = Source, color = Source), size = 1) +
  ylab("Percentage") + xlab("Year") +
  scale_y_continuous(limits = c(0,2)) +
  geom_vline(xintercept = "2008", color = "orange", linetype = "dotted") +
  ggtitle("Low Competency") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  theme(axis.text=element_text(size= 12))

## ggplots HIGH MORALITY
fc_news$HiMoral <- as.numeric(fc_news$HiMoral)

HiMoralg <- ggplot(data = fc_news, aes(y = HiMoral, x = Year)) +
  geom_line(aes(group = Source, color = Source), size = 1) +
  ylab("Percentage") + xlab("Year") +
  scale_y_continuous(limits = c(0,2)) +
  geom_vline(xintercept = "2008", color = "orange", linetype = "dotted") +
  ggtitle("High Morality") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  theme(axis.text=element_text(size = 12))

## ggplots LOW MORALITY
fc_news$LowMoral <- as.numeric(fc_news$LowMoral)

LoMoralg <- ggplot(data = fc_news, aes(y = LowMoral, x = Year)) +
  geom_line(aes(group = Source, color = Source), size = 1) +
  ylab("Percentage") + xlab("Year") +
  scale_y_continuous(limits = c(0,2)) +
  geom_vline(xintercept = "2008", color = "orange", linetype = "dotted") +
  ggtitle("Low Morality") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  theme(axis.text=element_text(size = 12))

fc_news_comp <- ggarrange(HiCompg, LoCompg,
                  ncol = 2, nrow = 1,
                  common.legend = TRUE, legend = "bottom")
fc_news_comp

fc_news_moral <- ggarrange(HiMoralg, LoMoralg,
                          ncol = 2, nrow = 1,
                          common.legend = TRUE, legend = "bottom")
fc_news_moral