### GRAPHING THE 2008 FINANCIAL CRISIS ###
ows <- subset(masterALL2, Year != 2006 & Year != 2007 & Year != 2008)

## ggplots HIGH COMPETENCY
HiComp_ows <- ggplot(data = ows, aes(y = HiComp, x = Year)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2011, color = "green", linetype = "dotted") +
  lims(y = c(0, 2)) + 
  scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013)) +
  ylab("Percentage") + xlab("Year") +
  ggtitle("High Competency") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  theme(axis.text=element_text(size = 12))

## ggplots LOW COMPETENCY
LoComp_ows <- ggplot(data = ows, aes(y = LowComp, x = Year)) +
  geom_line(size = 1) + 
  geom_vline(xintercept = 2011, color = "green", linetype = "dotted") +
  lims(y = c(0, 2)) + 
  scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013)) +
  ylab("Percentage") + xlab("Year") +
  ggtitle("Low Competency") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  theme(axis.text=element_text(size= 12))

## ggplots HIGH MORALITY
HiMoral_ows <- ggplot(data = ows, aes(y = HiMoral, x = Year)) +
  geom_line(size = 1) + 
  lims(y = c(0, 2)) +
  geom_vline(xintercept = 2011, color = "green", linetype = "dotted") +
  scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013)) +
  ylab("Percentage") + xlab("Year") +
  ggtitle("High Morality") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  theme(axis.text=element_text(size = 12))

## ggplots LOW MORALITY
LoMoral_ows <- ggplot(data = ows, aes(y = LowMoral, x = Year)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2011, color = "green", linetype = "dotted") +
  lims(y = c(0, 2)) +
  scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013)) +
  ylab("Percentage") + xlab("Year") +
  ggtitle("Low Morality") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  theme(axis.text=element_text(size = 12))

fig_comp_ows <- ggarrange(HiComp_ows, LoComp_ows,
                         ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")
fig_comp_ows

fig_moral_ows <- ggarrange(HiMoral_ows, LoMoral_ows,
                          ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")
fig_moral_ows