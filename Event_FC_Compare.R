fc_hc_compare <- ggarrange(fc_fox_hc, fc_msnbc_hc,
                        common.legend = TRUE, legend = "bottom")
title <- expression(atop(bold("2008 Financial Crisis"), 
                         scriptstyle("News Network Comparison")))
annotate_figure(fc_hc_compare,
                top=text_grob(title))

fc_lc_compare <- ggarrange(fc_fox_lc, fc_msnbc_lc,
                            common.legend = TRUE, legend = "bottom")
title <- expression(atop(bold("2008 Financial Crisis"), 
                         scriptstyle("News Network Comparison")))
annotate_figure(fc_lc_compare,
                top=text_grob(title))

fc_hm_compare <- ggarrange(fc_fox_hm, fc_msnbc_hm,
                            common.legend = TRUE, legend = "bottom")
title <- expression(atop(bold("2008 Financial Crisis"), 
                         scriptstyle("News Network Comparison")))
annotate_figure(fc_hm_compare,
                top=text_grob(title))

fc_lm_compare <- ggarrange(fc_fox_lm, fc_msnbc_lm,
                            common.legend = TRUE, legend = "bottom")
title <- expression(atop(bold("2008 Financial Crisis"), 
                         scriptstyle("News Network Comparison")))
annotate_figure(fc_lm_compare,
                top=text_grob(title))