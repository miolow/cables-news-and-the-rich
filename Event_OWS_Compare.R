ows_hc_compare <- ggarrange(ows_fox_hc, ows_msnbc_hc,
                        common.legend = TRUE, legend = "bottom")
title <- expression(atop(bold("2011 Occupy Wall Street"), 
                         scriptstyle("News Network Comparison")))
annotate_figure(ows_hc_compare,
                top=text_grob(title))

ows_lc_compare <- ggarrange(ows_fox_lc, ows_msnbc_lc,
                            common.legend = TRUE, legend = "bottom")
title <- expression(atop(bold("2011 Occupy Wall Street"), 
                         scriptstyle("News Network Comparison")))
annotate_figure(ows_lc_compare,
                top=text_grob(title))

ows_hm_compare <- ggarrange(ows_fox_hm, ows_msnbc_hm,
                            common.legend = TRUE, legend = "bottom")
title <- expression(atop(bold("2011 Occupy Wall Street"), 
                         scriptstyle("News Network Comparison")))
annotate_figure(ows_hm_compare,
                top=text_grob(title))

ows_lm_compare <- ggarrange(ows_fox_lm, ows_msnbc_lm,
                            common.legend = TRUE, legend = "bottom")
title <- expression(atop(bold("2011 Occupy Wall Street"), 
                         scriptstyle("News Network Comparison")))
annotate_figure(ows_lm_compare,
                top=text_grob(title))