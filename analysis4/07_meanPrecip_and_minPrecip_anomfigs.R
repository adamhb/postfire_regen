source("analysis4/07_make_figs_POSTFIREPRECIP_052423.R")
source("analysis4/07_make_figs_MINPRECIP.R")
library(cowplot)


NF_interaction_fig_for_panel <- NF_interaction_fig +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(seq(12,24,2)))


NF_Minprecip_interaction_fig_for_panel <- NF_Minprecip_interaction_fig +
  scale_x_continuous(breaks = c(seq(12,24,2)))

interaction_plots <- plot_grid(plotlist = list(NF_interaction_fig_for_panel, NF_Minprecip_interaction_fig_for_panel),
          align = "h", 
          labels = c("(a)","(b)"),
          label_x = -0.02, label_y = 1,rel_widths = c(1,1.1))


makePDF(fig =interaction_plots ,file_name = "interaction_plots", width = 11, height = 6,units = "in")
makePNG(fig = interaction_plots ,file_name = "interaction_plots", width = 11, height = 6,units = "in")
