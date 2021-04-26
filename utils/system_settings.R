options(dplyr.print_max = 3e2)
options(max.print=3e2)
options(tibble.print_max = 3e2)


psize <- 5
axis_size <- 20
title_size <- 25
legend_symbol_size  <- 7


adams_guides <- guides(color = guide_legend(override.aes = list(size=6)),
                       shape = guide_legend(override.aes = list(size=6)),
                       fill=guide_legend(title="PFT"))

color_guide <-  guides(color = guide_legend(override.aes = list(shape = 15)))

rec.y.axis <- ylab(expression(paste('N recruits [ha'^'-1','yr'^'-1',"]")))

#set theme for the plots
adams_theme <- theme(plot.title = element_text(hjust = 0.5, size = title_size),
                     strip.text.x = element_text(size = axis_size),
                     legend.title = element_blank (),
                     axis.title.x = element_text (size = axis_size), # change the axis title
                     axis.title.y = element_text (size = axis_size),
                     axis.title.y.right = element_text (size = axis_size, color = "blue"),
                     axis.text.x = element_text (size = axis_size, colour = "black"),
                     axis.text.y = element_text (size = axis_size, colour = "black"),
                     legend.text = element_text (size = axis_size),
                     legend.spacing.x = unit(0.3, 'cm'),
                     legend.spacing.y = unit(0.3, 'cm'), #this changes the spacing between groups of legend symbols
                     legend.key.size = unit(0.9, "cm"))
adams_theme <- theme_minimal() + adams_theme




makePNG <- function(fig, path_to_output.x = path_to_output, file_name = "unamed_graph",
                    height=PNGheight,  width=PNGwidth, units=PNGunits, res = PNGres){
  
  #fig = SMP_fig
  #  file_name = "sMP_fig"
  model_run_time_stamp <- Sys.time() %>% 
    sub(pattern = ":", replacement = "-") %>%
    sub(pattern = ":", replacement = "-") %>%
    sub(pattern = " ", replacement = "-")
  
  png(paste0(path_to_output.x,file_name,"_",model_run_time_stamp,".png"), height=height, width=width, units=units, res = res)
  print(fig)
  dev.off()
}
