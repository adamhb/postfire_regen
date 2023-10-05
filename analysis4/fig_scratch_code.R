load("analysis4/NF_mods_with_warm_dry/mod118_nf_megram_reduced_1000m__notes_2023-05-04-14-40-04.rda")


v <- ggplot(faithfuld, aes(waiting, eruptions, z = density))
v + geom_contour_filled()


ggplot(interaction_data_for_plot,aes(postFireMaxT,postFirePrecip,z = z)) +
  geom_contour_filled(binwidth = 0.2,leg)



craftbrewer_pal <- function (type = "seq", palette = 1, direction = 1) 
{
  pal <- scales:::pal_name(palette, type)
  force(direction)
  function(n) {
    n_max_palette <- RColorBrewer:::maxcolors[names(RColorBrewer:::maxcolors) == palette]
    
    if (n < 3) {
      pal <- suppressWarnings(RColorBrewer::brewer.pal(n, pal))
    } else if (n > n_max_palette){
      rlang::warn(paste(n, "colours used, but", palette, "has only",
                        n_max_palette, "- New palette created based on all colors of", 
                        palette))
      n_palette <- RColorBrewer::brewer.pal(n_max_palette, palette)
      colfunc <- grDevices::colorRampPalette(n_palette)
      pal <- colfunc(n)
    }
    else {
      pal <- RColorBrewer::brewer.pal(n, pal)
    }
    pal <- pal[seq_len(n)]
    if (direction == -1) {
      pal <- rev(pal)
    }
    pal
  }
}

scale_fill_craftfermenter <- function(..., type = "seq", palette = 1, direction = -1, na.value = "grey50", guide = "coloursteps", aesthetics = "fill") {
  type <- match.arg(type, c("seq", "div", "qual"))
  if (type == "qual") {
    warn("Using a discrete colour palette in a binned scale.\n  Consider using type = \"seq\" or type = \"div\" instead")
  }
  binned_scale(aesthetics, "fermenter", ggplot2:::binned_pal(craftbrewer_pal(type, palette, direction)), na.value = na.value, guide = guide, ...)
}

############
###pick up with this function
###if it even makes sense to have this as a function
##############3

plot_interaction <- function(x_var, y_var,x_units,y_units,x_max = 0.41,y_max = 2001){
  
  interaction_data_for_plot <- get_interaction_data(x_var)
  
  ggplot_data <- interaction_data_for_plot %>%
    filter(!!as.symbol(y_var) < y_max,
           !!as.symbol(x_var) < x_max) 
  
  for_breaks <- round(quantile(ggplot_data$z, seq(0, 1, 0.15),na.rm = T), 2)  
  
  Fig <- ggplot(ggplot_data,aes_string(x = x_var,
                                       y = y_var,
                                       z = "z"))  +
    xlab("Post-fire mean daily max temperture [C]") +
    ylab("Post-fire precipitation [mm yr-1]") +
    metR::geom_contour_fill(binwidth = 0.2) +
    #stat_contour(data = ggplot_data, aes_string(x = x_var,
    #                                           y = y_var,
    #                                           z = "z",
    #                                           colour = "..level.."),
    #            breaks = for_breaks,
    #            size = 2) +
    scale_color_continuous(name = "s(x)") +
    #geom_contour(aes(color = after_stat(z))) +
    scale_x_continuous(limits = c(12,24),breaks = seq(12,24,4)) +
    scale_y_continuous(limits = c(500,3000),breaks = seq(500,3000,500)) +
    labs(title = "") +
    scale_fill_craftfermenter(
      breaks = mybreaks, 
      palette = "BuGn", 
      limits = c(-2,11),
      guide = guide_colorsteps(
        frame.colour = "black", 
        ticks.colour = "black", # you can also remove the ticks with NA
        barwidth=20)
    ) +
    theme(legend.position = "bottom") +
    theme(legend.justification=c(1, 0), legend.position=c(0.95, 0.6),
          legend.title = "s(x)")# +
  #adams_theme 
  
  
  makePNG(fig = Fig,
          path_to_output.x ="analysis4/figs/",
          file_name = paste0(x_var,"_X_",y_var,"_"),height = 6,width = 6)
  return(Fig)
}



mybreaks <- seq(-1,2,0.5)
#plot interaction on nf best model
plot_interaction(x_var = "postFireMaxT",y_var = "postFirePrecip",x_units = "[degree C]",y_units = "[mm yr-1]",x_max = 24,y_max = 3000)


