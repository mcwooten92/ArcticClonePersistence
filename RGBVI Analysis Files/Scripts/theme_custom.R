# sizeTitle = 100
# sizeSubtitle = 50
# sizeAxes = 75
# sizeAxesText = 50
# sizePieText = 25
# sizeLegend = 75
# sizeLegendText = 50

theme_custom = function() {
  theme_bw(base_size = 50) + 
    theme(
    # TITLE
      # ...size and position
      plot.title = element_text(face = 'bold', hjust = 0.5), # element_text(size = sizeTitle, face = 'bold', hjust = 0.5),
      # ...margin (e.g., top, bottom)
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      # ...background color for facet plots
      strip.background = element_rect(fill = 'white'), 
      # ...label size for facet plots
      strip.text = element_text(size = 60, color = 'black', face = 'bold'), # element_text(color = 'black', face = 'bold', size = sizeLegendText),
    # SUBTITLE
      plot.subtitle = element_text(face = 'plain', colour = 'black'),
    # AXES 
      # ...title
      # axis.title = element_text(size = sizeAxes),
      # ...text
      axis.text = element_text(color = 'black'), # element_text(size = sizeAxesText, color = 'black'),  
      # ...ticks
      axis.ticks = element_line(color = 'black'),  
      # ...ticks length
      axis.ticks.length = unit(0.5, 'cm'),
    # LEGEND 
      # ...title size
      legend.title = element_text(face = 'bold', vjust = 5), # element_text(size = sizeLegend, face = 'bold', vjust = 5),  
      # ...text size
      # legend.text = element_text(size = sizeLegendText),
      # ...position
      legend.position = "right", 
      # ...size
      legend.key.width = unit(1, "cm"), 
      legend.key.height = unit(2, "cm"),
      # ...anchoring
      # legend.justification = c(1, 1),  
      # ...keys (color boxes) size
      # legend.key.size = unit(0.5, 'cm'),
      # ...space between legend items
      # legend.spacing.y = unit(0.2, 'cm'),
      # ...padding around legend box
      # legend.margin = margin(t = 0, unit='cm'),
      # ...background and border colour
      # legend.background = element_rect(fill = 'white', color = 'grey80'),  
      # ...direction
      # legend.direction = 'horizontal',
      # ...spacing between panels in facet plot
      panel.spacing = unit(1, 'lines'),
    )}

theme_custom_void = function() {
  theme_bw(base_size = 50) + 
    theme(
    # TITLE
      # ...size and position
      plot.title = element_text(face = 'bold', hjust = 0.5), # element_text(size = sizeTitle, face = 'bold', hjust = 0.5),
      # ...margin (e.g., top, bottom)
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      # ...background color for facet plots
      strip.background = element_rect(fill = 'white'), 
      # ...label size for facet plots
      strip.text = element_text(size = 60, color = 'black', face = 'bold'), # element_text(color = 'black', face = 'bold', size = sizeLegendText),
    # SUBTITLE
      plot.subtitle = element_text(face = 'plain', colour = 'black'),
    # AXES 
      # ...title
      # axis.title = element_text(size = sizeAxes),
      # ...text
      axis.text = element_blank(), # element_text(size = sizeAxesText, color = 'black'),  
      # ...ticks
      axis.ticks = element_blank(),  
      # ...ticks length
      axis.ticks.length = unit(0, "pt"),
    # LEGEND 
      # ...title size
      legend.title = element_text(face = 'bold', vjust = 5), # element_text(size = sizeLegend, face = 'bold', vjust = 5),  
      # ...text size
      # legend.text = element_text(size = sizeLegendText),
      # ...position
      legend.position = "right", 
      # ...size
      legend.key.width = unit(1, "cm"), 
      legend.key.height = unit(2, "cm"),
      # ...anchoring
      # legend.justification = c(1, 1),  
      # ...keys (color boxes) size
      # legend.key.size = unit(0.5, 'cm'),
      # ...space between legend items
      # legend.spacing.y = unit(0.2, 'cm'),
      # ...padding around legend box
      # legend.margin = margin(t = 0, unit='cm'),
      # ...background and border colour
      # legend.background = element_rect(fill = 'white', color = 'grey80'),  
      # ...direction
      # legend.direction = 'horizontal',
      # ...spacing between panels in facet plot
      panel.spacing = unit(1, 'lines'),
    )}
