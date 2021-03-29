theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      #plot.margin=unit(x=c(0,0,0,0),units="mm"),
      plot.title=element_text(size=9, face = 'bold', lineheight=.75, color ="grey30", hjust = 0.5),
      plot.subtitle = element_text(size=8, lineheight=.75, color ="grey30", hjust = 0.5),
      #plot.subtitle=element_text(size=8, lineheight=.75, color ="grey40"),
      plot.caption = element_text(size=8, lineheight=.5, hjust = 0, face = "italic"
                                  , color ="grey40"), #Default is hjust=1
      plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
      plot.caption.position =  "plot",
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      legend.title = element_text(size=7, face = 'bold',  lineheight=.75, color ="grey30"),
      #legend.title = element_blank(),
      legend.text = element_text(size=7, lineheight=.75, color ="grey30"),
      legend.position = 'bottom',
      legend.key.height = unit(3, "mm"),
      ...
    )
}

theme_plot <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(color = "#22211d"),
      #axis.line = element_blank(),
      axis.text.x = element_text(angle = 90, vjust=.5, size = 7, color = "#22211d"),
      axis.text.y = element_text(size = 7, color = "#22211d"),
      axis.title.y = element_text(size = 7, color = "#22211d"),
      #axis.ticks = element_blank(),
      #axis.title.x = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_blank(),
      panel.grid = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.background = element_rect(fill = "#f5f5f2", color = NA),
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      plot.title=element_text(size=9, face = 'bold', lineheight=.75, color ="grey30",hjust = 0.5),
      plot.subtitle=element_text(size=8, lineheight=.75, color ="grey40"),
      plot.caption = element_text(size=8, lineheight=.5, hjust = 0, face = "italic"
                                  , color ="grey40"), #Default is hjust=1
      plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
      plot.caption.position =  "plot",
      legend.title = element_text(size=8, face = 'bold',  lineheight=.75, color ="grey30"),
      legend.text = element_text(size=8, lineheight=.75, color ="grey30"),
      ...
    )
}

# ggplot themes ----
theme_plot2 <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(color = "#22211d"),
      #axis.line = element_blank(),
      axis.text.x = element_text(angle = 90, vjust=.5, size = 7, color = "#22211d"),
      axis.text.y = element_text(size = 7, color = "#22211d"),
      axis.title.y = element_text(size = 7, color = "#22211d"),
      #axis.ticks = element_blank(),
      #axis.title.x = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_blank(),
      panel.grid = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.background = element_rect(fill = "#f5f5f2", color = NA),
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      plot.title=element_text(size=9, face = 'bold', lineheight=.75, color ="grey30",hjust = 0.5),
      plot.subtitle=element_text(size=8, lineheight=.75, color ="grey40"),
      plot.caption = element_text(size=8, lineheight=.5, hjust = 0, face = "italic"
                                  , color ="grey40"), #Default is hjust=1
      plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
      plot.caption.position =  "plot",
      legend.title = element_text(size=8, face = 'bold',  lineheight=.75, color ="grey30"),
      legend.text = element_text(size=8, lineheight=.75, color ="grey30"),
      ...
    )
}

# ggplot themes ----
theme_plotCLC <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(color = "#22211d"),
      #axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 7, color = "#22211d"),
      axis.title.y = element_text(size = 7, color = "#22211d"),
      #axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_blank(),
      panel.grid = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.background = element_rect(fill = "#f5f5f2", color = NA),
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      plot.title=element_text(size=9, face = 'bold', lineheight=.75, color ="grey30",hjust = 0.5),
      plot.subtitle=element_text(size=8, lineheight=.75, color ="grey40"),
      plot.caption = element_text(size=8, lineheight=.5, hjust = 0, face = "italic"
                                  , color ="grey40"), #Default is hjust=1
      plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
      plot.caption.position =  "plot",
      legend.title = element_text(size=8, face = 'bold',  lineheight=.75, color ="grey30"),
      legend.text = element_text(size=8, lineheight=.75, color ="grey30"),
      legend.position="bottom",
      legend.key.height = unit(3, "mm"),
      ...
    )
}
