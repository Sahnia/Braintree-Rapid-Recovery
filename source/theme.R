# create PQIP theme
theme_PQIP <- function() {
  
  # Begin construction of chart
  theme_bw(base_size=8
          # ,
         #  base_family = "Century Gothic"
         ) +
    
    # Set the entire chart region to white
    theme(panel.background=element_rect(fill="white", color="white")) +
    theme(plot.background=element_rect(fill="white", color="white")) +
    theme(panel.border=element_rect(color="white")) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color="grey",size=.25)) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill="white")) +
    theme(legend.text = element_text(size=8,color="black")) +
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color="black", size=9, vjust=1.25)) +
    theme(axis.text.x=element_text(size=8,color="black", angle=90,vjust=0.5,hjust=1)) + 
    theme(axis.text.y=element_text(size=8,color="black")) +
    theme(axis.title.x=element_text(size=9,color="black", vjust=0)) +
    theme(axis.title.y=element_text(size=9,color="black", vjust=1.25)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

## create theme for T-charts
theme_PQIP_tchart <- function() {
  
  # Begin construction of chart
  theme_bw(base_size=7,
           base_family = "Century Gothic") +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill="white", color="white")) +
    theme(plot.background=element_rect(fill="white", color="white")) +
    theme(panel.border=element_rect(color="white")) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color="grey",size=.25)) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill="white")) +
    theme(legend.text = element_text(size=8,color="black")) +
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color="black", size=9, vjust=1.25)) +
    theme(axis.text.x=element_text(size=8,color="black", vjust = 0)) + 
    theme(axis.text.y=element_text(size=8,color="black")) +
    theme(axis.title.x=element_text(size=9,color="black", vjust=0)) +
    theme(axis.title.y=element_text(size=9,color="black", vjust=1.25)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}
# create PQIP theme
theme_PQIP_legend <- function() {
  
  # Begin construction of chart
  theme_bw(base_size=8) +
    #, base_family = "Century Gothic"
    
    # Set the entire chart region to white
    theme(panel.background=element_rect(fill="white", color="white")) +
    theme(plot.background=element_rect(fill="white", color="white")) +
    theme(panel.border=element_rect(color="white")) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color="grey",size=.25)) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.background = element_rect(fill="white")) +
    theme(legend.text = element_text(size=8,color="black")) +
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color="black", size=9, vjust=1.25)) +
    theme(axis.text.x=element_text(size=8,color="black", angle=90,vjust=0.5,hjust=1)) + 
    theme(axis.text.y=element_text(size=8,color="black")) +
    theme(axis.title.x=element_text(size=9,color="black", vjust=0)) +
    theme(axis.title.y=element_text(size=9,color="black", vjust=1.25)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}
# create multiplot function for DrEaM data
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

recode.section.status <- function(section){
  dplyr::recode_factor(section,  
                       `10`="Complete",
                       `-1`="Not set", `0`="Not saved", `5`="Disabled",
                       `20`="Incomplete", `30`="Errors", `60`="Transferred in", `70`="Transferred out",
                       .missing = "Not recorded")
}

#create colour ramp for figures
colfunc_red<-colorRampPalette(c("#E0CECE", "#A73638"))
colfunc_blue<-colorRampPalette(c("#e41a1c","#377eb8"))
colfunc_POMS<-colorRampPalette(c("#0571b0","#f7f7f7", "#ca0020"))
colfunc_day3_QOR <-colorRampPalette(c("#ca0020", "#f7f7f7", "#0571b0"))
colfunc_bauer <- colorRampPalette(c("#596E8F", "#DDE2E7"))

