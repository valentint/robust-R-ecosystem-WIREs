## Plot the R rankings according to PYPL and TIOB
##  as a circular diagram
##
##  Data from: 
##  - PYPL: https://pypl.github.io/PYPL.html
##  - TIOBE: https://www.tiobe.com/tiobe-index/
##

library(conflicted) # avoid masking of functions
library(ggplot2)
library(readxl)
library(gridExtra)

here::i_am("R-ranking.R")
library(here)

##  Figure 1: Ranking of R popularity according to PYPL and TIOBE indexes.
##  

##  data - a data frame with columns Language and Value
##  title - PYPL or TIOBE
##  ymax - upper limit of ylim
plot_circular <- function(data, title, ymax) {
    ## Order data
    tmp <- data[!is.na(data$Value), ]
    tmp <- tmp[order(tmp$Value, decreasing=TRUE), ]
    tmp$Language <- factor(tmp$Language, levels=tmp$Language[order(tmp$Value, decreasing=TRUE)])
    rownames(tmp) <- tmp$id <-seq(1, nrow(tmp))

    ## Get the name and the y position of each label
    ##  I subtract 0.5 because the letter must have the angle of the 
    ##      center of the bars. Not extreme right(1) or extreme left (0)
    label_tmp <- tmp
    number_of_bar <- nrow(label_tmp)
    angle <-  90 - 360 * (label_tmp$id - 0.5) / number_of_bar     
    label_tmp$hjust <- ifelse(angle < -90, 1, 0)
    label_tmp$angle <- ifelse(angle < -90, angle+180, angle)
    label_tmp$Language <- paste(label_tmp$Language, " (", round(label_tmp$Value, 1),")", sep="")

    ## Make the plot
    ## Note that id is a factor. If x is numeric, there is some 
    ##  space between the first bar
    p <- ggplot(tmp, aes(x=as.factor(id), y=Value)) +       
      geom_bar(stat="identity", fill=alpha("#377EB8", 0.8)) +
      ylim(-13, 1.4*ymax) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm") 
      ) +
      coord_polar(start = 0) + 
      geom_text(data=label_tmp, aes(x=id, y=Value+2, label=Language), 
        color="black", fontface="bold",alpha=0.6, size=3.6, 
        angle=label_tmp$angle, hjust=label_tmp$hjust, inherit.aes=FALSE) +
        annotate("text", x=5, y=-13, label=title, color="black", size=6, parse=TRUE)
    
    p
}

fname <- "R-ranking.xlsx"

## PYPL
data <- as.data.frame(read_excel(path=here::here("Data", fname), sheet="PYPL"))
p1 <- plot_circular(data, "bold(PYPL)", ymax=40)

p1

## PYPL
data <- as.data.frame(read_excel(path=here::here("Data", fname), sheet="TIOBE"))
p2 <- plot_circular(data, "bold(TIOBE)", ymax=30)
p2

cairo_pdf(filename=here::here("Output", "R-ranking.pdf"), width=12, height=6)
grid.arrange(p1, p2, nrow = 1)
dev.off()
