#!/usr/bin/Rscript

library(data.table)
library(ggplot2)

# load and mung data
plot.frame <- read.delim("data/2accessionsdata.txt", stringsAsFactors = FALSE)
plot.data.wide <- data.table(plot.frame)
setnames(plot.data.wide,
         c("Name_2", "Bar_Code", "Rachis_length", "Pb_nb", "Pb_length", "Sb_nb",
           "Sp_nb"),
         c("Accession", "Species", "Rachis length (cm)", "PB number",
           "PB length (cm)", "SB number", "Sp number"))
plot.data.wide[Species == "Og", Species := "O. glaberrima"]
plot.data.wide[Species == "Ob", Species := "O. barthii"]

# function to make each plot
makePanicleBoxplot <- function(column, plot.data = plot.data.wide) {
  ggplot(plot.data, aes(x = Species, y = plot.data[[column]], fill = Species)) +
    theme_light(base_size = 10, base_family = "Helvetica") +
    theme(axis.text.x = element_text(face = "italic"),
          axis.title = element_text(size = )) +
    xlab(NULL) + ylab(column) +
    scale_fill_grey(start = 0.3, end = 0.7) +
    stat_boxplot(geom = "errorbar", size = 0.25, width = 0.2) +
    geom_boxplot(size = 0.25, width = 0.5, show.legend = FALSE,
                 outlier.colour = NULL, outlier.size = NULL,
                 outlier.shape = NULL)
}

# list of columns to plot
plots <- c('Rachis length (cm)', 'PB length (cm)', 'PB number', 'SB number',
           'Sp number')

# make a plot for each column
p3.7 <- lapply(plots, makePanicleBoxplot, plot.data = plot.data.wide)

# layout plots
# 170 mm x 112.5 mm = 6.693 x 4.429 inches
pdf("Figure1.pdf", width = 6.693, height = 4.429, family = "Helvetica",
    pointsize = 10)

# grid layout with 6 rows and three columns
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(6, 3)))

# read the panicle photos
ob <- png::readPNG("data/B74_0375.png")
og <- png::readPNG("data/MG12_0486.png")

# place panicle photos in column 1
grid::grid.raster(ob, height = 0.95,
                  vp = grid::viewport(layout.pos.row = 1:3, layout.pos.col = 1))
grid::grid.raster(og, height = 0.95,
                  vp = grid::viewport(layout.pos.row = 4:6, layout.pos.col = 1))

# plot box plots in column 2 & 3
print(p3.7[[1]], vp = grid::viewport(layout.pos.row = 2:3, layout.pos.col = 2))
print(p3.7[[2]], vp = grid::viewport(layout.pos.row = 4:5, layout.pos.col = 2))
print(p3.7[[3]], vp = grid::viewport(layout.pos.row = 1:2, layout.pos.col = 3))
print(p3.7[[4]], vp = grid::viewport(layout.pos.row = 3:4, layout.pos.col = 3))
print(p3.7[[5]], vp = grid::viewport(layout.pos.row = 5:6, layout.pos.col = 3))

# draw panel labels
grid::grid.text("A", 0 + 0.01, 0.975, draw = TRUE,
                gp = grid::gpar(fontface = "bold"))
grid::grid.text("B", 1/3 + 0.01, 0.975, draw = TRUE,
                gp = grid::gpar(fontface = "bold"))

# draw species labels in panicle photos
grid::grid.text("O. barthii", x = 0.15, y = 0.055, hjust = 0,
                vp = grid::viewport(layout.pos.row = 1:3, layout.pos.col = 1),
                gp = grid::gpar(col = "white", fontface = "italic", cex = 0.8))
grid::grid.text("O. glaberrima", x = 0.15, y = 0.055, hjust = 0,
                vp = grid::viewport(layout.pos.row = 4:6, layout.pos.col = 1),
                gp = grid::gpar(col = "white", fontface = "italic", cex = 0.8))

dev.off()
