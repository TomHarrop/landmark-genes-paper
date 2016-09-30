#!/usr/bin/env Rscript

library(ggplot2)
library(data.table)
library(gtable)

# tidy up the data for the main figure
plot.data <- data.table(read.delim2("data/taillemeristem.txt",
                                    strip.white = TRUE, sep = "",
                                    stringsAsFactors = FALSE))
setnames(plot.data, c("species", "size.meristem"),
         c("Species", "Meristem width (µm)"))
plot.data[, Species := plyr::mapvalues(
  Species, c("Og", "Ob"), c("O. glaberrima", "O. barthii"))]
plot.data[, Stage :=
            plyr::mapvalues(Stage, c("3-1", "3-2", "4"),
                            c("PB initiation", "PB elongation",
                              "Early Sp\ndifferentiation"))]
plot.data[, Stage := factor(Stage,
                            levels = c("RM", "PB initiation", "PB elongation",
                                       "Early Sp\ndifferentiation"))]

# plot the main figure
main.figure <- ggplot(plot.data, aes(x = Stage, fill = Species,
                                     y = `Meristem width (µm)`)) +
  xlab(NULL) + 
  theme_light(base_size = 10, base_family = "Helvetica") +
  theme(legend.text = element_text(face = "italic", size = 8),
        axis.text.x = element_text(vjust = 0.5),
        # place the legend in the plot area
        #legend.direction = "horizontal",
        legend.position = c(0,0),
        legend.justification = c(0,0),
        legend.key.size = unit(8, "pt"),
        legend.background = element_rect(colour = "grey93", size = 0.125)) +
  guides(fill = guide_legend(title = NULL)) +
  scale_fill_grey(start = 0.3, end = 0.7) +
  stat_boxplot(geom = "errorbar", size = 0.25, width = 0.2,
               position=position_dodge(0.75)) +
  geom_boxplot(size = 0.25, width = 0.5,
               position=position_dodge(0.75),
               outlier.colour = NULL, outlier.size = NULL, outlier.shape = NULL)

# location of the pngs
png.files <- data.table(file = c("data/cropped/RM01.png",
                                 "data/cropped/RM02.png",
                                 "data/cropped/Ob4-3.png",
                                 "data/cropped/Og9-2.png",
                                 "data/cropped/Ob6-1.png"),
                        w.out = c(118, 118, 188, 188, 188))

# function to crop file to a tempfile, read the resulting png & render it to a
# rastergrob. This is so all the pngs are a uniform size.
cropAndRaster <- function(file, w.out){
  # w.out controls the width.
  tmp <- tempfile(fileext = ".png")
  resize <- paste0(w.out, "x188^")
  crop <- paste0(w.out, "x188")
  # -extent is for padding, use e.g. -crop 104x204+0+0 for cropping
  system(paste("convert", file, "-resize", resize,
               "-gravity center -background transparent -extent", crop,
               "+repage", tmp))
  png <- png::readPNG(tmp)
  grid::rasterGrob(png)
}

# set up the axes for the strip of images
p <- ggplot(data.frame(names = as.character(1:4)), aes(x = names)) +
  theme_void() +
  scale_x_discrete() +
  scale_y_continuous(limits = c(0, 188), expand = c(0, 0))

# crop and raster the pngs in png.files and plot them on the axes
b <- 0.6
image.plot <- p +
  annotation_custom(png.files[1, cropAndRaster(file, w.out)],
                    xmin = 0.49, xmax = 0.99, ymin = 0, ymax = 188) +
  annotation_custom(png.files[2, cropAndRaster(file, w.out)],
                    xmin = 1.01, xmax = 1.51, ymin = 0, ymax = 188) +
  annotation_custom(png.files[3, cropAndRaster(file, w.out)],
                    xmin = 1.6, xmax = 2.4, ymin = 0, ymax = 188) +
  annotation_custom(png.files[4, cropAndRaster(file, w.out)],
                    xmin = 2.6, xmax = 3.4, ymin = 0, ymax = 188) +
  annotation_custom(png.files[5, cropAndRaster(file, w.out)],
                    xmin = 3.6, xmax = 4.4, ymin = 0, ymax = 188)

# extract the panel from the image.plot
image.plot.grob <- ggplotGrob(image.plot)
image.plot.panel <- gtable_filter(image.plot.grob, "panel")

# find where to place the image.plot.panel in the main figure
main.figure.grob <- ggplotGrob(main.figure)
idx <- subset(main.figure.grob$layout, name == "axis-b") # the x-axis location

# add two lines, one for whitespace and one for the images
combined.figure.grob <- gtable_add_rows(
	main.figure.grob,
	grid::unit.c(unit(3, "pt"), # 3pt is axis.ticks.length
	unit(188*2/300, "inches")), idx$b) 

# place the grob
combined.figure.grob <- gtable_add_grob(combined.figure.grob, image.plot.panel,
                                        t = idx$b + 2, l = idx$l,
                                        b = idx$b + 2, r = idx$r)

# draw the plot in a pdf
pdf("Figure3.pdf", width = 6.693, height = 4.462, family = "Helvetica",
    pointsize = 10)
grid::grid.newpage()
grid::grid.draw(combined.figure.grob)
dev.off()
