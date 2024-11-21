################################################################################
## Name: bubbleplot_wt_kd_39genes_relevantes.R                                ##
## Author: Jean Resende (jean.s.s.resende@gmail.com)                          ##
## Date of last update: 20241121                                              ##
## #############################################################################

#install.packages("readxl")
#install.packages("RColorBrewer")
#install.packages("ggplot2")

library(readxl)
library(RColorBrewer)
library(ggplot2)

data <- read_excel("data_wt.xls")

# -- 
str(data)

data$Gene_symbol <- as.character(data$Gene_symbol)
data$log2FoldChange <- as.numeric(data$log2FoldChange)
data$padj <- as.numeric(data$padj)
data$Group <- as.factor(data$Group)

str(data)
summary(data)

head(data)
tail(data)

data$padj <- data[data$padj > 0.05] <- NA

# --
color_palette <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))(100)

ggplot(data, aes(x=Group, y=Gene_symbol, size=-log10(padj),
                 color = log2FoldChange))+
  geom_point()+
  scale_color_gradientn(colors=color_palette, limits=c(-5, 5))+
  scale_size_continuous(range=c(2, 6))+
  theme_minimal()+
  theme(axis.text.y = element_text("italic"))

