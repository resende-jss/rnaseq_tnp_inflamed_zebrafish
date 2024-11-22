################################################################################
## Name: bubbleplot_wt_kd_39genes_relevantes.R                                ##
## Author: Jean Resende (jean.s.s.resende@gmail.com)                          ##
## Date of last update: 20241121                                              ##
## #############################################################################

#install.packages("readxl")
#install.packages("RColorBrewer")
#install.packages("ggplot2")

g_bubleplot <- function(data,output_file_pdf, width = 8, height = 6){
  
  require(readxl)
  require(RColorBrewer)
  require(ggplot2)
  
  data$Gene_symbol <- as.character(data$Gene_symbol)
  data$log2FoldChange <- as.numeric(data$log2FoldChange)
  data$padj <- as.numeric(data$padj)
  data$Group <- factor(data$Group, levels = c("Wound","5 µM TnP","10 µM TnP"))
  data$Group_2 <- factor(data$Group_2, levels = c("WT","KD"))
  data$padj[data$padj > 0.05] <- NA
  
  color_palette <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))(100)
  
  g <- ggplot(data, aes(x=Group, y=Gene_symbol, size=-log10(padj),
                        color = log2FoldChange))+
    geom_point()+
    scale_color_gradientn(colors=color_palette, limits=c(-5, 5))+
    scale_size_continuous(range=c(2, 6))+
    theme_minimal()+
    theme(axis.text.y = element_text("italic", family = "sans"))+
    facet_wrap(~Group_2, scales = "free_y")
  
  ggsave(output_file_pdf, plot = g, device = "pdf", width = width, height = height)
  
  return(g)
}

data_kd_wt <- read_excel("data_kd_wt.xlsx")

g_bubleplot(data_kd_wt,"all.pdf")

################################################################################