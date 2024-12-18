library(ggplot2)
filename <-"Drug_predictions/drug_predictions_manual_w_3.csv"
risks <- c("high","high","high","high","high","high","high","high",
               "intermediate","intermediate","intermediate","intermediate","intermediate","intermediate","intermediate","intermediate","intermediate","intermediate","intermediate",
               "low","low","low","low","low","low","low","low","low")
labels <- c(2,2,2,2,2,2,2,2,
           1,1,1,1,1,1,1,1,1,1,1,
           0,0,0,0,0,0,0,0,0)
drugs <- c("azimilide","bepridil","disopyramide","dofetilide","ibutilide","quinidine","sotalol","vandetanib",
           "astemizole","chlorpromazine","cisapride","clarithromycin","clozapine","domperidone","droperidol","ondansetron","pimozide","risperidone","terfenadine",
           "diltiazem","loratadine","metoprolol","mexiletine","nifedipine","nitrendipine","ranolazine","tamoxifen","verapamil")
label_colors <- c("low" = "green", "intermediate" = "blue", "high" = "red")
drug_colors <- c("azimilide" = "red", "bepridil" = "red", "disopyramide" = "red", "dofetilide" = "red",
                 "ibutilide" = "red", "quinidine" = "red", "sotalol" = "red", "vandetanib" = "red",
                 "astemizole" = "blue", "chlorpromazine" = "blue", "cisapride" = "blue", "clarithromycin" = "blue",
                 "clozapine" = "blue", "domperidone" = "blue", "droperidol" = "blue", "ondansetron" = "blue",
                 "pimozide" = "blue", "risperidone" = "blue", "terfenadine" = "blue", "diltiazem" = "green",
                 "loratadine" = "green", "metoprolol" = "green", "mexiletine" = "green", "nifedipine" = "green",
                 "nitrendipine" = "green", "ranolazine" = "green", "tamoxifen" = "green", "verapamil" = "green")

data <- read.csv(filename)
results <- data.frame()
for (i in 1:length(drugs)) {
  correct_rate <- data[,i]
  temp <- data.frame(
    drug = drugs[i],
    correct_rate = correct_rate,
    risk = risks[i],
    label = labels[i]
  ) 
  results <- rbind(results,temp)
}
# Plot the results
tmsplotfun <- function(data, label_colors, drug_colors, title, file_name, tms_name){
  data$drug <- factor(data$drug, levels = unique(data$drug[order(data$label)]))
  tms <- tms_name
  plot <- ggplot(data, aes_string(x = tms_name, y = "drug", fill = "risk")) +
    geom_boxplot(color = "black", width = 0.5, size = 0.2, outlier.size = 0.1) +
    labs(title = title, x = tms, y = "") +
    scale_fill_manual(values = label_colors) + # Set the fill colors
    theme(plot.title = element_text(size = 20), # Title font size
          # Change axis title font sizes
          axis.title.x = element_text(size = 14), # X axis title font size
          axis.title.y = element_text(size = 14), # Y axis title font size
          # Change axis text font sizes
          axis.text.x = element_text(size = 12), # X axis text font size
          axis.text.y = element_text(size = 12, color = drug_colors[levels(data$drug)]), # Dynamically assign color
          # Change legend title and text font sizes
          legend.title = element_text(size = 10), # Legend title font size
          legend.text = element_text(size = 8) # Legend text font size
    )
  ggsave(file_name, plot, width = 8, height = 6, dpi = 900)
}
title <- "Manual weight 3"
file_name <- "drug_predictions_manual_w_3.jpg"
tms_name <- "correct_rate"
tmsplotfun(data = results,
           label_colors = label_colors,
           drug_colors = drug_colors,
           title = title,
           file_name = file_name,
           tms_name = tms_name)