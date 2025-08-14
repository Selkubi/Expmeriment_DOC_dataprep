#setwd("..")
library(ggplot2)
library(data.table)
library(ggtext)
if(!dir.exists("output/plots")) {dir.create("output/plots")}
source("R/plotting_functions.R")

####Import the data####
DOC_data <- data.table(read.csv("data/DOC_Final_pretreated_all.csv", col.names = c("Sample", "Date", "DOC", "DN")))
DOC_data$Date <- as.Date(DOC_data$Date, "%m/%d/%Y")

#Get the sample name as it's sampling occasion (data), replicate name and column order
DOC_data[, c("sample_date", "replicate", "column_no") := tstrsplit(Sample, "_")]
list(DOC_data,  DOC_data$sample_date) #Check the sample names for errors

#Same for the whole data, separate the name code to date, replicate name and column number
DOC_data[, c("sample_date", "replicate", "column_no") := tstrsplit(Sample, "_")]
DOC_data[. = lapply(.SD, as.factor), by = c("Sample", "Date", "DOC", "DN")] #Convvert to factor and set the levels, this is important bcs the names contadict the alphabetical order but importnant to level correclty for plotting
DOC_data$column_no <- factor(DOC_data$column_no, levels = c("C0", "C1", "C2", "C3"))

DOC_data <- DOC_data[DOC < 5000] # These are outliers (contamination in this case when DOC>5mgC/L)
DOC_data <- subset(DOC_data, subset =! (sample_date > "S10" & replicate == "O")) # These are an experimental error. So the replicate O was flooded at this date
write.csv(DOC_data, "output/DOC_per_column.csv")

C0 <- DOC_data[column_no == "C0"]
colnames(C0) = c("Sample", "Date","DOC_C0","DN_C0","sample_date", "replicate","column_no")
C1 <- DOC_data[column_no == "C1"]
colnames(C1) = c("Sample", "Date","DOC_C1","DN_C1","sample_date", "replicate","column_no")
C2 <- DOC_data[column_no == "C2"]
colnames(C2) = c("Sample", "Date","DOC_C2","DN_C2","sample_date", "replicate","column_no")
C3 <- DOC_data[column_no == "C3"]
colnames(C3) = c("Sample", "Date","DOC_C3","DN_C3","sample_date", "replicate","column_no")


DOC_consumption <- C0[C1, on = .(sample_date = sample_date, replicate = replicate)][C2, on = .(sample_date = sample_date, replicate = replicate)][C3, on = .(sample_date = sample_date, replicate = replicate)]
DOC_consumption[, `:=`(C1consumed = DOC_C0-DOC_C1, C2consumed = DOC_C1-DOC_C2, C3consumed = DOC_C2-DOC_C3)]

write.csv(DOC_consumption, "DOC_consumption.csv")

#Put the individual sample names to be deleted. These sampling dates follows the contamination days, so no need to have them either. 
#It is important to have their names individually
DOC_data <- DOC_data[!DOC_data$Sample %in% c("S07_A_C0","S08_A_C1", "S08_A_C2","S08_A_C3,",
                                  "S08_G_C0", "S08_G_C1","S08_G_C2","S08_G_C3,")]

#Calculate the means of the same day replications
mean_values <- DOC_data[, .(Mean = mean(DOC, na.rm = TRUE)), by = .(sample_date, column_no)]

#Calculate the carbon consumption of each column
table <- dcast(data = mean_values, sample_date ~ column_no)
table <- table[, .(col1 = C0-C1, col2 = C1-C2, col3 = C2-C3), by = .(sample_date)]
table <- melt(table)


data2 <- table[!table$sample_date %in% c("S05", "S06")]

convert_date_labels(data2)
### Other graphs
# DOC consumption in each column throughout the sampling time
ggplot(data2) +
  facet_wrap(~variable) +
  geom_point(aes(x = sample_date, y = value)) +
  geom_vline(xintercept = "S10", color = "red", linetype = "dashed")

### Biomass correction of c consumtion
Sample_biomass <- fread("data/Cell_Counts_standardized.csv")
Sample_biomass$col_no <- factor(Sample_biomass$col_no, levels = c("1", "2", "3"), labels = c("Col1", "Col2", "Col3"))

data_subset <- DOC_consumption[sample_date %in% c("S10","S11","S12" ,"S13", "S14","S15","S16", "S17","S18","S19")]
data_subset[sample_date == "S10"]$sample_date = "S09"
data_subset2 <- melt(data_subset, id.vars = c("replicate", "sample_date"), 
     measure.vars = c("C1consumed","C2consumed","C3consumed"))

data_subset2$variable <- factor(data_subset2$variable, levels = c("C1consumed", "C2consumed", "C3consumed"), 
                             labels = c("Col1", "Col2", "Col3"))

data_subset2 <- set_coloring_column(data_subset2)

DOC <- ggplot(data_subset2, aes(x = sample_date, y = (value))) +
  facet_wrap(~variable) +
  geom_boxplot(aes(fill = highlight, color = highlight), width=0.5) +
  geom_hline(yintercept = c(0), color = "red", linetype = "dashed") +
  observation_numbers +
  optical_plots_theme() + 
  fill_col_no + color_col_no +
  ylab("ΔDOC(Δ ug C/L)") + xlab("Days")

data_all <- merge(data_subset2, Sample_biomass, all.x = FALSE, 
                  by.x = c("replicate", "sample_date", "variable"), 
                  by.y = c("replicate", "Sample_date", "col_no"))

data_all[, log_cell_pro_ml := (log(Cell_pro_ml))]
data_all[, plotting_cell_pro_ml := (Cell_pro_ml)/10^8]
data_all[, log_normalized_doc := (value / log_cell_pro_ml)]
data_all[, plotting_normalized_doc := (value / plotting_cell_pro_ml)]
data_all <- set_coloring_column(data_all)
data_all <- convert_date_labels(data_all)
data_all <- convert_column_labels(data_all)
data_all <- data_all[-45, ]#quick remove the replicated sample
colnames(data_all)[4] <- "DOC"
data_melted <- melt(data_all, id.vars = c("replicate", "sample_date", "variable", "highlight"), 
                    measure.vars = c("DOC", "plotting_cell_pro_ml", "plotting_normalized_doc"), 
                    variable.name = "measurement")

# Grid plot

enzyme_ratio_names <- c(
  "DOC" = paste0("ΔDOC", "\n", "Δμg C L", '\u02C9', "¹"),
  "plotting_cell_pro_ml" = paste0("Biomass","\n", "10⁸ cell mL", '\u02C9', "¹"),
  "plotting_normalized_doc" = paste0("Normalised DOC","\n","Δμm C L", '\u02C9', "¹", "(10⁸ cell)", '\u02C9', "¹")
)

biomass_normDOC <- ggplot(data_melted, aes(x = sample_date, y = value)) +
  facet_grid(measurement ~ variable, scales = "free",  
             labeller = labeller(measurement = enzyme_ratio_names),
             switch = "y") +
  geom_boxplot(aes(fill = highlight, color = highlight), width = 0.5) +
  #observation_numbers +
  optical_plots_theme() + fill_col_no + color_col_no +
  xlab ("Days") + theme(axis.title.y = element_blank(), legend.position = "none") + 
  scale_y_continuous(position = "right", expand = c(0.2, 0))

facet_labels <- data.frame(
  variable = rep(levels(data_melted$variable), times = 3),  # Columns (left-to-right)
  measurement = rep(levels(data_melted$measurement), each = 3),  # Rows (top-to-bottom)
  label = paste0( "(",letters[1:9], ")")
)

all_biomass_norm <- biomass_normDOC +
  geom_text(data = facet_labels, aes(x = levels(data_melted$sample_date)[1],  # Leftmost position
                                     y = Inf,
                                     label = label), hjust = 1,  vjust = 1.4, size = 4  )

grDevices::cairo_pdf('output/plots/biomass_normDOC.pdf', width = 5, height = 5, family = "helvetica", 
                     pointsize = 12, symbolfamily = "helvetica")
plot(all_biomass_norm)
dev.off()

