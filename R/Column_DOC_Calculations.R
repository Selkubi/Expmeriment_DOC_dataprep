#setwd("..")
library(ggplot2)
library(data.table)

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
write.csv(DOC_data, "DOC_per_column.csv")

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

#The general average plot with S05 ans S06 discareded since these are still data that needs justiciation to set as outlier
source("R/plotting_functions.R")
ggplot(DOC_data[!(sample_date == "S05"| sample_date == "S06")]) +
 facet_grid( ~sample_date) +
 geom_line(aes(y = DOC, x = column_no, group = replicate, color = replicate)) +
 geom_line(data = mean_values[!(sample_date == "S05"| sample_date == "S06")], aes(x = column_no, y = Mean, group = sample_date), color = "red", lwd = 2) +
 geom_point(data = mean_values[!(sample_date  == "S05"| sample_date == "S06")], aes(x = column_no, y = Mean, group = sample_date), color = "blue", lwd = 5) +
 theme(legend.position = "right") + theme_bw()

ggplot(DOC_data[replicate == "F"]) +
  facet_grid( ~sample_date) +
  geom_line(aes(y = DOC, x = column_no, group = replicate), color = "yellow1", lwd = 2) +
  theme(legend.position = "right") + theme_bw()

#Calculate the carbon consumption of each column
table <- dcast(data = mean_values, sample_date ~ column_no)
table <- table[, .(col1 = C0-C1, col2 = C1-C2, col3 = C2-C3), by = .(sample_date)]
table <- melt(table)

#For different palettes for the facets, do interaction(variable, sample_date>"S10") but I do it with hue for now
ggplot(table) +
  facet_grid(~sample_date>"S10", scales = "free_x", labeller = as_labeller(c('FALSE' = "Before Treatment", 'TRUE' = "After Treatment"))) +
  scale_fill_manual(values = scico::scico(palette = "bamO", 6, alpha = 1, direction = -1, begin = 0.1, end = 0.7), 
                    labels = c('Col 1', 'Col 2', 'Col 3', 'Col 1', 'Col 2', 'Col 3'), name = NULL) +
  geom_col(aes(x = sample_date, y = value, fill = interaction(variable, sample_date>"S10"))) +
  theme_bw() + theme(panel.grid = element_blank()) +
  xlab("Sampling day") + ylab(expression(paste(Delta, "DOC (um C/L)")))

data2 <- table[!table$sample_date %in% c("S05", "S06")]

convert_date_labels(data2)
ggplot(data2) +
  facet_grid(~sample_date>"S10", scales = "free_x", labeller = as_labeller(c('FALSE' = "Before Treatment", 'TRUE' = "After Treatment"))) +
  scale_fill_manual(values = scico::scico(palette = "bamO", 6, alpha = 1, direction = -1, begin = 0.1, end = 0.7), 
                    labels = c('Col 1', 'Col 2', 'Col 3', 'Col 1', 'Col 2', 'Col 3'), name = NULL) +
  geom_col(aes(x = sample_date, y = value, fill = interaction(variable, sample_date>"S10"))) +
  theme_bw() + theme(panel.grid  =  element_blank()) +
  xlab("Sampling day") + ylab(expression(paste(Delta, "DOC (um C/L)")))


### Other graphs
# DOC consumption in each column throughout the sampling time
ggplot(data2) +
  facet_wrap(~variable) +
  geom_point(aes(x = sample_date, y = value)) +
  geom_vline(xintercept = "S10", color = "red", linetype = "dashed")

### Biomass correction of c consumtion
Sample_biomass <- fread("C:/Users/c7701233/Nextcloud/Column-Experiment/EEA/BActerial_Abundance/Cell_Counts_standardized.csv")
Sample_biomass$col_no <- factor(Sample_biomass$col_no, levels = c("1", "2", "3"), labels = c("Col1", "Col2", "Col3"))

data_subset <- DOC_consumption[sample_date %in% c("S10","S11","S12" ,"S13", "S14","S15","S16", "S17","S18","S19")]
data_subset[sample_date == "S10"]$sample_date = "S09"
data_subset2 <- melt(data_subset, id.vars = c("replicate", "sample_date"), 
     measure.vars = c("C1consumed","C2consumed","C3consumed"))

data_subset2$variable <- factor(data_subset2$variable, levels = c("C1consumed", "C2consumed", "C3consumed"), 
                             labels = c("Col1", "Col2", "Col3"))

data_all <- merge(data_subset2, Sample_biomass, all.x = FALSE, 
               by.x = c("replicate", "sample_date", "variable"), 
               by.y = c("replicate", "Sample_date", "col_no"))

data_subset2 <- set_coloring_column(data_subset2)
data_subset2 <- convert_date_labels(data_subset2)
data_subset2 <- convert_column_labels(data_subset2)

reservoir <- unique(data_subset[, .(DOC_C0, sample_date)])

DOC <- ggplot(data_subset2, aes(x = sample_date, y = (value))) +
  facet_wrap(~variable) +
  geom_boxplot(aes(fill = highlight, color = highlight), width=0.5) +
  geom_hline(yintercept = c(0), color = "red", linetype = "dashed") +
  observation_numbers +
  optical_plots_theme + 
  fill_col_no + color_col_no +
  ylab("ΔDOC(Δ ug C/L)") + xlab("Days")

data_all[, log_cell_pro_ml := (log(Cell_pro_ml))]
data_all[, plotting_cell_pro_ml := (Cell_pro_ml)/10^8]
data_all[, log_normalized_doc := (value / log_cell_pro_ml)]
data_all[, plotting_normalized_doc := (value / plotting_cell_pro_ml)]
data_all <- set_coloring_column(data_all)
data_all <- convert_date_labels(data_all)
data_all <- convert_column_labels(data_all)
data_all <- data_all[-45, ]#quick remove the replicated sample

NDOC <- ggplot(data_all, aes(y = (plotting_normalized_doc), x = sample_date)) +
  facet_grid(~variable) +
  geom_boxplot(aes(fill = highlight, color = highlight), width = 0.5) +
  geom_hline(yintercept = c(0), color = "red", linetype = "dashed") +
  observation_numbers +
  optical_plots_theme + fill_col_no + color_col_no +
  ylab(bquote(Δ~um~C~L^-1*~(10^8~cell)^-1)) + xlab("Days") 

Bio <- ggplot(data_all, aes(y = (Cell_pro_ml / 10^8), x = sample_date)) +
  facet_grid( ~variable) +
  geom_boxplot(aes(fill = highlight, color = highlight), width = 0.5) + 
  observation_numbers +
  optical_plots_theme + fill_col_no + color_col_no +
  ylab(bquote("Biomass "(10^8*cell~ml^-1))) + xlab ("Days")

colnames(data_all)[4] <- "DOC"
data_melted <- melt(data_all, id.vars = c("replicate", "sample_date", "variable", "highlight"), measure.vars = c("DOC", "plotting_cell_pro_ml", "plotting_normalized_doc"))

# Grid plot

dose.labs <- c("DOC", "plotting_cell_pro_ml", "plotting_normalized_doc DOC")
names(dose.labs) <- c("DOC", "plotting_cell_pro_ml", "plotting_normalized_doc")
supp.labs <- c("Col1", "Col2", "Col3")
names(supp.labs) <- c("Col1", "Col2", "Col3")

ggplot(data_melted, aes(x = sample_date, y = value)) +
  facet_grid(variable.1 ~ variable, scales = "free") +
  geom_boxplot(aes(fill = highlight, color = highlight), width = 0.5) +
  observation_numbers +
  optical_plots_theme + fill_col_no + color_col_no +
  xlab ("Days")
  
## Dissolved Nitrogen plot
DOC_data <- DOC_data[sample_date %in% c("S08", "S10","S11","S12" ,"S13", "S14","S15","S16", "S17","S18","S19")]
DOC_data[sample_date == "S10"]$sample_date = "S09"
DOC_data$column_no <- factor(DOC_data$column_no, levels = c("C0", "C1", "C2", "C3"), labels = c("Reservoir","Col1", "Col2", "Col3"))
colnames(DOC_data) <- c("Sample", "Date", "DOC", "DN", "sample_date", "replicate", "variable")

DOC_data <- set_coloring_column(DOC_data)
DOC_data = convert_date_labels(DOC_data)

ggplot(DOC_data, aes(y = (DN), x = sample_date))+
  facet_grid( ~variable)+
  geom_boxplot(aes(fill = highlight, color = highlight))+ 
 #observation_numbers()+
  optical_plots_theme + fill_col_no + color_col_no +
  ylab("DN (mg/L)")

#Anova on DOC consumption
anova <- aov((data_all$normalized_doc)~(sample_date*variable), data = data_all)
summary(anova)
TukeyHSD(anova)
