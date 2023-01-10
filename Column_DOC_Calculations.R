library(ggplot2)
library(data.table)

####Import the data####
DOC_data =data.table(read.csv("data/DOC_Final_pretreated_all.csv", col.names = c("Sample", "Date", "DOC", "DN")))
DOC_data$Date=as.Date(DOC_data$Date, "%m/%d/%Y")

# Define which data is the reservoir standard which are the samples
reservoir = data.table(DOC_data[DOC_data$Sample %like% c("Reservoir")])
samples = subset(DOC_data, !(DOC_data$Sample %like% c("Reservoir")))

#Get the sample name as it's sampling occasion (data), replicate name and column order
samples[,c("sample_date", "replicate", "column_no") := tstrsplit(Sample, "_")]
list(samples,  samples$sample_date) #Check the sample names for errors

#Same for the whole data, separate the name code to date, replicate name and column number
DOC_data[,c("sample_date", "replicate", "column_no") := tstrsplit(Sample, "_")]
DOC_data[.=lapply(.SD, as.factor), by=c("Sample", "Date", "DOC", "DN")] #Convvert to factor and set the levels, this is important bcs the names contadict the alphabetical order but importnant to level correclty for plotting
DOC_data$column_no=factor(DOC_data$column_no, levels=c("C0", "C1", "C2", "C3"))

DOC_data=DOC_data[DOC<5000] # These are outliers (contamination in this case when DOC>5mgC/L)
DOC_data=subset(DOC_data, subset=!(sample_date>"S10" & replicate=="O")) # These are an experimental error. So the replicate O was flooded at this date


C0=DOC_data[column_no=="C0"]
C1=DOC_data[column_no=="C1"]
C2=DOC_data[column_no=="C2"]
C3=DOC_data[column_no=="C3"]

C0$C1consumed=C0$DOC - C1$DOC[match(C0$replicate, C1$replicate)]
C1$C2consumed=C1$DOC - C2$DOC[match(C1$replicate, C2$replicate)]
C2$C3consumed=C2$DOC - C3$DOC[match(C2$replicate, C3$replicate)]

data=C0[C1[,c("sample_date", "replicate", "C2consumed")], on=.(sample_date, replicate)]
data=data[C2[,c("sample_date", "replicate", "C3consumed")], on=.(sample_date, replicate)]
data=data[,c("replicate", "sample_date", "C1consumed","C2consumed","C3consumed")]

write.csv(data, "DOC_consumption.csv")


#Put the individual sample names to be deleted. These sampling dates follows the contamination days, so no need to have them either. 
#It is important to have their names individually
DOC_data=DOC_data[!DOC_data$Sample %in% c("S07_A_C0","S08_A_C1", "S08_A_C2","S08_A_C3,",
                                  "S08_G_C0", "S08_G_C1","S08_G_C2","S08_G_C3,")]

#Calculate the means of the same day replications
mean_values=DOC_data[ ,.(Mean=mean(DOC, na.rm=T)), by=.(sample_date, column_no)]

#The general average plot with S05 ans S06 discareded since these are still data that needs justiciation to set as outlier
ggplot(DOC_data[!(sample_date=="S05"| sample_date=="S06")])+
 facet_grid( ~sample_date)+
 geom_line(aes(y=DOC, x=column_no, group=replicate, color=replicate))+
 geom_line(data=mean_values[!(sample_date=="S05"| sample_date=="S06")], aes(x=column_no, y=Mean, group=sample_date), color="red", lwd=2)+
 geom_point(data=mean_values[!(sample_date=="S05"| sample_date=="S06")], aes(x=column_no, y=Mean, group=sample_date), color="blue", lwd=5)+
 theme(legend.position="right")+theme_bw()

ggplot(DOC_data[replicate=="F"])+
  facet_grid( ~sample_date)+
  geom_line(aes(y=DOC, x=column_no, group=replicate), color="yellow1", lwd=2)+
  theme(legend.position="right")+theme_bw()

#Calculate the carbon consumption of each column
table = dcast(data=mean_values, sample_date ~ column_no)
table=table[,.(col1=C0-C1, col2=C1-C2, col3=C2-C3), by=.(sample_date)]
table=melt(table)
#Check for C increasing columns
ggplot(table)+
  facet_grid(~sample_date)+
  geom_col(aes(x=variable, y=value))

#For different palettes for the facets, do interaction(variable, sample_date>"S10") but I do it with hue for now
ggplot(table)+
  facet_grid(~sample_date>"S10", scales="free_x", labeller=as_labeller(c('FALSE'="Before Treatment", 'TRUE'="After Treatment")))+
  scale_fill_manual(values=scico::scico(palette="bamO", 6, alpha=1, direction=-1, begin=0.1, end=0.7), 
                    labels=c('Col 1', 'Col 2', 'Col 3', 'Col 1', 'Col 2', 'Col 3'), name=NULL)+
  geom_col(aes(x=sample_date, y=value, fill=interaction(variable, sample_date>"S10")))+theme_bw()+theme(panel.grid = element_blank())+
  xlab("Sampling day")+ylab(expression(paste(Delta, "DOC (um C/L)")))

data2=table[!table$sample_date%in%c("S05", "S06")]

ggplot(data2)+
  facet_grid(~sample_date>"S10", scales="free_x", labeller=as_labeller(c('FALSE'="Before Treatment", 'TRUE'="After Treatment")))+
  scale_fill_manual(values=scico::scico(palette="bamO", 6, alpha=1, direction=-1, begin=0.1, end=0.7), 
                    labels=c('Col 1', 'Col 2', 'Col 3', 'Col 1', 'Col 2', 'Col 3'), name=NULL)+
  geom_col(aes(x=sample_date, y=value, fill=interaction(variable, sample_date>"S10")))+theme_bw()+theme(panel.grid = element_blank())+
  xlab("Sampling day")+ylab(expression(paste(Delta, "DOC (um C/L)")))

### The proportional C consumption plots
ggplot(data2)+
  facet_grid(~sample_date>"S10", scales="free_x", labeller=as_labeller(c('FALSE'="Before Reversal", 'TRUE'="After Reversal")))+
  scale_fill_manual(values=scico::scico(palette="bamO", 6, alpha=1, direction=-1, begin=0.1, end=0.7), 
                    labels=c('Col 1', 'Col 2', 'Col 3', 'Col 1', 'Col 2', 'Col 3'), name=NULL)+
  geom_col(position="fill", aes(x=sample_date, y=value, fill=interaction(variable, sample_date>"S10")))+
  theme_bw()+theme(panel.grid = element_blank())

### Other graphs
# DOC consumption in each column throughout the sampling time
ggplot(data2)+
  facet_wrap(~variable)+
  geom_point(aes(x=sample_date, y=value))+
  geom_vline(xintercept="S10", color="red", linetype="dashed")

### Biomass correction of c consumtion
Sample_biomass=fread("C:/Users/c7701233/Nextcloud/Column-Experiment/EEA/Cell_Counts_standardized.csv")
Sample_biomass$col_no=factor(Sample_biomass$col_no, levels = c("1", "2", "3"), labels=c("Col1", "Col2", "Col3"))

data_subset=data[sample_date%in%c("S10", "S13", "S16", "S19")]
data_subset[sample_date=="S10"]$sample_date="S09"
colnames(data_subset)=c("replicate", "sample_date", "Col1", "Col2", "Col3")
data_subset2=melt(data_subset, id.vars = c("replicate", "sample_date"), 
     measure.vars=c("Col1","Col2","Col3"))
data_all=merge(data_subset2, Sample_biomass, all.x=F, 
      by.x=c("replicate", "sample_date", "variable"), 
      by.y=c("replicate", "Sample_date", "col_no"))

data_all[,normalized_doc:=(value/Cell_pro_ml)]

ggplot(data_all)+
  facet_grid( ~sample_date)+
  geom_line(aes(y=value, x=variable, group=replicate, color=replicate), lwd=2)+
  theme(legend.position="right")+theme_bw()

