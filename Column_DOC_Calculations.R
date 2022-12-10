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

data=table[!table$sample_date%in%c("S05", "S06")]

ggplot(data)+
  facet_grid(~sample_date>"S10", scales="free_x", labeller=as_labeller(c('FALSE'="Before Treatment", 'TRUE'="After Treatment")))+
  scale_fill_manual(values=scico::scico(palette="bamO", 6, alpha=1, direction=-1, begin=0.1, end=0.7), 
                    labels=c('Col 1', 'Col 2', 'Col 3', 'Col 1', 'Col 2', 'Col 3'), name=NULL)+
  geom_col(aes(x=sample_date, y=value, fill=interaction(variable, sample_date>"S10")))+theme_bw()+theme(panel.grid = element_blank())+
  xlab("Sampling day")+ylab(expression(paste(Delta, "DOC (um C/L)")))

