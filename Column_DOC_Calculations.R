library(ggplot2)
library(data.table)

####Final data ####
DOC_data =data.table(read.csv("data/DOC_Final_pretreated_all.csv", col.names = c("Sample", "Date", "DOC", "DN")))
DOC_data$Date=as.Date(DOC_data$Date, "%m/%d/%Y")

reservoir = data.table(DOC_data[DOC_data$Sample %like% c("Reservoir")])
samples = subset(DOC_data, !(DOC_data$Sample %like% c("Reservoir")))

samples[,c("sample_date", "replicate", "column_no") := tstrsplit(Sample, "_")]
list(samples,  samples$sample_date)

DOC_data[,c("sample_date", "replicate", "column_no") := tstrsplit(Sample, "_")]
DOC_data[.=lapply(.SD, as.factor), by=c("Sample", "Date", "DOC", "DN")]
DOC_data$column_no=factor(DOC_data$column_no, levels=c("C0", "C1", "C2", "C3"))

DOC_data=DOC_data[DOC<5000]

DOC_data=subset(DOC_data, subset=!(sample_date>"S10" & replicate=="O"))

#DOC_data=DOC_data[!DOC_data$sample_date%in%c("S05","S06", "S19", "S10", "S15")]
DOC_data=DOC_data[!DOC_data$Sample %in% c("S07_A_C0","S08_A_C1", "S08_A_C2","S08_A_C3,",
                                  "S08_G_C0", "S08_G_C1","S08_G_C2","S08_G_C3,")]

DOC_data[sample_date=="S08"]

mean_values=DOC_data[ ,.(mean=mean(DOC, na.rm=T)), by=.(sample_date, column_no)]
table = dcast(data=mean_values, sample_date ~ column_no)
table=table[,.(col1=C0-C1, col2=C1-C2, col3=C2-C3), by=.(sample_date)]
table=melt(table)

ggplot(DOC_data[!(sample_date=="S05"| sample_date=="S06")])+
 facet_grid( ~sample_date)+
 geom_line(aes(y=DOC, x=column_no, group=replicate, color=replicate))+
 geom_line(data=mean_values[!(sample_date=="S05"| sample_date=="S06")], aes(x=column_no, y=mean, group=sample_date), color="red", lwd=2)+
 geom_point(data=mean_values[!(sample_date=="S05"| sample_date=="S06")], aes(x=column_no, y=mean, group=sample_date), color="blue", lwd=5)+
 theme(legend.position="right")+theme_bw()

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
  
ggplot(table)+
  facet_grid(~sample_date>"S10", scales="free_x", labeller=as_labeller(c('FALSE'="Before Treatment", 'TRUE'="After Treatment")))+
  scale_fill_manual(values=scico::scico(palette="bamO", 3, alpha=0.95, direction=-1, begin=0.1, end=0.6), 
                    labels=c('Col 1', 'Col 2', 'Col 3'), name=NULL)+
  geom_col(aes(x=sample_date, y=value, fill=variable))+theme_bw()+theme(panel.grid = element_blank(),text = element_text(size=15))+
  xlab("Sampling day")+ylab(expression(paste(Delta, "DOC (um C/L)")))
  #scale_fill_hue(values=scico::scico(palette="bamO", 3, alpha=0.5, direction=-1, begin=0.1, end=0.5), h=c(0, 90))

