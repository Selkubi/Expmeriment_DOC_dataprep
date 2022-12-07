
ratio_calculation=function(data, acid_volume) {
  sample=data[["Sample_weight"]]-data[["Empty_DOC_vial"]]
  water=data[["Total_weight"]]-data[["Sample_weight"]]+acid_volume
  W_S_ratio = water/sample
  return(W_S_ratio)
}


volume_adjustion=function(data, ratio) {
  DOC_final=data*ratio
  return(DOC_final)
}


#### 17.09.21 data ####
DOC = read.csv("data/DOC_results_17.09.21.csv")
weights = read.csv("data/Sampling_calculations_17.09.21.csv")

weights$ratio=ratio_calculation(weights, acid_volume = 0.1)
DOC$final_volume=volume_adjustion(DOC$DOC, weights$ratio)

DOC_17.09.21=DOC
weights_17.09.21=weights

####29.09.21 data ####
DOC = read.csv("data/DOC_results_29.09.21.csv", col.names = c("Col_code", "date_received", "DOC", "DN", "IC"))
weights = read.csv("data/Sampling_calculations_29.09.21.csv")

weights$ratio=ratio_calculation(weights, acid_volume = 0.1)
DOC$final_volume=volume_adjustion(DOC$DOC, weights$ratio)

lapply(X=DOC[["Col_code"]], FUN=grepl, x="C1", fixed=F)

pilot
####29.09.21 data ####
DOC_pilot = read.csv("data/DOC_results_Pilot.csv", col.names = c("Sample", "Date", "DOC", "DN"))
DOC_pilot$Date=as.Date(DOC_pilot$Date, "%m/%d/%Y")
DOC_pilot[5,"Date"]=DOC_pilot[4,"Date"]


DOC_pilot$Sample=factor(DOC_pilot$Sample, levels=c("Reservoir", "C1", "C2", "C3", "C4"))
DOC_pilot$Sample=factor(DOC_pilot$Sample, levels=c("Reservoir", "C3R", "C2R", "C1R"), labels = c("Reservoir", "C1", "C2", "C3"))

library(ggplot2)
ggplot(DOC_direct[DOC_direct$Date == as.Date("2022-01-25", "%Y-%m-%d"),])+
  geom_line(aes(x=Sample, y=DOC, group=as.factor(Date), col=as.factor(Date)))+
  geom_line(data=DOC_reverse, aes(x=Sample, y=DOC, group=as.factor(Date), col=as.factor(Date)))
  



