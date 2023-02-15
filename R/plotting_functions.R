
optical_plots_theme = function(){
  theme_bw()+
    theme(axis.text = element_text(size=10), 
          axis.title = element_text(size=12), 
          text =  element_text(size=10),
          axis.text.x = element_text(size=10, angle=45, vjust=1, hjust=1))
}

set_coloring_column = function(data) {
  data$highlight = factor(ifelse(data$sample_date=="S09" & data$variable=="Col1", "before C1", 
                                 ifelse(data$sample_date=="S09" & data$variable=="Col2", "before C2",
                                        ifelse(data$sample_date=="S09" & data$variable=="Col3", "before C3", 
                                               
                                               ifelse(data$sample_date != c("S09") & data$variable=="Col1", "after C1",
                                                           ifelse(data$sample_date != c("S09") & data$variable=="Col2", "after C2",
                                                                      ifelse(data$sample_date != c("S09") & data$variable=="Col3", "after C3",
                                                                                         "Reservoirs")))))))
  return(data)
}

convert_date_labels = function(data) {
  data$sample_date=factor(data$sample_date, 
                              levels = c("S09","S11","S12" ,"S13", "S14","S15","S16", "S17","S18","S19"), 
                              labels=c("Day0", "Day1","Day2","Day3", "Day7","Day9", "Day10","Day12","Day14","Day17"))
  return(data)
}

fill_col_no = function(){
  scale_fill_manual( name =  "Column Location",
                     #labels=c("Column 1", "Column 2", "Column 3", "Before Reversal"), 
                     values=c("#1741a3", "#4e8fc8", "#a698cc", "white", "white", "white"),
                     guide="legend")
} 

color_col_no = function(){
  scale_color_manual( name =  "Column Location",
                      #labels=c("Column 1", "Column 2", "Column 3", "Before Reversal"), 
                      values=c("black", "black", "black", "#1741a3", "#4e8fc8", "#a698cc"),
                      guide="legend")
} 


n_fun = function(x){
  return(data.frame(y = max(x, na.rm=T), label = paste0(length(x))))
}                                                      

observation_numbers = function (x) {
  stat_summary(fun.data = n_fun, geom = "text", na.rm=T, aes(vjust=0))
}
