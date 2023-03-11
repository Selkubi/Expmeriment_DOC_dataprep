
optical_plots_theme = function(){
  theme_bw()+
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 12), 
          text =  element_text(size = 10),
          axis.text.x = element_text(size = 10, vjust = 1, hjust = 0.5))
}

set_coloring_column = function(data, col_name) {
  data$highlight = factor(ifelse(data$sample_date %in% c("S08", "S09") & data$variable == "Col1", "before C1", 
                                 ifelse(data$sample_date %in% c("S08", "S09") & data$variable == "Col2", "before C2",
                                        ifelse(data$sample_date %in% c("S08", "S09") & data$variable == "Col3", "before C3", 
                                               
                                               ifelse(!data$sample_date  %in% c("S08", "S09") & data$variable == "Col1", "after C1",
                                                           ifelse(!data$sample_date %in% c("S08", "S09")  & data$variable == "Col2", "after C2",
                                                                      ifelse(!data$sample_date  %in% c("S08", "S09")  & data$variable == "Col3", "after C3",
                                                                                         "Reservoir")))))))
  return(data)
}

convert_date_labels = function(data) {
  data$sample_date = factor(data$sample_date, 
                              levels = c("S08", "S09", "S11", "S12" ,"S13", "S14","S15", "S16", "S17","S18","S19",
                                         "S01", "S02", "S03", "S04", "S07", "S10"), 
                              labels = c("0", "0", "1","2","3", "7","9", "10","12","14","17",
                                       "S01", "S02", "S03", "S04", "S07", "0"))
  return(data)
}

fill_col_no = function(){
  scale_fill_manual( name =  "Column Location",
                     #labels = c("Column 1", "Column 2", "Column 3", "Before Reversal"), 
                     values = c("#1741a3", "#4e8fc8", "#a698cc", "white", "white", "white", "grey"),
                     guide = "legend")
} 

color_col_no = function(){
  scale_color_manual( name =  "Column Location",
                      #labels = c("Column 1", "Column 2", "Column 3", "Before Reversal"), 
                      values = c("black", "black", "black", "#1741a3", "#4e8fc8", "#a698cc", "black"),
                      guide = "legend")
} 


n_fun = function(x){
  return(data.frame(y = max(x, na.rm = TRUE), label = paste0(length(x))))
}                                                      

observation_numbers = function (x) {
  stat_summary(fun.data = n_fun, geom = "text", na.rm = TRUE, aes(vjust = 0))
}
