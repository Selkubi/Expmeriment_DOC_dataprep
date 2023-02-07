theme_boxplot = function () {
 theme_bw()+
    theme(axis.text = element_text(size=10), 
          axis.title = element_text(size=12), 
          text =  element_text(size=10))
}

fill_selected = function(){
  scale_fill_manual(
    name =  "Sample Date",
    labels=c("Before \nReversal", "Day1","Day3", "Day10","Day17"), 
    values=c("#bdd5e1", "#a698cc", "#4e8fc8", "#7373ca","#1741a3"),
    guide="legend")
} 

myColors <- ifelse(levels(data_subset2$highlight) == "after C1", "#bdd5e1" , 
            ifelse(levels(data_subset2$highlight)=="after C2", "#4e8fc8",
                   ifelse(levels(data_subset2$highlight)=="after C3", "#a698cc", 
                          ifelse(levels(data_subset2$highlight)=="before C1", "#a698cc",
                                 ifelse(levels(data_subset2$highlight)=="before C2", "#4e8fc8",
                                        ifelse(levels(data_subset2$highlight)=="before C3", "#bdd5e1",
                          "grey90" ))))))

fill_column_color = function(){
  scale_fill_manual(
    name =  "Column Location",
    labels=c("Column 1", "Column 2", "Column 3"), 
    values=myColors,
    guide="legend")
} 


myColors2 <- ifelse(levels(data_subset2$highlight) == "after C1", "#1741a3" , 
                   ifelse(levels(data_subset2$highlight)=="after C2", "#4e8fc8",
                          ifelse(levels(data_subset2$highlight)=="after C3", "#a698cc", 
                                 ifelse(levels(data_subset2$highlight)=="before C1", "grey90",
                                        ifelse(levels(data_subset2$highlight)=="before C2", "grey90",
                                               ifelse(levels(data_subset2$highlight)=="before C3", "grey90",
                                                      "grey90" ))))))
fill_sample_date_col_no = function(){
  scale_fill_manual(
    name =  "Column Location",
    labels=c("Column 1", "Column 2", "Column 3", "Before Reversal"), 
    values=myColors2,
    guide="legend")
} 
