theme_boxplot = function () {
 theme_bw()+
    theme(axis.text = element_text(size=16), 
          axis.title = element_text(size=16), 
          text =  element_text(size=16))
}

fill_selected = function(){
  scale_fill_manual(
    name =  "Sample Date",
    labels=c("Day0", "Day3", "Day10","Day17"), 
    values=c("#bdd5e1", "#a698cc", "#4e8fc8", "#1741a3"),
    guide="legend")
} 
