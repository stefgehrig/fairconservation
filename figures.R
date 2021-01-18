### Creation of data visualizations shown in main text ###
#Clear environment
rm(list = ls(all.names = TRUE))

#Load libraries
library(readr)
library(dplyr) 
library(ggplot2)
library(cowplot)

#Load adapted raincloud function
source("functions/function_raincloudplot.R")

#Import data
df  <- read_csv("exp_data.csv") #experimental data

#Prepare variables
df$fair     <- 6-df$fair
df$fair_fct <- as.factor(df$fair)
df$fair     <- as.numeric(as.character(df$fair))

df$accountab <- df %>% 
  dplyr::select(grep("vignette", names(.))) %>% 
  mutate(vignette1 = {ifelse(.[,1] == "a", 1, 0)}, 
         #vignette1: "a" reflects endorsement (see reference Konow 1996)
         vignette2 = {ifelse(.[,2] == "b", 1, 0)},
         #vignette2: "b" reflects endorsement (see reference Konow 1996)
         vignette3 = {ifelse(.[,3] == "a", 1, 0)},
         #vignette3: "a" reflects endorsement (see reference Konow 1996)
         vignette4 = {ifelse(.[,4] == "b", 1, 0)}) %>% 
  #vignette4: "b" reflects endorsement (see reference Konow 1996)
  rowSums

#set seed for jittering observations
set.seed(1001)

###################################
###################################
### Figure: Conservation effort ###
###################################
###################################

#Prepare variables
df$payrate_plot <- as.factor(ifelse(df$payrate==400, "Low pay rate", "High pay rate"))
df$payrate_plot <- ordered(df$payrate_plot, levels = c("Low pay rate", "High pay rate"))
df$grouptype_plot <- as.numeric(as.character(ifelse(df$grouptype == "equal", 2, 1)))
df$grouptype_name <- as.factor(ifelse(df$grouptype=="equal", "Equal", "Unequal"))
df$grouptype_name <- relevel(df$grouptype_name, ref = "Unequal")

#Create object
figure_bags <- ggplot(
  df,aes(x=grouptype_name, y=bags, fill = grouptype_name))+
  geom_flat_violin(position = position_nudge(x = 0, y = 0),adjust=1.7, trim = FALSE, alpha = 1, linetype = 0) +
  geom_point(
    aes(x = grouptype_plot-0.3, y = bags), position = position_jitter(width = .1), size = 1.1, shape = 1, color = "grey50") +
  geom_boxplot(aes(x = grouptype_plot, y = bags),outlier.color = NA, alpha = 1, width = .3, colour = "BLACK") +
  stat_boxplot(aes(x = grouptype_plot), geom = 'errorbar', width = .3, col = "BLACK") +
  ylab("Conservation effort (No. of bags)")+xlab("Pay distribution")+coord_flip()+theme_cowplot()+guides(fill = FALSE, colour = FALSE) + 
  facet_wrap(~payrate_plot, ncol = 1) +
  scale_colour_manual(values = c("#fdcdac","#cbd5e8")) +
  scale_fill_manual(values = c("#fdcdac","#cbd5e8")) +
  theme(axis.text=element_text(size=12),
        axis.text.y=element_text(angle=90, hjust = 0.5),
        axis.title=element_text(size=13,face="bold"),
        strip.text.x = element_text(size = 10.5),
        strip.background = element_rect(fill = "grey90")) +
  scale_x_discrete(labels=c("Unequal", "Equal"))

png("figure_effort.png", width = 1450, height = 1400, res = 320)
figure_bags
dev.off()

########################################
########################################
### Figure: Accountability agreement ###
########################################
########################################

#Prepare variables
acc_table <- data_frame(
  accscore = factor(c("0/4", "1/4", "2/4", "3/4", "4/4")),
  freq =    c(0, prop.table(table(df$accountab)))
)
prop.table(table(df$accountab))
table(df$accountab)

#Create object
figure_acc <- ggplot(acc_table, aes(y=freq, 
                                    x=accscore)) + 
  geom_bar(stat="identity", fill ="grey") +    
  labs(title = "Endorsement of Accountability Principle (AP)",
       x = "No. of vignette responses consistent with AP",
       y = "Proportion") +
  theme(axis.title=element_text(size=13,face="bold"),
        axis.text.y=element_text(size = 12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 13, face = "italic")) +
  scale_y_continuous(sec.axis = sec_axis(~.*sum(!is.na(df$accountab)), name = "N"))

png("figure_accountability.png", width = 1050, height = 1000, res = 230)
figure_acc
dev.off()

#################################
#################################
### Figure: Fairness judgment ###
#################################
#################################

#Prepare variables
fair_table <- data_frame(
  outcome = factor(rep(c("Very", "Somewhat", "Undecided", "Not so", "Not at all"), 2)),
  freq =    c(rev(prop.table(table(df$fair[df$grouptype=="equal"]))),
              rev(prop.table(table(df$fair[df$grouptype=="inequal"])))),
  treat =   c(rep("Equal", 5),
              rep("Unequal", 5))
)

fair_table$outcome <- factor(fair_table$outcome, levels = c("Very", "Somewhat", "Undecided", "Not so", "Not at all"))
fair_table$treat   <- factor(fair_table$treat, levels = c("Equal", "Unequal"))

#Create object
figure_judg <- ggplot(fair_table, aes(y=freq, 
                                      x=treat,
                                      fill=treat)) +
  geom_bar(stat="identity") +    
  facet_wrap(~outcome, ncol = 5,strip.position = "bottom") +
  labs(title= "How fair do you think your own pay rate was?",
       x = "Fairness judgment",
       y = "Proportion",
       fill = "Pay distribution:") +
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.line.x = element_blank(),
        axis.title=element_text(size=13,face="bold"),
        axis.text.y=element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.title=element_text(size=12, face ="bold"),
        legend.text=element_text(size=11),
        legend.position = c(0.74,0.78),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 13, face = "italic"),
        strip.text.x = element_text(size = 10.5),
        strip.background = element_rect(fill = "grey90")) +
  scale_y_continuous(sec.axis = sec_axis(~.*sum(!is.na(df$fair[df$grouptype=="equal"])), name = "N")) +
  guides(colour = guide_legend(reverse=T)) +
  scale_fill_manual(values = c("#cbd5e8", "#fdcdac"))

png("figure_fairness.png", width = 1400, height = 700, res = 250)
figure_judg
dev.off()

#################################
#################################
### Latter two figures in one ###
#################################
#################################

png("figure_accountability_fairness.png", width = 1950, height = 2300, res = 350)
plot_grid(figure_acc, figure_judg, 
          labels = c('A', 'B'), label_size = 16, ncol = 1, scale = 0.97)
dev.off()
