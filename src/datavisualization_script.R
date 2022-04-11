library(tidyverse)
library(cowplot)
library(ggplot2)

##PLOTS##
# I used ggplot to visualize the data summaries. Each parameter space had a summary for each scenario. 


#Total population plots
pop_plot_LHNN <- ggplot(pop_df_sum_LHNNfc, aes(x = year, y = mean_pop)) + 
  geom_line(data = pop_df_sum_LHNNfc, aes(color = "Full Connectivity"), size = 1.0) +
  geom_ribbon(data = pop_df_sum_LHNNfc, aes(ymin=mean_pop-CI, ymax=mean_pop+CI), fill="black", color="black", linetype=0, alpha=.08) +
  geom_line(data = pop_df_sum_LHNNsq, aes(color = "Status Quo"), size = 1.0, linetype="dashed") +
  geom_ribbon(data = pop_df_sum_LHNNsq, aes(ymin=mean_pop-CI, ymax=mean_pop+CI), fill="red", color="red", linetype=0, alpha=.08) +
  geom_line(data = pop_df_sum_LHNNo1, aes(color = "Opportunistic 1"), size = 1.0) +
  geom_ribbon(data = pop_df_sum_LHNNo1, aes(ymin=mean_pop-CI, ymax=mean_pop+CI), fill="darkorange", color="darkorange", linetype=0, alpha=.08) +
  geom_line(data = pop_df_sum_LHNNo2, aes(color = "Opportunistic 2"), size = 1.0) +
  geom_ribbon(data = pop_df_sum_LHNNo2, aes(ymin=mean_pop-CI, ymax=mean_pop+CI), fill="darkorange2", color="darkorange2", linetype=0, alpha=.08) +
  geom_line(data = pop_df_sum_LHNNo3, aes(color = "Opportunistic 3"), size = 1.0) +
  geom_ribbon(data = pop_df_sum_LHNNo3, aes(ymin=mean_pop-CI, ymax=mean_pop+CI), fill="darkorange3", color="darkorange3", linetype=0, alpha=.08) +
  geom_line(data = pop_df_sum_LHNNp1, aes(color = "Protect YCT 1"), size = 1.0) +
  geom_ribbon(data = pop_df_sum_LHNNp1, aes(ymin=mean_pop-CI, ymax=mean_pop+CI), fill="slateblue1", color="slateblue1", linetype=0, alpha=.08) +
  geom_line(data = pop_df_sum_LHNNp2, aes(color = "Protect YCT 2"), size = 1.0) +
  geom_ribbon(data = pop_df_sum_LHNNp2, aes(ymin=mean_pop-CI, ymax=mean_pop+CI), fill="slateblue3", color="slateblue3", linetype=0, alpha=.08) +
  geom_line(data = pop_df_sum_LHNNp3, aes(color = "Protect YCT 3"), size = 1.0) +
  geom_ribbon(data = pop_df_sum_LHNNp3, aes(ymin=mean_pop-CI, ymax=mean_pop+CI), fill="slateblue4", color="slateblue4", linetype=0, alpha=.08) +
  geom_line(data = pop_df_sum_ARNNsq, aes(color = "All Residents"), size = 1.0) +
  geom_ribbon(data = pop_df_sum_ARNNsq, aes(ymin=mean_pop-CI, ymax=mean_pop+CI), fill="green4", color="green4", linetype=0, alpha=.08) +
  labs(x="Year", y="Population Size", color = "Scenarios") +
  scale_color_manual(values = c("Full Connectivity"="black", "Status Quo"="red", "Opportunistic 1" = "darkorange", "Opportunistic 2" = "darkorange2", "Opportunistic 3" = "darkorange3", "Protect YCT 1" = "slateblue1", "Protect YCT 2" = "slateblue3", "Protect YCT 3" = "slateblue4", "All Residents" = "green4")) +
  scale_x_continuous(n.breaks = 5, limits = c(1,200)) +
  scale_y_continuous(n.breaks = 5, limits = c(25000, 60000)) +
  ggtitle("Life History Variation") +
  theme(legend.position = "right") +
  theme_bw(base_size = 14) +
  geom_vline(xintercept = 50)

#Relative Abundance plot
species_plot_HAM1NN <- ggplot(yctpercent_df_sum_HAM1NNfc, aes(x = year, y = mean_percent)) + 
  geom_line(data = yctpercent_df_sum_HAM1NNfc, aes(color = "Full Connectivity",linetype = "YCT"), size = 1.0) +
  geom_ribbon(data = yctpercent_df_sum_HAM1NNfc, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="black", color="black", linetype=0, alpha=.08) +
  geom_line(data = yctpercent_df_sum_HAM1NNsq, aes(color = "Status Quo",linetype = "YCT"), size = 1.0) +
  geom_ribbon(data = yctpercent_df_sum_HAM1NNsq, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="red", color="red", linetype=0, alpha=.08) +
  geom_line(data = yctpercent_df_sum_HAM1NNo1, aes(color = "Opportunistic 1",linetype = "YCT"), size = 1.0) +
  geom_ribbon(data = yctpercent_df_sum_HAM1NNo1, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="darkorange", color="darkorange", linetype=0, alpha=.08) +
  geom_line(data = yctpercent_df_sum_HAM1NNo2, aes(color = "Opportunistic 2",linetype = "YCT"), size = 1.0) +
  geom_ribbon(data = yctpercent_df_sum_HAM1NNo2, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="darkorange2", color="darkorange2", linetype=0, alpha=.08) +
  geom_line(data = yctpercent_df_sum_HAM1NNo3, aes(color = "Opportunistic 3",linetype = "YCT"), size = 1.0) +
  geom_ribbon(data = yctpercent_df_sum_HAM1NNo3, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="darkorange3", color="darkorange3", linetype=0, alpha=.08) +
  geom_line(data = yctpercent_df_sum_HAM1NNp1, aes(color = "Protect YCT 1",linetype = "YCT"), size = 1.0) +
  geom_ribbon(data = yctpercent_df_sum_HAM1NNp1, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="slateblue1", color="slateblue1", linetype=0, alpha=.08) +
  geom_line(data = yctpercent_df_sum_HAM1NNp2, aes(color = "Protect YCT 2",linetype = "YCT"), size = 1.0) +
  geom_ribbon(data = yctpercent_df_sum_HAM1NNp2, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="slateblue3", color="slateblue3", linetype=0, alpha=.08) +
  geom_line(data = yctpercent_df_sum_HAM1NNp3, aes(color = "Protect YCT 3",linetype = "YCT"), size = 1.0) +
  geom_ribbon(data = yctpercent_df_sum_HAM1NNp3, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="slateblue4", color="slateblue4", linetype=0, alpha=.08) +
  geom_line(data = cutbowpercent_df_sum_HAM1NNfc, aes(color = "Full Connectivity",linetype = "Hybrid"), size = 1.0) +
  geom_ribbon(data = cutbowpercent_df_sum_HAM1NNfc, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="black", color="black", linetype=0, alpha=.08) +
  geom_line(data = cutbowpercent_df_sum_HAM1NNsq, aes(color = "Status Quo",linetype = "Hybrid"), size = 1.0) +
  geom_ribbon(data = cutbowpercent_df_sum_HAM1NNsq, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="red", color="red", linetype=0, alpha=.08) +
  geom_line(data = cutbowpercent_df_sum_HAM1NNo1, aes(color = "Opportunistic 1",linetype = "Hybrid"), size = 1.0) +
  geom_ribbon(data = cutbowpercent_df_sum_HAM1NNo1, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="darkorange", color="darkorange", linetype=0, alpha=.08) +
  geom_line(data = cutbowpercent_df_sum_HAM1NNo2, aes(color = "Opportunistic 2",linetype = "Hybrid"), size = 1.0) +
  geom_ribbon(data = cutbowpercent_df_sum_HAM1NNo2, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="darkorange2", color="darkorange2", linetype=0, alpha=.08) +
  geom_line(data = cutbowpercent_df_sum_HAM1NNo3, aes(color = "Opportunistic 3",linetype = "Hybrid"), size = 1.0) +
  geom_ribbon(data = cutbowpercent_df_sum_HAM1NNo3, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="darkorange3", color="darkorange3", linetype=0, alpha=.08) +
  geom_line(data = cutbowpercent_df_sum_HAM1NNp1, aes(color = "Protect YCT 1",linetype = "Hybrid"), size = 1.0) +
  geom_ribbon(data = cutbowpercent_df_sum_HAM1NNp1, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="slateblue1", color="slateblue1", linetype=0, alpha=.08) +
  geom_line(data = cutbowpercent_df_sum_HAM1NNp2, aes(color = "Protect YCT 2",linetype = "Hybrid"), size = 1.0) +
  geom_ribbon(data = cutbowpercent_df_sum_HAM1NNp2, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="slateblue3", color="slateblue3", linetype=0, alpha=.08) +
  geom_line(data = cutbowpercent_df_sum_HAM1NNp3, aes(color = "Protect YCT 3",linetype = "Hybrid"), size = 1.0) +
  geom_ribbon(data = cutbowpercent_df_sum_HAM1NNp3, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="slateblue4", color="slateblue4", linetype=0, alpha=.08) +
  #geom_line(data = rbtpercent_df_sum_HAM2NNfc, aes(color = "Full Connectivity",linetype = "RBT"), size = 1.0) +
  #geom_line(data = rbtpercent_df_sum_HAM2NNsq, aes(color = "Status Quo",linetype = "RBT"), size = 1.0) +
  #geom_line(data = rbtpercent_df_sum_HAM2NNo1, aes(color = "Opportunistic 1",linetype = "RBT"), size = 1.0) +
  #geom_line(data = rbtpercent_df_sum_HAM2NNo2, aes(color = "Opportunistic 2",linetype = "RBT"), size = 1.0) +
  #geom_line(data = rbtpercent_df_sum_HAM2NNo3, aes(color = "Opportunistic 3",linetype = "RBT"), size = 1.0) +
  #geom_line(data = rbtpercent_df_sum_HAM2NNp1, aes(color = "Protect YCT 1",linetype = "RBT"), size = 1.0) +
  #geom_line(data = rbtpercent_df_sum_HAM2NNp2, aes(color = "Protect YCT 2",linetype = "RBT"), size = 1.0) +
  #geom_line(data = rbtpercent_df_sum_HAM2NNp3, aes(color = "Protect YCT 3",linetype = "RBT"), size = 1.0) +
  labs(x="Year", y="Percent of Population", color = "Scenarios", linetype = "Species") +
  scale_color_manual(values = c("Full Connectivity"="black", "Status Quo"="red", "Opportunistic 1" = "darkorange", "Opportunistic 2" = "darkorange2", "Opportunistic 3" = "darkorange3", "Protect YCT 1" = "slateblue1", "Protect YCT 2" = "slateblue3", "Protect YCT 3" = "slateblue4"),
                     breaks = c("Full Connectivity", "Status Quo", "Opportunistic 1", "Opportunistic 2", "Opportunistic 3", "Protect YCT 1", "Protect YCT 2", "Protect YCT 3")) +
  scale_linetype_manual(values = c("YCT"="solid", "Hybrid"="dashed"), breaks = c("YCT", "Hybrid")) +
  scale_x_continuous(n.breaks = 5, limits = c(1,200)) +
  scale_y_continuous(breaks = c(0,25,50,75,100), limits = c(-10,110)) +
  ggtitle("Hybridization AM1") +
  theme(legend.position = "right") +
  theme_bw(base_size = 14) +
  geom_vline(xintercept = 50)

migrantPercent_plot_HLHAM1 <- ggplot(MigrantPercent_df_sum_HLHAM1fc, aes(x = year, y = mean_percent)) + 
  geom_line(data = MigrantPercent_df_sum_HLHAM1fc, aes(color = "Full Connectivity"), size = 1.0) +
  geom_ribbon(data = MigrantPercent_df_sum_HLHAM1fc, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="black", color="black", linetype=0, alpha=.08) +
  geom_line(data = MigrantPercent_df_sum_HLHAM1sq, aes(color = "Status Quo"), size = 1.0, linetype="dashed") +
  geom_ribbon(data = MigrantPercent_df_sum_HLHAM1sq, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="red", color="red", linetype=0, alpha=.08) +
  geom_line(data = MigrantPercent_df_sum_HLHAM1o1, aes(color = "Opportunistic 1"), size = 1.0) +
  geom_ribbon(data = MigrantPercent_df_sum_HLHAM1o1, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="darkorange", color="darkorange", linetype=0, alpha=.08) +
  geom_line(data = MigrantPercent_df_sum_HLHAM1o2, aes(color = "Opportunistic 2"), size = 1.0) +
  geom_ribbon(data = MigrantPercent_df_sum_HLHAM1o2, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="darkorange2", color="darkorange2", linetype=0, alpha=.08) +
  geom_line(data = MigrantPercent_df_sum_HLHAM1o3, aes(color = "Opportunistic 3"), size = 1.0) +
  geom_ribbon(data = MigrantPercent_df_sum_HLHAM1o3, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="darkorange3", color="darkorange3", linetype=0, alpha=.08) +
  geom_line(data = MigrantPercent_df_sum_HLHAM1p1, aes(color = "Protect YCT 1"), size = 1.0) +
  geom_ribbon(data = MigrantPercent_df_sum_HLHAM1p1, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="slateblue1", color="slateblue1", linetype=0, alpha=.08) +
  geom_line(data = MigrantPercent_df_sum_HLHAM1p2, aes(color = "Protect YCT 2"), size = 1.0) +
  geom_ribbon(data = MigrantPercent_df_sum_HLHAM1p2, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="slateblue3", color="slateblue3", linetype=0, alpha=.08) +
  geom_line(data = MigrantPercent_df_sum_HLHAM1p3, aes(color = "Protect YCT 3"), size = 1.0) +
  geom_ribbon(data = MigrantPercent_df_sum_HLHAM1p3, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="slateblue4", color="slateblue4", linetype=0, alpha=.08) +
  #geom_line(data = MigrantPercent_df_sum_ARsq, aes(color = "All Residents"), size = 1.0, linetype="dotted") +
  labs(x="Year", y="% Migrants", color = "Scenarios") +
  scale_color_manual(values = c("Full Connectivity"="black", "Status Quo"="red", "Opportunistic 1" = "darkorange", "Opportunistic 2" = "darkorange2", "Opportunistic 3" = "darkorange3", "Protect YCT 1" = "slateblue1", "Protect YCT 2" = "slateblue3", "Protect YCT 3" = "slateblue4")) +
  scale_x_continuous(n.breaks = 5, limits = c(1,200)) +
  ggtitle("Hybridization AM1 + Life History Variation + Brook/Brown Trout") +
  scale_y_continuous(n.breaks = 5, limits = c(6, 13)) +
  theme(legend.position = "right") +
  theme_bw(base_size = 14)

migrantPercent_plot_cLHV <- ggplot(migrantpercentC_LHfc, aes(x = year, y = migrantpercentC)) + 
  geom_line(data = migrantpercentC_LHfc, aes(color = "Full Connectivity"), size = 1.0) +
  geom_line(data = migrantpercentC_LHsq, aes(color = "Status Quo"), size = 1.0, linetype="dashed") +
  geom_line(data = migrantpercentC_LHo1, aes(color = "Opportunistic 1"), size = 1.0) +
  geom_line(data = migrantpercentC_LHo2, aes(color = "Opportunistic 2"), size = 1.0) +
  geom_line(data = migrantpercentC_LHo3, aes(color = "Opportunistic 3"), size = 1.0) +
  geom_line(data = migrantpercentC_LHp1, aes(color = "Protect YCT 1"), size = 1.0) +
  geom_line(data = migrantpercentC_LHp2, aes(color = "Protect YCT 2"), size = 1.0) +
  geom_line(data = migrantpercentC_LHp3, aes(color = "Protect YCT 3"), size = 1.0) +
  #geom_line(data = MigrantPercent_df_sum_ARsq, aes(color = "All Residents"), size = 1.0, linetype="dotted") +
  labs(x="Year", y="difference in % Migrants from Status Quo", color = "Scenarios") +
  scale_color_manual(values = c("Full Connectivity"="black", "Status Quo"="red", "Opportunistic 1" = "chocolate1", "Opportunistic 2" = "chocolate3", "Opportunistic 3" = "chocolate 4", "Protect YCT 1" = "slateblue1", "Protect YCT 2" = "slateblue3", "Protect YCT 3" = "slateblue4")
  ) +
  scale_x_continuous(n.breaks = 5, limits = c(1,200)) +
  ggtitle("Life History Variation") +
  scale_y_continuous(n.breaks = 5, limits = c(-2.5, 2.5)) +
  theme(legend.position = "right") +
  theme_bw(base_size = 14)

#percent large YCT
yctlargepercent_plot_HLHAM3NN <- ggplot(yctlargepercent_df_sum_HLHAM3NNfc, aes(x = year, y = mean_percent)) + 
  geom_line(data = yctlargepercent_df_sum_HLHAM3NNfc, aes(color = "Full Connectivity"), size = 1.0) +
  geom_ribbon(data = yctlargepercent_df_sum_HLHAM3NNfc, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="black", color="black", linetype=0, alpha=.08) +
  geom_line(data = yctlargepercent_df_sum_HLHAM3NNsq, aes(color = "Status Quo"), size = 1.0, linetype="dashed") +
  geom_ribbon(data = yctlargepercent_df_sum_HLHAM3NNsq, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="red", color="red", linetype=0, alpha=.08) +
  geom_line(data = yctlargepercent_df_sum_HLHAM3NNo1, aes(color = "Opportunistic 1"), size = 1.0) +
  geom_ribbon(data = yctlargepercent_df_sum_HLHAM3NNo1, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="darkorange", color="darkorange", linetype=0, alpha=.08) +
  geom_line(data = yctlargepercent_df_sum_HLHAM3NNo2, aes(color = "Opportunistic 2"), size = 1.0) +
  geom_ribbon(data = yctlargepercent_df_sum_HLHAM3NNo2, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="darkorange2", color="darkorange2", linetype=0, alpha=.08) +
  geom_line(data = yctlargepercent_df_sum_HLHAM3NNo3, aes(color = "Opportunistic 3"), size = 1.0) +
  geom_ribbon(data = yctlargepercent_df_sum_HLHAM3NNo3, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="darkorange3", color="darkorange3", linetype=0, alpha=.08) +
  geom_line(data = yctlargepercent_df_sum_HLHAM3NNp1, aes(color = "Protect YCT 1"), size = 1.0) +
  geom_ribbon(data = yctlargepercent_df_sum_HLHAM3NNp1, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="slateblue1", color="slateblue1", linetype=0, alpha=.08) +
  geom_line(data = yctlargepercent_df_sum_HLHAM3NNp2, aes(color = "Protect YCT 2"), size = 1.0) +
  geom_ribbon(data = yctlargepercent_df_sum_HLHAM3NNp2, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="slateblue3", color="slateblue3", linetype=0, alpha=.08) +
  geom_line(data = yctlargepercent_df_sum_HLHAM3NNp3, aes(color = "Protect YCT 3"), size = 1.0) +
  geom_ribbon(data = yctlargepercent_df_sum_HLHAM3NNp3, aes(ymin=mean_percent-CI, ymax=mean_percent+CI), fill="slateblue4", color="slateblue4", linetype=0, alpha=.08) +
  #geom_line(data = yctlargepercent_df_sum_ARNNsq, aes(color = "All Residents"), size = 1.0, linetype="dotted") +
  labs(x="Year", y="% Migratory YCT >300mm", color = "Scenarios") +
  scale_color_manual(values = c("Full Connectivity"="black", "Status Quo"="red", "Opportunistic 1" = "darkorange", "Opportunistic 2" = "darkorange2", "Opportunistic 3" = "darkorange3", "Protect YCT 1" = "slateblue1", "Protect YCT 2" = "slateblue3", "Protect YCT 3" = "slateblue4")) +
  scale_x_continuous(n.breaks = 5, limits = c(1,200)) +
  ggtitle("Self-Preference + Life History Variation") +
  scale_y_continuous(n.breaks = 5, limits = c(0, 0.5)) +
  theme(legend.position = "right") +
  theme_bw(base_size = 14)


hybridpercent_plot_HLHBB <- ggplot(HybridPercent_df_sum_HLHBBfc, aes(x = year, y = mean_percent)) + 
  geom_line(data = HybridPercent_df_sum_HLHBBfc, aes(color = "Full Connectivity"), size = 1.0) +
  geom_line(data = HybridPercent_df_sum_HLHBBsq, aes(color = "Status Quo"), size = 1.0) +
  geom_line(data = HybridPercent_df_sum_HLHBBo1, aes(color = "Opportunistic 1"), size = 1.0) +
  geom_line(data = HybridPercent_df_sum_HLHBBho2, aes(color = "Opportunistic 2"), size = 1.0) +
  geom_line(data = HybridPercent_df_sum_HLHBBo3, aes(color = "Opportunistic 3"), size = 1.0) +
  geom_line(data = HybridPercent_df_sum_HLHBBp1, aes(color = "Protect YCT 1"), size = 1.0) +
  geom_line(data = HybridPercent_df_sum_HLHBBp2, aes(color = "Protect YCT 2"), size = 1.0) +
  geom_line(data = HybridPercent_df_sum_HLHBBp3, aes(color = "Protect YCT 3"), size = 1.0) +
  labs(x="Year", y="% Hybrids", color = "Scenarios") +
  scale_color_manual(values = "Full Connectivity"="black", "Status Quo"="red", "Opportunistic 1" = "chocolate1", "Opportunistic 2" = "chocolate3", "Opportunistic 3" = "chocolate 4", "Protect YCT 1" = "slateblue1", "Protect YCT 2" = "slateblue3", "Protect YCT 3" = "slateblue4")+ +
  scale_x_continuous(n.breaks = 5, limits = c(1,200)) +
  scale_y_continuous(n.breaks = 5, limits = c(0, 100)) +
  ggtitle("Hybridization+Life History Variation and no Brook/Brown trout") +
  theme(legend.position = "right") +
  theme_bw(base_size = 14)


# Code to adjust plots for publications

pop_plot_LHV <- pop_plot_LHV + theme(plot.title = element_text(size=13, face="bold"), legend.title= element_text(size=13, face="bold"), axis.title=element_text(size=13)) +scale_y_continuous(n.breaks = 5, limits = c(25000, 60000)) + ggtitle("Life History Variation + Brook/Brown Trout")
pop_plot_LHVNN <- pop_plot_LHVNN + theme(plot.title = element_text(size=13, face ="bold"), legend.title= element_text(size=13, face="bold"), axis.title=element_text(size=13)) + ggtitle("Life History Variation")
migrantPercent_plot_LHV <- migrantPercent_plot_LHV +theme(plot.title = element_text(size=12))
migrantPercent_plot_LHVNN <- migrantPercent_plot_LHVNN + theme(plot.title = element_text(size=12))
migrantPercent_plot_HLHAM1 <- migrantPercent_plot_HLHAM1 +theme(plot.title = element_text(size=7))
migrantPercent_plot_HLHAM1NN <- migrantPercent_plot_HLHAM1NN +theme(plot.title = element_text(size=7))
migrantPercent_plot_HLHAM2 <- migrantPercent_plot_HLHAM2 +theme(plot.title = element_text(size=7))
migrantPercent_plot_HLHAM2NN <- migrantPercent_plot_HLHAM2NN +theme(plot.title = element_text(size=7))
migrantPercent_plot_HLHAM3 <- migrantPercent_plot_HLHAM3 +theme(plot.title = element_text(size=7))
migrantPercent_plot_HLHAM3NN <- migrantPercent_plot_HLHAM3NN +theme(plot.title = element_text(size=7))

yctlargepercent_plot_LHV <- yctlargepercent_plot_LHV + theme(plot.title = element_text(size=13), axis.title=element_text(size=13),legend.title = element_text(size=13), axis.text = element_text(size=13)) + scale_y_continuous(n.breaks = 5, limits = c(0, 0.25))
yctlargepercent_plot_LHVNN <- yctlargepercent_plot_LHVNN + theme(plot.title = element_text(size=13), axis.title=element_text(size=13),legend.title = element_text(size=13), axis.text = element_text(size=13)) + scale_y_continuous(n.breaks = 5, limits = c(0, 0.25))
yctlargepercent_plot_HLHAM1 <- yctlargepercent_plot_HLHAM1 + ggtitle("Random Mating + Life History Variation \n + Brook/Brown Trout") + theme(plot.title = element_text(size=10), axis.title=element_text(size=10),legend.title = element_text(size=10), axis.text = element_text(size=10)) + scale_y_continuous(n.breaks = 5, limits = c(0, 0.25))
yctlargepercent_plot_HLHAM1NN <- yctlargepercent_plot_HLHAM1NN + theme(plot.title = element_text(size=10), axis.title=element_text(size=10),legend.title = element_text(size=10), axis.text = element_text(size=10)) + scale_y_continuous(n.breaks = 5, limits = c(0, 0.25))
yctlargepercent_plot_HLHAM2 <- yctlargepercent_plot_HLHAM2 + ggtitle("Intraspecific Mating + Life History Variation \n + Brook/Brown Trout")+ theme(plot.title = element_text(size=10), axis.title=element_text(size=10),legend.title = element_text(size=10), axis.text = element_text(size=10)) + scale_y_continuous(n.breaks = 5, limits = c(0, 0.25))
yctlargepercent_plot_HLHAM2NN <- yctlargepercent_plot_HLHAM2NN + theme(plot.title = element_text(size=10), axis.title=element_text(size=10),legend.title = element_text(size=10), axis.text = element_text(size=10)) + scale_y_continuous(n.breaks = 5, limits = c(0, 0.25))
yctlargepercent_plot_HLHAM3 <- yctlargepercent_plot_HLHAM3 + ggtitle("Self-Preference Mating + Life History Variation \n + Brook/Brown Trout") + theme(plot.title = element_text(size=10), axis.title=element_text(size=10),legend.title = element_text(size=10), axis.text = element_text(size=10)) + scale_y_continuous(n.breaks = 5, limits = c(0, 0.25))
yctlargepercent_plot_HLHAM3NN <- yctlargepercent_plot_HLHAM3NN + theme(plot.title = element_text(size=10), axis.title=element_text(size=10),legend.title = element_text(size=10), axis.text = element_text(size=10)) + scale_y_continuous(n.breaks = 5, limits = c(0, 0.25))


species_plot_HAM1 <- species_plot_HAM1 + theme(plot.title = element_text(size=12, face="bold"), axis.title = element_text(size=12), legend.title = element_text(size=12, face="bold"), legend.text = element_text(size=12), axis.text = element_text(size=12)) +  scale_color_manual(values = c("Full Connectivity"="black", "Status Quo"="red", "Opportunistic 1" = "darkorange", "Opportunistic 2" = "darkorange2", "Opportunistic 3" = "darkorange3", "Protect YCT 1" = "slateblue1", "Protect YCT 2" = "slateblue3", "Protect YCT 3" = "slateblue4"), breaks = c("Full Connectivity", "Status Quo", "Opportunistic 1", "Opportunistic 2", "Opportunistic 3", "Protect YCT 1", "Protect YCT 2", "Protect YCT 3")) + scale_linetype_manual(values = c("YCT"="solid", "Hybrid"="dashed"), breaks = c("YCT", "Hybrid")) + scale_x_continuous(n.breaks = 5, limits = c(1,200)) + ggtitle("Random Mating + Brook/Brown Trout") + labs(x="Year", y="Relative Abundance (%)", linetype = "Species", color = "Scenarios")
species_plot_HAM1NN <- species_plot_HAM1NN+ theme(plot.title = element_text(size=12, face="bold"), axis.title = element_text(size=12), legend.title = element_text(size=12, face="bold"), legend.text = element_text(size=12), axis.text = element_text(size=12)) +  scale_color_manual(values = c("Full Connectivity"="black", "Status Quo"="red", "Opportunistic 1" = "darkorange", "Opportunistic 2" = "darkorange2", "Opportunistic 3" = "darkorange3", "Protect YCT 1" = "slateblue1", "Protect YCT 2" = "slateblue3", "Protect YCT 3" = "slateblue4"), breaks = c("Full Connectivity", "Status Quo", "Opportunistic 1", "Opportunistic 2", "Opportunistic 3", "Protect YCT 1", "Protect YCT 2", "Protect YCT 3")) + scale_linetype_manual(values = c("YCT"="solid", "Hybrid"="dashed"), breaks = c("YCT", "Hybrid")) + scale_x_continuous(n.breaks = 5, limits = c(1,200)) + ggtitle("Random Mating") + labs(x="Year", y="Relative Abundance (%)", linetype = "Species", color = "Scenarios")
species_plot_HAM2 <- species_plot_HAM2+ theme(plot.title = element_text(size=12, face="bold"), axis.title = element_text(size=12), legend.title = element_text(size=12, face="bold"), legend.text = element_text(size=12), axis.text = element_text(size=12)) +  scale_color_manual(values = c("Full Connectivity"="black", "Status Quo"="red", "Opportunistic 1" = "darkorange", "Opportunistic 2" = "darkorange2", "Opportunistic 3" = "darkorange3", "Protect YCT 1" = "slateblue1", "Protect YCT 2" = "slateblue3", "Protect YCT 3" = "slateblue4"), breaks = c("Full Connectivity", "Status Quo", "Opportunistic 1", "Opportunistic 2", "Opportunistic 3", "Protect YCT 1", "Protect YCT 2", "Protect YCT 3")) + scale_linetype_manual(values = c("YCT"="solid", "Hybrid"="dashed"), breaks = c("YCT", "Hybrid")) + scale_x_continuous(n.breaks = 5, limits = c(1,200)) + ggtitle("Intraspecific Mating + Brook/Brown Trout") + labs(x="Year", y="Relative Abundance (%)", linetype = "Species", color = "Scenarios")
species_plot_HAM2NN <- species_plot_HAM2NN+ theme(plot.title = element_text(size=12, face="bold"), axis.title = element_text(size=12), legend.title = element_text(size=12, face="bold"), legend.text = element_text(size=12), axis.text = element_text(size=12)) +  scale_color_manual(values = c("Full Connectivity"="black", "Status Quo"="red", "Opportunistic 1" = "darkorange", "Opportunistic 2" = "darkorange2", "Opportunistic 3" = "darkorange3", "Protect YCT 1" = "slateblue1", "Protect YCT 2" = "slateblue3", "Protect YCT 3" = "slateblue4"), breaks = c("Full Connectivity", "Status Quo", "Opportunistic 1", "Opportunistic 2", "Opportunistic 3", "Protect YCT 1", "Protect YCT 2", "Protect YCT 3")) + scale_linetype_manual(values = c("YCT"="solid", "Hybrid"="dashed"), breaks = c("YCT", "Hybrid")) + scale_x_continuous(n.breaks = 5, limits = c(1,200)) + ggtitle("Intraspecific Mating + Brook/Brown Trout") + labs(x="Year", y="Relative Abundance (%)", linetype = "Species", color = "Scenarios")
species_plot_HAM3 <- species_plot_HAM3+ theme(plot.title = element_text(size=12, face="bold"), axis.title = element_text(size=12), legend.title = element_text(size=12, face="bold"), legend.text = element_text(size=12), axis.text = element_text(size=12)) + scale_color_manual(values = c("Full Connectivity"="black", "Status Quo"="red", "Opportunistic 1" = "darkorange", "Opportunistic 2" = "darkorange2", "Opportunistic 3" = "darkorange3", "Protect YCT 1" = "slateblue1", "Protect YCT 2" = "slateblue3", "Protect YCT 3" = "slateblue4"), breaks = c("Full Connectivity", "Status Quo", "Opportunistic 1", "Opportunistic 2", "Opportunistic 3", "Protect YCT 1", "Protect YCT 2", "Protect YCT 3")) + scale_linetype_manual(values = c("YCT"="solid", "Hybrid"="dashed"), breaks = c("YCT", "Hybrid"))  + scale_x_continuous(n.breaks = 5, limits = c(1,200)) +ggtitle("Self Preference Mating + Brook/Brown Trout") + labs(x="Year", y="Relative Abundance (%)", linetype = "Species", color = "Scenarios")
species_plot_HAM3NN <- species_plot_HAM3NN+ theme(plot.title = element_text(size=12, face="bold"), axis.title = element_text(size=12), legend.title = element_text(size=12, face="bold"), legend.text = element_text(size=12), axis.text = element_text(size=12)) + scale_color_manual(values = c("Full Connectivity"="black", "Status Quo"="red", "Opportunistic 1" = "darkorange", "Opportunistic 2" = "darkorange2", "Opportunistic 3" = "darkorange3", "Protect YCT 1" = "slateblue1", "Protect YCT 2" = "slateblue3", "Protect YCT 3" = "slateblue4"), breaks = c("Full Connectivity", "Status Quo", "Opportunistic 1", "Opportunistic 2", "Opportunistic 3", "Protect YCT 1", "Protect YCT 2", "Protect YCT 3")) + scale_linetype_manual(values = c("YCT"="solid", "Hybrid"="dashed"), breaks = c("YCT", "Hybrid"))  + scale_x_continuous(n.breaks = 5, limits = c(1,200)) +ggtitle("Self Preference Mating") + labs(x="Year", y="Relative Abundance (%)", linetype = "Species", color = "Scenarios")

species_plot_HLHAM1 <- species_plot_HLHAM1 + theme(plot.title = element_text(size=12, face="bold"), axis.title = element_text(size=12), legend.title = element_text(size=12, face="bold"), legend.text = element_text(size=12), axis.text = element_text(size=12)) + scale_color_manual(values = c("Full Connectivity"="black", "Status Quo"="red", "Opportunistic 1" = "darkorange", "Opportunistic 2" = "darkorange2", "Opportunistic 3" = "darkorange3", "Protect YCT 1" = "slateblue1", "Protect YCT 2" = "slateblue3", "Protect YCT 3" = "slateblue4"), breaks = c("Full Connectivity", "Status Quo", "Opportunistic 1", "Opportunistic 2", "Opportunistic 3", "Protect YCT 1", "Protect YCT 2", "Protect YCT 3")) + scale_linetype_manual(values = c("YCT"="solid", "Hybrid"="dashed"), breaks = c("YCT", "Hybrid"))  + scale_x_continuous(n.breaks = 5, limits = c(1,200)) +ggtitle("Random Mating \n + Life History Variation \n + Brook/Brown Trout") + labs(x="Year", y="Relative Abundance (%)", linetype = "Species", color = "Scenarios")
species_plot_HLHAM1NN <- species_plot_HLHAM1NN + theme(plot.title = element_text(size=12, face="bold"), axis.title = element_text(size=12), legend.title = element_text(size=12, face="bold"), legend.text = element_text(size=12), axis.text = element_text(size=12)) +scale_color_manual(values = c("Full Connectivity"="black", "Status Quo"="red", "Opportunistic 1" = "darkorange", "Opportunistic 2" = "darkorange2", "Opportunistic 3" = "darkorange3", "Protect YCT 1" = "slateblue1", "Protect YCT 2" = "slateblue3", "Protect YCT 3" = "slateblue4"), breaks = c("Full Connectivity", "Status Quo", "Opportunistic 1", "Opportunistic 2", "Opportunistic 3", "Protect YCT 1", "Protect YCT 2", "Protect YCT 3")) + scale_linetype_manual(values = c("YCT"="solid", "Hybrid"="dashed"), breaks = c("YCT", "Hybrid"))  + scale_x_continuous(n.breaks = 5, limits = c(1,200)) +ggtitle("Random Mating \n + Life History Variation") + labs(x="Year", y="Relative Abundance (%)", linetype = "Species", color = "Scenarios")
species_plot_HLHAM2 <- species_plot_HLHAM2 + theme(plot.title = element_text(size=12, face="bold"), axis.title = element_text(size=12), legend.title = element_text(size=12, face="bold"), legend.text = element_text(size=12), axis.text = element_text(size=12)) + scale_color_manual(values = c("Full Connectivity"="black", "Status Quo"="red", "Opportunistic 1" = "darkorange", "Opportunistic 2" = "darkorange2", "Opportunistic 3" = "darkorange3", "Protect YCT 1" = "slateblue1", "Protect YCT 2" = "slateblue3", "Protect YCT 3" = "slateblue4"), breaks = c("Full Connectivity", "Status Quo", "Opportunistic 1", "Opportunistic 2", "Opportunistic 3", "Protect YCT 1", "Protect YCT 2", "Protect YCT 3")) + scale_linetype_manual(values = c("YCT"="solid", "Hybrid"="dashed"), breaks = c("YCT", "Hybrid"))  + scale_x_continuous(n.breaks = 5, limits = c(1,200)) +ggtitle("Intraspecific Mating \n + Life History Variation \n + Brook/Brown Trout") + labs(x="Year", y="Relative Abundance (%)", linetype = "Species", color = "Scenarios")
species_plot_HLHAM2NN <- species_plot_HLHAM2NN + theme(plot.title = element_text(size=12, face="bold"), axis.title = element_text(size=12), legend.title = element_text(size=12, face="bold"), legend.text = element_text(size=12), axis.text = element_text(size=12)) +scale_color_manual(values = c("Full Connectivity"="black", "Status Quo"="red", "Opportunistic 1" = "darkorange", "Opportunistic 2" = "darkorange2", "Opportunistic 3" = "darkorange3", "Protect YCT 1" = "slateblue1", "Protect YCT 2" = "slateblue3", "Protect YCT 3" = "slateblue4"), breaks = c("Full Connectivity", "Status Quo", "Opportunistic 1", "Opportunistic 2", "Opportunistic 3", "Protect YCT 1", "Protect YCT 2", "Protect YCT 3")) + scale_linetype_manual(values = c("YCT"="solid", "Hybrid"="dashed"), breaks = c("YCT", "Hybrid"))  + scale_x_continuous(n.breaks = 5, limits = c(1,200)) + ggtitle("Intraspecific Mating \n + Life History Variation") + labs(x="Year", y="Relative Abundance (%)", linetype = "Species", color = "Scenarios")
species_plot_HLHAM3 <- species_plot_HLHAM3 + theme(plot.title = element_text(size=12, face="bold"), axis.title = element_text(size=12), legend.title = element_text(size=12, face="bold"), legend.text = element_text(size=12), axis.text = element_text(size=12)) + scale_color_manual(values = c("Full Connectivity"="black", "Status Quo"="red", "Opportunistic 1" = "darkorange", "Opportunistic 2" = "darkorange2", "Opportunistic 3" = "darkorange3", "Protect YCT 1" = "slateblue1", "Protect YCT 2" = "slateblue3", "Protect YCT 3" = "slateblue4"), breaks = c("Full Connectivity", "Status Quo", "Opportunistic 1", "Opportunistic 2", "Opportunistic 3", "Protect YCT 1", "Protect YCT 2", "Protect YCT 3")) + scale_linetype_manual(values = c("YCT"="solid", "Hybrid"="dashed"), breaks = c("YCT", "Hybrid"))  + scale_x_continuous(n.breaks = 5, limits = c(1,200)) +ggtitle("Self Preference Mating \n + Life History Variation \n + Brook/Brown Trout") + labs(x="Year", y="Relative Abundance (%)", linetype = "Species", color = "Scenarios")
species_plot_HLHAM3NN <- species_plot_HLHAM3NN + theme(plot.title = element_text(size=12, face="bold"), axis.title = element_text(size=12), legend.title = element_text(size=12, face="bold"), legend.text = element_text(size=12), axis.text = element_text(size=12)) + scale_color_manual(values = c("Full Connectivity"="black", "Status Quo"="red", "Opportunistic 1" = "darkorange", "Opportunistic 2" = "darkorange2", "Opportunistic 3" = "darkorange3", "Protect YCT 1" = "slateblue1", "Protect YCT 2" = "slateblue3", "Protect YCT 3" = "slateblue4"), breaks = c("Full Connectivity", "Status Quo", "Opportunistic 1", "Opportunistic 2", "Opportunistic 3", "Protect YCT 1", "Protect YCT 2", "Protect YCT 3")) + scale_linetype_manual(values = c("YCT"="solid", "Hybrid"="dashed"), breaks = c("YCT", "Hybrid")) + scale_x_continuous(n.breaks = 5, limits = c(1,200)) +ggtitle("Self Preference Mating \n + Life History Variation") + labs(x="Year", y="Relative Abundance (%)", linetype = "Species", color = "Scenarios")


# Code to arrange Plots
library(ggpubr)


PopPlotLHVBoth <- ggarrange(pop_plot_LHVNN, pop_plot_LHV, ncol=2, nrow=1, common.legend=TRUE, legend = "top", labels="AUTO")
MigrantPercentLHVBoth <- ggarrange(migrantPercent_plot_LHV, migrantPercent_plot_LHVNN, ncol =2, nrow=1,common.legend=TRUE)+theme(legend.position = "bottom")
MigrantPercentHLHAll <- ggarrange(migrantPercent_plot_HLHAM1, migrantPercent_plot_HLHAM2, migrantPercent_plot_HLHAM3, migrantPercent_plot_HLHAM1NN, migrantPercent_plot_HLHAM2NN, migrantPercent_plot_HLHAM3NN, ncol=3, nrow=2,common.legend=TRUE)+theme(legend.position = "bottom")
SpeciesPlotHLHAll <- ggarrange(species_plot_HLHAM1,species_plot_HLHAM2,species_plot_HLHAM3, ncol =3, nrow=1,common.legend=TRUE)+theme(legend.position = "top")
SpeciesPlotHLHAll_nn <- ggarrange(species_plot_HLHAM1NN,species_plot_HLHAM2NN,species_plot_HLHAM3NN, ncol =3, nrow=1,common.legend=TRUE)+theme(legend.position = "top")
SpeciesPlotHybAll <- ggarrange(species_plot_HAM1,species_plot_HAM2,species_plot_HAM3, ncol =3, nrow=1,common.legend=TRUE)+theme(legend.position = "top")
SpeciesPlotHybAll_nn <- ggarrange(species_plot_HAM1NN,species_plot_HAM2NN,species_plot_HAM3NN, ncol =3, nrow=1,common.legend=TRUE)+theme(legend.position = "top")

largeYCTLHVboth <- ggarrange(yctlargepercent_plot_LHVNN, yctlargepercent_plot_LHV, ncol=2, nrow=1, common.legend=TRUE, legend = "top", labels="AUTO")
largeYCThybALL <- ggarrange(yctlargepercent_plot_HLHAM1NN, yctlargepercent_plot_HLHAM2NN, yctlargepercent_plot_HLHAM3NN, yctlargepercent_plot_HLHAM1, yctlargepercent_plot_HLHAM2, yctlargepercent_plot_HLHAM3, ncol=3, nrow=2, common.legend = TRUE, legend="top", labels ="AUTO")

SpeciesPlot3by3 <- ggarrange(species_plot_HAM1NN, species_plot_HAM2NN, species_plot_HAM3NN, species_plot_HLHAM1NN, species_plot_HLHAM2NN, species_plot_HLHAM3NN, species_plot_HLHAM1, species_plot_HLHAM2, species_plot_HLHAM3,ncol=3, nrow=3,common.legend=TRUE, legend="top", labels="AUTO")
SpeciesPlot3by4 <- ggarrange(species_plot_HAM1, species_plot_HAM2, species_plot_HAM3, species_plot_HAM1NN, species_plot_HAM2NN, species_plot_HAM3NN, species_plot_HLHAM1, species_plot_HLHAM2, species_plot_HLHAM3, species_plot_HLHAM1NN, species_plot_HLHAM2NN, species_plot_HLHAM3NN,ncol=3, nrow=4,common.legend=TRUE, legend="bottom")


