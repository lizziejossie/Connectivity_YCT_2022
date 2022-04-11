library(tidyverse)
library(cowplot)


setwd("")

#create a dataframe of every individual, by pulling in all of the outputs created after running CDMetaPOP
data_df = list.files(pattern = paste("ind5|ind25|ind50|ind75|ind100|ind125|ind150|ind175|ind199"), #this is the pattern for pulling in files
                     full.names = TRUE, 
                     recursive = TRUE, 
                     include.dirs = TRUE) %>% 
  map_df(function(x) read_csv(x, col_types = cols(.default = "c")) %>% mutate(filename=paste(dirname((x)),basename(x),sep="/")))

#break up the column with the file name to delineate the year of the output
data_df <- separate(data = data_df, col = filename, into = c('junk1', 'replicate', 'year'), sep = "/")
data_df <- separate(data = data_df, col = year, into = c('junk2', 'year'), sep = "d")
data_df <- separate(data = data_df, col = year, into = c('year', 'junk3'), sep = ".c")


#set column data types after you remove columns that we do not want. You will likely want different sets of columns depending on your research question.

data_df$junk <- NULL
data_df$junk1 <- NULL
data_df$junk2 <- NULL
data_df$junk3 <- NULL

data_df$ID <- NULL
data_df$sex <- NULL
data_df$mature <- NULL
data_df$newmature <- NULL
data_df$layeggs <- NULL
data_df$capture <- NULL
data_df$infection <- NULL
data_df$Species <- NULL
data_df$recapture <- NULL
data_df$SubPatchID <- NULL
data_df$ClassFile <- NULL

data_df$L0A0 <-  NULL
data_df$L0A1 <- NULL
data_df$L1A0 <-  NULL
data_df$L1A1 <- NULL
data_df$L0A2 <- NULL
data_df$L0A3 <- NULL
data_df$L0A4 <- NULL
data_df$L0A5 <- NULL
data_df$L0A6 <- NULL
data_df$L0A7 <- NULL
data_df$L0A8 <- NULL
data_df$L0A9 <- NULL
data_df$L0A10 <- NULL
data_df$L0A11 <- NULL
data_df$L0A12 <- NULL
data_df$L0A13 <- NULL
data_df$L0A14 <- NULL
data_df$L0A15 <- NULL
data_df$L0A16 <- NULL
data_df$L0A17 <- NULL
data_df$L0A18 <- NULL
data_df$L0A19 <- NULL
data_df$L1A2 <- NULL
data_df$L1A3 <- NULL
data_df$L1A4 <- NULL
data_df$L1A5 <- NULL
data_df$L1A6 <- NULL
data_df$L1A7 <- NULL
data_df$L1A8 <- NULL
data_df$L1A9 <- NULL
data_df$L1A10 <- NULL
data_df$L1A11 <- NULL
data_df$L1A12 <- NULL
data_df$L1A13 <- NULL
data_df$L1A14 <- NULL
data_df$L1A15 <- NULL
data_df$L1A16 <- NULL
data_df$L1A17 <- NULL
data_df$L1A18 <- NULL
data_df$L1A19 <- NULL
data_df$L2A0 <- NULL
data_df$L2A1 <- NULL
data_df$L2A2 <- NULL
data_df$L2A3 <- NULL
data_df$L2A4 <- NULL
data_df$L2A5 <- NULL
data_df$L2A6 <- NULL
data_df$L2A7 <- NULL
data_df$L2A8 <- NULL


data_df$XCOORD <- NULL
data_df$YCOORD <- NULL

data_df$year <- as.numeric(data_df$year)
data_df$size <- as.numeric(data_df$size)
data_df$age <- as.numeric(data_df$age)
data_df$Hindex <- as.numeric(data_df$Hindex)
data_df$CDist <- as.numeric(data_df$CDist)

#one last clean up step: removing the initialization individuals (year == -1) and year 0 individuals

data_df <- data_df %>%
  filter(year != "-1" & year != "1" & year != "0" & year != "51" & year != "52" & year!="53" & year!="54" & year !="55" & year!= "56" & year!= "57" & year!= "58" & year!="59")




#write.csv(data_df)

#SUMMARIES

pop_df <- data_df %>%
  group_by(replicate, year, .drop = FALSE) %>% #we use .drop = FALSE here in case some years don't have any individuals, but we still want that data point for plotting
  summarise(pop = n())

pop_df_sum <- pop_df %>%
  group_by(year) %>%
  summarise(mean_pop = mean(pop),
            stdev_pop = sd(pop),
            CI = 1.96*(sd(pop)/sqrt(n())))

write.csv(pop_df_sum, file = "pop_df_sum.csv")


## LIFE HISTORY SUMMARIES ##

largepercent_df <- data_df %>%
  group_by(replicate,year, .drop=FALSE) %>%
  summarize(largepercent = sum((CDist>0 & size>300)/n()*100))

largepercent_df_sum <- largepercent_df %>%
  group_by(year) %>%
  summarize(mean_percent = mean(largepercent),
            stdev_percent = sd(largepercent),
            CI = 1.96*(stdev_percent/sqrt(n())))
write.csv(largepercent_df_sum, file = "largepercent.csv")

MigrantPercent_df<- data_df %>%
  group_by(replicate, year, .drop = FALSE) %>%
  summarize(migrantpercent = sum(CDist>0)/n()*100)

MigrantPercent_df_sum <- MigrantPercent_df %>%
  group_by(year) %>%
  summarize(mean_percent = mean(migrantpercent),
            stdev_percent = sd(migrantpercent))
write.csv(MigrantPercent_df_sum, file = "migrantpercent_sum.csv")


yctlargepercent_df <- data_df %>%
  group_by(replicate,year, .drop=FALSE) %>%
  summarize(largepercent = sum(CDist>0 & size>300 & Hindex<0.20)/n()*100)

yctlargepercent_df_sum <- yctlargepercent_df %>%
  group_by(year) %>%
  summarize(mean_percent = mean(largepercent),
            stdev_percent = sd(largepercent),
            CI = 1.96*(stdev_percent/sqrt(n())))
write.csv(yctlargepercent_df_sum, file = "yctlargepercent.csv")



## HYBRID SUMMARIES ##

Hindex_df <- data_df %>%
  group_by(replicate, year, .drop = FALSE) %>%
  summarize(average = mean(Hindex), stdev = sd(Hindex))

Hindex_df_sum <- Hindex_df %>%
  group_by(year) %>%
  summarize(mean_Hindex = mean(average), stdev_Hindex = sd(average))
write.csv(Hindex_df_sum, file = "Hindex_sum.csv")

yctpercent_df <- data_df %>%
  group_by(replicate, year, .drop = FALSE)  %>%
  summarize(yct = sum(Hindex >= 0 & Hindex <= 0.20)/n()*100)
write.csv(yctpercent_df, file = "yctdf.csv")

yctpercent_df_sum <- yctpercent_df %>%
  group_by(year) %>%
  summarize(mean_percent = mean(yct), stdev_percent = sd(yct))
write.csv(yctpercent_df_sum, file = "yctpercent_sum.csv")

rbtpercent_df <- data_df %>%
  group_by(replicate, year, .drop = FALSE) %>%
  summarize(rbt = sum(Hindex <= 1 & Hindex >= 0.80)/n()*100)

rbtpercent_df_sum <- rbtpercent_df %>%
  group_by(year) %>%
  summarize(mean_percent = mean(rbt), stdev_percent = sd(rbt))
write.csv(rbtpercent_df_sum, file = "rbtpercent_sum.csv")

cutbowpercent_df <- data_df %>%
  group_by(replicate, year, .drop = FALSE) %>%
  summarize(cutbow = sum(Hindex > 0.20 & Hindex < 0.80)/n()*100)

cutbowpercent_df_sum <- cutbowpercent_df %>%
  group_by(year) %>%
  summarize(mean_percent = mean(cutbow), stdev_percent = sd(cutbow))
write.csv(cutbowpercent_df_sum, file = "cutbowpercent_sum.csv")


