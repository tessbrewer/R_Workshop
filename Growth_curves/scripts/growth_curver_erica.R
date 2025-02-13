#loading growth curve data from the plate readers and making growth curves
library(readxl) #read_excel
library(reshape2) #melt
library(RColorBrewer) #brewer color tools
library(scales) #show_col
library(growthcurver) #growth curver
library(ggforce) #geom_circle
library(ggsignif) #significance

#################start up#################
#set working directory so you don't have to use full paths
setwd('~/Documents/munich/R_Workshop/Growth_curves/data/')

#creating mapping file from plate map 
#you can do this in excel but this one has a hiearchy that's not bad too replicate in base R

#for the labels
label = c(rep(c('CML_0.05', 'Lys_0.05', 'CML_0.125', 'Lys_0.125', 'no_aa'), each = 3))
one_media = c(rep(label, 2), 'CML_blank', 'Lys_blank', 'no_aa_blank', rep('blank', each = 3))


#the others are just slightly different versions of those commands
mapping <- data.frame(wells = paste(rep(LETTERS[1:6], each = 12), rep(seq(1:12), 6), sep = ''),
                      conditions = rep(one_media, 2),
                      media = c(rep(c('glucose', 'glycerol'), each = 36)),
                      strain = rep(c(rep(1, each = 15), rep(10, each = 15), rep('blank', each = 6)), 2)
                      )

#################cleaning and formatting data#################

clariostar_load <- function(measurements, excel_range) {
  od <- readxl::read_excel(measurements, range = excel_range)
  od <- od[-1,]
  first_meas <- od[,1][[1]][2]
  cycle_time = as.numeric(strsplit(first_meas, " ")[[1]][3])
  print(paste('guessing that cycle time is', cycle_time))
  od$time <- c(seq(0, ((nrow(od) - 1) * cycle_time), cycle_time))
  od <- od[,-1]
  od <- melt(od, id.vars = 'time')
  colnames(od) <- c('time', 'wells', 'value')
  od$wells <- gsub('([[:alpha:]])0', '\\1', od$wells) #just turns A01 -> A1
  od$value <- as.numeric(od$value)
  return(od)
}

measurements = 'erica_4.10.24_clariostar_data.xlsx' #plate reader output file

#look at the plate reader excel file, these are the dimensions of the actual data, 
#SKIPPING THE Raw Data column
excel_range = c('B11:CT112')
od <- clariostar_load(measurements, excel_range)

#i like to have everything in one dataframe, so I'll add the mapping info to the od measurements
od$cond <- mapping$conditions[match(od$wells, mapping$wells)]
od$media <- mapping$media[match(od$wells, mapping$wells)]
od$strain <- mapping$strain[match(od$wells, mapping$wells)]
od <- na.omit(od)

#################check out blanks#################

#erica likes complex things so there are eight different types of blanks
blanks <- subset(od, strain == 'blank')
table(blanks$cond, blanks$media)

#let's take a look at them to see if any are contaminated
ggplot(od, aes(x = time, y = value)) +
  facet_wrap(~media) +
  geom_smooth(aes(group = wells, color = cond), linewidth = 1.1, se = F) +
  xlab("Time (seconds)") +
  ylab("OD 600") +
  scale_y_log10() + 
  theme_classic() 

#nothing jumps out

#let's see if the media conditions are significantly different
t.test(value ~ media, blanks) # yeah

blank_glu <- mean(subset(blanks, media == 'glucose')$value)
blank_gly <- mean(subset(blanks, media == 'glycerol')$value)

od$corrected_od <- ifelse(od$media == 'glucose', od$value - blank_glu, od$value - blank_gly)
od <- subset(od, strain != 'blank') #don't need blanks anymore


#################analysis#################
show_col(brewer.pal(4, 'Blues'))
show_col(brewer.pal(4, 'Greens'))
colorz <- c(brewer.pal(4, 'Blues')[c(2,4)], brewer.pal(4, 'Greens')[c(2,4)], 'black')

#let's take a look
ggplot(od, aes(x = time, y = corrected_od)) +
  facet_grid(strain~media) +
  geom_smooth(aes(group = wells, color = cond), linewidth = 1.1, se = F) +
  xlab("Time (seconds)") +
  ylab("OD 600") +
  scale_color_manual(values = colorz, name = 'Conditions') +
  scale_y_log10() + 
  theme_classic() 

ggplot(od, aes(x = time, y = corrected_od)) +
  facet_grid(cond~media, scales = 'free_y') +
  geom_smooth(aes(group = wells, color = strain), linewidth = 1.1, se = F) +
  xlab("Time (seconds)") +
  ylab("OD 600") +
  scale_color_manual(values = c('#603162', '#84EAC0'), name = 'Passages') +
  scale_y_log10() + 
  theme_classic() 

#combine replicates and add error bars
#get mean value for line
mean_val <- aggregate(corrected_od ~ time * paste(cond, media, strain, sep = '_'), od, mean)
colnames(mean_val)[2] <- c('name')

#get sd for error bars
mean_sd <- aggregate(corrected_od ~ time * paste(cond, media, strain, sep = '_'), od, sd)
colnames(mean_sd)[2] <- c('name')
mean_val$sd <- mean_sd$corrected_od[match(mean_val$name, mean_sd$name)]
mean_val <- subset(mean_val, grepl('no_aa', name) == FALSE) #don't need the blanks here
mean_val$media <- str_split_fixed(mean_val$name, '_', 4)[,3] 
mean_val$strain <- str_split_fixed(mean_val$name, '_', 4)[,4] 
mean_val$cond <- str_split_fixed(mean_val$name, '_', 4)[,1] 
mean_val$conc <- str_split_fixed(mean_val$name, '_', 4)[,2] 


ggplot(mean_val, aes(x = time, y = corrected_od)) +
  facet_grid(cond~media) +
  geom_errorbar(aes(ymin = corrected_od - sd, 
                    ymax = corrected_od + sd, color = paste(strain, conc)),
                width = 0.2, position = position_dodge(width = 0.9), alpha = 0.5) +
  geom_line(aes(group = name, color = paste(strain, conc)), linewidth = 1.1, se = F) +
  xlab("Time (seconds)") +
  ylab("OD 600") +
  scale_color_manual(values = c('#b674b9', '#603162', '#c5f5e1', '#1fb678'), name = 'Passage / concentration', 
                     labels = c('Passage 1, 0.05 \u03BCM', 'Passage 1, 0.125 \u03BCM', 
                                'Passage 10, 0.05 \u03BCM', 'Passage 10, 0.125 \u03BCM')) +
  scale_y_log10() + 
  theme_classic() +   
  theme(text = element_text(family="Helvetica", size=14, color = "black"))



#################growth curver#################
#put your data how growthcurver wants them -- we need to see how it wants them first
? SummarizeGrowthByPlate

#okay it says we need time in one column, then one column for each well (wide format)
growth_data <- dcast(od, time ~ wells, value.var = 'corrected_od')
growth_data$time <- growth_data$time / 60 #expects hours, not minutes according to help file

#we already did blank correction, but growthcurver can do that

growth_curve <- SummarizeGrowthByPlate(growth_data)

#always check the notes!!!!! growth curver is telling us it could not fit many wells to its model
mapping$t_gen <- growth_curve$t_gen[match(mapping$well, growth_curve$sample)]
mapping$comments <- growth_curve$note[match(mapping$well, growth_curve$sample)]
mapping$condition_only <- str_split_fixed(mapping$conditions, '_', 2)[,1] #this splits these characters based on '_' two times
mapping$concentration <- str_split_fixed(mapping$conditions, '_', 2)[,2] 

growth_plot <- subset(mapping, strain != 'blank' & conditions != 'no_aa')

#plot generation time versus metrics
ggplot(growth_plot, aes(x = concentration, y = t_gen, fill = strain)) +
  facet_grid(media~condition_only, scales = 'free_y') +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_point(size = 3, pch = 21, position = position_jitterdodge()) +
  ylab('Generation time (hours)') +
  xlab(expression(paste('Concentration ', mu, 'M'))) +
  theme_classic() +
  scale_fill_manual(values = c('#603162', '#84EAC0'), name = 'Passages') +
  theme(text = element_text(family="Helvetica", size=14, color = "black"))

#################significance tests and plotting#################
#do significance tests
growth_plot$replicate <- rep(1:3, nrow(growth_plot) / 3)
#get data into good format
growth_tests <- dcast(growth_plot, strain * replicate ~ conditions * media, value.var = 't_gen')
#now we can do t.tests like this if we're dumb
t.test(CML_0.05_glycerol ~ strain, growth_tests)
#but i don't want to have to type so much

pvalz <- list()
namez <- list()
for (i in colnames(growth_tests)[3:ncol(growth_tests)]){
  testy <- t.test(growth_tests[[i]] ~ growth_tests[['strain']])
  print(paste(i, testy$p.value))
  namez <- append(namez, i)
  pvalz <- append(pvalz, testy$p.value)
}

results <- data.frame(names = unlist(namez), pvalues = unlist(pvalz))

#prefer to do glucose and glycerol separetely
growth_plot$label <- paste(growth_plot$condition_only, ' ', growth_plot$concentration, '\u03BCM', sep = '')

ggplot(subset(growth_plot, media == 'glucose'), aes(x = as.character(strain), y = t_gen, fill = strain)) +
  facet_wrap(~label, nrow = 1) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_signif(comparisons = list(c('1', '10')), test = 't.test', y_position = 10.5, tip_length = 0.025) +
  geom_point(size = 3, pch = 21, position = position_jitterdodge()) +
  ggtitle('Growth comparision on glucose') +
  ylab('Generation time (hours)') +
  ylim(c(0, 12)) +
  scale_fill_manual(values = c('#603162', '#84EAC0'), name = 'Passages') +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(text = element_text(family="Helvetica", size=14, color = "black"))

ggplot(subset(growth_plot, media == 'glycerol'), aes(x = as.character(strain), y = t_gen, fill = strain)) +
  facet_grid(media~label) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_signif(comparisons = list(c('1', '10')), test = 't.test', y_position = 3.5, tip_length = 0.025) +
  geom_point(size = 3, pch = 21, position = position_jitterdodge()) +
  ggtitle('Growth comparision on glycerol') +
  ylab('Generation time (hours)') +
  ylim(c(0, 4)) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_fill_manual(values = c('#603162', '#84EAC0'), name = 'Passages') +
  theme(text = element_text(family="Helvetica", size=14, color = "black"))


#can also do 
ggplot(subset(growth_plot, media == 'glucose'), aes(x = concentration, y = t_gen, fill = strain)) +
  facet_grid(media~condition_only, scales = 'free_y') +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_point(size = 3, pch = 21, position = position_jitterdodge()) +
  stat_compare_means(aes(group = strain), label = "p.format", method = 't.test', label.y = 10.5) +
  ylab('Generation time (hours)') +
  xlab('Concentration (\u03BCM)') +
  ylim(0, 11) +
  theme_classic() +
  scale_fill_manual(values = c('#603162', '#84EAC0'), name = 'Passages') +
  theme(text = element_text(family="Helvetica", size=14, color = "black"))

ggplot(subset(growth_plot, media == 'glycerol'), aes(x = concentration, y = t_gen, fill = strain)) +
  facet_grid(media~condition_only, scales = 'free_y') +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_point(size = 3, pch = 21, position = position_jitterdodge()) +
  stat_compare_means(aes(group = strain), label = "p.format", method = 't.test', label.y = 3.75) +
  ylab('Generation time (hours)') +
  xlab('Concentration (\u03BCM)') +
  ylim(0, 4) +
  theme_classic() +
  scale_fill_manual(values = c('#603162', '#84EAC0'), name = 'Passages') +
  theme(text = element_text(family="Helvetica", size=14, color = "black"))



