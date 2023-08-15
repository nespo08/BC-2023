library(needs)
devtools::install_github("bdilday/GeomMLBStadiums")
remotes::install_github("hughjonesd/ggmagnify")
needs(dplyr,ggmagnify,jsonlite,pdftools,grid,ggplot2,ggforce,gridExtra,gtable,reshape2,ggpubr,tidyverse,stringr,scales,sqldf,gridtext,lubridate,GeomMLBStadiums,sf,RCurl,png,DescTools)
options(dplyr.summarise.inform = FALSE)

####################################################################################################################################################################################
BCCOLS <- c('Fastball' = '#FE0707', 'Curveball' = '#07D2FE', 'Changeup' = '#03CD30', 'ChangeUp' = '#03CD30','Sinker' = '#FF9900', 'Splitter' = '#5DD89C','Slider' = '#F1C232',
            'Cutter' = '#783F04', 'Knuckle Ball' = '#000000', 'Four Seam Fastball' = '#FE0707','Two Seam Fastball' = '#FF9900', '2Fastball' = '#FF9900','Drop Curve' = "#07D2FE",
            'Dropball' = "#F1C232", 'Riseball' = "#FF9900",'Screwball' =  "#000000", 'Undefined' = '#000000')
###############################################################################################################################################################################

target <- c('Fastball', 'Sinker', 'Cutter', 'Curveball', 'Slider', 'Changeup', 'Splitter', 'Knuckle Ball', 'Overall')


##### 2022 MLB Pitching Averages - Comment out read_rds lines when using this file ##### ----------------------------------
#MLB_AVGS <- read_rds("data/SSW-data/MLB_2022_Metric_Avgs.rds")

# *** Update averages using Clean_Pitching_Data.R annually - this is when you will read in the rds files for viewing ***
FIND_MLB_AVGS <- function(){
  pitch_type <- c("Fastball", "Curveball", "Cutter", "Changeup", "Sweeper", "Sinker", "Sinker", "Slider",       
                  "Slider", "Fastball", "Changeup", "Splitter", "Knuckle-curve", "Cutter", "Knuckle-curve", "Curveball",   
                  "Other", "Sweeper", "Slurve", "Slurve", "Pitch-out", "Splitter", "Eephus", "Other",        
                  "Eephus", "Knuckleball")
  p_throws <- c("R","R","R", "R", "R", "R", "L", "L", "R", "L", "L", "R", "L", "L", "R", "L", "R", "L", "R", "L", "R",
                "L", "R", "L", "L", "R")
  avg_spin <- c(2285.6646, 2543.3675, 2389.4458, 1767.4321, 2596.6540, 2155.3518, 2104.8108, 2338.5179, 2427.2066,
                2243.1842, 1716.3831, 1422.2889, 2323.9590, 2287.3294, 2554.3403, 2453.2947, 1715.9051, 2630.6791, 
                2427.5096, 2914.5465, 2194.1111, 1029.3438, 1214.7007, 2215.1667, 1171.4462, 582.5263)
  avg_velocity <- c(94.27169, 79.14098, 89.55927, 85.94675, 82.42232, 93.51160, 92.58221, 84.66435, 85.10981, 92.98527,
                    84.14162, 87.15033, 79.63269, 87.15580, 81.67851, 77.99167, 68.39573, 79.51880, 83.37887, 80.93711,
                    89.61389, 84.18160, 50.49975, 74.57778, 45.10462, 55.36842)
  avg_extension <- c(6.397813, 6.232067, 6.315884, 6.354551, 6.376447, 6.339266, 6.286744, 6.291832, 6.269691, 6.379847,
                     6.309692, 6.301014, 6.101393, 6.155506, 6.350967, 6.244668, 5.024652, 5.828646, 6.096762, 5.613842,
                     6.161111, 6.622569, 4.519950, 5.683333, 5.003077, 4.684211)
  MLB_AVGS <- data.frame(pitch_type, p_throws, avg_spin, avg_velocity, avg_extension)
}

# MLB_AVGS will be used throughout
MLB_AVGS <- FIND_MLB_AVGS()

MLB_EFFICIENCY_AVGS <- read_rds("data/SSW-data/MLB_2022_Eff_Avgs.rds")
FIND_MLB_EFF_AVGS <- function(){
  pitch_type <- c("Fastball", "Fastball", "Sinker", "Sinker", "Cutter", "Cutter", "Changeup", "Changeup", "Slider", "Slider",
                  "Curveball", "Curveball")
  p_throws <- c("L", "R", "L", "R", "L", "R", "L", "R", "L", "R", "L", "R")
  avg_efficiency <- c(92.76416, 92.06916, 91.52275, 89.79604, 44.20189, 46.43194, 
                      92.80000, 90.62537, 34.19313, 36.62321, 67.32747, 67.97289)
  MLB_EFFICIENCY_AVGS <- data.frame(pitch_type, p_throws, avg_efficiency)
}

# MLB_EFFICIENCY_AVGS will be used throughout
MLB_EFFICIENCY_AVGS <- FIND_MLB_EFF_AVGS()

##### MAIN CODE #####

ZONECA <- function(x, y){
  ifelse(x > -.83 & x < -.277 & y > 1.52 & y < 2.26, '1',
         ifelse(x > -.277 & x < .277 & y > 1.52 & y < 2.26, '2',
                ifelse(x > .277 & x < .83 & y > 1.52 & y < 2.26, '3',
                       ifelse(x > -.83 & x < -.277 & y > 2.26 & y < 3, '4',
                              ifelse(x > -.277 & x < .277 & y > 2.26 & y < 3, '5',
                                     ifelse(x > .277 & x < .83 & y > 2.26 & y < 3, '6',
                                            ifelse(x > -.83 & x < -.277 & y > 3 & y < 3.73, '7',
                                                   ifelse(x > -.277 & x < .277 & y > 3 & y < 3.73, '8',
                                                          ifelse(x > .277 & x < .83 & y > 3 & y < 3.73, '9',
                                                                 ifelse(x > -10 & x < 0 & y > 3.73 & y < 10, '11',
                                                                        ifelse(x > -10 & x < -.83 & y > 2.625 & y < 3.73, '11',
                                                                               ifelse(x > 0 & x < 10 & y > 3.73 & y < 10, '12',
                                                                                      ifelse(x > .83 & x < 10 & y > 2.625 & y < 3.73, '12',
                                                                                             ifelse(x > -10 & x < 0 & y > -10 & y < 1.52, '13',
                                                                                                    ifelse(x > -10 & x < -.83 & y > 1.52 & y < 2.625, '13',
                                                                                                           ifelse(x > 0 & x < 10 & y > -10 & y < 1.52, '14',
                                                                                                                  ifelse(x > .83 & x < 10 & y > 1.52 & y < 2.625, '14','0')))))))))))))))))
} # Zones
ZONECB <- function(x, y){
  ifelse(x > -.83 & x < -.277 & y > 1.52 & y < 2.26, '1',
         ifelse(x > -.277 & x < .277 & y > 1.52 & y < 2.26, '2',
                ifelse(x > .277 & x < .83 & y > 1.52 & y < 2.26, '3',
                       ifelse(x > -.83 & x < -.277 & y > 2.26 & y < 3, '4',
                              ifelse(x > -.277 & x < .277 & y > 2.26 & y < 3, '5',
                                     ifelse(x > .277 & x < .83 & y > 2.26 & y < 3, '6',
                                            ifelse(x > -.83 & x < -.277 & y > 3 & y < 3.73, '7',
                                                   ifelse(x > -.277 & x < .277 & y > 3 & y < 3.73, '8',
                                                          ifelse(x > .277 & x < .83 & y > 3 & y < 3.73, '9',
                                                                 ifelse(x > -10 & x < 10 & y > 3.73 & y < 10, '10',
                                                                        ifelse(x > -10 & x < -.83 & y > 1.52 & y < 3.73, '11',
                                                                               ifelse(x > -10 & x < 10 & y > -10 & y < 1.52, '12',
                                                                                      ifelse(x > .83 & x < 10 & y > 1.52 & y < 3.73, '13','0')))))))))))))
} # Zones
ZONEHALF <- function(x, y){
  ifelse(x > -10 & x < 0, 'R', # R and L will match the handedness of the pitcher, so 'R' means the zone is on the righty's armside
         ifelse(x > 0 & x < 10, 'L',0))
} # Split zones in half down the middle

PITCON <- function(x){
  ifelse(x %in% c('Fastball', 'Sinker', 'Cutter', '2FB'), 'FAST',
         ifelse(x %in% c('Splitter', 'Changeup', 'ChangeUp'), 'OFF', 'BREAK'))
} # Adjusting pitch classifications

STATFUN <- function(DF){
  COL3 <- DF %>% mutate(Pitches = nrow(.)) %>% distinct(Pitches)
  #Batters Faced
  COL4 <- DF %>% filter(PitchofPA == 1) %>% summarize(BF = sum(as.numeric(as.character(PitchofPA)), na.rm = T))
  if(dim(COL4)[1] == 0){
    COL4 <- data.frame(BF = '-', PBF = '-')
  }else{
    COL4 <- COL4 %>% mutate(PBF = sprintf('%.1f', COL3$Pitches/BF))
  }
  COL6 <- DF %>% mutate(PlayResult = factor(PlayResult, levels = c('Single', 'Double', 'Triple', 'Homerun', 'HomeRun', 'Home Run'))) %>% group_by(PlayResult) %>% count(PlayResult, .drop = F) %>% 
    filter(PlayResult != 'NA') %>% ungroup() %>% mutate(Hits = sum(n, na.rm = T)) %>% filter(., PlayResult != 'Single') %>% mutate(XBH = sum(n, na.rm = T)) %>% 
    distinct(Hits, XBH) %>% slice(1)
  COL2 <- DF %>% mutate(Runs = sum(as.numeric(as.character(runs_scored)), na.rm = T)) %>% distinct(Runs) %>% slice(1)
  #Max Velocity
  COL5 <- DF %>% summarize(MaxVelo = round(max(RelSpeed, na.rm = T),1))
  #Strikeouts
  COL8 <- DF %>% mutate(KorBB = factor(KorBB, levels = c('Strikeout'))) %>% group_by(KorBB) %>% count(KorBB, .drop = F) %>% filter(KorBB != 'NA') %>% ungroup() %>% summarize(K = sum(n, na.rm = T))
  #Walks
  COL9 <- DF %>% mutate(KorBB = factor(KorBB, levels = c('Walk'))) %>% group_by(KorBB) %>% count(KorBB, .drop = F) %>% filter(KorBB != 'NA') %>% ungroup() %>% summarize(BB = sum(n, na.rm = T))
  
  #Ball %
  COL10 <- DF %>% mutate(PitchCall = factor(PitchCall, levels = c('BallCalled', 'Ball', 'HitByPitch'))) %>% group_by(PitchCall) %>% count(PitchCall, .drop = F) %>% 
    filter(PitchCall != 'NA') %>% ungroup() %>% summarize(WalkP = paste0(sprintf('%.1f', (sum(n, na.rm = T)/nrow(DF))*100), '%'))
  #Strike %
  COL11 <- DF %>% mutate(PitchCall = factor(PitchCall, levels = c('StrikeCalled', 'StrikeSwinging', 'FoulBall', 'Foul', 'FoulTip', 'InPlay', 'In Play', 'Called Strike'))) %>% 
    group_by(PitchCall) %>% count(PitchCall, .drop = F) %>% filter(PitchCall != 'NA') %>% ungroup() %>% summarize(StrikeP = paste0(sprintf('%.1f', (sum(n, na.rm = T)/nrow(DF))*100),'%'))
  #First Pitch Strike %
  COL12 <- DF %>% filter(PitchofPA == 1) %>% mutate(PitchCall = factor(PitchCall, levels = c('StrikeSwinging', 'FoulBall', 'StrikeCalled', 'Foul', 'FoulTip', 'InPlay', 'In Play', 'Called Strike'))) %>% 
    group_by(PitchCall) %>% count(PitchCall, .drop = F) %>% filter(PitchCall != 'NA') %>% ungroup() %>% summarize(FStrike = paste0(sprintf('%.1f', (sum(n, na.rm = T)/nrow(DF %>% filter(PitchofPA == 1)))*100),'%'))
  #Whiff %
  COL14 <- DF %>% mutate(PitchCall = factor(PitchCall, levels = c('StrikeSwinging'))) %>% group_by(PitchCall) %>% count(PitchCall, .drop = F) %>% filter(PitchCall != 'NA') %>% 
    ungroup() %>% summarize(WhiffP = paste0(sprintf('%.1f', (sum(n, na.rm = T)/nrow(DF %>% filter(PitchCall %in% c('StrikeSwinging', 'FoulBall', 'FoulTip', 'Foul', 'InPlay', 'In Play'))))*100),'%'))
  #Usage
  COL15 <- data.frame(Usage = ' ')
  #Combine all the data together
  STATDF <- COL3 %>% bind_cols(., COL4, COL8, COL9, COL6, COL2, COL5,COL10, COL11, COL12, COL14, COL15)
  #This makes the table
  df1 <- STATDF
  colours <- matrix('#FFFFFF', nrow(df1), ncol(df1))
  FACE <- matrix(1, nrow(df1), ncol(df1))
  FSIZE <- matrix(36, nrow(df1), ncol(df1))
  P1n <- dim(df1)[1]
  tt <- ttheme_default(core = list(bg_params=list(fill=colours, col = 'black', lwd = 3.5),fg_params = list(fontsize = FSIZE,  col = 'black', fontface = FACE)), colhead = list(bg_params=list(fill='#d5d9e0', col='black', lwd = 3.5),fg_params = list(fontsize = 34)))
  tt2 <- ttheme_default(core = list(bg_params=list(fill='#2a2682', col = 'black', lwd = 3.5),fg_params = list(fontsize = 32,  col = 'white', fontface = 'bold')), colhead = list(bg_params=list(fill='#a9d4a1', col='black', lwd = 3.5),fg_params = list(fontsize = 40, col = 'black')))
  tab3 <- tableGrob(STATDF[1:dim(STATDF)[1], 1:14], rows = NULL, cols = c('Pitches','BF','Pitches/PA','Strikeouts','Walks','Hits','XBH','Runs','Max Velo','Ball %','Strike %','f-Strike %','Whiff %','Usage'), theme = tt)
  header2 <- tableGrob(STATDF[1,c(1:2)], rows=NULL, cols=c('Stat Line (Temporary)', 'Percentages (Temporary)'), theme = tt2) 
  jn2 <- gtable_combine(header2[1,], tab3, along=2, join = 'outer')
  jn2$widths <- rep(max(jn2$widths), length(jn2$widths)) # make column widths equal
  jn2$layout[1:4 , c("l", "r")] <- list(c(1,10),c(9,14))
  g <- jn2
  g <- gtable_add_grob(g,grobs = rectGrob(gp = gpar(fill = NA, lwd = 3)),t = 1, b = nrow(g), l = 1, r = 14)
  g <- gtable_add_grob(g,grobs = rectGrob(gp = gpar(fill = NA, lwd =1.5)),t = 1, b = 2, l = 1, r = 14)
  g$widths <- unit(c(.0714, .0675,.0753, rep(.7858/11, 11)), 'npc')
  g$heights <- unit(c(.27, .27, .375),'npc')
  STATGROB <- g
  
} # Stat line for full game
STATBULLPEN <- function(DF, Hand){
  # Grab pitch types and number of pitches
  PITCHTYPES <- DF %>% 
    distinct(TaggedPitchType) %>%
    arrange(match(TaggedPitchType, target)) %>%
    pull(TaggedPitchType)
  
  # Ensure pitch vector is of length 8, including blank spaces, for formatting purposes
  while(length(PITCHTYPES) < 8){
    PITCHTYPES <- append(PITCHTYPES, "-")
  }
  
  NUMPITCHES <- length(PITCHTYPES)
  
  #Number of pitches
  COL1 <- DF %>% mutate(Pitches = nrow(.)) %>% distinct(Pitches)
  #Velo Ranges
  COL2 <- DF %>% 
    group_by(TaggedPitchType) %>%
    summarize(MinVelo = sprintf('%.1f', quantile(RelSpeed, 0.025, na.rm = T)),
              MaxVelo = sprintf('%.1f', quantile(RelSpeed, 0.975, na.rm = T)),
              VeloRange = paste0(MinVelo, '-', MaxVelo)) %>%
    arrange(match(TaggedPitchType, target)) %>%
    select(-MinVelo, -MaxVelo, -TaggedPitchType) %>%
    pivot_wider(names_from = 'VeloRange', values_from = 'VeloRange')
  
  #Account for blank spaces
  while(length(COL2) < 8){
    COL2[,ncol(COL2) + 1] <- "-"
  }
  
  colnames(COL2) <- PITCHTYPES
  
  # Calculate ball%, strike%, etc based off of zones (no swinging strikes)
  STRIKEZONE <- c(1,2,3,4,5,6,7,8,9)
  
  PITCHCALL <- DF %>% 
    mutate(PitchCallZone = ifelse(Zone %in% STRIKEZONE, 'Strike', 'Ball')) %>% 
    group_by(PitchCallZone) %>% 
    count(PitchCallZone, .drop = F) %>% 
    filter(PitchCallZone != 'NA') %>% 
    ungroup()
  GLOVESIDECALL <- DF %>% 
    filter(SplitZone != Hand) %>% # Filter for only gloveside pitches
    mutate(GloveSideZone = ifelse(Zone %in% STRIKEZONE, T, F)) %>% 
    group_by(GloveSideZone) %>% count(GloveSideZone, .drop = F) %>% 
    filter(GloveSideZone != 'NA') %>% 
    ungroup()
  ARMSIDECALL <- DF %>% 
    filter(SplitZone == Hand) %>% # Filter for only armside pitches
    mutate(ArmSideZone = ifelse(Zone %in% STRIKEZONE, T, F)) %>% 
    group_by(ArmSideZone) %>% 
    count(ArmSideZone, .drop = F) %>% 
    filter(ArmSideZone != 'NA') %>% 
    ungroup()
  
  #Ball %
  COL10 <- DF %>% summarize(WalkP = paste0(sprintf('%.1f', (PITCHCALL$n[1]/nrow(DF))*100), '%'))
  #Strike %
  COL11 <- DF %>% summarize(StrikeP = paste0(sprintf('%.1f', (PITCHCALL$n[2]/nrow(DF))*100), '%'))
  #Glove Side Strike %
  COL12 <- DF %>% summarize(GloveSideP = paste0(sprintf('%.1f', (GLOVESIDECALL$n[2]/(GLOVESIDECALL$n[1] + GLOVESIDECALL$n[2]))*100), '%'))
  #Arm Side Strike %
  COL14 <- DF %>% summarize(ArmSideP = paste0(sprintf('%.1f', (ARMSIDECALL$n[2]/(ARMSIDECALL$n[1] + ARMSIDECALL$n[2]))*100), '%'))
  #Usage
  COL15 <- data.frame(Usage = ' ')
  #Combine all the data together
  STATDF <- COL1 %>% bind_cols(., COL2, COL10, COL11, COL12, COL14, COL15)
  #This makes the table
  df1 <- STATDF
  colours <- matrix('#FFFFFF', nrow(df1), ncol(df1))
  FACE <- matrix(1, nrow(df1), ncol(df1))
  FSIZE <- matrix(30, nrow(df1), ncol(df1))
  P1n <- dim(df1)[1]
  tt <- ttheme_default(core = list(bg_params=list(fill=colours, col = 'black', lwd = 3.5),fg_params = list(fontsize = FSIZE,  col = 'black', fontface = FACE)), colhead = list(bg_params=list(fill='#d5d9e0', col='black', lwd = 3.5),fg_params = list(fontsize = 28)))
  tt2 <- ttheme_default(core = list(bg_params=list(fill='#2a2682', col = 'black', lwd = 3.5),fg_params = list(fontsize = 32,  col = 'white', fontface = 'bold')), colhead = list(bg_params=list(fill='#a9d4a1', col='black', lwd = 3.5),fg_params = list(fontsize = 40, col = 'black')))
  tab3 <- tableGrob(STATDF[1:dim(STATDF)[1], 1:(NUMPITCHES + 6)], rows = NULL, cols = c('Pitches',PITCHTYPES,'Ball%','Strike%','Glove Side\nStrike%','Arm Side\nStrike%','Usage'), theme = tt)
  header2 <- tableGrob(STATDF[1,c(1:2)], rows=NULL, cols=c('Bullpen Stats', 'Percentages'), theme = tt2) 
  jn2 <- gtable_combine(header2[1,], tab3, along=2, join = 'outer')
  jn2$widths <- rep(max(jn2$widths), length(jn2$widths)) # make column widths equal
  jn2$layout[1:4 , c("l", "r")] <- list(c(1,10),c(9, 14))
  g <- jn2
  g <- gtable_add_grob(g,grobs = rectGrob(gp = gpar(fill = NA, lwd = 3)),t = 1, b = nrow(g), l = 1, r = 14)
  g <- gtable_add_grob(g,grobs = rectGrob(gp = gpar(fill = NA, lwd =1.5)),t = 1, b = 2, l = 1, r = 14)
  g$widths <- unit(c(.0714, .0675,.0753, rep(.7858/11, 11)), 'npc')
  g$heights <- unit(c(.27, .27, .375),'npc')
  STATGROB <- g
  
} # Stat line for bullpen
PIEG <- function(DF){
  FSD <- DF %>% group_by(TaggedPitchType) %>% count(TaggedPitchType) %>% filter(., TaggedPitchType != '')
  PGR <- ggplot(FSD, aes(x="", y=n, fill=TaggedPitchType)) +
    geom_bar(stat="identity", width=1, color="black") +
    coord_polar("y", start=0)+
    scale_fill_manual(values = BCCOLS)+
    theme_void()+
    theme(legend.position = 'none')
  
  PGRG <- ggarrange(NULL, NULL, NULL,
                    NULL, PGR, NULL,
                    NULL, NULL, NULL,ncol = 3, nrow = 3, widths = c(.21, .58, .21), heights = c(.575, .4,.025))
  
} # Pie chart for pitch usage

SSWZONES <- function(DF,Hand,P, BREAK){
  
  # Zone values
  ZONEXY <- data.frame(Zone = c('1', '2', '3', '4', '5', '6','7', '8', '9', '13', '11', '14', '12'),
                       Side = c(-.375, 0, .375, -.375, 0, .375, -.375, 0, .375, -.375, -.375, .375, .375),
                       Height = c(2.1, 2.1, 2.1, 2.8, 2.8, 2.8, 3.5, 3.5, 3.5, 2.1, 3.5, 2.1, 3.5))
  
  # Find overall averages for HB/IVB by pitch but NOT by zone
  HMWH <- DF %>% filter(., BatterSide == Hand) %>% 
    mutate(TaggedPitchType = factor(TaggedPitchType, levels = c(unique(TaggedPitchType)))) %>%
    group_by(TaggedPitchType, .drop = F) %>% summarize(OVRAHB = sprintf('%.1f', mean(total_move_x, na.rm = T)),
                                                       OVRAIVB = sprintf('%.1f', mean(total_move_z, na.rm = T)),
                                                       OVRATB = sprintf('%.1f', mean(TotalBreak, na.rm = T)))
  
  # Calculate zone percentages
  HMWH <- DF %>% filter(., BatterSide == Hand) %>% 
    mutate(Zone = factor(Zone, levels = c('1', '2', '3', '4', '5', '6', '7', '8','9', '11', '12', '13', '14'))) %>% 
    mutate(TaggedPitchType = factor(TaggedPitchType, levels = c(unique(TaggedPitchType)))) %>%
    group_by(Zone, TaggedPitchType) %>% count(Zone, .drop = F) %>% ungroup(Zone) %>% mutate(Total = sum(n, na.rm = T)) %>% 
    mutate(Perc = n/Total) %>% mutate(Perc = ifelse(is.na(Perc) == TRUE | Perc %in% c('NA', 'NaN', Inf), 0, as.numeric(Perc))) %>% 
    distinct(Zone, TaggedPitchType, Perc)  %>%
    left_join(., HMWH, by = c('TaggedPitchType'))
  
  # Find IVB, HB by pitch, zone
  HMWH <- DF %>% filter(., BatterSide == Hand) %>% 
    mutate(TaggedPitchType = factor(TaggedPitchType, levels = c(unique(TaggedPitchType)))) %>%
    mutate(Zone = factor(Zone, levels = c('1', '2', '3', '4', '5', '6', '7', '8','9', '11', '12', '13', '14'))) %>% 
    group_by(TaggedPitchType, Zone, .drop = F) %>% summarize(AHB = sprintf('%.1f', mean(total_move_x, na.rm = T)),
                                                             AIVB = sprintf('%.1f', mean(total_move_z, na.rm = T)),
                                                             ATB = sprintf('%.1f', mean(TotalBreak, na.rm = T))) %>% 
    distinct(Zone, AIVB, AHB, ATB) %>% left_join(., ZONEXY, by = 'Zone') %>% 
    mutate(IVBreak = ifelse(AIVB != 'NaN', AIVB, '-')) %>%
    mutate(TotalBreak = ifelse(ATB != 'NaN', ATB, '-')) %>%
    mutate(HBreak = ifelse(AHB != 'NaN', AHB, '-')) %>%
    left_join(., HMWH, by = c('Zone', 'TaggedPitchType'))
  
  # Find differences in break from average (Current - Avg)
  HMWH <- HMWH %>%
    mutate(TBDIFF = as.numeric(ATB) - as.numeric(OVRATB)) %>%
    mutate(HBDIFF = as.numeric(AHB) - as.numeric(OVRAHB)) %>%
    mutate(IVBDIFF = as.numeric(AIVB) - as.numeric(OVRAIVB))
  
  
  # Filter by pitch type
  HMWH <- HMWH %>% 
    filter(TaggedPitchType == P) 
  
  # Minimum and maximum difference for this pitch
  max_TBDIFF <- max(HMWH$TBDIFF, na.rm = T)
  min_TBDIFF <- min(HMWH$TBDIFF, na.rm = T)
  
  max_HBDIFF <- max(HMWH$HBDIFF, na.rm = T)
  min_HBDIFF <- min(HMWH$HBDIFF, na.rm = T)
  
  max_IVBDIFF <- max(HMWH$IVBDIFF, na.rm = T)
  min_IVBDIFF <- min(HMWH$IVBDIFF, na.rm = T)
  
  RBCLAB <- data.frame(Zone = c('13', '11', '14', '12'), LAB = c(-.57, -.57, .57, .57), LAH = c(1.55, 4, 1.55, 4))
  RCB <- HMWH %>% filter(., Zone %in% c('11', '12', '13','14')) %>% left_join(., RBCLAB, by = 'Zone')
  RCP <- HMWH %>% filter(., !(Zone %in% c('11', '12', '13','14')))
  
  if(BREAK == 'TB'){
    # Plot with TB avgs
    G1 <- ggplot(RCB, aes(x=Side, y = Height))+
      geom_tile(aes(fill = TBDIFF))+ # Fill with deviation from avg
      geom_tile(data = RCP, aes(x=Side, y = Height, fill = TBDIFF))+
      geom_text(aes(x=LAB, y = LAH, label= TotalBreak), fontface = 'bold', size = 7)+
      geom_text(data = RCP, aes(x = Side, y = Height, label = TotalBreak), fontface = 'bold', size = 8) +
      scale_fill_gradientn(colors = c(low = 'dodgerblue', mid = 'white', high = 'red'), values = NULL, breaks =(0.25*0:4), 
                           limits = c(min_TBDIFF,max_TBDIFF), labels = percent(0.25*0:4), oob = squish)+
      geom_segment(aes(x=-.56, xend = .56, y = 1.75, yend = 1.75), size = 1, color = 'black')+
      geom_segment(aes(x=-.56, xend=.56, y=3.85, yend=3.85), size = 1, color = 'black')+
      geom_segment(aes(x=-.56, xend = -.56, y = 1.75, yend = 3.85), size = 1, color = 'black')+
      geom_segment(aes(x=.56, xend = .56, y = 1.75, yend = 3.85), size = 1, color = 'black')+
      geom_segment(aes(x=-.19, xend = -.19, y = 1.75, yend = 3.85), size = 1, color = 'black')+
      geom_segment(aes(x=.19, xend = .19, y = 1.75, yend = 3.85), size = 1, color = 'black')+
      geom_segment(aes(x=-.56, xend = .56, y = 2.45, yend = 2.45), size = 1, color = 'black')+
      geom_segment(aes(x=-.56, xend = .56, y = 3.15, yend = 3.15), size = 1, color = 'black')+
      geom_segment(aes(x=0,xend=0,y=1.75,yend=1.40), size = 1, color = 'black')+
      geom_segment(aes(x=0, xend=0, y=3.85, yend = 4.20), size = 1, color = 'black')+
      geom_segment(aes(x=-.75, xend=-.56, y = 2.8, yend = 2.8), size = 1, color = 'black')+
      geom_segment(aes(x=.75, xend = .56, y = 2.8, yend = 2.8), size = 1, color = 'black')+
      geom_segment(aes(x=-.75, xend = .75, y = 1.40, yend = 1.40), size = 1, color = 'black')+
      geom_segment(aes(x=-.75, xend = -.75, y = 1.40, yend = 4.2), size = 1, color = 'black')+
      geom_segment(aes(x=-.75, xend = .75, y = 4.2, yend = 4.2), size = 1, color = 'black')+
      geom_segment(aes(x=.75, xend = .75, y = 4.2, yend = 1.4), size = 1, color = 'black')+
      
      geom_segment(aes(x = -0.56, y = .5, xend = 0.56, yend = .5 ), col = "black")+
      geom_segment(aes(x = -0.56, y = .35, xend = -0.56, yend = .5), col = "black") +
      geom_segment(aes(x = -0.56, y = .35, xend = 0, yend = .15), col = "black") +
      geom_segment(aes(x = 0, y = .15, xend = 0.56, yend = 0.35), col = "black") +
      geom_segment(aes(x = 0.56, y = 0.35, xend = 0.56, yend = .5), col = "black")+
      
      geom_segment(aes(x=-1.975, xend = -.9, y = .5, yend =.50))+
      geom_segment(aes(x=-.9, xend = -.9, y = .50, yend =-.4))+
      geom_segment(aes(x=-1.975, xend = -1, y = .38, yend = .38))+
      geom_segment(aes(x=-1, xend = -1, y = .38, yend =-.4))+ 
      geom_segment(aes(x=2.2, xend = .9, y = .50, yend =.50))+
      geom_segment(aes(x=.9, xend = .9, y = .50, yend =-.4))+
      geom_segment(aes(x=2.2, xend = 1, y = .38, yend = .38))+
      geom_segment(aes(x=1, xend = 1, y = .38, yend =-.4))+
      
      
      coord_cartesian(ylim = c(0,4.5), xlim = c(-0.9,0.9))+
      
      theme_bw()+
      theme(panel.background = element_blank(), axis.text = element_blank(), axis.title = element_blank(),
            axis.ticks = element_blank(), panel.grid = element_blank(), legend.position = 'none', 
            plot.title = element_text(size = 30, hjust = 0.5, face = 'bold'))+
      ggtitle(paste0('Total Break ',P, ' - ', paste0(substr(Hand, 1,1), 'HH')))
  }
  else if(BREAK == 'HB'){
    # Plot with HB avgs
    G1 <- ggplot(RCB, aes(x=Side, y = Height))+
      geom_tile(aes(fill = HBDIFF))+ # Fill with deviation from avg
      geom_tile(data = RCP, aes(x=Side, y = Height, fill = HBDIFF))+
      geom_text(aes(x=LAB, y = LAH, label= HBreak), fontface = 'bold', size = 7)+
      geom_text(data = RCP, aes(x = Side, y = Height, label = HBreak), fontface = 'bold', size = 8) +
      scale_fill_gradientn(colors = c(low = 'dodgerblue', mid = 'white', high = 'red'), values = NULL, breaks =(0.25*0:4), 
                           limits = c(min_HBDIFF,max_HBDIFF), labels = percent(0.25*0:4), oob = squish)+
      geom_segment(aes(x=-.56, xend = .56, y = 1.75, yend = 1.75), size = 1, color = 'black')+
      geom_segment(aes(x=-.56, xend=.56, y=3.85, yend=3.85), size = 1, color = 'black')+
      geom_segment(aes(x=-.56, xend = -.56, y = 1.75, yend = 3.85), size = 1, color = 'black')+
      geom_segment(aes(x=.56, xend = .56, y = 1.75, yend = 3.85), size = 1, color = 'black')+
      geom_segment(aes(x=-.19, xend = -.19, y = 1.75, yend = 3.85), size = 1, color = 'black')+
      geom_segment(aes(x=.19, xend = .19, y = 1.75, yend = 3.85), size = 1, color = 'black')+
      geom_segment(aes(x=-.56, xend = .56, y = 2.45, yend = 2.45), size = 1, color = 'black')+
      geom_segment(aes(x=-.56, xend = .56, y = 3.15, yend = 3.15), size = 1, color = 'black')+
      geom_segment(aes(x=0,xend=0,y=1.75,yend=1.40), size = 1, color = 'black')+
      geom_segment(aes(x=0, xend=0, y=3.85, yend = 4.20), size = 1, color = 'black')+
      geom_segment(aes(x=-.75, xend=-.56, y = 2.8, yend = 2.8), size = 1, color = 'black')+
      geom_segment(aes(x=.75, xend = .56, y = 2.8, yend = 2.8), size = 1, color = 'black')+
      geom_segment(aes(x=-.75, xend = .75, y = 1.40, yend = 1.40), size = 1, color = 'black')+
      geom_segment(aes(x=-.75, xend = -.75, y = 1.40, yend = 4.2), size = 1, color = 'black')+
      geom_segment(aes(x=-.75, xend = .75, y = 4.2, yend = 4.2), size = 1, color = 'black')+
      geom_segment(aes(x=.75, xend = .75, y = 4.2, yend = 1.4), size = 1, color = 'black')+
      
      geom_segment(aes(x = -0.56, y = .5, xend = 0.56, yend = .5 ), col = "black")+
      geom_segment(aes(x = -0.56, y = .35, xend = -0.56, yend = .5), col = "black") +
      geom_segment(aes(x = -0.56, y = .35, xend = 0, yend = .15), col = "black") +
      geom_segment(aes(x = 0, y = .15, xend = 0.56, yend = 0.35), col = "black") +
      geom_segment(aes(x = 0.56, y = 0.35, xend = 0.56, yend = .5), col = "black")+
      
      geom_segment(aes(x=-1.975, xend = -.9, y = .5, yend =.50))+
      geom_segment(aes(x=-.9, xend = -.9, y = .50, yend =-.4))+
      geom_segment(aes(x=-1.975, xend = -1, y = .38, yend = .38))+
      geom_segment(aes(x=-1, xend = -1, y = .38, yend =-.4))+ 
      geom_segment(aes(x=2.2, xend = .9, y = .50, yend =.50))+
      geom_segment(aes(x=.9, xend = .9, y = .50, yend =-.4))+
      geom_segment(aes(x=2.2, xend = 1, y = .38, yend = .38))+
      geom_segment(aes(x=1, xend = 1, y = .38, yend =-.4))+
      
      
      coord_cartesian(ylim = c(0,4.5), xlim = c(-0.9,0.9))+
      
      theme_bw()+
      theme(panel.background = element_blank(), axis.text = element_blank(), axis.title = element_blank(),
            axis.ticks = element_blank(), panel.grid = element_blank(), legend.position = 'none', 
            plot.title = element_text(size = 30, hjust = 0.5, face = 'bold'))+
      ggtitle(paste0('HB ',P, ' - ', paste0(substr(Hand, 1,1), 'HH')))
  }
  else{
    # Plot with IVB avgs
    G1 <- ggplot(RCB, aes(x=Side, y = Height))+
      geom_tile(aes(fill = IVBDIFF))+ # Fill with deviation from avg
      geom_tile(data = RCP, aes(x=Side, y = Height, fill = IVBDIFF))+
      geom_text(aes(x=LAB, y = LAH, label= IVBreak), fontface = 'bold', size = 7)+
      geom_text(data = RCP, aes(x = Side, y = Height, label = IVBreak), fontface = 'bold', size = 8) +
      scale_fill_gradientn(colors = c(low = 'dodgerblue', mid = 'white', high = 'red'), values = NULL, breaks =(0.25*0:4), 
                           limits = c(min_IVBDIFF,max_IVBDIFF), labels = percent(0.25*0:4), oob = squish)+
      geom_segment(aes(x=-.56, xend = .56, y = 1.75, yend = 1.75), size = 1, color = 'black')+
      geom_segment(aes(x=-.56, xend=.56, y=3.85, yend=3.85), size = 1, color = 'black')+
      geom_segment(aes(x=-.56, xend = -.56, y = 1.75, yend = 3.85), size = 1, color = 'black')+
      geom_segment(aes(x=.56, xend = .56, y = 1.75, yend = 3.85), size = 1, color = 'black')+
      geom_segment(aes(x=-.19, xend = -.19, y = 1.75, yend = 3.85), size = 1, color = 'black')+
      geom_segment(aes(x=.19, xend = .19, y = 1.75, yend = 3.85), size = 1, color = 'black')+
      geom_segment(aes(x=-.56, xend = .56, y = 2.45, yend = 2.45), size = 1, color = 'black')+
      geom_segment(aes(x=-.56, xend = .56, y = 3.15, yend = 3.15), size = 1, color = 'black')+
      geom_segment(aes(x=0,xend=0,y=1.75,yend=1.40), size = 1, color = 'black')+
      geom_segment(aes(x=0, xend=0, y=3.85, yend = 4.20), size = 1, color = 'black')+
      geom_segment(aes(x=-.75, xend=-.56, y = 2.8, yend = 2.8), size = 1, color = 'black')+
      geom_segment(aes(x=.75, xend = .56, y = 2.8, yend = 2.8), size = 1, color = 'black')+
      geom_segment(aes(x=-.75, xend = .75, y = 1.40, yend = 1.40), size = 1, color = 'black')+
      geom_segment(aes(x=-.75, xend = -.75, y = 1.40, yend = 4.2), size = 1, color = 'black')+
      geom_segment(aes(x=-.75, xend = .75, y = 4.2, yend = 4.2), size = 1, color = 'black')+
      geom_segment(aes(x=.75, xend = .75, y = 4.2, yend = 1.4), size = 1, color = 'black')+
      
      geom_segment(aes(x = -0.56, y = .5, xend = 0.56, yend = .5 ), col = "black")+
      geom_segment(aes(x = -0.56, y = .35, xend = -0.56, yend = .5), col = "black") +
      geom_segment(aes(x = -0.56, y = .35, xend = 0, yend = .15), col = "black") +
      geom_segment(aes(x = 0, y = .15, xend = 0.56, yend = 0.35), col = "black") +
      geom_segment(aes(x = 0.56, y = 0.35, xend = 0.56, yend = .5), col = "black")+
      
      geom_segment(aes(x=-1.975, xend = -.9, y = .5, yend =.50))+
      geom_segment(aes(x=-.9, xend = -.9, y = .50, yend =-.4))+
      geom_segment(aes(x=-1.975, xend = -1, y = .38, yend = .38))+
      geom_segment(aes(x=-1, xend = -1, y = .38, yend =-.4))+ 
      geom_segment(aes(x=2.2, xend = .9, y = .50, yend =.50))+
      geom_segment(aes(x=.9, xend = .9, y = .50, yend =-.4))+
      geom_segment(aes(x=2.2, xend = 1, y = .38, yend = .38))+
      geom_segment(aes(x=1, xend = 1, y = .38, yend =-.4))+
      
      
      coord_cartesian(ylim = c(0,4.5), xlim = c(-0.9,0.9))+
      
      theme_bw()+
      theme(panel.background = element_blank(), axis.text = element_blank(), axis.title = element_blank(),
            axis.ticks = element_blank(), panel.grid = element_blank(), legend.position = 'none', 
            plot.title = element_text(size = 30, hjust = 0.5, face = 'bold'))+
      ggtitle(paste0('IVB ',P, ' - ', paste0(substr(Hand, 1,1), 'HH')))
  }
  
} # LHH/RHH visuals

SSW <- function(DF, DF2){
  #Pitches
  GPIT <- DF %>% distinct(TaggedPitchType) %>% mutate(TaggedPitchType =as.character(TaggedPitchType)) %>% filter(., !(TaggedPitchType %in% c('NA','')))
  
  #Freq (pitch usage)
  COL1 <- DF %>% mutate(TaggedPitchType = factor(TaggedPitchType, levels = c(GPIT$TaggedPitchType))) %>% group_by(TaggedPitchType) %>% 
    count(TaggedPitchType, .drop = F) %>% filter(., TaggedPitchType != 'NA') %>% rename(., Freq = n) %>% ungroup(TaggedPitchType) %>%
    mutate(TotalPitches = sum(Freq, na.rm = TRUE),
           Usage = paste0(sprintf('%.1f', (Freq/TotalPitches)*100),'%')) %>%
    distinct(TaggedPitchType, Usage)
  
  # Tilt handling
  DTILT <- DF %>% select(Tilt, Pitch = TaggedPitchType) %>% filter(., !(Tilt %in% c('', '-', 'NA', ' ', 'NA:'))) %>%
    mutate(Tilt = str_replace_all(Tilt, 'h', '')) %>% mutate(Tilt = str_replace_all(Tilt, 'm', '')) %>%
    separate(Tilt, into = c('Hour', 'Min')) %>% mutate_at(vars(Hour, Min), ~as.numeric(as.character(.))) %>%
    mutate(Hour = ifelse(Hour == 0, 12, Hour)) %>% filter(., Hour != 'NA')
  
  # Tilt
  COL2 <- DTILT %>% mutate(Pitch = factor(Pitch, levels = c(GPIT$TaggedPitchType))) %>%
    group_by(Pitch, .drop = F) %>% summarize(Max = max(Hour, na.rm = T), Min = min(Hour, na.rm = T)) %>%
    mutate(Range = Max-Min) %>% mutate(NEG = ifelse(Range > 6, 'TRUE', 'FALSE')) %>% select(Pitch, NEG) %>%
    left_join(DTILT, ., by = c('Pitch')) %>%
    mutate(CHour = case_when(NEG == 'TRUE' & Hour == 12 ~ 0, NEG == 'TRUE' & Hour == 11 ~ -1,
                             NEG == 'TRUE' & Hour == 10 ~ -2, NEG == 'TRUE' & Hour == 9 ~ -3,
                             NEG == 'TRUE' & Hour == 8 ~ -4, NEG == 'TRUE' & Hour == 7 ~ -5, T ~ Hour)) %>%
    mutate(CMin = ifelse(CHour < 0, Min-60, Min)) %>% mutate(CMins = CMin/100) %>%
    mutate(TestT = CHour+CMins) %>% group_by(Pitch, .drop = F) %>% summarize(NewTilt = round(median(TestT, na.rm = T), 2)) %>%
    mutate(Neg = ifelse(NewTilt < 0, TRUE, FALSE)) %>% mutate(CTilt = ifelse(Neg == 'TRUE', NewTilt*-1, NewTilt)) %>%
    mutate(CTilt = sprintf('%.2f', CTilt)) %>% separate(CTilt, into = c('MinHour', 'MinMinute')) %>%
    mutate_at(vars(MinHour, MinMinute), ~as.numeric(as.character(.))) %>%
    mutate_at(vars(MinHour, MinMinute), ~ifelse(. == 'NA' | is.na(.) == 'TRUE', 0, .)) %>% mutate_at(vars(MinHour, MinMinute), ~as.numeric(as.character(.))) %>%
    mutate(HourF = ifelse(Neg == 'TRUE' & MinHour == 0 | MinHour == 0, 12, ifelse(Neg == 'TRUE' & MinHour == 1, 11,
                                                                                  ifelse(Neg == 'TRUE' & MinHour == 2, 10, ifelse(Neg == 'TRUE' & MinHour == 3,9, ifelse(Neg == 'TRUE' & MinHour == 4, 8,
                                                                                                                                                                         ifelse(Neg == 'TRUE' & MinHour == 5, 7, MinHour))))))) %>% mutate_at(vars(MinMinute, MinHour), ~as.numeric(as.character(.))) %>%
    mutate(NMIN = ifelse(MinMinute > 60, MinMinute-60, MinMinute)) %>% mutate_at(vars(MinHour:NMIN), ~as.numeric(as.character(.))) %>%
    mutate(NHOUR = ifelse(MinMinute > 60 & MinHour < 12, MinHour+1,ifelse(MinMinute > 60 & MinHour > 11, 1, HourF))) %>%
    mutate(MinMinute2 = ifelse(MinMinute > 60, round((NMIN/100)*60,0), MinMinute)) %>%
    mutate(MinMinute2 = RoundTo(MinMinute2,5)) %>% mutate(MinF = ifelse(Neg == 'TRUE', 60-MinMinute2, MinMinute2)) %>%
    mutate(NHOUR = as.numeric(as.character(NHOUR))) %>% mutate(NHOUR = ifelse(NHOUR != 12 & MinF == 60, NHOUR+1, ifelse(NHOUR == 12 & MinF == 60, 1, NHOUR))) %>%
    mutate(MinF = ifelse(MinF == 60, 0, MinF)) %>% mutate_at(vars(MinHour:MinF), ~as.numeric(as.character(.))) %>%
    mutate(MinF = ifelse(MinF < 10, paste0(0, MinF), as.character(MinF))) %>%
    mutate(AVG = paste0(NHOUR, ':', MinF)) %>% select(TaggedPitchType = Pitch, Tilt = AVG) %>%
    filter(., !(TaggedPitchType %in% c('NA', '')))
  
  ### SSW Section - uses averages from SSW model
  COL3 <- DF2 %>%
    mutate_at(vars(-TaggedPitchType), ~ sprintf('%.3f',.))
  
  # Order pitches by order in 'target'
  target <- c('Fastball', 'Sinker', 'Cutter', 'Curveball', 'Slider', 'Changeup', 'Splitter', 'Knuckle Ball', 'Overall')
  PLATETB <- Reduce(function(x,y) merge(x,y, by.all = c('TaggedPitchType')), list(COL1, COL2, COL3)) %>%
    arrange(match(TaggedPitchType, target)) %>% mutate_all(., ~ifelse(. %in% c('NA', 'NaN%', 'NaN'), '-', as.character(.)))
  colours <- PLATETB %>% mutate(ID = row_number()) %>% 
    mutate_at(vars(-ID), ~ifelse(ID%%2 == 0, '#e8f0e6', '#ffffff')) %>%
    select(-ID) %>%
    as.matrix()
  df1 <- PLATETB
  
  P1n <- dim(df1)[1]
  nums_cols <- dim(df1)[2]
  tt <- ttheme_default(core = list(bg_params=list(fill=colours, col = 'black'),fg_params = list(fontsize = 32,  col = 'black', fontface = 1)), colhead = list(bg_params=list(fill='#d5d9e0', col='black'),fg_params = list(fontsize = 28)))
  tt2 <- ttheme_default(core = list(bg_params=list(fill='gray45', col = 'black'),fg_params = list(fontsize = 32,  col = 'white', fontface = 'bold')), colhead = list(bg_params=list(fill='#a9d4a1', col='black'),fg_params = list(fontsize = 40, col = 'black')))
  tab3 <- tableGrob(df1[1:P1n, 1:nums_cols], rows = NULL, cols = c('Pitch', 'Usage', 'Tilt\n(hh:mm)', 'Magnus HB\n(in.)', 'Magnus IVB\n(in.)', 'Drag HB\n(in.)', 'Drag IVB\n(in.)', 'Seam HB\n(in.)', 'Seam IVB\n(in.)', 'Total HB\n(in.)', 'Total IVB\n(in.)'), theme = tt)
  header2 <- tableGrob(df1[1,1:1], rows=NULL, cols=c('Pitch Usage & SSW Summary'), theme = tt2) 
  jn2 <- gtable_combine(header2[1,], tab3, along=2, join = 'outer')
  jn2$widths <- rep(max(jn2$widths), length(jn2$widths)) # make column widths equal
  jn2$layout[1:2 , c("l", "r")] <- list(c(1),c(nums_cols))
  g <- jn2
  g <- gtable_add_grob(g,grobs = rectGrob(gp = gpar(fill = NA, lwd = 3)),t = 1, b = nrow(g), l = 1, r = nums_cols)
  g <- gtable_add_grob(g,grobs = rectGrob(gp = gpar(fill = NA, lwd =1.5)),t = 1, b = 2, l = 1, r = nums_cols)
  g$widths <- unit(c(rep(1/nums_cols, nums_cols)), 'npc')
  g$heights <- unit(c(.13, .13, rep(.74/(nrow(g)-2), (nrow(g)-2))), 'npc')
  SWDECGROB <- g 
} # Stat line and percentages, plus SSW metrics

MLB_COMPS <- function(DF, Hand){
  #Pitches
  GPIT <- DF %>% distinct(TaggedPitchType) %>% mutate(TaggedPitchType =as.character(TaggedPitchType)) %>% filter(., !(TaggedPitchType %in% c('NA','')))
  
  # Pitch Type
  COL1 <- DF %>% mutate(TaggedPitchType = factor(TaggedPitchType, levels = c(GPIT$TaggedPitchType))) %>% group_by(TaggedPitchType) %>% 
    count(TaggedPitchType, .drop = F) %>% filter(., TaggedPitchType != 'NA') %>% rename(., Freq = n) %>% ungroup(TaggedPitchType) %>%
    distinct(TaggedPitchType)
  
  # Contains averages for spin, velo, and extension
  CLEAN_MLB_AVGS <- MLB_AVGS %>%
    rename(TaggedPitchType = pitch_type) %>% # Fix col name to max original dataframe
    mutate(TaggedPitchType = factor(TaggedPitchType, levels = c(GPIT$TaggedPitchType))) %>%
    filter(p_throws == Hand & TaggedPitchType %in% GPIT$TaggedPitchType)
  
  # Contains averages for efficiency
  CLEAN_MLB_EFFICIENCY_AVGS <- MLB_EFFICIENCY_AVGS %>%
    rename(TaggedPitchType = pitch_type) %>% # Fix col name to max original dataframe
    mutate(TaggedPitchType = factor(TaggedPitchType, levels = c(GPIT$TaggedPitchType))) %>%
    filter(p_throws == Hand & TaggedPitchType %in% GPIT$TaggedPitchType)
  
  # Merge dataframes on pitch type
  CLEAN_MLB_AVGS <- merge(CLEAN_MLB_AVGS, CLEAN_MLB_EFFICIENCY_AVGS, by = 'TaggedPitchType')
  
  # Remove extra p_throws column
  CLEAN_MLB_AVGS <- CLEAN_MLB_AVGS %>%
    select(-p_throws.x, -p_throws.y)
  
  # Velo, Extension, Spin Rate with MLB comps
  COL2 <- DF %>%  mutate(TaggedPitchType = factor(TaggedPitchType, levels = c(GPIT$TaggedPitchType))) %>% group_by(TaggedPitchType, .drop = F) %>%
    summarize(VELO = sprintf('%.1f', mean(RelSpeed, na.rm = T)),
              SPIN = sprintf('%.0f', mean(SpinRate, na.rm = T)),
              EXT = sprintf('%.2f', mean(Extension, na.rm = T)),
              EFF = paste0(sprintf('%.1f', mean(Efficiency, na.rm = T)),'%')) %>% 
    filter(., TaggedPitchType != 'NA')
  
  COL3 <- CLEAN_MLB_AVGS %>%
    mutate_at(vars(avg_spin), ~ sprintf('%.0f',.)) %>%
    mutate_at(vars(avg_velocity), ~ sprintf('%.1f',.)) %>%
    mutate_at(vars(avg_extension), ~ sprintf('%.2f',.)) %>%
    mutate_at(vars(avg_efficiency), ~ paste0(sprintf('%.1f',.), '%')) %>%
    rename(MLBSPIN = avg_spin,
           MLBVELO = avg_velocity,
           MLBEXT = avg_extension,
           MLBEFF = avg_efficiency)
  
  target <- c('Fastball', 'Sinker', 'Cutter', 'Curveball', 'Slider', 'Changeup', 'Splitter', 'Knuckle Ball', 'Overall')
  PLATETB <- Reduce(function(x,y) merge(x,y, by.all = c('TaggedPitchType')), list(COL1, COL2, COL3)) %>%
    arrange(match(TaggedPitchType, target)) %>% mutate_all(., ~ifelse(. %in% c('NA', 'NaN%', 'NaN'), '-', as.character(.))) %>%
    select(TaggedPitchType, VELO, MLBVELO, SPIN, MLBSPIN, EXT, MLBEXT, EFF, MLBEFF)
  colours <- PLATETB %>% 
    mutate(VELO_DIFF = as.numeric(VELO) - as.numeric(MLBVELO),
           SPIN_DIFF = as.numeric(SPIN) - as.numeric(MLBSPIN),
           EXT_DIFF = as.numeric(EXT) - as.numeric(MLBEXT),
           EFF_DIFF = as.numeric(EFF) - as.numeric(MLBEFF)) %>%
    mutate(ID = row_number()) %>% 
    mutate_at(vars(-c(ID, VELO_DIFF, SPIN_DIFF, EXT_DIFF, EFF_DIFF)), ~ifelse(ID%%2 == 0, '#e8f0e6', '#ffffff')) %>%
    mutate(VELO = ifelse(VELO_DIFF > 0, 'green', as.character(VELO)), SPIN = ifelse(SPIN_DIFF > 0, 'green', as.character(SPIN)), EXT = ifelse(EXT_DIFF > 0, 'green', as.character(EXT)), EFF = as.character(EFF)) %>% 
    select(-c(ID, VELO_DIFF, SPIN_DIFF, EXT_DIFF, EFF_DIFF)) %>%
    as.matrix()
  df1 <- PLATETB
  
  P1n <- dim(df1)[1]
  nums_cols <- dim(df1)[2]
  tt <- ttheme_default(core = list(bg_params=list(fill=colours, col = 'black'),fg_params = list(fontsize = 32,  col = 'black', fontface = 1)), colhead = list(bg_params=list(fill='#d5d9e0', col='black'),fg_params = list(fontsize = 28)))
  tt2 <- ttheme_default(core = list(bg_params=list(fill='gray45', col = 'black'),fg_params = list(fontsize = 32,  col = 'white', fontface = 'bold')), colhead = list(bg_params=list(fill='#a9d4a1', col='black'),fg_params = list(fontsize = 40, col = 'black')))
  tab3 <- tableGrob(df1[1:P1n, 1:nums_cols], rows = NULL, cols = c('Pitch', 'Velocity\n(mph)',paste0('MLB Velocity - ', paste0(Hand,'HP'), '\n(mph)'), 'Spin Rate\n(rpm)', paste0('MLB Spin Rate - ', paste0(Hand,'HP'), '\n(rpm)'), 
                                                                   'Extension\n(in.)', paste0('MLB Extension - ', paste0(Hand,'HP'), '\n(in.)'), 'Efficiency', paste0('MLB Efficiency - ', paste0(Hand,'HP'))), theme = tt)
  header2 <- tableGrob(df1[1,1:1], rows=NULL, cols=c('Pitch Metrics vs. MLB Averages'), theme = tt2) 
  jn2 <- gtable_combine(header2[1,], tab3, along=2, join = 'outer')
  jn2$widths <- rep(max(jn2$widths), length(jn2$widths)) # make column widths equal
  jn2$layout[1:2 , c("l", "r")] <- list(c(1),c(nums_cols))
  g <- jn2
  g <- gtable_add_grob(g,grobs = rectGrob(gp = gpar(fill = NA, lwd = 3)),t = 1, b = nrow(g), l = 1, r = nums_cols)
  g <- gtable_add_grob(g,grobs = rectGrob(gp = gpar(fill = NA, lwd =1.5)),t = 1, b = 2, l = 1, r = nums_cols)
  g$widths <- unit(c(rep(1/nums_cols, nums_cols)), 'npc')
  g$heights <- unit(c(.13, .13, rep(.74/(nrow(g)-2), (nrow(g)-2))), 'npc')
  SWDECGROB <- g 
} # Pitcher's averages against MLB averages

VECTORPLOTS <- function(DF, DF2, P){
  # Filter by pitch type
  VPLOT <- DF %>% 
    filter(TaggedPitchType == P)
  
  AVGS <- DF2 %>%
    filter(TaggedPitchType == P)
  
  # Find endpoints for vectors
  DRAG_X <- (AVGS$Drag_x)
  DRAG_Z <- (AVGS$Drag_z)
  
  MAG_X <- (DRAG_X + AVGS$Magnus_x)
  MAG_Z <- (DRAG_Z + AVGS$Magnus_z)
  
  SEAM_X <- AVGS$Total_x
  SEAM_Z <- AVGS$Total_z
  
  # Put all points in a dataframe and find the minimum, maximum values for magnification purposes
  DFR <- data.frame(x = c(0, -DRAG_X, -MAG_X), xe = c(-DRAG_X, -MAG_X, -SEAM_X),
                    y = c(0, DRAG_Z, MAG_Z), ye = c(DRAG_Z, MAG_Z, SEAM_Z),
                    Vector = c('Drag', 'Magnus', 'Seam')) %>% 
    mutate_at(vars(x:ye), ~.*.5)
  
  XCOORD_MIN <- min(DFR$x[3], DFR$xe[3]) - 1
  XCOORD_MAX <- max(DFR$x[3], DFR$xe[3]) + 1
  YCOORD_MIN <- min(DFR$y[3], DFR$ye[3]) - 1
  YCOORD_MAX <- max(DFR$y[3], DFR$ye[3]) + 1
  
  MAGNIFY_X <- c(0, 0) # Min, max
  MAGNIFY_Y <- c(YCOORD_MAX+2, YCOORD_MAX+8) # Min, max
  MARGIN <- c(0,0)
  
  if(DF$PitcherThrows[1] == "Right"){
    # Move magnifying window to top left
    MAGNIFY_X[1] <- -4
    MAGNIFY_X[2] <- -12
    MARGIN[1] <- 60
    MARGIN[2] <- 30
  }else{ # Move magnifying window to top right
    MAGNIFY_X[1] <- 4
    MAGNIFY_X[2] <- 12
    MARGIN[1] <- 30
    MARGIN[2] <- 60
  }
  
  # Density plot
  P1 <- ggplot(DFR) +
    geom_circle(aes(x0=0, y0=0, r=12), color='black',lwd=1.5, inherit.aes=FALSE) +
    geom_segment(aes(x = x, xend = xe, y = y, yend = ye, color = Vector), arrow = arrow(length=unit(0.30,"cm")), lwd = 1.5)+
    scale_color_manual(values = c('Drag' = 'black', 'Magnus' = 'red', 'Seam' = 'dodgerblue3'))+
    theme(panel.background = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x = element_text(size = 30, face="bold"),
          axis.title.y = element_blank(),
          legend.position = 'none',
          plot.margin = ggplot2::margin(30,MARGIN[2], 30, MARGIN[1]))+
    coord_cartesian(clip = "off") +
    geom_magnify(from = c(xmin = XCOORD_MIN,xmax = XCOORD_MAX,ymin = YCOORD_MIN,ymax = YCOORD_MAX), to = c(xmin = min(MAGNIFY_X),xmax =  max(MAGNIFY_X),ymin = 4,ymax = 12),
                 shadow = TRUE, axes = 'none', proj = 'corresponding')+
    labs(x = P)
}

PITCHMETRICS <- function(DF, Hand, P){
  # Filter by pitch
  METRICS <- DF %>% 
    filter(TaggedPitchType == P)
  
  CLEAN_MLB_AVGS <- MLB_AVGS %>%
    rename(TaggedPitchType = pitch_type) %>% # Fix col name to max original dataframe
    filter(p_throws == Hand & TaggedPitchType == P)
  
  CLEAN_MLB_EFFICIENCY_AVGS <- MLB_EFFICIENCY_AVGS %>%
    rename(TaggedPitchType = pitch_type) %>% # Fix col name to max original dataframe
    filter(p_throws == Hand & TaggedPitchType == P)
  
  # Merge dataframes on pitch type
  CLEAN_MLB_AVGS <- merge(CLEAN_MLB_AVGS, CLEAN_MLB_EFFICIENCY_AVGS, by = 'TaggedPitchType')
  
  # Remove extra p_throws column
  CLEAN_MLB_AVGS <- CLEAN_MLB_AVGS %>%
    select(-p_throws.x, -p_throws.y)
  
  # Velo, Extension, Spin Rate with MLB comps
  COL1 <- METRICS %>%
    mutate(VELO = sprintf('%.1f', mean(RelSpeed, na.rm = T)),
           SPIN = sprintf('%.0f', mean(SpinRate, na.rm = T)),
           EXT = sprintf('%.2f', mean(Extension, na.rm = T)),
           EFF = paste0(sprintf('%.1f', mean(Efficiency, na.rm = T)))) %>%
    select(TaggedPitchType, VELO, SPIN, EXT, EFF) %>% 
    distinct(TaggedPitchType, VELO, SPIN, EXT, EFF)
  
  COL2 <- CLEAN_MLB_AVGS %>%
    mutate_at(vars(avg_spin), ~ sprintf('%.0f',.)) %>%
    mutate_at(vars(avg_velocity), ~ sprintf('%.1f',.)) %>%
    mutate_at(vars(avg_extension), ~ sprintf('%.2f',.)) %>%
    mutate_at(vars(avg_efficiency), ~ sprintf('%.1f',.)) %>%
    rename(MLBSPIN = avg_spin,
           MLBVELO = avg_velocity,
           MLBEXT = avg_extension,
           MLBEFF = avg_efficiency)
  
  PLATETB <- Reduce(function(x,y) merge(x,y, by.all = c('TaggedPitchType')), list(COL1, COL2)) %>% mutate_all(., ~ifelse(. %in% c('NA', 'NaN%', 'NaN'), '-', as.character(.))) %>%
    select(VELO, MLBVELO, SPIN, MLBSPIN, EXT, MLBEXT, EFF, MLBEFF)
  df1 <- PLATETB
  
  P1n <- dim(df1)[1]
  nums_cols <- dim(df1)[2]
  tt <- ttheme_default(core = list(bg_params=list(fill=colours, col = 'black'),fg_params = list(fontsize = 32, col = 'black', fontface = 1)), colhead = list(bg_params=list(fill='#d5d9e0', col='black'),fg_params = list(fontsize = 28)))
  tt2 <- ttheme_default(core = list(bg_params=list(fill='gray45', col = 'black'),fg_params = list(fontsize = 32,col = 'white', fontface = 'bold')), colhead = list(bg_params=list(fill='#a9d4a1', col='black'),fg_params = list(fontsize = 40, col = 'black')))
  tab3 <- tableGrob(df1[1:P1n, 1:nums_cols], rows = NULL, cols = c('Velocity\n(mph)',
                                                               paste0('MLB Velocity - ', paste0(Hand,'HP'), '\n(mph)'), 
                                                               'Spin Rate\n(rpm)', 
                                                               paste0('MLB Spin Rate - ', paste0(Hand,'HP'), '\n(rpm)'), 
                                                               'Extension\n(in.)', 
                                                               paste0('MLB Extension - ', paste0(Hand,'HP'), '\n(in.)'), 
                                                               'Efficiency', 
                                                               paste0('MLB Efficiency - ', paste0(Hand,'HP'))), 
                    theme = tt)
  header2 <- tableGrob(df1[1,1:1], rows=NULL, cols=c('Pitch Metrics vs. MLB Averages'), theme = tt2) 
  jn2 <- gtable_combine(header2[1,], tab3, along=2, join = 'outer')
  jn2$widths <- rep(max(jn2$widths), length(jn2$widths)) # make column widths equal
  jn2$layout[1:2 , c("l", "r")] <- list(c(1),c(nums_cols))
  g <- jn2
  g <- gtable_add_grob(g,grobs = rectGrob(gp = gpar(fill = NA, lwd = 3)),t = 1, b = nrow(g), l = 1, r = nums_cols)
  g <- gtable_add_grob(g,grobs = rectGrob(gp = gpar(fill = NA, lwd =1.5)),t = 1, b = 2, l = 1, r = nums_cols)
  g$widths <- unit(c(rep(1/nums_cols, nums_cols)), 'npc')
  g$heights <- unit(c(.13, .13, rep(.74/(nrow(g)-2), (nrow(g)-2))), 'npc')
  SWDECGROB <- g 
} # Single pitch mlb comps

SSWPBP <- function(DF, DF2, P){
  # Filter by pitch
  SSWAVGS_PBP <- DF2 %>%
    filter(TaggedPitchType == P)
  
  #Pitches
  GPIT <- DF %>% distinct(TaggedPitchType) %>% mutate(TaggedPitchType =as.character(TaggedPitchType)) %>% filter(., !(TaggedPitchType %in% c('NA','')))
  
  #Freq (pitch usage)
  COL1 <- DF %>% mutate(TaggedPitchType = factor(TaggedPitchType, levels = c(GPIT$TaggedPitchType))) %>% group_by(TaggedPitchType) %>% 
    count(TaggedPitchType, .drop = F) %>% filter(., TaggedPitchType != 'NA') %>% rename(., Freq = n) %>% ungroup(TaggedPitchType) %>%
    mutate(TotalPitches = sum(Freq, na.rm = TRUE),
           Usage = paste0(sprintf('%.1f', (Freq/TotalPitches)*100),'%')) %>%
    distinct(TaggedPitchType, Usage) %>%
    filter(TaggedPitchType == P) # Pull only usage of current pitch
  
  # Filter by pitch on original DF
  SSWPITCH <- DF %>% 
    filter(TaggedPitchType == P)
  
  # Tilt handling
  DTILT <- SSWPITCH %>% select(Tilt, Pitch = TaggedPitchType) %>% filter(., !(Tilt %in% c('', '-', 'NA', ' ', 'NA:'))) %>%
    mutate(Tilt = str_replace_all(Tilt, 'h', '')) %>% mutate(Tilt = str_replace_all(Tilt, 'm', '')) %>%
    separate(Tilt, into = c('Hour', 'Min')) %>% mutate_at(vars(Hour, Min), ~as.numeric(as.character(.))) %>%
    mutate(Hour = ifelse(Hour == 0, 12, Hour)) %>% filter(., Hour != 'NA')
  
  # Tilt
  COL2 <- DTILT %>% mutate(Pitch = factor(Pitch)) %>%
    group_by(Pitch, .drop = F) %>% summarize(Max = max(Hour, na.rm = T), Min = min(Hour, na.rm = T)) %>%
    mutate(Range = Max-Min) %>% mutate(NEG = ifelse(Range > 6, 'TRUE', 'FALSE')) %>% select(Pitch, NEG) %>%
    left_join(DTILT, ., by = c('Pitch')) %>%
    mutate(CHour = case_when(NEG == 'TRUE' & Hour == 12 ~ 0, NEG == 'TRUE' & Hour == 11 ~ -1,
                             NEG == 'TRUE' & Hour == 10 ~ -2, NEG == 'TRUE' & Hour == 9 ~ -3,
                             NEG == 'TRUE' & Hour == 8 ~ -4, NEG == 'TRUE' & Hour == 7 ~ -5, T ~ Hour)) %>%
    mutate(CMin = ifelse(CHour < 0, Min-60, Min)) %>% mutate(CMins = CMin/100) %>%
    mutate(TestT = CHour+CMins) %>% group_by(Pitch, .drop = F) %>% summarize(NewTilt = round(median(TestT, na.rm = T), 2)) %>%
    mutate(Neg = ifelse(NewTilt < 0, TRUE, FALSE)) %>% mutate(CTilt = ifelse(Neg == 'TRUE', NewTilt*-1, NewTilt)) %>%
    mutate(CTilt = sprintf('%.2f', CTilt)) %>% separate(CTilt, into = c('MinHour', 'MinMinute')) %>%
    mutate_at(vars(MinHour, MinMinute), ~as.numeric(as.character(.))) %>%
    mutate_at(vars(MinHour, MinMinute), ~ifelse(. == 'NA' | is.na(.) == 'TRUE', 0, .)) %>% mutate_at(vars(MinHour, MinMinute), ~as.numeric(as.character(.))) %>%
    mutate(HourF = ifelse(Neg == 'TRUE' & MinHour == 0 | MinHour == 0, 12, ifelse(Neg == 'TRUE' & MinHour == 1, 11,
                                                                                  ifelse(Neg == 'TRUE' & MinHour == 2, 10, ifelse(Neg == 'TRUE' & MinHour == 3,9, ifelse(Neg == 'TRUE' & MinHour == 4, 8,
                                                                                                                                                                         ifelse(Neg == 'TRUE' & MinHour == 5, 7, MinHour))))))) %>% mutate_at(vars(MinMinute, MinHour), ~as.numeric(as.character(.))) %>%
    mutate(NMIN = ifelse(MinMinute > 60, MinMinute-60, MinMinute)) %>% mutate_at(vars(MinHour:NMIN), ~as.numeric(as.character(.))) %>%
    mutate(NHOUR = ifelse(MinMinute > 60 & MinHour < 12, MinHour+1,ifelse(MinMinute > 60 & MinHour > 11, 1, HourF))) %>%
    mutate(MinMinute2 = ifelse(MinMinute > 60, round((NMIN/100)*60,0), MinMinute)) %>%
    mutate(MinMinute2 = RoundTo(MinMinute2,5)) %>% mutate(MinF = ifelse(Neg == 'TRUE', 60-MinMinute2, MinMinute2)) %>%
    mutate(NHOUR = as.numeric(as.character(NHOUR))) %>% mutate(NHOUR = ifelse(NHOUR != 12 & MinF == 60, NHOUR+1, ifelse(NHOUR == 12 & MinF == 60, 1, NHOUR))) %>%
    mutate(MinF = ifelse(MinF == 60, 0, MinF)) %>% mutate_at(vars(MinHour:MinF), ~as.numeric(as.character(.))) %>%
    mutate(MinF = ifelse(MinF < 10, paste0(0, MinF), as.character(MinF))) %>%
    mutate(AVG = paste0(NHOUR, ':', MinF)) %>% select(TaggedPitchType = Pitch, Tilt = AVG) %>%
    filter(., !(TaggedPitchType %in% c('NA', '')))
  
  ### SSW Section - uses averages from SSW model
  COL3 <- SSWAVGS_PBP %>%
    mutate_at(vars(-TaggedPitchType), ~ sprintf('%.3f',.))
  
  # Order pitches by order in 'target'
  PLATETB <- Reduce(function(x,y) merge(x,y, by.all = c('TaggedPitchType')), list(COL1, COL2, COL3)) %>% 
    mutate_all(., ~ifelse(. %in% c('NA', 'NaN%', 'NaN'), '-', as.character(.)))
  colours <- PLATETB %>% mutate(ID = row_number()) %>% 
    mutate_at(vars(-ID), ~ifelse(ID%%2 == 0, '#e8f0e6', '#ffffff')) %>%
    select(-ID) %>%
    as.matrix()
  df1 <- PLATETB
  
  P1n <- dim(df1)[1]
  nums_cols <- dim(df1)[2]
  tt <- ttheme_default(core = list(bg_params=list(fill=colours, col = 'black'),fg_params = list(fontsize = 32,  col = 'black', fontface = 1)), colhead = list(bg_params=list(fill='#d5d9e0', col='black'),fg_params = list(fontsize = 28)))
  tt2 <- ttheme_default(core = list(bg_params=list(fill='gray45', col = 'black'),fg_params = list(fontsize = 32,  col = 'white', fontface = 'bold')), colhead = list(bg_params=list(fill='#a9d4a1', col='black'),fg_params = list(fontsize = 40, col = 'black')))
  tab3 <- tableGrob(df1[1:P1n, 1:nums_cols], rows = NULL, cols = c('Pitch', 'Usage', 'Tilt\n(hh:mm)', 'Magnus HB\n(in.)', 'Magnus IVB\n(in.)', 'Drag HB\n(in.)', 'Drag IVB\n(in.)', 'Seam HB\n(in.)', 'Seam IVB\n(in.)', 'Total HB\n(in.)', 'Total IVB\n(in.)'), theme = tt)
  header2 <- tableGrob(df1[1,1:1], rows=NULL, cols=c('Pitch Usage & SSW Summary'), theme = tt2) 
  jn2 <- gtable_combine(header2[1,], tab3, along=2, join = 'outer')
  jn2$widths <- rep(max(jn2$widths), length(jn2$widths)) # make column widths equal
  jn2$layout[1:2 , c("l", "r")] <- list(c(1),c(nums_cols))
  g <- jn2
  g <- gtable_add_grob(g,grobs = rectGrob(gp = gpar(fill = NA, lwd = 3)),t = 1, b = nrow(g), l = 1, r = nums_cols)
  g <- gtable_add_grob(g,grobs = rectGrob(gp = gpar(fill = NA, lwd =1.5)),t = 1, b = 2, l = 1, r = nums_cols)
  g$widths <- unit(c(rep(1/nums_cols, nums_cols)), 'npc')
  g$heights <- unit(c(.13, .13, rep(.74/(nrow(g)-2), (nrow(g)-2))), 'npc')
  SWDECGROB <- g 
} # Stat line and percentages, plus SSW metrics


### Load in Sproat's averages, comment to run in AnalyzR
#ssw_averages1 <- data.frame(read.csv('data/SSW-data/SproatAverages.csv'))
dataframe <-  data.frame(read.csv('data/SSW-data/SproatResults.csv'))

input <- fromJSON('data/SFA39.json') # Use locally

# Uncomment to run in AnalyzR
#input <- input[[1]]
#json_input <- fromJSON(input$fileName)
#out_FileName <- input$outFileName
#dataframe <- input$result$data
cataframe <- input$result$comparision_data
logo <- input$result$logo$default

# input <- input[[1]]
#dataframe <- input$result$data # Use locally
logo <- input$result$logo$default # Use locally
# MSLOGO <- readPNG('BC_Icon_ColorS_Large.png')
MSLOGO <- readPNG(getURLContent(logo))
CSTLOGO <- ggarrange(rasterGrob(MSLOGO, height = .9, width = .95), NULL,NULL, NULL,ncol = 2, nrow = 2, widths = c(.375,.625), heights = c(.7,.3))

new_names <- c('TaggedPitchType' = 'pitch_type', 'TaggedPitchType' = 'taggedpitchtype', 'RelSpeed' = 'velocity','RelSpeed' = 'relspeed','SpinRate' = 'spin_rate', 'SpinRate' = 'spinrate', 'PitchNo' = 'pitch_number', 'PitchNo' ='pitchno',
               'Date' = 'date', 'Date' = 'event_time', 'Pitcher' = 'pitcher_name', 'Pitcher' = 'pitcher', 'PitcherThrows' = 'pitcher_handedness', 'PitcherThrows' = 'pitcherthrows','PlateLocSide' = 'side_at_plate', 'PlateLocSide' = 'platelocside', 
               'PlateLocHeight' = 'height_at_plate','PlateLocHeight' = 'platelocheight', 'BatterSide' = 'batter_handedness', 'BatterSide' = 'batterside', 'Balls' = 'balls', 'Strikes' = 'strikes', 'PitchCall' = 'pitch_call', 'PitchCall' = 'pitchcall', 
               'KorBB' = 'korbb', 'PlayResult' = 'play_result', 'PlayResult' = 'playresult', 'ExitSpeed' = 'exit_velocity', 'ExitSpeed' = 'exitspeed','Angle' = 'launch_angle', 'Angle' = 'angle', 'Direction' = 'direction', 'Distance' = 'distance',
               'Bearing' = 'bearing', 'BatterTeam' = 'batter_team', 'BatterTeam' = 'batterteam', 'Batter' = 'batter_name', 'Batter' = 'batter', 'Time' = 'time','Inning' = 'inning','InducedVertBreak' = 'induced_vertical_break', 'InducedVertBreak' = 'inducedvertbreak',
               'PAofInning' = 'plate_appearance', 'PAofInning' =  'paofinning', 'PitchofPA' = 'pitch_of_plate_appearance', 'PitchofPA' = 'pitchofpa', 'PitcherTeam' = 'pitcher_team',  'PitcherTeam' = 'pitcherteam', 'AutoPitchType' = 'auto_pitch_type', 'AutoPitchType' = 'autopitchtype',
               'HorzBreak' = 'horizontal_break', 'HorzBreak' = 'horzbreak', 'Tilt' = 'tilt', 'SpinAxis' = 'spin_axis', 'SpinAxis' = 'spinaxis', 'Top.Bottom' = 'inning_half', 'Top.Bottom' = 'topbottom', 'RelHeight' = 'release_height', 'RelHeight' = 'relheight',
               'RelSide' = 'release_side', 'RelSide' = 'relside', 'Extension' = 'extension','POC_Y' = 'hit_location_y', 'POC_X' = 'hit_location_x', 'POC_Z' = 'hit_location_z', 'runs_scored' = 'RunsScored', 'runs_scored' = 'runsscored',
               'Efficiency' = 'yt_efficiency')

ad_col <-c(RelSpeed = NA, SpinRate = NA, ExitSpeed = NA, Bearing = NA, Direction = NA, Distance = NA, Angle = NA, PlateLocSide = NA, PlateLocHeight = NA,
           PitchNo = NA, PAofInning = NA, Inning = NA, PitchofPA = NA, Pitcher = 'No Name', PitcherThrows = 'NA', PlayResult = 'NA', KorBB = 'NA', PitchCall = 'NA',
           TaggedPitchType = 'NA', BatterSide = 'NA')

VHA <- dataframe %>% rename(any_of(new_names)) %>% add_column(., !!!ad_col[setdiff(names(ad_col), names(.))]) %>% 
  mutate(PlateLocSideRev = PlateLocSide*-1) %>% mutate(Count = paste0(Balls, '-', Strikes)) %>% 
  mutate(Zone = ZONECA(PlateLocSideRev,PlateLocHeight)) %>% mutate(PitcherThrows = recode(PitcherThrows, 'L' = 'Left', 'R' = 'Right'),
                                                                   BatterSide = ifelse(BatterSide == 'Switch' & PitcherThrows == 'Right', 'Left',ifelse(BatterSide == 'Switch' & PitcherThrows == 'Left', 'Right',
                                                                                                                                                        as.character(BatterSide))), BatterSide = recode(BatterSide, 'L' = 'Left', 'R' = 'Right')) %>% mutate_at(vars(Pitcher, Batter), ~str_squish(.)) %>%
  mutate(TaggedPitchType = str_to_title(TaggedPitchType)) %>% mutate(TaggedPitchType = recode(TaggedPitchType, 'Change Up' = 'Changeup', 'Riser' = 'Riseball',
                                                                                              'Change' = 'Changeup','4-Seam Fastball' = 'Fastball')) %>% mutate(PitchCall = recode(PitchCall, 'Foul' = 'FoulBall','BallinDirt' = 'BallCalled',
                                                                                                                                                                                   'FoulTip' = 'FoulBall', 'Swinging Strike' = 'StrikeSwinging', 'Strike Swinging' = 'StrikeSwinging','Called Strike' = 'StrikeCalled',
                                                                                                                                                                                   'Strike Called' = 'StrikeCalled', 'Hit By Pitch' = 'HitByPitch', 'Ball' = 'BallCalled', 'In Play' = 'InPlay')) %>% 
  mutate(TaggedPitchType = recode(TaggedPitchType, 'Four-Seam' = 'Fastball', 'ChangeUp' = 'Changeup')) %>% 
  mutate(PCAT = PITCON(TaggedPitchType)) %>%
  mutate(Zone2 = ZONECB(PlateLocSideRev,PlateLocHeight)) %>% filter(.,!(Pitcher %in% c('', ' ', 'NA'))) %>% 
  mutate(TaggedPitchType = ifelse(is.na(TaggedPitchType) == TRUE, 'Undefined', as.character(TaggedPitchType))) %>%
  mutate(TotalBreak = sqrt(total_move_x^2 + total_move_z^2)) %>% # Total = sqrt(HB^2 + IVB^2)
  mutate(SplitZone = ZONEHALF(PlateLocSideRev,PlateLocHeight)) %>%
  separate(Date, into = c('Date', 'Time'), sep = ' ')

# Pick players here
PIT <- VHA %>% distinct(Pitcher, PitcherTeam) %>% mutate(Pitcher = as.character(as.factor(Pitcher))) %>% 
  mutate(Comma = grepl(',', Pitcher)) %>% separate(Pitcher, into = c('Last' ,'First'),sep = ',', remove = FALSE) %>%
  mutate(YFirst = Last) %>% mutate(YLast = First) %>% mutate(PNM = ifelse(Comma == TRUE, paste0(First, ' ',Last ), paste0(Pitcher))) %>% 
  mutate(PDFNM = paste0(gsub(' ', '', PitcherTeam), gsub(' ', '', PNM))) %>% arrange(desc(PitcherTeam)) %>% 
  select(Pitcher, PNM, PDFNM) %>% filter(., is.na(Pitcher) == FALSE)
df <- data.frame(name = ' ', fileName = '')

i <- 1
repeat {
  
  PITCHERS <- PIT[i,]
  DF <- VHA %>% filter(., Pitcher == PITCHERS$Pitcher) %>% mutate(PAofGame = dense_rank(interaction(Inning, PAofInning, Pitcher, Batter, lex.order = T)))
  DATER <- DF %>% mutate(Date = format(as.Date(Date, tryFormats = c('%m/%d/%Y', '%m/%d/%y', '%-m/%d/%Y', '%d/%m/%Y', '%Y-%m-%d')), '%m/%d/%y')) %>% distinct(Date) %>% arrange(Date) %>% slice(1)
  
  # Grab pitch types
  PITCHES <- DF %>% 
    group_by(TaggedPitchType) %>%
    count(TaggedPitchType) %>%
    ungroup() %>% 
    mutate(TaggedPitchType =as.character(TaggedPitchType)) %>% 
    filter(., !(TaggedPitchType %in% c('NA','')) & n > 5) %>%
    select(-n) %>%
    arrange(match(TaggedPitchType, target)) %>%
    pull(TaggedPitchType)
  
  # Grab pitcher handedness - only need 'L' or 'R'
  P_THROWS <- substr(unique(DF$PitcherThrows), 1, 1)
  
  DF <- DF %>%
    filter(., TaggedPitchType %in% PITCHES)
  
  # Create dataframe with SSW averages 
  ssw_averages <- DF %>%
    group_by(TaggedPitchType) %>%
    summarize_at(vars(Magnus_move_x, Magnus_move_z, drag_move_x, drag_move_z, seam_move_x, seam_move_z, total_move_x, total_move_z), list(name = mean)) %>%
    rename(Magnus_x = Magnus_move_x_name,
           Magnus_z = Magnus_move_z_name,
           Drag_x = drag_move_x_name,
           Drag_z = drag_move_z_name,
           Seam_x = seam_move_x_name,
           Seam_z = seam_move_z_name,
           Total_x = total_move_x_name,
           Total_z = total_move_z_name)
  
  TOPGROB <- STATBULLPEN(DF,P_THROWS)
  PIEUSAG <- PIEG(DF)
  TB1 <- SSW(DF, ssw_averages)
  TB2 <- MLB_COMPS(DF, P_THROWS)
  
  # Iterate over all pitches to create each heatmap and vector plots
  j <- 1
  repeat {
    PIT1 <- PITCHES[j]
    # Total Break
    LHHM_TB <- paste0('L-TB', j)
    assign(LHHM_TB, SSWZONES(DF, 'Left',PIT1, 'TB'))
    RHHM_TB <- paste0('R-TB', j)
    assign(RHHM_TB, SSWZONES(DF, 'Right',PIT1, 'TB'))
    
    # Horz. Break
    LHHM_HB <- paste0('L-HB', j)
    assign(LHHM_HB, SSWZONES(DF, 'Left',PIT1, 'HB'))
    RHHM_HB <- paste0('R-HB', j)
    assign(RHHM_HB, SSWZONES(DF, 'Right',PIT1, 'HB'))
    
    # Induced Vert. Break
    LHHM_IVB <- paste0('L-IVB', j)
    assign(LHHM_IVB, SSWZONES(DF, 'Left',PIT1, 'IVB'))
    RHHM_IVB <- paste0('R-IVB', j)
    assign(RHHM_IVB, SSWZONES(DF, 'Right',PIT1, 'IVB'))
    
    # Vector Plots
    VP <- paste0('VP', j)
    assign(VP, VECTORPLOTS(DF,ssw_averages,PIT1))
    j <- j + 1
    if (j == length(PITCHES) + 1){
      break()
    }
  }
  
  # Total Break
  PITCHPLOTS_TB_L <- list()    
  for(k in 1:6) {
    tab_element <- paste0('L-TB', k)
    PITCHPLOTS_TB_L[length(PITCHPLOTS_TB_L) + 1] <- ifelse(exists(tab_element) == TRUE, list(get(tab_element)), list(NULL))
    PITCHPLOTS_TB_L[length(PITCHPLOTS_TB_L) + 1] <- list(NULL)}
  PITCHPLOTS_TB_L[[12]] <- NULL
  
  PITCHPLOTS_TB_R <- list()    
  for(k in 1:6) {
    tab_element <- paste0('R-TB', k)
    PITCHPLOTS_TB_R[length(PITCHPLOTS_TB_R) + 1] <- ifelse(exists(tab_element) == TRUE, list(get(tab_element)), list(NULL))
    PITCHPLOTS_TB_R[length(PITCHPLOTS_TB_R) + 1] <- list(NULL)}
  PITCHPLOTS_TB_R[[12]] <- NULL
  
  # Horz. Break
  PITCHPLOTS_HB_L <- list()    
  for(k in 1:6) {
    tab_element <- paste0('L-HB', k)
    PITCHPLOTS_HB_L[length(PITCHPLOTS_HB_L) + 1] <- ifelse(exists(tab_element) == TRUE, list(get(tab_element)), list(NULL))
    PITCHPLOTS_HB_L[length(PITCHPLOTS_HB_L) + 1] <- list(NULL)}
  PITCHPLOTS_HB_L[[12]] <- NULL
  
  PITCHPLOTS_HB_R <- list()    
  for(k in 1:6) {
    tab_element <- paste0('R-HB', k)
    PITCHPLOTS_HB_R[length(PITCHPLOTS_HB_R) + 1] <- ifelse(exists(tab_element) == TRUE, list(get(tab_element)), list(NULL))
    PITCHPLOTS_HB_R[length(PITCHPLOTS_HB_R) + 1] <- list(NULL)}
  PITCHPLOTS_HB_R[[12]] <- NULL
  
  # Induced Vert. Break
  PITCHPLOTS_IVB_L <- list()    
  for(k in 1:6) {
    tab_element <- paste0('L-IVB', k)
    PITCHPLOTS_IVB_L[length(PITCHPLOTS_IVB_L) + 1] <- ifelse(exists(tab_element) == TRUE, list(get(tab_element)), list(NULL))
    PITCHPLOTS_IVB_L[length(PITCHPLOTS_IVB_L) + 1] <- list(NULL)}
  PITCHPLOTS_IVB_L[[12]] <- NULL
  
  PITCHPLOTS_IVB_R <- list()    
  for(k in 1:6) {
    tab_element <- paste0('R-IVB', k)
    PITCHPLOTS_IVB_R[length(PITCHPLOTS_IVB_R) + 1] <- ifelse(exists(tab_element) == TRUE, list(get(tab_element)), list(NULL))
    PITCHPLOTS_IVB_R[length(PITCHPLOTS_IVB_R) + 1] <- list(NULL)}
  PITCHPLOTS_IVB_R[[12]] <- NULL
  
  # Vector Plots
  VECPLOTS_1 <- list()    
  for(k in 1:3) {
    tab_element <- paste0('VP', k)
    VECPLOTS_1[length(VECPLOTS_1) + 1] <- ifelse(exists(tab_element) == TRUE, list(get(tab_element)), list(NULL))
    VECPLOTS_1[length(VECPLOTS_1) + 1] <- list(NULL)}
  VECPLOTS_1[[6]] <- NULL
  
  VECPLOTS_2 <- list()    
  for(k in 4:6) {
    tab_element <- paste0('VP', k)
    VECPLOTS_2[length(VECPLOTS_2) + 1] <- ifelse(exists(tab_element) == TRUE, list(get(tab_element)), list(NULL))
    VECPLOTS_2[length(VECPLOTS_2) + 1] <- list(NULL)}
  VECPLOTS_2[[6]] <- NULL
  
  # Total Break
  TB_PLOT_L <- ggarrange(plotlist = PITCHPLOTS_TB_L, ncol = 11, nrow = 1, 
                         widths = c(.15,.02,.15,.02,.15,.02,.15,.02,.15,.02,.15,.02,.15,.02,.15,.02,.15), heights = c(1))
  TB_PLOT_R <- ggarrange(plotlist = PITCHPLOTS_TB_R, ncol = 11, nrow = 1, 
                         widths = c(.15,.02,.15,.02,.15,.02,.15,.02,.15,.02,.15,.02,.15,.02,.15,.02,.15), heights = c(1))
  # Horz. Break
  HB_PLOT_L <- ggarrange(plotlist = PITCHPLOTS_HB_L, ncol = 11, nrow = 1, 
                         widths = c(.15,.02,.15,.02,.15,.02,.15,.02,.15,.02,.15,.02,.15,.02,.15,.02,.15), heights = c(1))
  HB_PLOT_R <- ggarrange(plotlist = PITCHPLOTS_HB_R, ncol = 11, nrow = 1, 
                         widths = c(.15,.02,.15,.02,.15,.02,.15,.02,.15,.02,.15,.02,.15,.02,.15,.02,.15), heights = c(1))
  # Induced Vert. Break
  IVB_PLOT_L <- ggarrange(plotlist = PITCHPLOTS_IVB_L, ncol = 11, nrow = 1, 
                          widths = c(.15,.02,.15,.02,.15,.02,.15,.02,.15,.02,.15,.02,.15,.02,.15,.02,.15), heights = c(1))
  IVB_PLOT_R <- ggarrange(plotlist = PITCHPLOTS_IVB_R, ncol = 11, nrow = 1, 
                          widths = c(.15,.02,.15,.02,.15,.02,.15,.02,.15,.02,.15,.02,.15,.02,.15,.02,.15), heights = c(1))
  # Vector Plots
  VP1 <- ggarrange(plotlist = VECPLOTS_1, ncol = 5, nrow = 1, 
                   widths = c(.3267, .01,.3267,.01, .3267), heights = c(1))
  VP2 <- ggarrange(plotlist = VECPLOTS_2, ncol = 5, nrow = 1, 
                   widths = c(.3267, .01,.3267,.01, .3267), heights = c(1))
  
  DFNA <- DF %>% filter(., TaggedPitchType != 'NA')
  LEG <- get_legend(ggplot(DFNA, aes(x=RelSpeed, y = SpinRate))+geom_point(aes(fill = TaggedPitchType), pch = 21, size = 13)+
                      scale_fill_manual(values = c(BCCOLS), limits = c(unique(as.character(DFNA$TaggedPitchType))))+labs(fill = 'Pitch Type: ')+
                      guides(fill = guide_legend(nrow = 1))+theme_bw()+theme(legend.title = element_text(size = 34, face = 'bold'),
                                                                             legend.text = element_text(size = 32),legend.position = 'bottom'))
  
  # Make legend for vector plots
  DFVP <- data.frame(Forces = c('Drag', 'Magnus', 'Seam'), x = c(1,1,1), y = c(1,1,1))
  VP_LEG <- get_legend(ggplot(DFVP)+geom_segment(aes(x = x, xend = 0, y = y, yend = 1, color = Forces), arrow = arrow(length=unit(0.30,"cm")), lwd = 2)+
                         scale_color_manual(values = c('Drag' = 'black', 'Magnus' = 'red', 'Seam' = 'dodgerblue3'))+
                         theme_bw()+theme(legend.title = element_text(size = 34, face = 'bold'),
                                          legend.text = element_text(size = 32),legend.position = 'bottom', legend.key.width = unit(5, "cm")))
  
  # Make legend for break plots
  DFBREAK <- data.frame(Color = c('Below PITCHERNAME Average','Above PITCHERNAME Average'),
                        x = c(1,1), y = c(1,1), NewVar = c(0,1))
  BREAK_LEG <- get_legend(ggplot(DFBREAK)+geom_point(aes(x = x, y = y, fill = NewVar), pch = 21, color = 'black', size = 12)+
                            scale_fill_gradientn(colors = c('dodgerblue3', 'white', 'red'), na.value = NA, limits = c(0,100), oob = squish,
                                                 labels = c('Below Average', 'Average', 'Above Average'), breaks = c(0,50,100))+labs(fill = '')+theme_bw()+
                            theme(legend.position = 'bottom',  legend.text = element_text(size = 32), legend.key.width = unit(4, 'cm'),
                                  legend.box.margin = margin(r=12.5,l=0,t=35,b=0), legend.title = element_text(size = 40, face = 'bold', vjust = 1.175, hjust= 1)))
  
  
  # First page
  q <- as.numeric(as.POSIXlt(round(Sys.time(), 'mins'), 'UTC'))
  pdf(paste0(q,'_',PITCHERS$PDFNM, '.pdf'), height = 28, width = 40) # This generates the PDF using grid package
  # pdf('BC_PostGamePitcher_Sample.pdf', height = 28, width = 40)
  layout <- rbind(c(5,9,9,9,9,9,9,7),
                  c(9,9,9,9,9,9,9,9),
                  c(1,1,1,1,1,1,2,1),
                  c(9,9,9,9,9,9,9,9),
                  c(3,3,3,3,3,3,3,3),
                  c(9,9,9,9,9,9,9,9),
                  c(4,4,4,4,4,4,4,4)) #### ADD MLB COMPS HERE
  grid.arrange(TOPGROB,PIEUSAG,TB1,TB2,CSTLOGO,
               widths = c(.15,.21,.13,.01,.01,.415,.07,.005),heights = c(.08,.005,.14,.01,.375,.045,.345),
               vp = viewport(width = 0.95, height = 0.95), layout_matrix = layout, newpage = FALSE)
  grid.text(paste0(PITCHERS$PNM, ' SSW Report: Main Page'),x = unit(.105, 'npc'), y = unit(.95, 'npc'), just = 'left', gp=gpar(fontsize =75, fontface = 2, fontfamily = 'NimbusRom'))
  # grid.text(paste0('Sample - Post Game Report'),x = unit(.105, 'npc'), y = unit(.95, 'npc'), just = 'left', gp=gpar(fontsize =75, fontface = 2, fontfamily = 'NimbusRom'))
  grid.segments(x0 = unit(.105, "npc"), y0 = unit(.92, "npc"),x1 = unit(.985, "npc"), y1 = unit(.92, "npc"),default.units = "npc",gp = gpar(lwd = 1))
  grid.rect(.5,.6425, width=unit(.98,"npc"), height=unit(0.5225,"npc"),gp=gpar(lwd=1, fill=NA, col="black"))
  grid.rect(.5,.195, width=unit(.98,"npc"), height=unit(0.3625,"npc"),gp=gpar(lwd=1, fill=NA, col="black"))
  grid.text(paste0('Date: ',DATER$Date),x = unit(.985, 'npc'), y = unit(.94, 'npc'), just = 'right', gp=gpar(fontsize =40, fontface = 2, fontfamily = 'NimbusRom'))
  
  # Second page for new plots - Total Break Plots
  grid.newpage()
  layout <- rbind(c(4,9,9,9,9,9,9,7),
                  c(9,9,9,9,9,9,9,9),
                  c(1,1,1,1,1,1,1,1),
                  c(9,9,9,9,9,9,9,9),
                  c(2,2,2,2,2,2,2,2),
                  c(9,9,9,9,9,9,9,9),
                  c(3,3,3,3,3,3,3,3),
                  c(9,9,9,9,9,9,9,9))
  grid.arrange(TB_PLOT_L, TB_PLOT_R, BREAK_LEG, CSTLOGO,
               widths = c(.15,.21,.13,.01,.01,.415,.07,.005),heights = c(.08,.005,.35,.02,.35,.02, 0.15, 0.025),
               vp = viewport(width = 0.95, height = 0.95), layout_matrix = layout, newpage = FALSE)
  grid.text(paste0(PITCHERS$PNM, ' SSW Report: Total Pitch Break'),x = unit(.105, 'npc'), y = unit(.95, 'npc'), just = 'left', gp=gpar(fontsize =75, fontface = 2, fontfamily = 'NimbusRom'))
  # grid.text(paste0('Sample - Post Game Report'),x = unit(.105, 'npc'), y = unit(.95, 'npc'), just = 'left', gp=gpar(fontsize =75, fontface = 2, fontfamily = 'NimbusRom'))
  grid.segments(x0 = unit(.105, "npc"), y0 = unit(.92, "npc"),x1 = unit(.985, "npc"), y1 = unit(.92, "npc"),default.units = "npc",gp = gpar(lwd = 1))
  #grid.rect(.5,.6425, width=unit(.98,"npc"), height=unit(0.5225,"npc"),gp=gpar(lwd=1, fill=NA, col="black"))
  #grid.rect(.5,.195, width=unit(.98,"npc"), height=unit(0.3625,"npc"),gp=gpar(lwd=1, fill=NA, col="black"))
  grid.text(paste0('Date: ',DATER$Date),x = unit(.985, 'npc'), y = unit(.94, 'npc'), just = 'right', gp=gpar(fontsize =40, fontface = 2, fontfamily = 'NimbusRom'))
  grid.text(paste0('*Note: \'Average\' refers to ', PITCHERS$PNM, '\'s break averages*'),x = unit(.985, 'npc'), y = unit(.015, 'npc'), just = 'right', gp=gpar(fontsize =36, fontface = 2, fontfamily = 'NimbusRom'))
  
  # Third page for new plots - Horz Break Plots
  grid.newpage()
  layout <- rbind(c(4,9,9,9,9,9,9,7),
                  c(9,9,9,9,9,9,9,9),
                  c(1,1,1,1,1,1,1,1),
                  c(9,9,9,9,9,9,9,9),
                  c(2,2,2,2,2,2,2,2),
                  c(9,9,9,9,9,9,9,9),
                  c(3,3,3,3,3,3,3,3),
                  c(9,9,9,9,9,9,9,9))
  grid.arrange(HB_PLOT_L, HB_PLOT_R, BREAK_LEG, CSTLOGO,
               widths = c(.15,.21,.13,.01,.01,.415,.07,.005),heights = c(.08,.005,.35,.02,.35,.02, 0.15, 0.025),
               vp = viewport(width = 0.95, height = 0.95), layout_matrix = layout, newpage = FALSE)
  grid.text(paste0(PITCHERS$PNM, ' SSW Report: Horizontal Pitch Break'),x = unit(.105, 'npc'), y = unit(.95, 'npc'), just = 'left', gp=gpar(fontsize =75, fontface = 2, fontfamily = 'NimbusRom'))
  # grid.text(paste0('Sample - Post Game Report'),x = unit(.105, 'npc'), y = unit(.95, 'npc'), just = 'left', gp=gpar(fontsize =75, fontface = 2, fontfamily = 'NimbusRom'))
  grid.segments(x0 = unit(.105, "npc"), y0 = unit(.92, "npc"),x1 = unit(.985, "npc"), y1 = unit(.92, "npc"),default.units = "npc",gp = gpar(lwd = 1))
  #grid.rect(.5,.6425, width=unit(.98,"npc"), height=unit(0.5225,"npc"),gp=gpar(lwd=1, fill=NA, col="black"))
  #grid.rect(.5,.195, width=unit(.98,"npc"), height=unit(0.3625,"npc"),gp=gpar(lwd=1, fill=NA, col="black"))
  grid.text(paste0('Date: ',DATER$Date),x = unit(.985, 'npc'), y = unit(.94, 'npc'), just = 'right', gp=gpar(fontsize =40, fontface = 2, fontfamily = 'NimbusRom'))
  grid.text(paste0('*Note: \'Average\' refers to ', PITCHERS$PNM, '\'s break averages*'),x = unit(.985, 'npc'), y = unit(.015, 'npc'), just = 'right', gp=gpar(fontsize =36, fontface = 2, fontfamily = 'NimbusRom'))
  
  # Fourth page for new plots - Induced Vertical Break Plots
  grid.newpage()
  layout <- rbind(c(4,9,9,9,9,9,9,7),
                  c(9,9,9,9,9,9,9,9),
                  c(1,1,1,1,1,1,1,1),
                  c(9,9,9,9,9,9,9,9),
                  c(2,2,2,2,2,2,2,2),
                  c(9,9,9,9,9,9,9,9),
                  c(3,3,3,3,3,3,3,3),
                  c(9,9,9,9,9,9,9,9))
  grid.arrange(IVB_PLOT_L, IVB_PLOT_R, BREAK_LEG, CSTLOGO,
               widths = c(.15,.21,.13,.01,.01,.415,.07,.005),heights = c(.08,.005,.35,.02,.35,.02, 0.15, 0.025),
               vp = viewport(width = 0.95, height = 0.95), layout_matrix = layout, newpage = FALSE)
  grid.text(paste0(PITCHERS$PNM, ' SSW Report: Induced Vertical Pitch Break'),x = unit(.105, 'npc'), y = unit(.95, 'npc'), just = 'left', gp=gpar(fontsize =75, fontface = 2, fontfamily = 'NimbusRom'))
  # grid.text(paste0('Sample - Post Game Report'),x = unit(.105, 'npc'), y = unit(.95, 'npc'), just = 'left', gp=gpar(fontsize =75, fontface = 2, fontfamily = 'NimbusRom'))
  grid.segments(x0 = unit(.105, "npc"), y0 = unit(.92, "npc"),x1 = unit(.985, "npc"), y1 = unit(.92, "npc"),default.units = "npc",gp = gpar(lwd = 1))
  #grid.rect(.5,.6425, width=unit(.98,"npc"), height=unit(0.5225,"npc"),gp=gpar(lwd=1, fill=NA, col="black"))
  #grid.rect(.5,.195, width=unit(.98,"npc"), height=unit(0.3625,"npc"),gp=gpar(lwd=1, fill=NA, col="black"))
  grid.text(paste0('Date: ',DATER$Date),x = unit(.985, 'npc'), y = unit(.94, 'npc'), just = 'right', gp=gpar(fontsize =40, fontface = 2, fontfamily = 'NimbusRom'))
  grid.text(paste0('*Note: \'Average\' refers to ', PITCHERS$PNM, '\'s break averages*'),x = unit(.985, 'npc'), y = unit(.015, 'npc'), just = 'right', gp=gpar(fontsize =36, fontface = 2, fontfamily = 'NimbusRom'))
  
  # Fifth for new plots - Vector Plots
  grid.newpage()
  layout <- rbind(c(4,9,9,9,9,9,9,7),
                  c(9,9,9,9,9,9,9,9),
                  c(1,1,1,1,1,1,1,1),
                  c(9,9,9,9,9,9,9,9),
                  c(2,2,2,2,2,2,2,2),
                  c(9,9,9,9,9,9,9,9),
                  c(3,3,3,3,3,3,3,3),
                  c(9,9,9,9,9,9,9,9))
  grid.arrange(VP1, VP2, VP_LEG, CSTLOGO,
               widths = c(.15,.21,.13,.01,.01,.415,.07,.005),heights = c(.08,.0025,.43,.0025,.43,.0025,.05,.0025),
               vp = viewport(width = 0.95, height = 0.95), layout_matrix = layout, newpage = FALSE)
  grid.text(paste0(PITCHERS$PNM, ' SSW Report: Movement Due to Drag, Magnus, Seam Forces'),x = unit(.105, 'npc'), y = unit(.95, 'npc'), just = 'left', gp=gpar(fontsize =60, fontface = 2, fontfamily = 'NimbusRom'))
  # grid.text(paste0('Sample - Post Game Report'),x = unit(.105, 'npc'), y = unit(.95, 'npc'), just = 'left', gp=gpar(fontsize =75, fontface = 2, fontfamily = 'NimbusRom'))
  grid.segments(x0 = unit(.105, "npc"), y0 = unit(.92, "npc"),x1 = unit(.985, "npc"), y1 = unit(.92, "npc"),default.units = "npc",gp = gpar(lwd = 1))
  #grid.rect(.5,.6425, width=unit(.98,"npc"), height=unit(0.5225,"npc"),gp=gpar(lwd=1, fill=NA, col="black"))
  #grid.rect(.5,.195, width=unit(.98,"npc"), height=unit(0.3625,"npc"),gp=gpar(lwd=1, fill=NA, col="black"))
  grid.text(paste0('Date: ',DATER$Date),x = unit(.985, 'npc'), y = unit(.94, 'npc'), just = 'right', gp=gpar(fontsize =40, fontface = 2, fontfamily = 'NimbusRom'))
  grid.text(paste0('*Note: Plots are from pitcher\'s POV*'),x = unit(.985, 'npc'), y = unit(.015, 'npc'), just = 'right', gp=gpar(fontsize =36, fontface = 2, fontfamily = 'NimbusRom'))
  
  # Sixth for new plots - Test
  for(i in PITCHES){
    PITMET1 <- PITCHMETRICS(DF, P_THROWS, i)
    VPTEMP <- VECTORPLOTS(DF,ssw_averages,i)
    SSW1 <- SSWPBP(DF, ssw_averages, i)
    
    grid.newpage()
    layout <- rbind(c(4,9,9,9,9,9,9,7),
                    c(9,9,9,9,9,9,9,9),
                    c(1,1,1,1,1,1,1,1),
                    c(2,2,2,2,2,2,2,2),
                    c(9,9,9,9,9,9,9,9),
                    c(3,3,3,3,9,9,9,9),
                    c(9,9,9,9,9,9,9,9),
                    c(9,9,9,9,9,9,9,9))
    grid.arrange(PITMET1, SSW1, VPTEMP, CSTLOGO,
                 widths = c(.11,.11,.11,.01,.01,.575,.07,.005),heights = c(.08,.0025,.2175,.2175,.0025,.43,.05,.0025),
                 vp = viewport(width = 0.95, height = 0.95), layout_matrix = layout, newpage = FALSE)
    grid.text(paste0(PITCHERS$PNM, ' Pitch by Pitch SSW Report'),x = unit(.105, 'npc'), y = unit(.95, 'npc'), just = 'left', gp=gpar(fontsize =60, fontface = 2, fontfamily = 'NimbusRom'))
    # grid.text(paste0('Sample - Post Game Report'),x = unit(.105, 'npc'), y = unit(.95, 'npc'), just = 'left', gp=gpar(fontsize =75, fontface = 2, fontfamily = 'NimbusRom'))
    grid.segments(x0 = unit(.105, "npc"), y0 = unit(.92, "npc"),x1 = unit(.985, "npc"), y1 = unit(.92, "npc"),default.units = "npc",gp = gpar(lwd = 1))
    #grid.rect(.5,.6425, width=unit(.98,"npc"), height=unit(0.5225,"npc"),gp=gpar(lwd=1, fill=NA, col="black"))
    #grid.rect(.5,.195, width=unit(.98,"npc"), height=unit(0.3625,"npc"),gp=gpar(lwd=1, fill=NA, col="black"))
    grid.text(paste0('Date: ',DATER$Date),x = unit(.985, 'npc'), y = unit(.94, 'npc'), just = 'right', gp=gpar(fontsize =40, fontface = 2, fontfamily = 'NimbusRom'))
    grid.text(paste0('*Note: Plots are from pitcher\'s POV*'),x = unit(.005, 'npc'), y = unit(.015, 'npc'), just = 'right', gp=gpar(fontsize =36, fontface = 2, fontfamily = 'NimbusRom'))
  }
  
  dev.off()
  
  lf <- data.frame(name = PIT1$Pitcher, fileName = paste0(q,'_',PIT1$PDFNM, '.pdf'))
  df <- rbind(df, lf)
  
  i <- i + 1
  if (i == dim(PIT)[1]+1){
    break()
  }}

df <- subset(df, name != ' ')
cat(toJSON(df), file = out_FileName)