
## This document supplements statcast lectures. The reason for this document 
## to exist is that the full statcast dataset is too large to compile when 
## knitting a markdown document.

# load in statcast data (2017-2021) and important libraries, correcting paths 
# as needed
library(tidyverse)
library(Lahman)
sc <- read_csv("~/Desktop/STAT430/statcast.csv")
head(sc)
tail(sc)

## we now add player names to the statcast dataset. This can be done in a number of ways, 
## but I am going to borrow the name/id conversions from the seam package following the 
## steps below
# 1. clone the seam package (https://github.com/ecklab/seam/) into your STAT430 directory
# 2. run the code below, correcting paths as needed
b_lu = as.data.frame(readRDS("~/Desktop/STAT430/seam/data/b-lu.Rds")) %>% dplyr::select(-team) 
p_lu = as.data.frame(readRDS("~/Desktop/STAT430/seam/data/p-lu.Rds")) 
sc <- sc %>% inner_join(b_lu) %>% 
	inner_join(p_lu)



# get balls in play and isolate relevant variables 
sc_bip <- sc %>% filter(type == "X") %>% 
	dplyr::select(game_date, events, batter_name, stand, p_throws, pitcher_name, pitch_type, 
				 launch_speed, launch_angle, hc_x, hc_y, release_speed, release_spin_rate, 
				 #spin_dir,
				 pfx_x, pfx_z, plate_x, plate_z, if_fielding_alignment, 
				 estimated_ba_using_speedangle, estimated_woba_using_speedangle, 
				 of_fielding_alignment, batter, pitcher)
dim(sc_bip)
head(sc_bip)

# store data se, correcting paths
write_csv(sc_bip, file = "~/Desktop/STAT430/sc_bip_small.csv")








