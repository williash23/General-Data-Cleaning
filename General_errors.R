#  Sara Williams
#  10/27/2015
#  General data cleaning.
################################################################################

#  Load packages
library(plyr)
library(dplyr)

################################################################################
#  Find minor errors.

#  Read in raw whale observations (all observations through summer 2015).
dat <- read.csv("C:/Users/sara.williams/Documents/GitHub/General-Data-Cleaning/Input_data/Whales_0615.csv")
###  7645 rows

#  Adjust column names
names(dat) <- c("cruise_event_ID","unique_event_ID","same_whale_ID","event_date","CPA",
								 "ob_type","ob_order_CPA","ob_order_time", "year","ship", 
								 "observer" , "observer_location", "approx","species", "count",
								 "IS_WW","WW_Speed","LAT"  ,"LONG" , "Y_PROJ",
								 "X_PROJ","NDateS","NDateD","waypoint_ID","DateD" , 
								 "TimeD","DateTime","DateTxt", "TimeTxt","track_ID",
								 "vessel_azimuth","X_GPS_UTM","Y_GPS_UTM","ship_speed","X_ship_UTM",
								 "Y_ship_UTM", "X_whale_UTM","Y_whale_UTM",
								 "ship_whale_azimuth","ship_whale_dist","ship_whale_bearing",
								 "subarea_1","subarea_2","subarea_3","ship_depth_m","ship_dist_shore_m",
								 "whale_depth_m","whale_dist_shore_m", "radio_comm", "radio_contact",
								 "radio_by", "radio_resp", "radio_time", "radio_y_n",
								 "month","season","whale_direction","whale_orientation",
								 "whale_behavior", "clouds","rain", "wind","waves","visibility", "shape_fn")


# # #   Any of following adjustments IF DESIRED:

# # #   Remove data from 2006 & 2007 (for consistent protocol)
# # #   dat2 <- filter(dat, year > 2007)

# # #   Keep points where whale distance to shore is greater than 0 (i.e., not on land).
#  # #  Keep points where depth at whale location is less than 0 (i.e., below sea surface level).
# # #   dat2 <- filter(dat, whale_dist_shore_m > 0)
###     Result is 7352 rows
# # #   dat3 <- filter(dat2, whale_depth_m < 0)
###     Result is 7069 rows


#   Look for errors where group has more than 1 CPA.
check_CPA <- dat %>%
							group_by(same_whale_ID) %>%
							summarise(sum(CPA == "Y")) %>%
							as.data.frame(.)
colnames(check_CPA) <- c("id", "NumCPA")
###  4651rows == 4651 unique same_whale_ID/"bursts" of the same whale/group	
t <- check_CPA[ which(check_CPA$NumCPA > 1),] 	
t
###  2009-05-27-K-078
###  2010-07-25-K-007 
###  2015-07-06-K-004
# # #  Can correct if desired.
# # #dat$ob_order_CPA[dat$unique_event_ID == "2010-07-25-K-009"] <- 3
# # #dat$ob_order_CPA[dat$unique_event_ID == "2010-07-25-K-008"] <- 2
# # #dat$CPA[dat$unique_event_ID == "2010-07-25-K-008"] <- "N"

################################################################################
#  Check for erroneously connected unique_event_ID and same_whale_ID.

#  Example error:
#   Last two instances of same_whale_ID: 2008-05-11-N-019, but this whale is earlier associated 
#   with same_whale_ID: 2008-05-11-N-018. Based on Count, I think that 2008-05-11-N-019 and
#   2008-05-11-N-021 are actually erroneously associated with 2008-05-11-N-018.

		###  cruise_event_ID       	unique_event_ID       		same_whale_ID		 count
		###  2008-05-06-N 				2008-05-06-N-005 	2008-05-06-N-005     1
		### 2008-05-06-N 				2008-05-06-N-006	2008-05-06-N-005     1
		###  2008-05-11-N 				2008-05-11-N-018 	2008-05-11-N-018     4
		###  2008-05-11-N 				2008-05-11-N-019	2008-05-11-N-018     2
		###  2008-05-11-N 				2008-05-11-N-021 	2008-05-11-N-018     2
		###  2008-05-11-N 				2008-05-11-N-020 	2008-05-11-N-019     2
		###  2008-05-11-N 				2008-05-11-N-023 	2008-05-11-N-019     2

evb <- as.character(dat$unique_event_ID)
swb <-as.character(dat$same_whale_ID)

#  Determine where errors might be...
#   If the ID  in column unique_event_ID occurs under column same_whale_ID in THE SAME row, it's okay.
#   If the ID  in column unique_event_ID occurs under column same_whale_ID in A DIFFERENT row, 
#   it's an error.		 

#  Find matches of evb in swb
find <- evb %in% swb
#  Decide if any of the matches occur on the same row
find_t <- which(find)
errors <- sapply(find_t, function(i){
				same_tmp <- evb[i] == swb
				#  If occurs in the same row...else...
				if(same_tmp[i]){
					out <- NULL
				}else{
					out <-evb[i]
				}
			return(out)
			}, simplify = T)

as.data.frame(unlist(errors))
# # #NULL

dat <- dat %>%
			 arrange(same_whale_ID, ob_order_time) %>%
			 as.data.frame(.)
write.csv(dat, "Whales_0615_general_clean.csv")

### Now, errors list is empty (NULL).