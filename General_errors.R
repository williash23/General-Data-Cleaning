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
dat <- read.csv("C:/Users/sara.williams/Documents/GitHub/General-Data-Cleaning/Input_data/Whales_0615_updated.csv")
###  7642 rows

# # #  Remove data from 2006 & 2007 (for consistent protocol).
# # dat2 <- filter(dat, year > 2007)
# # ###  6957 rows

# # #   Remove points where whale distance to shore is less than 0 (i.e., on land).
# # dat3 <- filter(dat2, whale_dist_shore_m > 0)
# # # weird_shore <- filter(data2, whale_dist_shore_m <= 0)
# # ###  6713 rows

# # #   Fix row where "count" is errorneously entered as 0. Changed this value to 1. 
# # dat3$count[dat3$count == 0] <- 1
# # ###  6713 rows

#   Look for errors where group has more than 1 CPA.
check_CPA <- dat %>%
							group_by(same_whale_ID) %>%
							summarise(sum(CPA == "Y")) %>%
							as.data.frame(.)
colnames(check_CPA) <- c("id", "NumCPA")

###  4673 rows == 4673 unique same_whale_ID/"bursts" of the same whale/group	

t <- check_CPA[ which(check_CPA$NumCPA > 1),] 	
t

		###  This shows errors at: 
		###  2010-07-25-K-007 
		###  2015-07-06-K-004
		###  2015-07-27-K-029
		###  2015-06-28-K-052
		
		# error_CPA <- data %>%
								   # filter(same_whale_ID == "2010-07-25-K-007" | same_whale_ID == "2015-07-06-K-004" |same_whale_ID == "2015-07-27-K-029" |same_whale_ID == "2015-06-28-K-052") %>%
								   # as.data.frame(.)
								   
		###  cruise_event_ID  	unique_event_ID    same_whale_ID 		CPA 	X_whale_UTM 	Y_whale_UTM 	ob_type 		ob_order_CPA 		ob_order_time		observer 
		###  2010-07-25-K 			2010-07-25-K-007 	2010-07-25-K-007  	Y   		471228.4     			6454306  				MultiOb            1    			        	1 				   			    Karin             	
		###  2010-07-25-K 			2010-07-25-K-008	 	2010-07-25-K-007     Y    		471033.6     			6454407  				MultiOb            1      				      	2				       				Karin             	
		###  2010-07-25-K 			2010-07-25-K-009 	2010-07-25-K-007     N    	471044.0    			6453885  				MultiOb            2            				3 			      				Karin        	

		###  2015-07-27-K 			2015-07-27-K-029 	2015-07-27-K-029     N    	438059.3  			6484626  				MultiOb            3             				1									Katja        	
		###  2015-07-27-K 			2015-07-27-K-033 	2015-07-27-K-029     Y    		438204.4   			6484393   			MultiOb            1             				3 								Katja         	
		###  2015-07-27-K 			2015-07-27-K-310 	2015-07-27-K-029     Y    		438104.3     			6484589  				MultiOb            2             				2 							    Katja       

		###  2015-07-06-K 			2015-07-06-K-100 	2015-07-06-K-004     Y    		439355.0     			6494502  				MultiOb            1             				2 							    Katja             	
		###  2015-07-06-K 			2015-07-06-K-004 	2015-07-06-K-004     N   		439465.0     			6494591  				MultiOb            2             				1 							    Katja              	
		###  2015-07-06-K 			2015-07-06-K-005 	2015-07-06-K-004     Y     	439397.4     			6494503  				MultiOb            1             				2 							    Katja              	

		### Will correct based on distance to ship and observation time.
 
dat3$ob_order_CPA[dat3$unique_event_ID == "2010-07-25-K-009"] <- 3
dat3$ob_order_CPA[dat3$unique_event_ID == "2010-07-25-K-008"] <- 2
dat3$CPA[dat3$unique_event_ID == "2010-07-25-K-008"] <- "N"

dat3$CPA[dat3$unique_event_ID == "2015-07-27-K-310"] <- "N"

dat3$ob_order_CPA[dat3$unique_event_ID == "2015-07-06-K-004"] <- 3
dat3$ob_order_CPA[dat3$unique_event_ID == "2015-07-06-K-100"] <- 2
dat3$CPA[dat3$unique_event_ID == "2015-07-06-K-100"] <- "N"
dat3$ob_order_time[dat3$unique_event_ID == "2015-07-06-K-005"] <- 3
###  6713 rows

dat3 <- dat3 %>%
				 arrange(same_whale_ID, ob_order_time) %>%
				 as.data.frame(.)
write.csv(dat3, "Whales_0615_general_clean.csv")
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

# # # 2007-08-08-N-008
# # # 2007-08-15-K-039
# # # 2007-08-29-K-033
# # # 2008-05-11-N-019
# # # 2008-05-11-N-021
# # # 2008-07-13-K-101
# # # 2008-07-19-N-022
# # # 2009-05-27-K-079
# # # 2009-07-01-K-091
# # # 2009-09-28-K-081
# # # 2010-06-13-N-011
# # # 2010-06-27-N-049
# # # 2010-09-20-N-014
# # # 2011-07-08-N-013
# # # 2011-07-10-N-004
# # # 2011-07-16-N-007
# # # 2011-08-24-N-031
# # # 2012-07-22-N-013
# # # 2013-06-11-N-026
# # # 2013-06-29-N-007
# # # 2013-07-15-N-041
# # # 2013-08-15-N-007
# # # 2013-09-16-N-019
# # # 2013-09-16-N-027
# # # 2015-06-26-K-021
# # # 2015-06-28-K-005
# # # 2015-06-28-K-051
# # # 2015-07-22-K-043
# # # 2015-07-28-K-034
# # # 2015-07-28-K-052


#  Using this error list, adjust to the data set to correct these same_whale_ID's so that they match the correct ID. 
#   This must be done manually in the excel data sheet because
dat4 <- dat3 %>%
				 filter(same_whale_ID != "2008-05-11-N-019") %>%
				 filter(same_whale_ID != "2008-05-11-N-021") %>%
				 filter(same_whale_ID != "2008-07-13-K-101") %>%
				 filter(same_whale_ID != "2008-07-19-N-022") %>%
				 filter(same_whale_ID != "2009-05-27-K-079") %>%
				 filter(same_whale_ID != "2009-07-01-K-091") %>%
				 filter(same_whale_ID != "2009-09-28-K-081") %>%
				 filter(same_whale_ID != "2010-06-13-N-011") %>%
				 filter(same_whale_ID != "2010-06-27-N-049") %>%
				 filter(same_whale_ID != "2010-09-20-N-014") %>%
				 filter(same_whale_ID != "2011-07-08-N-013") %>%
				 filter(same_whale_ID != "2011-07-10-N-004") %>%
				 filter(same_whale_ID != "2011-07-16-N-007") %>%
				 filter(same_whale_ID != "2011-08-24-N-031") %>%
				 filter(same_whale_ID != "2012-07-22-N-013") %>%
				 filter(same_whale_ID != "2013-06-11-N-026") %>%
				 filter(same_whale_ID != "2013-06-29-N-007") %>%
				 filter(same_whale_ID != "2013-07-15-N-041") %>%
				 filter(same_whale_ID != "2013-08-15-N-007") %>%
				 filter(same_whale_ID != "2013-09-16-N-019") %>%
				 filter(same_whale_ID != "2013-09-16-N-027") %>%
				 filter(same_whale_ID != "2015-06-26-K-021") %>%
				 filter(same_whale_ID != "2015-07-28-K-034") %>%
				 filter(same_whale_ID != "2015-07-22-K-043") %>%
				 as.data.frame(.)
###  6676 rows
				 
#  Check for errors with cleaned data set.
evb <- as.character(dat4$unique_event_ID)
swb <-as.character(dat4$same_whale_ID)

#  Determine where errors might be...
#   If the ID  in column EvB_Wpt_Id occurs under column SwB_Wpt_ID in THE SAME row, it's okay.
#   If the ID  in column EvB_Wpt_Id occurs under column SwB_Wpt_ID in A DIFFERENT row, 
#   it's likely an error.		 

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

unlist(errors)
### Now, errors list is empty (NULL).