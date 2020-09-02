




###########################################################################
###########################################################################
###                                                                     ###
###                         CLEAN FIELDING DATA                         ###
###        TRANSFORM XY COORDINATES AND PERFORM FEATURE ENCODING        ###
###                                                                     ###
###########################################################################
###########################################################################

#################################################################
##                    Author: Cole Witaszek                    ##
##                      Fielding Project                       ##
#################################################################




#################################################################################################
##                                    FUNCTION: find_fielder                                   ##
##  PARAMETERS: position of first fielder to touch batted ball, id of player at each position  ##
##                      RETURN: id of first fielder to touch batted ball                       ##
##                  Purpose: determine relevant fielder for each batted ball                   ##
#################################################################################################

# note that this function uses matrix notation because it is meant to be used with mutate
# and so a vector of player ids is passed in for each position.

find_fielder <- function(first_responder, f3, f4, f5, f6, f7, f8, f9) {
  
  vec <- matrix(c(f3,f4,f5,f6,f7,f8,f9), ncol = 7)
  
  return(vec[,first_responder - 2])
}




####################################################################################################
##                                    FUNCTION: ClosestMatch2                                     ##
##                        PARAMETERS: string and vector of strings to query                       ##
##                                RETURN: closesr matching string                                 ##
##  PURPOSE: return closest matching names for players with naming inconsistensies across tables  ##
####################################################################################################

# code taken from https://stackoverflow.com/questions/5721883/agrep-only-return-best-matches

ClosestMatch2 = function(string, stringVector) {
  
  stringVector[amatch(string, stringVector, maxDist=Inf)]
  
}


# ID Map merged with a Lahman biographical table. Used to lookup throwing hand for each fielder
handedness_ref <- fread('ID_map.csv', sep = ",") %>% as.data.frame() %>%  
  select(Name = PLAYERNAME, playerid = IDPLAYER, mlbamid = MLBID) %>% 
  inner_join(Master %>% select(playerid = playerID, throws), by = 'playerid') %>%
  select(-playerid)


# average initial fielding position for each player at each position across different scenarios as laid out on baseball savant
# https://baseballsavant.mlb.com/visuals/fielder-positioning. CSV contains this data from 2016-2020

fielder_location <- fread('fielder_location_updated.csv', sep = ',') %>% 
  as.data.frame() %>% 
  mutate(runners = ifelse(runners > 1, 2, runners)) %>% distinct() %>% 
  group_by(shift, handedness, mlbamid, runners, Pos, Name, Season) %>% 
  summarise(
    Depth = mean(Depth),
    Angle = mean(Angle),
    .groups = 'drop'
  )


# statcast data on every pitch from 2015-2019
# follow link for csv documentation: https://baseballsavant.mlb.com/csv-docs

all_pitches <- fread('all_pitches.csv', sep = ',') %>% as.data.frame()


g = 21.9219*1.467 # gravity in fps


# select relevant columns and filter out batted balls hit behind home plate
# first responder is the fielder who first touched the batted ball

fielded <- all_pitches %>%
  select(season = game_year, first_responder = hit_location, contains('fielder'), launch_speed,
         launch_angle, hc_x, hc_y, hit_distance_sc, contains('alignment'),
         description, events, batter_handedness = stand, plate_z, home_team, on_1b, on_2b, on_3b) %>% 
  mutate(first_responder = as.numeric(first_responder)) %>% 
  filter(!is.na(first_responder), first_responder > 2, hc_y < 199.01)


# create columns putout and caught (differing in that putout includes infield groundouts)
# create column first_responder_id that lists the fielder who first touched the batted ball

fielded %<>% mutate(putout = ifelse(grepl("out|play|sac", events, ignore.case = TRUE), 1, 0)) %>% 
  mutate(caught = ifelse(grepl("field_out|sac_fly", events, ignore.case = TRUE), 1, 0)) %>% 
  group_by(first_responder) %>% 
  mutate(first_responder_id = find_fielder(first_responder %>% unique(),
                                           fielder_3, fielder_4, fielder_5, fielder_6, fielder_7, fielder_8, fielder_9)) %>% 
  as.data.frame()


# determine whether or not the first responder was shifted and where there were runners

fielded %<>% filter(!is.na(if_fielding_alignment), !is.na(of_fielding_alignment)) %>% 
  mutate(shift = ifelse(first_responder < 7, 
                                   ifelse(if_fielding_alignment == 'Standard', 0, 1),
                                   ifelse(of_fielding_alignment == 'Standard', 0, 1))) %>% 
  mutate(runners = ifelse(is.na(on_1b) & is.na(on_2b) & is.na(on_3b), 0,
                          ifelse(!is.na(on_1b) & is.na(on_2b) & is.na(on_3b), 1, 2
                          )))

# drop cols
fielded %<>% select(-on_1b, -on_2b, -on_3b, -if_fielding_alignment, -of_fielding_alignment) %>% na.omit()


# get throwing hand for fielders
fielded %<>% inner_join(handedness_ref, by = c('first_responder_id' = 'mlbamid')) %>% 
  rename(first_responder_throws = throws) %>% 
  mutate(first_responder_throws = as.numeric(first_responder_throws) - 1)


# get approximate starting location for first responder on each play
fielded %<>% left_join(fielder_location %>% 
                         select(mlbamid, Pos, Angle, Depth, Season, batter_handedness = handedness, shift, runners),
                       by = c('season' = 'Season','first_responder' =  'Pos', 'first_responder_id' = 'mlbamid',
                              'shift', 'runners', 'batter_handedness')) %>% distinct() 


# if there is no info on the given player's average positioning in the given situation (position, shift, runners, season)
# then take the average of that players positioning in the same situation across different seasons

fielded %<>% 
  group_by(first_responder, first_responder_id, batter_handedness, shift, runners) %>% 
  mutate(Angle = ifelse(is.na(Angle), mean(Angle, na.rm = T), Angle),
         Depth = ifelse(is.na(Depth), mean(Depth, na.rm = T), Depth)) %>% 
  ungroup() 

fielded$Depth[is.nan(fielded$Depth)] = NA
fielded$Angle[is.nan(fielded$Angle)] = NA


# if there is still no info about that players positioning 
# (i.e. he has not recorded significant time playing that position in the given situation in any season)
# then take the average of other fielders at the same position in the same situation
#
# Note: not grouped by season bc Savant lacks data on 2015 positioning as a whole so those would still be NA
fielded %<>% group_by(first_responder,  batter_handedness, shift, runners) %>%
  mutate(Angle = ifelse(is.na(Angle) , round(mean(Angle, na.rm = T), 1), round(Angle, 1)),
         Depth = ifelse(is.na(Depth), round(mean(Depth, na.rm = T), 1), round(Depth,1))) %>% ungroup()


# angle goes from -pi/4 rad in LF to pi/4 rad in RF but we want angle to go from pi/4 to 3pi/4 for trig to work
# so we subtract angle from pi/2
fielded %<>% mutate(pos_x = round(Depth*cos(pi/2 - Angle*pi/180 ), 0),
                    pos_y = round(Depth*sin(pi/2 - Angle*pi/180 ), 0))


# convert launch speed to ft/s and launch angle to rad and then calulate hang time in sec
fielded %<>% 
  mutate(launch_speed_fps = 1.467*launch_speed, launch_angle_rad = launch_angle*pi/180) %>% 
  mutate(hang_time = 
           (-launch_speed_fps*sin(launch_angle_rad) - sqrt((launch_speed_fps*sin(launch_angle_rad))^2 + 2*g*plate_z))/(-g)) %>%
  mutate(hang_time = round(pmax(hang_time, 0), 2) + .001) 


# transform coordinates so that origin of batted balls is (0,0) and measurements are in feet from orgigin
fielded <- mlbam_xy_transformation(fielded, column_suffix = '') 


# we again take the original angle found (given by atan) and subtract it from pi/2 to scale between pi/4 and 3pi/4 rad
# hc_x and hc_y are where the ball was fielded or hit the wall. We want to know where the ball would have landed if unimpeded
# hit_distance_sc does give us the unimpeded projected distance so we then just multiply by spray angle.
# Finally, get distance from fielder to ball.

# NOTE: you may notice a potential source of error here: we derive the spray angle using the coordinates of the ball when
# impeded by a wall or a fielder and use this to project unimpeded difference. If the true spray angle was different from
# the spray angle calculated, then that could cause issues.
#
# Not to worry! Assuming no wind,
# The ball travels in a straight line in the xy plane. To impede the ball, the fielder or wall would have to be located 
# somewhere along that line and every point on that line makes the same angle with home plate. Spray angle isn't super
# sensitive to moderate wind conditions so this estimate is accurate most of the time.

fielded %<>% mutate(spray_angle = pi/2 - atan((hc_x)/(hc_y))) %>% 
  mutate(actual_hc_x = round(hit_distance_sc*cos(spray_angle), 0)) %>% 
  mutate(actual_hc_y = round(hit_distance_sc*sin(spray_angle), 0)) %>% 
  mutate(dist_x = pos_x - actual_hc_x,
         dist_y = pos_y - actual_hc_y) %>% 
  mutate(dist_to_ball = round(sqrt((dist_x)^2 + (dist_y)^2), 0 ))


# calculate sprint speed in fps for outfielders only. Sprint speed for infielders doesn't make much sense to calculate
fielded %<>% 
  mutate(sprint_speed = ifelse(caught == 1, ifelse(hang_time > 1, dist_to_ball/(1.467*hang_time), NA), NA)) %>% 
  mutate(sprint_speed = ifelse(first_responder > 6, sprint_speed, NA)) 


# if it's greater than the top MLB avg sprint speed of 21, it's probably inaccurate. This sprint speed is already going
# to be relatively inaccurate because there is estimations both on positioning and hit location. I just calculate it
# so I can filter out obviously erroneous data

fielded %<>% filter(sprint_speed < 21 | is.na(sprint_speed)) %>% 
  mutate(dist_covered = ifelse(caught == 1, dist_to_ball, NA)) %>% 
  mutate(dist_covered = ifelse(is.na(sprint_speed), NA, dist_covered))


# angle fielder takes to the ball, zone just maps these angles into groups
fielded %<>% 
  mutate(angle_to_ball = (180/pi)*atan2(dist_y/dist_to_ball, dist_x/dist_to_ball)) %>% 
  mutate(zone = ifelse(between(angle_to_ball, -45, 45), 0,
                       ifelse(between(angle_to_ball, 45, 135), 1,
                              ifelse(between(angle_to_ball, -135, 135), 2, 3)))) %>% 
  mutate(zone = as.factor(zone))


# record whether or not the ball was hit to the fielder's gloveside and whethere or not it was hit infront of him

fielded %<>% mutate(batter_handedness= as.numeric(as.factor(batter_handedness)) - 1) %>% 
  mutate(hit_to_gloveside = ifelse(dist_x < 0 & first_responder_throws == 1, 1, 
                                   ifelse(dist_x > 0 & first_responder_throws == 0, 1, 0))) %>% 
  mutate(hit_infront = as.numeric(dist_y > 0))






