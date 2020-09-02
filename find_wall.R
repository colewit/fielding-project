
############################################################################
############################################################################
###                                                                      ###
###                              FIND_WALL                               ###
###    CALCULATES SIGNED DISTANCE FROM NEAREST POINT ON OUTFIELD WALL    ###
###                                                                      ###
############################################################################
############################################################################

#################################################################
##                    Author: Cole Witaszek                    ##
##                      Fielding Project                       ##
#################################################################


# Get dimensions of all MLB stadiums from geomMLBStadiums library
stadium_data <- MLBStadiumsPathData

# we only want the outfield dimensions and the foul lines to differentiate fair and foul
dimensions <- stadium_data %>% filter(segment %in% c('foul_lines','outfield_outer')) %>%
  arrange('team') %>% 
  mlbam_xy_transformation(x = 'x', y = 'y', column_suffix = "")

# create lookup table of team keys so we can map team name to the 3 letter abreviation

team <- c("angels", "astros", "athletics", "blue_jays", "braves","brewers", "cardinals", "cubs", "diamondbacks", "dodgers",     
 "giants", "indians", "mariners", "marlins", "mets", "nationals", "orioles", "padres", "phillies", "pirates", "rangers",
 "rays", "red_sox", "reds", "rockies", "royals", "tigers", "twins", "white_sox", "yankees")

abb <- c("LAA", "HOU", "OAK", "TOR", "ATL", "MIL", "STL", "CHC", "ARI", "LAD",
         "SF", "CLE", "SEA", "MIA", "NYM", "WSH", "BAL", "SD", "PHI", "PIT",
         "TEX", "TB", "BOS", "CIN", "COL", "KC", "DET", "MIN", "CWS", "NYY" )

team_key <- data.frame(Abb = abb, Team = team, Team_Key = 1:30)

# split by team
dimensions %<>% inner_join(team_key, by = c('team' = 'Team')) %>% group_by(team) %>% group_split()

# add full home team name to fielded df
fielded %<>% inner_join(team_key, by = c('home_team' = 'Abb')) 


#
# Splining a polygon.
#
#   The rows of 'xy' give coordinates of the boundary vertices, in order.
#   'vertices' is the number of spline vertices to create.
#              (Not all are used: some are clipped from the ends.)
#   'k' is the number of points to wrap around the ends to obtain
#       a smooth periodic spline.
#
#   Returns an array of points. 
# 
#
# Code taken from https://gis.stackexchange.com/questions/24827/smoothing-polygons-in-contour-map/24929#24929

spline.poly <- function(xy, vertices, k=3, ...) {
  # Assert: xy is an n by 2 matrix with n >= k.
  
  # Wrap k vertices around each end.
  n <- dim(xy)[1]
  if (k >= 1) {
    data <- rbind(xy[(n-k+1):n,], xy, xy[1:k, ])
  } else {
    data <- xy
  }
  
  # Spline the x and y coordinates.
  data.spline <- spline(1:(n+2*k), data[,1], n=vertices)
  x <- data.spline$x
  x1 <- data.spline$y
  x2 <- spline(1:(n+2*k), data[,2], n=vertices, ...)$y
  
  # Retain only the middle part.
  cbind(x1, x2)[k < x & x <= n+k, ]
}


# fit a spline through each set of outfield dimensions. The reason is so that we will now have more coordinates even though
# this fit will be tight enough that they are describing essentially the same object. We later try to find the closest
# point on the wall wo where the ball hit so having more points means we can be more precise

s <- lapply(1:length(dimensions), function(i) {spline.poly(xy = dimensions[[i]] %>% filter(segment == 'outfield_outer') %>% 
                                                             select(x,y) %>% as.matrix(), vertices = 1000)})


# return only the coordinates from the spline fits between the two foul lines, also calculate the angle to homeplate from
# each point and the distance from home. Make column for stadium id and return as dataframe
get_dimension_subsets <- function() {
  
  fitted_dimensions <- list()
  for (i in 1:30) {
    dt <- dimensions[[i]]
    foul_lines <- dt %>% filter(segment == 'foul_lines') %>% select(x,y)

    index_x_max = which.max(foul_lines$x)
    index_x_min = which.min(foul_lines$x)
    
    x_min = foul_lines$x[index_x_min]
    x_max = foul_lines$x[index_x_max]
  
    y_min = min(foul_lines$y[index_x_min], foul_lines$y[index_x_max])
    
    fitted_dimensions[[i]] <- s[[i]] %>% as.data.frame() %>% 
      select(x = x1, y = x2) %>% 
      filter(between(x, x_min, x_max), y > y_min) %>% 
      mutate(
        stadium_id = i,
        angle_to_home = pi/2 - atan((x)/(y)),
        dist = sqrt(x^2 + y^2)
      )
      
  }

  return(bind_rows(fitted_dimensions))
}

all_stadium_dimensions <- get_dimension_subsets()


#######################################################################################################
##                                  FUNCTION: calculate_dist_to_wall                                 ##
##            PARAMETERS: first responder position, stadium, hit distance and spray angle            ##
##  RETURN: vector of signed distances to wall if the ball was unimpeded (negative means past wall)  ##
##              PURPOSE: determine distance to wall to better understand play difficulty             ##
#######################################################################################################

calculate_dist_to_wall <- function(responder, stadium_key, hit_distance, spray_angle) {

  # not relevant for infielders
  if (responder < 7) {
    return(NA)
  }

  # get appropriate stadium
  subset = all_stadium_dimensions %>% filter(stadium_key == stadium_id)
  
  if (nrow(subset) == 0) {
    return(NA)
  }
  
  # find closest angle to home of any point from subset to the given spray angle and return its index.
  # It is written so that you can pass a vector of spray angles and hit distances
  index = spray_angle %>% as.data.frame() %>% setNames('angle') %>% 
    rowwise() %>% 
    mutate(index = which.min(abs(subset$angle_to_home - angle))) %>% 
    extract2('index')
  
  # signed distance from closest point on wall
  return(subset$dist[index] - hit_distance)
  
}

# get projected signed dist from wall for each batted ball (if it had been unimpeded) and record whether or not it was off wall
# would have been if not caught (not perfectly accurate because we didn't account for wind).
fielded %<>% 
  group_by(Team_Key, first_responder) %>% 
  mutate(dist_to_wall = calculate_dist_to_wall(first_responder %>% unique(), Team_Key %>% unique(), hit_distance_sc, spray_angle),
         off_wall = as.numeric(dist_to_wall < 0))


wall_ball = fielded %>% filter(off_wall == 1) 

# plot as sanity check
wall_ball %>%  mutate(team = Team) %>% 
  ggplot( aes(x = actual_hc_x, y = actual_hc_y, color = as.factor(caught))) +     
  geom_spraychart( stadium_ids =  "all_mlb",
                   stadium_transform_coords = TRUE,
                   stadium_segments = 'all') +
  theme_void() + 
  coord_fixed() + 
  facet_wrap(~team) + 
  theme(legend.position = "bottom")





