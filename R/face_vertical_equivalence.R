#' Mapping Between Regular SSM Vertical Faces and Atlantis Polygons
#'
#' Creates the correspondence between regular grid SSM cells and Atlantis polygons for vertical faces
#' (i.e., the borders between polygons). Two CSV files are written in `R/code`:
#' `uxy2_uv_code.csv` and `face_composition_uv.csv`.
#' If these files already exist, you can skip to Step 3.
#'
#' @param SSM_file_path Path to a SSM file. It is used to load the `siglev` and `siglay` information.
#' @param SSM_file_name SSM file name (Hydrologic file).
#' @param RegularSSM_file_path Path to a SSM file with regular grid after interpolation.
#' @param RegularSSM_file_name Regular grid SSM file containing UV variables.
#'
#' @return No value is returned. Two CSV files are written in `R/code`:
#' `uxy2_uv_code.csv` and `face_composition_uv.csv`.
#'
#' @examples
#' SSM_file_path        <- "/nfsdata/SSM_example/HYD"
#' SSM_file_name        <- "ssm_00117.nc"
#' RegularSSM_file_path <- "here(File_regular_grid/bau/2011)"
#' RegularSSM_file_name <- "regular_grid_UVW_2011.nc"
#' face_vertical_equivalence(SSM_file_path, SSM_file_name,
#'                           RegularSSM_file_path, RegularSSM_file_name)
#'
#' @export

face_vertical_equivalence <- function(SSM_file_path,SSM_file_name,
                                      RegularSSM_file_path,RegularSSM_file_name) {
  select <- dplyr::select
  map <- purrr::map

###########################################################################
# Read data
# read first ROMS data for depth and grid info - assumes this does not change between time steps and ROMS files
roms <- tidync(paste0(RegularSSM_file_path,"/", RegularSSM_file_name))


# read Atlantis BGM
atlantis_bgm <- read_bgm(here("R/PugetSound_89b_070116.bgm"))
# Read SSM data for position of the siglev and the siglay --> they were interpolated in Javier code and then
# it is not the exact value anymore
ssm <- tidync(paste0(SSM_file_path,"/", SSM_file_name))

###########################################################################
# Retrieve data

#Atlantis geometry as an sf shapefile
atlantis_sf <- atlantis_bgm %>% box_sf()

# get list of ssm variables
ssm_vars <- tidync::hyper_grids(ssm) %>% # all available grids in the ssm ncdf
  pluck("grid") %>% # for each grid, pull out all the variables associated with that grid and make a reference table
  purrr::map_df(function(x){
    ssm %>% tidync::activate(x) %>% tidync::hyper_vars() %>%
      dplyr::mutate(grd=x)
  })

# # extract horizontal exact values

# Siglev = bottom of the cell vertically
siglev_dim <- ssm_vars %>% dplyr::filter(name==c("siglev")) %>% pluck('grd')
siglev_coord <- unname(unlist(ssm %>% tidync::activate(siglev_dim) %>% tidync::hyper_tibble() %>%
                                dplyr::select(siglev)))



###########################################################################

# get list of ROMS variables
roms_vars <- tidync::hyper_grids(roms) %>% # all available grids in the ROMS ncdf
  pluck("grid") %>% # for each grid, pull out all the variables associated with that grid and make a reference table
  purrr::map_df(function(x){
    roms %>% tidync::activate(x) %>% tidync::hyper_vars() %>%
      dplyr::mutate(grd=x)
  })

# # extract time
time_grd <- roms_vars %>% filter(name=="time_vector") %>% pluck('grd')
roms_time <- roms %>% activate(time_grd) %>% hyper_tibble() %>% pull()

# ROMS grids
# Rho grid
# Horizontal coordinates: on of rho points

lat_dim <- roms_vars %>% dplyr::filter(name==c("latitude")) %>% pluck('grd')
lat_coord <- unname(unlist(roms %>% tidync::activate(lat_dim) %>% tidync::hyper_tibble() %>%
                             dplyr::select(latitude)))

#
lon_dim <- roms_vars %>% dplyr::filter(name==c("longitude")) %>% pluck('grd')
lon_coord <- unname(unlist(roms %>% tidync::activate(lon_dim) %>% tidync::hyper_tibble() %>%
                             dplyr::select(longitude)))

#
siglay_dim <- roms_vars %>% dplyr::filter(name==c("siglay")) %>% pluck('grd')
siglay_coord <- unname(unlist(roms %>% tidync::activate(siglay_dim) %>% tidync::hyper_tibble() %>%
                                dplyr::select(siglay)))

#
grid <- expand.grid(longitude = lon_coord, latitude = lat_coord)
roms_rho <- as_tibble(grid)
roms_rho <- roms_rho %>% dplyr::mutate(rhoidx=dplyr::row_number())

h_coord <- expand.grid(longitude = lat_coord, latitude = lon_coord, sigma_layer = siglay_coord)
h_coord <- as_tibble(h_coord)%>%  dplyr::rename()

# we create a variable with actually siglay in it (coordinate between cells)
# we use the coordinate of the bottom of the cell
h_coord$siglev <- h_coord$sigma_layer
for (i in 1:length(unique(h_coord$siglev))){
  h_coord$siglev[h_coord$siglev==sort(unique(h_coord$siglev))[i]] <- sort(siglev_coord)[i]
}

############################################################################################
############################################################################################
############################################################################################

# append coordinates to the native lat lon from ROMS
append_xy_coords <- function(lonlatdat,xyproj=atlantis_bgm$extra$projection,lon_col="lon_rho",lat_col="lat_rho"){
  lonlatdat %>%
    st_as_sf(coords=c(lon_col,lat_col),crs=4326,remove=F) %>%  # convert to spatial object
    st_transform(xyproj) %>%  # convert to Atlantis coords  --> this one is not working, return EMPTY
    mutate(x = st_coordinates(.)[,1],
           y = st_coordinates(.)[,2]) # grab x and y coordinates and add them as attributes
}

rhoxy<- append_xy_coords(roms_rho,lon_col="longitude",lat_col="latitude") %>% mutate(rhoidx=dplyr::row_number())
uxy <-  rhoxy %>%
  rename(uidx = "rhoidx")
vxy <- rhoxy %>%
  rename(vidx = "rhoidx")
h_xy<- append_xy_coords(h_coord,lon_col="latitude",lat_col="longitude") %>% mutate(rhoidx=dplyr::row_number())


# In our case, velocity and physical variables measures are done at the same coordinate point
# join ROMS grids with Atlantis boxes (rho grid) and faces (u and v grids)
# Boxes
boxes_rho_join <- atlantis_sf %>% st_join(rhoxy)

# Faces
faces <- atlantis_bgm$faces %>% select(-label)

faces_sf <- atlantis_bgm %>% face_sf() %>%
  mutate(label = 0:(length(label)-1)) %>% # creates a new index 'face_id' starting from 0 and increasing, as the 'label' column produced by rbgm::face_sf() is incorrect (tested in R 4.0.4)
  # join attribute data
  left_join(faces,by=c('label'='.fx0')) %>%
  rename(.fx0=label)

# construct a buffer around each face
faces_buffer <- st_buffer(faces_sf,dist=600) #TODO: make this an argument
# Dist = the distance of cells kept to calculate cells on a boundary
# join u points
faces_u_join <- faces_buffer %>% st_join(uxy)
# join v points
faces_v_join <- faces_buffer %>% st_join(vxy)
plot(faces_v_join$longitude ,faces_v_join$latitude)
points(faces_u_join$longitude ,faces_u_join$latitude,col = "darkred")


# ... and one for which faces do not intercept u and v points
empty_faces_u<- faces_u_join %>%
  st_set_geometry(NULL) %>%
  filter(is.na(uidx)) %>%
  select(.fx0) %>%
  distinct() %>% pull()
print(paste0("Atlantis faces with no u points are faces ",paste(empty_faces_u,collapse = ",")))

empty_faces_v<- faces_v_join %>%
  st_set_geometry(NULL) %>%
  filter(is.na(vidx)) %>%
  select(.fx0) %>%
  distinct() %>% pull()
print(paste0("Atlantis faces with no v points are faces ",paste(empty_faces_v,collapse = ",")))


######################### Atlantis depth
# enter the depth breaks in the model
atlantis_z <-sort(unique(atlantis_sf$botz),decreasing = T)[-1] # Obtain the depth of atlantis layer (without surface value)


# small function to build enough depth layers for each box, starting with the defined layers above
# in this function, botz is the bottom depth given for each box, which is available in the .bgm file
build_Atlantis_depths <- function(botz,lyrs){
  # bottom of each layer, starting from shallowest
  lyrbot<-lyrs
  # layers to use are all those that are shallower than the given botz
  lyr_vec <- lyrbot[lyrbot>botz]
  # the depth of the deepest layer is equal to botz
  lyr_vec <- c(lyr_vec,botz)
  # in Atlantis, each box has the same number of depth layers, but some layers have zero thickness
  # so we have to pad with zeroes to make all boxes have the same number of layers
  nzeroes <- length(lyrs)-length(lyr_vec)
  lyr_vec <- c(lyr_vec,rep(0,nzeroes))
  return(lyr_vec)
}

# function to estimate the overlap between a ROMS cell an Atlantis cell that are overlapping
# the function return the proportion of the ROMS cells that is in the Atlantis cell
overlap <- function(x){
  top_atlantis <- x[1]
  bottom_atlantis <- x[2]
  top_ROMS <- x[3]
  bottom_ROMS <- x[4]
  if(findInterval(top_ROMS, c(bottom_atlantis,top_atlantis))==1){
    if(findInterval(bottom_ROMS, c(bottom_atlantis,top_atlantis))==1){p =1}
    else{p = (bottom_atlantis-top_ROMS)/(bottom_ROMS-top_ROMS)}
  }else{
    if(findInterval(bottom_ROMS, c(bottom_atlantis,top_atlantis))==1){
      p = (bottom_ROMS-top_atlantis)/(bottom_ROMS-top_ROMS)
    }
    else{p = (bottom_atlantis-top_atlantis)/(bottom_ROMS-top_ROMS)}
  }

  return(p)
}

# construct the depth profiles of each Atlantis box

atlantis_depths <- atlantis_bgm$boxes %>% select(.bx0,botz) %>%
  # apply the function above to create the layers
  mutate(maxz=purrr::map(botz,~build_Atlantis_depths(.,lyrs=atlantis_z))) %>%
  unnest(cols=c(maxz)) %>%
  # add a minimum depth for each layer
  group_by(.bx0) %>%
  mutate(minz=lag(maxz,1,default = 0),atlantis_layer=1:length(atlantis_z)) %>%
  # add a layer thickness calculation
  mutate(dz=minz-maxz) %>%
  # "dummy" layers (layers too deep for a given Atlantis box) should have minz and dz=0
  mutate(minz=ifelse(maxz==0,0,minz),dz=ifelse(maxz==0,0,dz)) %>%
  ungroup() %>%
  select(.bx0,atlantis_layer,minz,maxz,dz)

atlantis_depths_ROMS <- atlantis_bgm$boxes %>% select(.bx0,botz) %>%
  # apply the function above to create the layers
  mutate(siglev=purrr::map(botz,~siglev_coord[-1])) %>%
  unnest(cols=c(siglev)) %>%
  mutate(sigma = siglev)%>% # Keep siglev information in sigma
  mutate(siglev = -siglev * botz) %>% # calculate the depth of each layer
  # add a minimum depth for each layer
  group_by(.bx0) %>%
  mutate(minz=lag(siglev,1,default = 0),roms_layer=1:length(siglev_coord[-1])) %>%
  # add a layer thickness calculation
  mutate(dz=minz-siglev) %>%
  # "dummy" layers (layers too deep for a given Atlantis box) should have minz and dz=0
  mutate(minz=ifelse(siglev==0,0,minz),dz=ifelse(siglev==0,0,dz)) %>%
  ungroup() %>%
  select(.bx0,roms_layer,minz,siglev,dz,sigma)

# Keep only polygone of sea
atlantis_depths <- atlantis_depths %>%
  filter(dz > 0)

# Create
atlantis_depth_overlap <- left_join(atlantis_depths, atlantis_depths_ROMS, by = ".bx0")%>%
  filter(siglev <= minz.x, minz.y >= maxz) # Keep only ROMS cell that are overlapping Atlantis cells vertically

# temporary data frame for the next step
x <- data.frame(top_atlantis = atlantis_depth_overlap$minz.x, bottom_atlantis = atlantis_depth_overlap$maxz,
                top_ROMS = atlantis_depth_overlap$minz.y, bottom_ROMS = atlantis_depth_overlap$siglev)

# Calculate proportion overlap between ROMS cells and atlantis cells vertically
# with the overlap function
atlantis_depth_overlap$p_overlapping <- apply(x, FUN = overlap, MARGIN = 1)

atlantis_depth_overlap <- atlantis_depth_overlap %>%
  filter(p_overlapping == 0 |p_overlapping >= 0.45) # Keep overlap > 0.5, to avoid duplicate ROMS cells
# 0.45 avoid to have cells without value, keeping a value for box 5 and layer 1

# Merge with horizontal overlap
box_composition <- merge(atlantis_depth_overlap, boxes_rho_join, by=".bx0")
table(box_composition$.bx0,box_composition$atlantis_layer) #check

############################################################################################
############################################################################################
############################################################################################
# This part allows to have faces_depths_uvpts
# the face per layer are associated with a ROMS cell

{# face depths (for later flux calcs)
  face_depths <- faces %>%
    left_join(atlantis_depths %>% select(.bx0,atlantis_layer,dz),by=c("left"=".bx0")) %>%
    mutate(left_area=length*dz) %>%
    rename(dz_left=dz) %>%
    left_join(atlantis_depths %>% select(.bx0,atlantis_layer,dz),by=c("right"=".bx0","atlantis_layer")) %>%
    mutate(right_area=length*dz) %>%
    rename(dz_right=dz) %>%
    # area of the face is the smallest area, maintaining NAs (if one box is deeper than its neighbor, no flux)
    rowwise() %>%
    mutate(dz_max = max(dz_left,dz_right),
           face_area=ifelse((left_area>0 & right_area >0), min(left_area, right_area), NA))


  # ... and for Atlantis faces
  faces_u_thin <- faces_u_join %>%
    st_set_geometry(NULL) %>%
    select(.fx0,uidx) %>%
    drop_na()

  faces_u_join_with_depth <- face_depths %>%
    left_join(faces_u_thin, by = c(".fx0")) %>%
    ungroup() %>%
    #drop_na() %>% # turn this on or off depending on whether we want to have NA fluxes in non-existing layers or not, cannot recall what HydroCOnstruct wants
    select(.fx0,cosine,sine,atlantis_layer,dz_max,uidx,face_area) %>%
    group_by(uidx,.fx0) %>%
    mutate(maxz=-cumsum(dz_max),minz=-lag(-maxz,default=0)) %>%
    ungroup()%>%
    select(-dz_max)

  # again for v
  faces_v_thin <- faces_v_join %>%
    st_set_geometry(NULL) %>%
    select(.fx0,vidx) %>%
    drop_na()
  faces_v_join_with_depth <- face_depths %>%
    left_join(faces_v_thin, by = c(".fx0")) %>%
    ungroup() %>%
    #drop_na() %>%
    select(.fx0,atlantis_layer,dz_max,vidx,face_area) %>%
    group_by(vidx,.fx0) %>%
    mutate(maxz=-cumsum(dz_max),minz=-lag(-maxz,default=0)) %>%
    ungroup()%>%
    select(-dz_max)




  # make vectors of u point indices
  faces_depths_upts <- faces_u_join_with_depth %>%
    group_by(.fx0,atlantis_layer,minz,maxz) %>%
    nest(upts=c(uidx)) %>%
    mutate(uvec=map(upts,~pluck(.,'uidx'))) %>%
    ungroup()
  # and v
  faces_depths_vpts <- faces_v_join_with_depth %>%
    group_by(.fx0,atlantis_layer,minz,maxz) %>%
    nest(vpts=c(vidx)) %>%
    mutate(vvec=map(vpts,~pluck(.,'vidx'))) %>%
    ungroup()
  # now join
  faces_depths_uvpts <- faces_depths_upts %>%
    full_join(faces_depths_vpts, by = c('.fx0', 'atlantis_layer', 'maxz', 'minz', 'face_area')) %>%
    rename(FaceID = .fx0)
}




###############################################################
###############################################################
###############################################################
###############################################################

face_idx <- rbind(faces %>% select(left,.fx0) %>% set_names('Polygon_number','Face_number'),
                  faces %>% select(right,.fx0) %>% set_names('Polygon_number','Face_number')) %>%
  distinct()%>%
  arrange(Polygon_number,Face_number) %>%
  group_by(Polygon_number) %>%
  mutate(Face_new=row_number()) %>%
  ungroup()

face_data <- face_idx %>%
  left_join(faces %>% select(.fx0,left,right),by=c('Face_number'='.fx0')) %>%
  rowwise() %>%
  mutate(adjacent_box=ifelse(Polygon_number==right,left,right),
         prop =1,
         comments=0) %>%
  ungroup() %>%
  select(Polygon_number,Face_new,adjacent_box,prop,comments,Face_number) %>%
  set_names('Polygon #','Face #','adjacent box','prop','comments','FaceID')




face_layer_data <- faces_depths_uvpts %>%
  left_join(face_data %>% select('Polygon #','Face #','adjacent box','FaceID'), by = c('FaceID')) %>%
  rowwise() %>%
  ungroup()# %>%
#select(Polygon_number, Face_new, adjacent_box, prop, comments, Face_number) #%>%
#set_names('Polygon #', 'Face #', 'adjacent box', 'prop', 'comments', 'FaceID')


box_depth <- box_composition %>% distinct(atlantis_layer,.bx0,roms_layer) %>%
  set_names('atlantis_layer', 'Polygon #', 'roms_layer')

full_face_composition <- face_layer_data %>%
  left_join(box_depth, by = c('atlantis_layer','Polygon #')) %>%
  rowwise() %>%
  ungroup()

full_face_composition$u <- NA
full_face_composition$v <- NA
full_face_composition$lat <- NA
full_face_composition$lon <- NA
full_face_composition <- full_face_composition[!is.na(full_face_composition$face_area),]
full_face_composition <- full_face_composition %>% select(-upts,-vpts,-vvec)
full_face_composition$uvec <- as.character(full_face_composition$uvec)


# ##### Test to see if the transformation from list to character to list again is valid --< if nothing is printed, it is valid
# for (i in 1:3439){
#   if (sum(full_face_composition$uvec[[i]] == full_face_composition$uvec3[[i]])!=length(full_face_composition$uvec[[i]])){
#     print(i)
#     print(full_face_composition$uvec[[i]])
#   }
#     }
uxy2 <- data.frame(longitude = uxy$longitude, latitude = uxy$latitude, uidx = uxy$uidx)

write.csv(uxy2,
          paste0(here("R/code/uxy2_uv_code.csv")), row.names =F)
write.csv(full_face_composition,
          paste0(here("R/code/face_composition_uv.csv")), row.names =F)

}


