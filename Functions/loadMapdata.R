loadMapdata <- function(){
  ## Esri basemap URLs ####
  world_gray <<-  paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')
  # Load data
  hydroid.order <<- read_csv('Data/hydroid_order2.csv')
  
  tribdata <<- read_csv('Data/LMtribs_seasonal.csv') %>% 
    group_by(hydroid) %>% 
    summarise_if(is.numeric, first) %>% 
    left_join(hydroid.order)
  
  raw.4269  <<-  st_as_sf(tribdata, coords = c('longitude', 'latitude'), crs = 4269) %>% 
    mutate(np = (nitrate_mgl/14.01)/(srp_mgl/30.97)) 
  raw.6350 <<- raw.4269 %>% st_transform(crs = 6350)
  
  # Lake Michigan catchments
  catchments.6350 <<- st_read('GIS/MIcatchments.shp', stringsAsFactors = F) %>% 
    st_set_crs(6350)
  # st_set_crs(CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'))
  # catchments.6350.simple <- st_simplify(catchments.6350, preserveTopology = TRUE, dTolerance = 30)
  catchments.4269 <<- catchments.6350 %>% st_transform(crs = 4269)
  
  catchments.4269.use <<- catchments.4269 %>% filter(HydroID %in% raw.4269$hydroid)
  catchments.4269.notuse <<- catchments.4269 %>% filter(!HydroID %in% raw.4269$hydroid)
  
  # Lake Michigan
  lakeMI.4269 <<- st_read('GIS/hydro_p_LakeMichigan.shp', stringsAsFactors = F) 
  lakeMI.6350 <<- lakeMI.4269 %>% st_transform(crs = 6350)
}