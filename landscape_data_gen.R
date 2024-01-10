##### CODE FOR GENERATING LANDSCAPE BUFFERS AND METRICS ########################

#### INITIAL R STUFF ####

library(raster)
library(sp)
library(rgeos)
library(sf)
library(xlsx)
library(dplyr)# tidyverse
library(ggplot2)

# set location of landscape_data directory
#dt_dir <- getwd()
dt_dir <- "C:/n4s_local/map_resources/landscape_data"

#### GET DATA ####

### a) get spatial points

n4s_sp <- rgdal::readOGR("C:/n4sdgs/r_analysis/data/proc/cross_cutting_datasets",
                         "n4s_sp_CONFIDENTIAL")
#n4s_sp <- n4s_sp[!is.na(n4s_sp$sett_id),] # remove setts with no sett_id for now

### b) get buffer sizes
buffers <- read.csv(encoding="UTF-8",stringsAsFactors = FALSE, 
         "./data/proc/land_cover_inputs/buffers.csv")

### c) get vector of land cover imagery tif files (not loading directly due to memory)
lc_files <- list.files(paste(dt_dir,"/landcover_imagery",sep=""),full.name=T,pattern=".tif")
#lc_files <- list.files("D:/n4s_local/map_resources/from_glc",full.name=T,pattern=".tif")
lc_files <- lc_files[!grepl(".aux",lc_files)]

# We selected the best peforming imagery we could find for each site:
# ACES, P4GES, DELTAS, 3 settlements in Peru and SPACES use 10m maps From_GLC: http://data.ess.tsinghua.edu.cn/
# ASSETS and Miyun use 30m maps from the projects
# Orissa and Sentinel use 25m India NRSC Imagery: https://www.nrsc.gov.in/EO_LULC_Objective?language_content_entity=en
# PIMA uses Sentinel 2 30m: http://geoportal.rcmrd.org/layers/servir%3Atanzania_sentinel2_lulc2016

### d) generate lookup table matching raster files to point locations
# using buffer to get multiple images where on edge
sett_lookup <- read.csv(encoding="UTF-8",stringsAsFactors = FALSE, 
                        "./data/proc/land_cover_inputs/site_sett_lookup.csv")
lc_lookup <- data.frame() # get empty data frame
for(i in 1:length(lc_files)){
  rs <- raster(lc_files[i]) # load raster
  int <- n4s_sp # get points and align crs if needed
  int@data <- left_join(int@data,sett_lookup[c("sett_id","buffer_m")]) # bind buffer
  if(as.character(crs(int))!=as.character(crs(rs))){
    int <- spTransform(int,crs(rs))
  }
  #int <- int[!is.na(int$buffer_m),]
  int <- rgeos::intersect(buffer(int,width=int$buffer_m,dissolve=F),rs) # get intersection of shp and raster
  lc_lookup <- bind_rows(lc_lookup, data.frame( # bind to dataframe
    pjnm = int$pjnm,
    sett_id = int$sett_id,
    lc_file = rep(lc_files[i],nrow(int))
  ))
  # tidy temp memory
  rm(rs)
  invisible(gc())
}
lc_lookup <- mutate_all(lc_lookup,as.character) # make all character vector
table(n4s_sp$pjnm[!(n4s_sp$sett_id%in%lc_lookup$sett_id)]) # check all setts have lc

## sort overlaps 
lc_lookup <- lc_lookup[!(lc_lookup$pjnm=="sp" & grepl("Tanz", lc_lookup$lc_file)),]
lc_lookup <- lc_lookup[!(lc_lookup$sett_id%in%c("asp/s52/Pueblo","asp/s50/Monte","asp/s49/La Uni") & 
                           grepl("assets", lc_lookup$lc_file)),]
lc_lookup <- lc_lookup[!(!(lc_lookup$sett_id%in%c("asp/s52/Pueblo","asp/s50/Monte","asp/s49/La Uni")) &
                           lc_lookup$pjnm=="asp" &
                           grepl("fromglc", lc_lookup$lc_file)),]
lc_lookup <- lc_lookup[!(lc_lookup$pjnm=="ac" & grepl("mal_", lc_lookup$lc_file)),]

site <- read.csv(encoding="UTF-8",stringsAsFactors = FALSE, 
                 "./data/proc/initial/n4s_sites.csv")

nilg <- site$sett_id[site$site_short=="SENTINEL NILGIRIS"]
wayan <- site$sett_id[site$site_short=="SENTINEL WAYANAD"]
lc_lookup <- lc_lookup[!(lc_lookup$sett_id %in% nilg & grepl("wayanad",lc_lookup$lc_file)),]
lc_lookup <- lc_lookup[!(lc_lookup$sett_id %in% wayan & grepl("nilgris",lc_lookup$lc_file)),]

### e) make lookup table for reclassification
# from expert opinion exercises

# get site level/grouped lookup table with reclassifications
# rclas_lookup <- read.csv(encoding="UTF-8",stringsAsFactors = FALSE, 
#                     paste(dt_dir,"/n4s_data/reclassify_lookup_example.csv",sep=""))
rclas_lookup <- read.csv(encoding="UTF-8",stringsAsFactors = FALSE,
                    "./data/proc/land_cover_inputs/reclassify_lookup.csv")

# get lookup table to match sites/groupings in rcals_lookup with settlements
sett_lookup <- read.csv(encoding="UTF-8",stringsAsFactors = FALSE, 
                        "./data/proc/land_cover_inputs/site_sett_lookup.csv")

sett_lookup <- sett_lookup[which(sett_lookup$site_or_settlement!=""),]

sett_lookup$rts_id <- as.numeric(paste(999,row.names(sett_lookup),sep="")) # ad PA id

# expand rclas_lookup to be by settlement
rclas_lookup <- left_join(rclas_lookup[-1],sett_lookup,
                           by=c("site_or_settlement"))

# add rts id
rclas_lookup$rts_id <- as.numeric(as.factor(paste(rclas_lookup$sett_id,
                                                  rclas_lookup$new_rts_name)))

# add PA and mangrove info
rclas_lookup <- sett_lookup %>%
  filter(pa_status=="PA") %>%
  mutate(new_rts_name = paste(sector,res_type,de_facto,sep="\n")) %>%
  dplyr::select(pjnm:og_vlid,new_rts_name,sector:de_facto,rts_id) %>%
  bind_rows(rclas_lookup,.)

rclas_lookup <- sett_lookup %>%
  filter(mang_add=="y") %>%
  dplyr::select(pjnm:og_vlid,starts_with("mang_"),rts_id) %>%
  rename_with(~gsub("mang_", "", .),starts_with("mang_")) %>%
  mutate(new_rts_name = paste(sector,res_type,de_facto,sep="\n")) %>%
  mutate(rts_id = as.numeric(gsub("^.{0,3}", "998", rts_id))) %>%
  bind_rows(rclas_lookup,.)

### f) generate protected area data (combine source layers)
# using project data where available/valid, otherwise...
# using WDPA data, checked for each of our sites: https://www.protectedplanet.net/en/thematic-areas/wdpa

# get cropped WDPA protected area data
wdpa <- rgdal::readOGR(paste(dt_dir,"/n4s_data",sep=""),
                       "wdpa_checked")
wdpa <- wdpa[which(wdpa$pjnm!="pa"),] # remove p4ges
wdpa <- wdpa[which(wdpa$pjnm!="pi"),] 
wdpa <- wdpa[which(wdpa$pjnm!="se"),] 
wdpa <- wdpa[which(wdpa$pjnm!="od"),] 

# get other PA data
# p4ges
pa_pa <- rgdal::readOGR(paste(dt_dir,"/n4s_data/p4ges_pa_data",sep=""),
                        "CAZ_PAs")
pa_pa <-  as(st_make_valid(st_as_sf(pa_pa)),"Spatial") # fix a validity issue
pa_pa <- spTransform(pa_pa,crs(wdpa)) # align crs
# india
ind_pa <- rgdal::readOGR(paste(dt_dir,"/n4s_data/India-PA",sep=""),
                        "pa_bounds")
# pima
pi_pa <- rgdal::readOGR(paste(dt_dir,"/n4s_data",sep=""),
                         "wdpa_july2015_tza_shapefile_polygons")

# # combine all data
# # we lose the names here and difficult to restore. but can do if needed
wdpa <- as(wdpa,"SpatialPolygons")
pa_pa <- as(pa_pa,"SpatialPolygons")
ind_pa <- as(ind_pa,"SpatialPolygons")
pi_pa <- as(pi_pa,"SpatialPolygons")
pas <- bind(wdpa,pa_pa)
pas <- bind(pas,ind_pa)
pas <- bind(pas,pi_pa)

# fix topolgy error
pas <- gBuffer(pas, byid=TRUE, width=0)

rm(wdpa,pa_pa,ind_pa,pi_pa)

### g) make look up table for dealing with multiple RTS in land class
# get site level disambiguation table
multi_rts <- read.csv(encoding="UTF-8",stringsAsFactors = FALSE, 
                      "./data/proc/land_cover_inputs/multiple_rts - Sheet1.csv")
# expand to be by settlement
multi_rts <- left_join(multi_rts[-1],sett_lookup,
                           by=c("sett_id"))
# drop NAs because masked out in other ways
multi_rts <- multi_rts[which(!is.na(multi_rts$est_pc)),]

# make id for each reclassification
multi_rts$id <- paste(multi_rts$sett_id,multi_rts$rts_name,sep="-") 

# empty list for example
# eg <- list()

### h) load mangrove data for SPACES sites
# Kenya http://geoportal.rcmrd.org/layers/servir%3Akenya_mangrove_cover
man_ke <- rgdal::readOGR(paste(dt_dir,"/n4s_data/kenya_mangrove_data",sep=""),
                         "kenya_mangrove_cover")
# Moz http://geoportal.rcmrd.org/layers/servir%3Amozambique_mangrove_cover
man_mz <- rgdal::readOGR(paste(dt_dir,"/n4s_data/mozambique_mangrove_cover/CD Mangroves",sep=""),
                         "CD_mangroves_2013")
# extra moz mangrove cover in lalane from Tim
man_lal <- rgdal::readOGR(paste(dt_dir,"/n4s_data/lalane_polygons",sep=""),
                         "lalane_mangrove")

# extra Kenya mangrove cover
man_van <- rgdal::readOGR(paste(dt_dir,"/n4s_data/Vanga & Tsunza/Vanga",sep=""),
                          "Vanga")
man_tsu <- rgdal::readOGR(paste(dt_dir,"/n4s_data/Vanga & Tsunza/Tsunza",sep=""),
                          "Reitz")

# mangroves/sunderbans bangladesh
man_bgd <- rgdal::readOGR(paste(dt_dir,"/n4s_data/bangladesh_mangroves/iucn-imma-layer-shapefile_v2.4/iucn-imma/iucn-imma",sep=""),
                          "iucn-imma_oct20")
man_bgd <- man_bgd[man_bgd$Identcode=="25NIOSEA181919IMMA",]

# # combine all data
# # we lose the names here and difficult to restore. but can do if needed
man_ke <- spTransform(man_ke,crs(man_lal)) # align crs
man_mz <- spTransform(man_mz,crs(man_lal)) # align crs
man_bgd <- spTransform(man_bgd,crs(man_lal))
man_van <- spTransform(man_bgd,crs(man_lal))
man_tsu <- spTransform(man_bgd,crs(man_lal))

man_ke <- as(man_ke,"SpatialPolygons")
man_mz <- as(man_mz,"SpatialPolygons")
man_lal <- as(man_lal,"SpatialPolygons")
man_bgd <- as(man_bgd,"SpatialPolygons")
man_tsu <- as(man_tsu,"SpatialPolygons")
man_van <- as(man_van,"SpatialPolygons")

mangroves <- bind(man_ke,man_mz)
mangroves <- bind(mangroves,man_lal)
mangroves <- bind(mangroves,man_bgd)
mangroves <- bind(mangroves,man_van)
mangroves <- bind(mangroves,man_tsu)

rm(man_ke,man_mz,man_lal,man_van,man_tsu)

# fix topolgy error
mangroves <- gBuffer(mangroves, byid=TRUE, width=0)


#### CODE ####

### a) Loop to load and reclassify land cover maps for each settlement, inc. PAs
#i <- 31

# settlements to update
sts <- c("sp/s232/vamizi")
i <- match(sts,n4s_sp$sett_id)
# #sts <- n4s_sp$sett_id[n4s_sp$pjnm=="od"]
# sts <- match(sts,n4s_sp$sett_id)

#for(i in 1:nrow(n4s_sp)){
#for(i in sts){
for(i in 1:nrow(n4s_sp)){  
  # get settlement point
  pt <- n4s_sp[i,]
  # eg$pt <- pt # get example
  
  # get rasters. combine where necesarry
  rsdt <- lc_lookup$lc_file[lc_lookup$sett_id==pt$sett_id]
  
  if(length(rsdt)>1){
    tp <- list()
    for(k in rsdt){
      tp[length(tp)+1] <- raster(k)
    }
    #tp$tolerance <- 0.1
    #rs <- raster::merge(tp[[1]],tp[[2]],tolerance=0.1)
    #tp[[2]] <- projectRaster(from = tp[[2]], to = tp[[1]], alignOnly = T)
    rs <- do.call(merge,list(x = tp[[1]], y = tp[[2]], tolerance=0.4))
    rm(tp)
    removeTmpFiles()
    invisible(gc())
    #return(rs)
  }
  
  if(length(rsdt)<=1){
    rs <- raster(rsdt)
  }
  
  # align crs if needed
  if(as.character(crs(pt))!=as.character(crs(rs))){
    pt <- spTransform(pt,crs(rs))
  }
  
  # crop in large buffer (approx. 2 times bigger than any landscape buffer will be)
  rs <- crop(rs, 
             buffer(pt,width=sett_lookup$buffer_m[which(sett_lookup$sett_id==pt$sett_id)]*2))
  # eg$lc_orig <- rs # get example
  
  # reclassify land classes to RTS
  lc <- unique(rs)# get old lc values
  rc <- rclas_lookup[which(rclas_lookup$sett_id==pt$sett_id),] # get new values
  rcdf <- data.frame( # make reclassify dataframe
    from = lc,
    to = rc$rts_id[match(lc,rc$land_class_code)]
  )
  rs <- raster::reclassify(rs, rcl = rcdf) # reclassify
  #eg$lc_lookup <- rcdf
  # eg$lc_new_nopa <- rs # get example
  #eg$old_lc <- rc

  
  # classify mangroves if present
  man_if <- sett_lookup[sett_lookup$sett_id==pt$sett_id,] # get PA info
  man_if$rts_id <- as.numeric(gsub("^.{0,3}", "998", man_if$rts_id))
  if(man_if$mang_add == "y"){ # run if there is a mangrove
    man_int <- mangroves # get data
    if(as.character(crs(man_int))!=as.character(crs(rs))){ # align crs if needed
      man_int <- spTransform(pt,crs(man_int))
    }
    man_int <- invisible(raster::intersect(man_int, rs)) # get interesction of PA and raster data
    if(length(man_int)>0){ # run if there are PAs
      man_rs <- mask(rs, man_int) # make new raster with PA areas as 99
      man_rs[!is.na(man_rs)] <- man_if$rts_id
      rs[!is.na(man_rs)] <- man_rs[!is.na(man_rs)] # update values in main raster that are PA
    }
  }
  
  # classify protected areas if present
  pa_if <- sett_lookup[sett_lookup$sett_id==pt$sett_id,] # get PA info
  if(pa_if$pa_status == "PA"){ # run if there is a defacto PA
    pa_int <- pas # get PA data
    if(as.character(crs(pa_int))!=as.character(crs(rs))){ # align crs if needed
      pa_int <- spTransform(pt,crs(pa_int))
    }
    pa_int <- invisible(raster::intersect(pa_int, rs)) # get interesction of PA and raster data
    if(length(pa_int)>0){ # run if there are PAs
      pa_rs <- mask(rs, pa_int) # make new raster with PA areas as 99
      pa_rs[!is.na(pa_rs)] <- pa_if$rts_id
      rs[!is.na(pa_rs)] <- pa_rs[!is.na(pa_rs)] # update values in main raster that are PA
    }
  }
    # eg$pa_int <- pa_int
    # eg$lc_new_wpa <- rs # get example
    
  
  # save reclassified tif to directory
  nm <- gsub("/","-",pt$sett_id)
  writeRaster(rs,paste(dt_dir,"/reclassified_imagery/",nm,".tif",sep=""),
              overwrite=TRUE)
  
  # tidy temp memory
  rm(rs,pa_int,man_int,pa_rs,pt,tp)
  removeTmpFiles()
  invisible(gc())
  flrmv <- list.files(tempdir(), full.names = T,recursive =T, pattern=".gri")
  file.remove(flrmv)
  
}

# save example
# save(eg, file = paste(dt_dir,"/n4s_data/example.rda",sep=""))

### b) make function to generate rts landscape metrics from land cover data, with sensitivity analysis
#i <- 1
#st_id <- n4s_sp$sett_id[i]
st_id <- "de/s66/Nalton" # example settlement
 sensitivity <- 1

rts_metrics <- function(st_id,sensitivity){
    # st_id = sett_id for settlement of interest
    # sensitivity = the proportion by which to decrease/increase the buffer (1 if not doing sensitivity)  
  
    # get point and buffer, buffer multiplied by sensitivity paramteres
    pt <- n4s_sp[which(n4s_sp$sett_id==st_id),]
    bf <- sett_lookup$buffer_m[which(sett_lookup$sett_id==pt$sett_id)]# main buffer
    pt <- raster::buffer(pt,bf*sensitivity)
    pt <- SpatialPolygonsDataFrame(pt, n4s_sp@data[n4s_sp$sett_id==st_id,], match.ID=F) # restore attributes
    crs(pt) <- crs(n4s_sp)
    
    # get rts names from reclassify table, plus PAs
    lc <- rclas_lookup[which(rclas_lookup$sett_id==pt$sett_id),]
    
    # get raster
    rs <- raster(paste(dt_dir,"/reclassified_imagery/",gsub("/","-",pt$sett_id),".tif",sep=""))
    
    # align crs if needed
    if(as.character(crs(pt))!=as.character(crs(rs))){
      pt <- spTransform(pt,crs(rs))
    }
    
    # get proportion and extent of land classes in buffer
    #lc_pc <- unlist(raster::extract(rs,pt))
    #lc_pc <- table(lc_pc)/sum(!is.na(lc_pc))
    lc_pc <- mask(rs, pt) # mask by buffer
    lc_pc <- table(getValues(lc_pc))/sum(!is.na(getValues(lc_pc)))
    lc_pc <- as.data.frame(lc_pc)
    names(lc_pc) <- c("rts_id","extent_pc")
    lc_pc$extent_ha <- lc_pc$extent_pc * (raster::area(pt)/10000)
    
    # update names with RTS
    lc_pc$rts_id <- as.numeric(as.character(lc_pc$rts_id))
    lc_pc$rts_name <- lc$new_rts_name[match(lc_pc$rts_id,lc$rts_id)]
    
    
    # append all to dataframe
    res <- data.frame(
      pjnm = pt$pjnm,
      cn = pt$cn,
      sett_id = pt$sett_id,
      main_buffer= bf,
      sensitivity = sensitivity,
      rts_id = lc_pc$rts_id,
      rts_name = lc_pc$rts_name,
      buffer_area = raster::area(pt)/10000,
      extent_pc = lc_pc$extent_pc,
      extent_ha = lc_pc$extent_ha
    )
    
    # tidy temp memory
    rm(rs, lc_pc)
    invisible(gc())
    
    return(res)
  }

### c) update n4s_landcover classes with multiple RTS where necessary

# mt_id <- unique(multi_rts$id)[1] # example
# lc_dat <- n4s_ls

fix_multi_rts <- function(mt_id,lc_dat){
  # has to be run after rts_metrics function, with output of rts_metrics as lc_dat
  # mt_id = id from multi_rts dataframe
  # lc_dat = output of rts_metrics
    
    # get multiple rts info for this instance
    mrts <- multi_rts[which(multi_rts$id==mt_id),]
    
    # generate updated rts data from subset of lc_dat
    lc <- lc_dat[which(lc_dat$sett_id==unique(mrts$sett_id) & # get subset
                          lc_dat$rts_name==unique(mrts$rts_name)),]
    lc <- lc[rep(1:nrow(lc),nrow(mrts)),] # expand to actual number of rts
    lc$extent_pc <- lc$extent_pc * mrts$est_pc # update values
    lc$extent_ha <- lc$extent_ha * mrts$est_pc
    lc$rts_name <- mrts$rts_new
    
    # substitute updated rts data into n4s_landcover
    lc_dat <- lc_dat[which(!(lc_dat$sett_id==unique(mrts$sett_id) & # remove old
                                       lc_dat$rts_name==unique(mrts$rts_name))),]
    lc_dat <- bind_rows(lc_dat, lc) # add new
    row.names(lc_dat) <- row.names(seq(1,nrow(lc_dat)))
    
    return(lc_dat)
}


## generate multi_rts input file
library(tidyr)
multi_rts_out <- n4s_ls %>%
  dplyr::select(pjnm,sett_id,rts_name) %>%
  filter((grepl("rivate",rts_name) & grepl("egulated",rts_name)) | # private/CPR
          (grepl("Share",rts_name) & grepl("egulated",rts_name)) | # private/CPR
          (grepl("Share",rts_name) & grepl("ommunal",rts_name)) |
         (grepl("rotected",rts_name) & grepl("egulated",rts_name)) |  # PA/cpr 
         (grepl("Unregulated access-public",rts_name) & grepl("Regulated access",rts_name)) | # reg/unreg
          (grepl("Unregulated access-community",rts_name) & grepl("Regulated access",rts_name))
          ) %>%
  filter(pjnm!="se") %>%
  separate(rts_name,into=as.character(c(1:20)),sep="\n",remove=F) %>%
  gather("num","rts_name_new",-pjnm:-rts_name) %>%
  dplyr::select(-num) %>%
  filter(!is.na(rts_name_new))

write.csv(multi_rts_out,"./data/proc/land_cover_inputs/multi_rts_out.csv")

n4s_ls_tmp <- n4s_ls
# run multi_rts to fix multiple rts
for(i in unique(multi_rts$id)){
  n4s_ls <- fix_multi_rts(i, n4s_ls)
}

#n4s_ls <- read.csv("./data/proc/cross_cutting_datasets/n4s_ls.csv")

# write landscape raster codes
write.csv(rclas_lookup,"./data/proc/land_cover_inputs/n4s_ls_raster_codes.csv")

n4s_ls <- n4s_ls
n4s_ls$rts_name[is.na(n4s_ls$rts_name)] <- 
  rclas_lookup$new_rts_name[match(n4s_ls$rts_id[is.na(n4s_ls$rts_name)],
                                  rclas_lookup$rts_id)]

# remove NAs from edge pixels in India
n4s_ls <- n4s_ls %>%
  filter(!(cn=="IND" & is.na(rts_id)))


## work around to stop duplication of PA extents
# first fix \n in genuine PA extents
#n4s_ls <- read.csv("./data/proc/cross_cutting_datasets/n4s_ls.csv")
rp <- n4s_ls[grepl("Protected", n4s_ls$rts_name),]
rp <- rp %>%
  filter(!(grepl("Protected",rts_name) &
            (grepl("egulated",rts_name) |
               grepl("Private",rts_name)
            )
  ))
n4s_ls$rts_name[n4s_ls$rts_name%in%rp$rts_name] <- 
  gsub("\n"," ",n4s_ls$rts_name[n4s_ls$rts_name%in%rp$rts_name])

# then remove PA from rts_name where multiple RTS
n4s_ls$sbstr <- strsplit(n4s_ls$rts_name,"\n") # get substrings
#head(lapply(n4s_ls$sbstr,length))
n4s_ls$sbstr_new <- n4s_ls$sbstr
n4s_ls$sbstr_new[lapply(n4s_ls$sbstr,length)>1] <- # where multi RTS
  lapply(n4s_ls$sbstr[lapply(n4s_ls$sbstr,length)>1], # apply function to substrings to remove entries with PA
                           function(z) lapply(z, 
                                              function(x) ifelse(grepl("rotected",x),"",x)))
n4s_ls$sbstr_new <- lapply(n4s_ls$sbstr_new,function(x) paste0(unlist(x),collapse=" ")) # combine back together
n4s_ls$rts_name <- n4s_ls$sbstr_new
n4s_ls$rts_name <- as.character(n4s_ls$rts_name)
n4s_ls <- n4s_ls %>%
  dplyr::select(-contains("sbstr"))

# write land scape data in n4s_ls
write.csv(n4s_ls,"./data/proc/cross_cutting_datasets/n4s_ls.csv")


### add final updates
n4s_ls <- read.csv("./data/proc/cross_cutting_datasets/n4s_ls.csv")

### malawi updates
mwup <- read.csv("./data/proc/land_cover_inputs/asm_multi_rts_update.csv")
mwup <- mwup %>%
  dplyr::select(sett_id,rts_name=rts_new,est_pc) %>%
  drop_na()

# get and combine forest rows of n4s_ls
tmp <- n4s_ls %>%
  filter(sett_id %in% mwup$sett_id) %>%
  filter(grepl("Forest",rts_name)) %>%
  dplyr::select(-contains("X."),-X) %>%
  group_by(pjnm,cn,sett_id,main_buffer,sensitivity,rts_id) %>%
    summarise(
      buffer_area = sum(buffer_area),
      extent_pc =  sum(extent_pc),
      extent_ha =  sum(extent_ha)
    ) %>%
  full_join(mwup,"sett_id")
# convert
tmp <- tmp %>%
  mutate_at(vars(buffer_area:extent_ha),~.*est_pc) %>%
  dplyr::select(-est_pc)

# switch in new rts
n4s_ls <- n4s_ls %>%
  filter(!(sett_id %in% mwup$sett_id & grepl("Forest",rts_name))) %>%
  dplyr::select(-contains("X."),-X) %>%
  bind_rows(tmp)

# remove sensitivity
n4s_ls$sensitivity <- NULL

write.csv(n4s_ls,"./data/proc/cross_cutting_datasets/n4s_ls.csv")

### colombia updates
n4s_ls <- read.csv("./data/proc/cross_cutting_datasets/n4s_ls.csv")

# y <- n4s_ls[n4s_ls$pjnm=="asc" & # if colombia
#                         (grepl("Forest", n4s_ls$rts_name) | # and forest or water bodies
#                            grepl("Water bodies", n4s_ls$rts_name)),]

n4s_ls$rts_name[n4s_ls$pjnm=="asc" & # if colombia
                  (grepl("Forest", n4s_ls$rts_name) | # and forest or water bodies
                     grepl("Water bodies", n4s_ls$rts_name))] <- # then
  gsub("Unregulated access-community","Regulated access-community", # sub regulated for it
       n4s_ls$rts_name[n4s_ls$pjnm=="asc" & # if colombia
                         (grepl("Forest", n4s_ls$rts_name) | # and forest or water bodies
                            grepl("Water bodies", n4s_ls$rts_name))]
  )

write.csv(n4s_ls,"./data/proc/cross_cutting_datasets/n4s_ls.csv")
              
              
### h) make settlement level dataframe with landscape metrics

## get n4s_ls
n4s_ls <- read.csv(encoding="UTF-8",stringsAsFactors = FALSE, 
                   "./data/proc/cross_cutting_datasets/n4s_ls.csv")

# rts_dat <- read.csv(encoding="UTF-8",stringsAsFactors = FALSE, 
#                     "./data/proc/cross_cutting_datasets/n4s_rts.csv")

## use rules to make simple rts and cultivated/uncultivated vars
n4s_ls_simple <- n4s_ls

n4s_ls_simple$tn_cat <- NA
n4s_ls_simple$tn_cat[grepl("rivate",n4s_ls_simple$rts_name)] <- "Private"
n4s_ls_simple$tn_cat[grepl("rotected",n4s_ls_simple$rts_name) &
                       is.na(n4s_ls_simple$tn_cat)] <- "Protected area"
n4s_ls_simple$tn_cat[grepl("nregulated",n4s_ls_simple$rts_name) &
                       grepl("ommunity",n4s_ls_simple$rts_name) &
                       is.na(n4s_ls_simple$tn_cat)] <- "Unregulated-community"
n4s_ls_simple$tn_cat[grepl("nregulated",n4s_ls_simple$rts_name) &
                       grepl("ublic",n4s_ls_simple$rts_name) &
                       is.na(n4s_ls_simple$tn_cat)] <- "Unregulated-public"
n4s_ls_simple$tn_cat[grepl("egulated",n4s_ls_simple$rts_name) &
                       grepl("comm",n4s_ls_simple$rts_name) &
                       is.na(n4s_ls_simple$tn_cat)] <- "Regulated-community"
n4s_ls_simple$tn_cat[grepl("egulated",n4s_ls_simple$rts_name) &
                       is.na(n4s_ls_simple$tn_cat)] <- "Regulated"
n4s_ls_simple$tn_cat[grepl("ettlement",n4s_ls_simple$rts_name) &
                       is.na(n4s_ls_simple$tn_cat)] <- "Settlement"
n4s_ls_simple$tn_cat[grepl("arren",n4s_ls_simple$rts_name) &
                       is.na(n4s_ls_simple$tn_cat)] <- "Unregulated-public"
n4s_ls_simple$tn_cat[grepl("arren",n4s_ls_simple$rts_name) &
                       is.na(n4s_ls_simple$tn_cat)] <- "Unregulated-public"
n4s_ls_simple$tn_cat[grepl("Wetlands/Water Bodies",n4s_ls_simple$rts_name) &
                       is.na(n4s_ls_simple$tn_cat)] <- "Unregulated-public"
n4s_ls_simple$tn_cat[grepl("Grass/Grazing",n4s_ls_simple$rts_name) &
                       is.na(n4s_ls_simple$tn_cat)] <- "Unregulated-public"
n4s_ls_simple$tn_cat[is.na(n4s_ls_simple$tn_cat)] <- "Unknown landcover (NA)"

n4s_ls_simple$uncultivated <- "Uncultivated"
n4s_ls_simple$uncultivated[grepl("rivate",n4s_ls_simple$tn_cat)] <- "Cultivated"
n4s_ls_simple$uncultivated[grepl("ettlement",n4s_ls_simple$tn_cat)] <- "Settlement"
n4s_ls_simple$uncultivated[n4s_ls_simple$tn_cat=="Unknown landcover (NA)"] <- NA

n4s_ls_sett <- n4s_ls_simple %>%
  group_by(sett_id) %>%
  summarise(tn_ext_private = sum(extent_pc[tn_cat=="Private"],na.rm=T),
            tn_ext_unregulated_community = sum(extent_pc[tn_cat=="Unregulated-community"],na.rm=T),
            tn_ext_settlement = sum(extent_pc[tn_cat=="Settlement"],na.rm=T),
            tn_ext_regulated = sum(extent_pc[tn_cat=="Regulated"],na.rm=T),
            tn_ext_unregulated_public = sum(extent_pc[tn_cat=="Unregulated-public"],na.rm=T),
            tn_ext_protected_area = sum(extent_pc[tn_cat=="Protected area"],na.rm=T),
            tn_ext_regulated_community = sum(extent_pc[tn_cat=="Regulated-community"],na.rm=T),
            tn_ext_unknown_na = sum(extent_pc[tn_cat=="Unknown landcover (NA)"],na.rm=T),
            rs_ext_uncultivated = sum(extent_pc[uncultivated=="Uncultivated"],na.rm=T),
            rs_ext_cultivated = sum(extent_pc[uncultivated=="Cultivated"],na.rm=T)
  )

write.csv(n4s_ls_sett,"../data/proc/cross_cutting_datasets/n4s_ls_sett.csv")


