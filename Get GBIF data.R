
# North vs South global distribution

df <- read_xlsx("GLOBAL NNS DATA FINAL.xlsx")

head(df)

df <- df[!duplicated(df$ScientificName), ]

spn <- unique(df$Taxon)
length(spn) # 36,803 sp
j<-setdiff(spn, dois$Species) %>% as.data.frame()

# Download GBIF data:
## get GBIF keys ----
n <- spn[1]
counter <- 1
dois <- tibble(Species = character(), keys = character())

for (n in spn) {
  # Try to fetch data and handle potential errors or missing data explicitly
  result <- tryCatch({
    sp <- occ_data(scientificName = n, hasCoordinate = TRUE, occurrenceStatus = "PRESENT", limit = 3)
    sp <- sp[["data"]]
    if (nrow(sp) > 0) {
      key <- sp$acceptedTaxonKey[1]
      dois <- rbind(dois, data.frame("Species" = n, "DOI" = key))
      cat(n, "---> ", "key:", key, "\n")
    } else {
      cat(n, "---> ", "No data found", "\n")
      dois <- rbind(dois, data.frame("Species" = n, "DOI" = NA))
    }
  }, error = function(e) {
    cat(n, "---> ", "Error:", e$message, "\n")
    dois <- rbind(dois, data.frame("Species" = n, "DOI" = NA))
  })
  cat(counter, "/", length(spn), "\n")
  counter <- counter + 1
}

write_xlsx(dois, "keys.xlsx")


## get GBIF keys ----

df <- read_xlsx("keys.xlsx") %>% drop_na()
counter <- 1

spn <- unique(df$Species)
sp <- spn[1]

dois <- tibble(Species = character(), DOI = character(), n =numeric())

results_file <- "DoisResults.xlsx"
if (file.exists(results_file)) {
  dois <- read_xlsx(results_file)
} else {
  dois <- tibble(Species = character(), DOI = character(), n = numeric())
}

processed_species <- dois$Species  
to_process <- setdiff(spn, processed_species) 


for(sp in to_process){
  df1 <- df[df$Species== sp, ]
  tryCatch({
    x <- occ_download(
      pred_in("taxonKey", df1$Key),
      pred("hasCoordinate", TRUE),
      user = "***",
      pwd = "*****",
      email = "******"
    )
    status <- occ_download_meta(x)$status
    
    while (status != "SUCCEEDED") {
      Sys.sleep(30)  
      status <- occ_download_meta(x)$status  # Update status
    }
    
    cat("Download sp data", "\n")
    z <- occ_download_meta(x)
    
    # Save the dois
    dois <- rbind(dois, tibble(Species = sp, DOI = z$doi, n = z$totalRecords))
    
    # Download .zip
    z1 <- z %>% as.character()
    z2 <- z1[1]
       
      #download .zip
      dat <- occ_download_get(z2, overwrite=T) %>%
      occ_download_import()
    
  }, error = function(e) {
    cat("Error: ", e$message, "\n") 
    dois <- rbind(dois, tibble(Species = sp, DOI = NA, n = NA))  
  })
  cat(counter, "/", length(to_process), "\n")
  counter <- counter + 1
  write_xlsx(dois, path = results_file)
  
}


write_xlsx(all_GBIF, "GBIF1.xlsx")
write_xlsx(dois, "dois1.xlsx")



getwd()
setwd("C:/Users/Propietario/Desktop/North_vs_South_Global/GBIF_data")


results_file <- "all_GBIF.xlsx"
if (file.exists(results_file)) {
  all_GBIF <- read_xlsx(results_file)
} else {
  all_GBIF <- data.frame()
}

counter<- 1
files <- list.files(pattern = ".zip")
target_file <- "occurrence.txt"

processed_species <- all_GBIF$name
to_process <- setdiff(files, processed_species)
i <- to_process[1]

for(i in files){
  cat( counter, "/", length(files), "\n")
  counter<- counter + 1
  
  if( i %in% unique(all_GBIF$name)) { next }
  tryCatch({
  unzipped_files <- unzip(i, list = TRUE)
  if(i =="0061397-240506114902167.zip") {
    unzip(i, files = target_file)
    occurrence_data = fread(target_file, 
 select = c("species", "occurrenceStatus","basisOfRecord",
            "decimalLatitude", "decimalLongitude"), 
                        showProgress = FALSE)
  } else {
  if(target_file %in% unzipped_files$Name) {
    unzip(i, files = target_file)
    
    occurrence_data <- fread(target_file , 
   select = c("species","acceptedTaxonKey","year", "occurrenceStatus","basisOfRecord","hasCoordinate","decimalLatitude", "decimalLongitude",
   "coordinateUncertaintyInMeters","coordinatePrecision","countryCode"))
  } else {
    print(paste("NA"))
  }
  }
  cols_need <- c("species","acceptedTaxonKey","year", "occurrenceStatus","basisOfRecord","hasCoordinate","decimalLatitude", "decimalLongitude",
                 "coordinateUncertaintyInMeters","coordinatePrecision","countryCode")
  occurrence_data1 <- occurrence_data[, ..cols_need]
  
  missing_columns <- setdiff(cols_need, names(occurrence_data1))
  for (col in missing_columns) {
    occurrence_data1[, (col) := NA]  
  }
  
  occurrence_data1<- occurrence_data1 %>% filter(!basisOfRecord %in% c("FOSSIL_SPECIMEN","PRESERVED_SPECIMEN")) %>% 
    filter(!occurrenceStatus == "ABSENT")
  
  occurrence_data2 <- occurrence_data1 %>%   filter(species != "") %>% 
    mutate(hemisphere = ifelse(decimalLatitude >= 0, 'Northern', 'Southern')) 
  
  occurrence_data3 <- occurrence_data2 %>% filter(species != "") %>% group_by(species,hemisphere) %>%
    summarise(records = n())
  
  overall_summary <- occurrence_data3 %>%
    group_by(species, hemisphere) %>%
    summarise(
      total_records = sum(records, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(species) %>%
    summarise(
      Distribution = if_else(all(hemisphere == "Northern"), "Northern",
                             if_else(all(hemisphere == "Southern"), "Southern", "Both Hemispheres")),
      North_point = sum(total_records[hemisphere == "Northern"], na.rm = TRUE),
      South_point = sum(total_records[hemisphere == "Southern"], na.rm = TRUE),
      .groups = "drop" ) %>% mutate(Source ="Own PC")
  
  overall_summary$name <- i
  if(nrow(occurrence_data1)>0){ 
    all_GBIF <- rbind(all_GBIF, overall_summary)
    write_xlsx(all_GBIF, results_file)
    
  } else{ next}
  rm(occurrence_data, occurrence_data1, occurrence_data2,occurrence_data3,overall_summary,records)
  gc()
  Sys.sleep(5)

 }, error = function(e) {
    print(paste("F en el chat:", i, "Error:", e$message))
  })
}

