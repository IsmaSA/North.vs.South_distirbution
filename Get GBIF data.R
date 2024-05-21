
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
      user = "ismaelsoto",
      pwd = "Ismaputas123.",
      email = "isma-sa@hotmail.com"
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




all_GBIF <- data.frame()
# read the .zip and take tha data
files <- list.files(pattern = z2)
unzipped_files <- unzip(files, list = TRUE)
target_file <- "occurrence.txt"

if(target_file %in% unzipped_files$Name) {
  unzip(files, files = target_file)
  
  occurrence_data <- fread(target_file)
} else {
  print(paste("File", target_file, "not found in the zip archive."))
}

cols_need <- c("decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", 
               "year", "basisOfRecord", "countryCode")

occurrence_data1 <- occurrence_data[, ..cols_need]

missing_columns <- setdiff(cols_need, names(occurrence_data1))
for (col in missing_columns) {
  occurrence_data1[, (col) := NA]  
}

occurrence_data1$species <- sp
occurrence_data1$GBIF_key <- df1$Key

all_GBIF <- rbind(all_GBIF, occurrence_data1)







occurrence_data <- dat

# Latitude > 0 == North otherwise == South

occurrence_data1 <- occurrence_data %>%
  mutate(hemisphere = ifelse(decimalLatitude >= 0, 'Northern', 'Southern'))


occurrence_data1 <- occurrence_data1[,c(38,98,99,224)]
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_sf() +  
  geom_point(data = occurrence_data1, aes(x = decimalLongitude, y = decimalLatitude, color = hemisphere), size = 1, alpha = 0.6) +
  labs( x = "Longitude", y = "Latitude") +
  scale_color_manual(values = c("Southern" = "blue", "Northern" = "red")) +  # Adjust colors as necessary
  theme_bw()

# Group by species and identify those present in both hemispheres
bi_hemispheric_species <- occurrence_data %>%
  group_by(species) %>%
  summarise(hemispheres = unique(hemisphere)) %>%
  filter(length(hemispheres) > 1)
