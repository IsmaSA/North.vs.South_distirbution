# try again but with chunks of species
process_species_chunk <- function(species_chunk, df, results_file) {
  dois <- if (file.exists(results_file)) {
    read_xlsx(results_file)
  } else {
    tibble(Species = character(), DOI = character(), n = numeric())
  }
  
  processed_species <- dois$Species
  to_process <- setdiff(species_chunk, processed_species)
  
    df_chunk <- df %>% 
      filter(Species %in% to_process)
    
    taxon_keys <- unique(df_chunk$Key)
    tryCatch({
      x <- occ_download(
        pred_in("taxonKey", taxon_keys),
        pred("hasCoordinate", TRUE),
        user = "**", 
        pwd = "**",  
        email = "sarita19417@gmail.com"
      )
      
      while (occ_download_meta(x)$status != "SUCCEEDED") {
        Sys.sleep(30)  # Adjust timing based on your needs and API limits
      }
      
      z <- occ_download_meta(x)
      dois <- dois %>% 
        add_row(Species = df_chunk$Species, DOI = z$doi, n = z$totalRecords)      
    }, error = function(e) {
      message("Error with species ", sp, ": ", e$message)
      add_row(Species = as.character(sp), DOI = NA_character_, n = NA_real_)
    })
    
    write_xlsx(dois, path = results_file)
    cat("goal")
  }


df <- read_xlsx("keys.xlsx") %>% drop_na()
species_list <- unique(df$Species)
results_file <- "DoisResults.xlsx"

chunk_size <- 4
num_chunks <- ceiling(length(species_list) / chunk_size)

for (i in 1:num_chunks) {
  start <- (i - 1) * chunk_size + 1
  end <- min(i * chunk_size, length(species_list))
  species_chunk <- species_list[start:end]
  
  process_species_chunk(species_chunk, df, results_file)
  cat("goal")
}
