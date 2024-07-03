process_stf <- function(db){
  colnames(db) <- toupper(colnames(db))
  
  db[which(is.na(db[, which(colnames(db) == "POPULAR")])), which(colnames(db) == "POPULAR")] = 0
  db[which(is.na(db[, which(colnames(db) == "RESIDENCIAL")])), which(colnames(db) == "RESIDENCIAL")] = 0
  db[which(is.na(db[, which(colnames(db) == "COMERCIAL")])), which(colnames(db) == "COMERCIAL")] = 0
  
  db[, which(colnames(db) == "TERMINALES DE USO PUBLICO")] <- lapply(db[, which(colnames(db) == "TERMINALES DE USO PUBLICO")], as.numeric)
  db[which(is.na(db[, which(colnames(db) == "TERMINALES DE USO PUBLICO")])), which(colnames(db) == "TERMINALES DE USO PUBLICO")] = 0
  
  db <- db |> mutate(total = POPULAR + RESIDENCIAL + COMERCIAL + `TERMINALES DE USO PUBLICO`)
  
  try({
    db <- db |> group_by(`COD DE PARROQUIA`) |>
      summarise(
        total = sum(total)
      )
  }, silent = TRUE
  )
  try({
    db <- db |> group_by(CODIGO_PARROQUIA) |>
      summarise(
        total = sum(total)
      )
  }, silent = TRUE
  )
  
  db
}