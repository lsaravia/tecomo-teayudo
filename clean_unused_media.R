#-------------------------------------------------------------
# Limpiar imágenes y videos no utilizados en un .Rmd
#-------------------------------------------------------------

clean_unused_media <- function(
    rmd = "te_como_o_te_ayudo.Rmd",
    media_dir = "Images",
    delete_unused = FALSE,
    move_to = NULL      # por ejemplo "Images/unused"
){
  
  stopifnot(file.exists(rmd))
  stopifnot(dir.exists(media_dir))
  
  texto <- readLines(rmd, warn = FALSE)
  
  # extensiones soportadas
  ext <- c(
    "png","jpg","jpeg","gif","svg","webp",
    "mp4","mov","avi","mkv","webm"
  )
  
  patron <- paste0(
    media_dir,
    "/[^\"'()<>\\s]+\\.(",
    paste(ext, collapse="|"),
    ")"
  )
  
  usados <- unique(unlist(regmatches(
    texto,
    gregexpr(patron, texto, perl = TRUE)
  )))
  
  usados <- normalizePath(usados,
                          winslash="/",
                          mustWork = FALSE)
  
  todos <- list.files(
    media_dir,
    recursive = TRUE,
    full.names = TRUE
  )
  
  todos <- todos[grepl(
    paste0("\\.(", paste(ext, collapse="|"), ")$"),
    todos,
    ignore.case = TRUE
  )]
  
  todos <- normalizePath(todos,
                         winslash="/",
                         mustWork = FALSE)
  
  no_usados <- setdiff(todos, usados)
  
  cat("\n---------------------------------\n")
  cat("Archivos multimedia:", length(todos), "\n")
  cat("Utilizados:", length(usados), "\n")
  cat("No utilizados:", length(no_usados), "\n")
  cat("---------------------------------\n\n")
  
  if(length(no_usados)==0){
    message("No hay archivos sin usar.")
    return(invisible(no_usados))
  }
  
  print(data.frame(
    archivo = basename(no_usados),
    ruta = no_usados,
    row.names = NULL
  ))
  
  if(delete_unused){
    
    if(is.null(move_to)){
      
      file.remove(no_usados)
      message(length(no_usados), " archivos eliminados.")
      
    } else {
      
      dir.create(move_to,
                 recursive = TRUE,
                 showWarnings = FALSE)
      
      destino <- file.path(move_to,
                           basename(no_usados))
      
      ok <- file.rename(no_usados, destino)
      
      message(sum(ok), " archivos movidos a ", move_to)
      
    }
    
  }
  
  invisible(no_usados)
  
}