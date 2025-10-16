# ------------------------------------------------------------
# bundle_project(): Liste y agrupe el contenido del proyecto
# Gabriel: base R, reproducible, sin prints innecesarios.
# ------------------------------------------------------------
bundle_project <- function(
    root = ".",
    output = "bundle_for_ai.md",
    include_ext = c("qmd","md","r","rmd","yml","yaml","toml","json","css","scss","js"),
    include_always = c(
      "_quarto.yml",
      "index.qmd",
      "about.qmd",
      "cv.qmd",
      "styles.css",
      "publicaciones/index.qmd",
      "docencia/index.qmd",
      "proyectos/index.qmd"
    ),
    include_globs = c(
      "publicaciones/items/*.qmd",
      "docencia/*/index.qmd",
      "proyectos/*/index.qmd"
    ),
    exclude_dirs = c("_site", ".git", ".Rproj.user", "renv/library", "node_modules", ".quarto", ".cache"),
    max_file_bytes = 200000L,    # límite por archivo (~200 KB)
    max_total_bytes = Inf,       # o pon un número si quieres recortar el total
    encoding_read = "UTF-8"
) {
  # Normaliza root
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(root)
  
  # 1) LISTA DE ARCHIVOS (manifest)
  all_files <- list.files(
    path = ".",
    recursive = TRUE,
    all.files = FALSE,
    include.dirs = FALSE,
    full.names = FALSE
  )
  
  # Excluir directorios no deseados (por prefijo de ruta)
  is_excluded <- function(path) {
    any(startsWith(path, paste0(exclude_dirs, "/"))) ||
      any(exclude_dirs %in% strsplit(path, "/")[[1]])
  }
  files <- all_files[!vapply(all_files, is_excluded, logical(1))]
  
  # Filtra por extensiones de texto + incluye algunos siempre
  ext_of <- function(x) {
    m <- regexpr("\\.([^.]+)$", x)
    ifelse(m > 0, tolower(substring(x, m + 1L)), "")
  }
  keep_by_ext <- ext_of(files) %in% tolower(include_ext)
  files_text <- unique(c(files[keep_by_ext], include_always))
  files_text <- files_text[file.exists(files_text)]
  
  # Expande globs simples tipo "carpeta/*/index.qmd"
  expand_glob <- function(pattern) {
    # Soporte básico para '*' (un segmento)
    if (!grepl("\\*", pattern, fixed = TRUE)) {
      return(if (file.exists(pattern)) pattern else character(0))
    }
    parts <- strsplit(pattern, "/", fixed = TRUE)[[1]]
    # Construye recursivamente
    expand_part <- function(base, idx) {
      if (idx > length(parts)) return(base)
      p <- parts[idx]
      if (p == "*") {
        # lista subdirectorios de 'base'
        subs <- list.dirs(path = if (length(base)) base else ".", recursive = FALSE, full.names = FALSE)
        # mantener solo nombres (no ".")
        subs <- subs[subs != "."]
        if (length(base)) subs <- file.path(base, subs)
        unlist(lapply(subs, expand_part, idx = idx + 1), use.names = FALSE)
      } else {
        nxt <- if (length(base)) file.path(base, p) else p
        if (grepl("\\*", p, fixed = TRUE)) {
          # comodín en el mismo segmento (no implementado: usar pattern en list.files)
          # solución simple: listar en base y filtrar por grepl
          base_dir <- if (length(base)) base else "."
          cand <- list.files(base_dir, full.names = FALSE)
          cand <- cand[grepl(paste0("^", gsub("\\*", ".*", p), "$"), cand)]
          if (length(base)) cand <- file.path(base, cand)
          unlist(lapply(cand, expand_part, idx = idx + 1), use.names = FALSE)
        } else {
          if (file.exists(nxt) || dir.exists(nxt)) expand_part(nxt, idx + 1) else character(0)
        }
      }
    }
    unique(expand_part(character(0), 1))
  }
  
  extra_from_globs <- unique(unlist(lapply(include_globs, expand_glob), use.names = FALSE))
  files_text <- unique(c(files_text, extra_from_globs))
  files_text <- files_text[file.exists(files_text)]
  
  # Orden sugerido: core primero, luego por ruta
  core_order <- c("_quarto.yml", "index.qmd", "about.qmd", "cv.qmd", "styles.css")
  files_text <- unique(c(core_order[core_order %in% files_text],
                         sort(setdiff(files_text, core_order))))
  
  # Manifiesto con tamaño y fecha
  file_info <- function(f) {
    si <- file.info(f)
    c(path = f,
      size = as.numeric(si$size),
      mtime = as.character(si$mtime))
  }
  manifest <- do.call(rbind, lapply(files_text, file_info))
  manifest <- as.data.frame(manifest, stringsAsFactors = FALSE)
  manifest$size <- as.numeric(manifest$size)
  
  # 2) CONSTRUYE BUNDLE MARKDOWN
  lang_for <- function(f) {
    e <- ext_of(f)
    switch(e,
           "qmd"  = "markdown",
           "md"   = "markdown",
           "r"    = "r",
           "rmd"  = "markdown",
           "yml"  = "yaml",
           "yaml" = "yaml",
           "json" = "json",
           "toml" = "toml",
           "css"  = "css",
           "scss" = "scss",
           "js"   = "javascript",
           "")
  }
  
  header <- c(
    "# Bundle del proyecto para IA",
    "",
    paste0("- Generado: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste0("- Raíz: ", normalizePath(".", winslash = "/", mustWork = FALSE)),
    paste0("- Archivos incluidos: ", length(files_text)),
    "",
    "## Manifiesto",
    "",
    "| Archivo | Tamaño (bytes) | Modificado |",
    "|---|---:|---|"
  )
  man_rows <- paste0("| ", manifest$path, " | ", manifest$size, " | ", manifest$mtime, " |")
  
  # Lee archivos (con límites) y arma secciones
  total_bytes <- 0
  sections <- list()
  for (f in files_text) {
    si <- file.info(f)
    if (is.na(si$size)) next
    if (si$size > max_file_bytes) {
      # salta archivos demasiado grandes
      next
    }
    if (total_bytes + si$size > max_total_bytes) break
    
    content <- tryCatch(
      paste(readLines(f, warn = FALSE, encoding = encoding_read), collapse = "\n"),
      error = function(e) sprintf("<<ERROR LEYENDO %s: %s>>", f, conditionMessage(e))
    )
    total_bytes <- total_bytes + nchar(content, type = "bytes")
    
    sections[[length(sections) + 1L]] <- paste0(
      "\n\n---\n\n",
      "## ", f, "\n\n",
      "```", lang_for(f), "\n",
      content, "\n",
      "```\n"
    )
  }
  
  bundle_txt <- paste(c(header, man_rows, unlist(sections)), collapse = "\n")
  
  # 3) ESCRIBE ARCHIVO DE SALIDA
  con <- file(output, open = "wb", encoding = encoding_read)
  on.exit(close(con), add = TRUE)
  writeLines(bundle_txt, con = con, useBytes = TRUE)
  
  # Retorno (no imprime)
  invisible(list(
    manifest = manifest,
    bundle   = bundle_txt,
    output   = normalizePath(output, winslash = "/", mustWork = FALSE)
  ))
}

# -------------------------
# Uso recomendado:
# -------------------------
res <- bundle_project(
  root = ".",
  output = "bundle_for_ai.md",
  max_file_bytes = 200000L,
  max_total_bytes = Inf
)
# res$manifest            # Data frame con el listado
# res$output              # Ruta del markdown generado
# # Para inspeccionar rápidamente:
# # cat(substr(res$bundle, 1, 2000))
