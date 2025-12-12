# app.R
# ===========================================================
# Shiny: Actividad + Temperatura (perfiles ZT, dygraphs, t-tests)
# Versión: UI Paso 2 unificada (configurar columnas una sola vez)
# + MEJORAS: ZT original (si existe), detección intervalo base, trim global, resampling opcional
# ===========================================================

# ---------------------------
# Paquetes (NO instalar aquí)
# ---------------------------
required_pkgs <- c(
  "shiny", "DT", "readr", "readxl",
  "dplyr", "tidyr", "ggplot2",
  "lubridate", "stringr", "scales",
  "dygraphs", "xts", "htmlwidgets",
  "colourpicker", "tibble", "rlang"
)

missing <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop(
    "Faltan paquetes: ", paste(missing, collapse = ", "),
    "\nInstálalos antes de correr la app, por ejemplo:\n",
    "install.packages(c(", paste(sprintf('\"%s\"', missing), collapse = ", "), "))\n"
  )
}

library(shiny)
library(DT)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(scales)
library(dygraphs)
library(xts)
library(htmlwidgets)
library(colourpicker)
library(tibble)
library(rlang)

# ---------------------------
# Configuración global
# ---------------------------
APP_TITLE <- "Análisis de Actividad y Temperatura (ZT)"
DEFAULT_START_DATE <- "2022-01-21 17:00:01"
DEFAULT_INTERVAL_MIN <- 30L

ZT_NIGHT_START <- 12
ZT_NIGHT_END   <- 24

DEFAULT_CONTROL_GROUP_NAME <- "Control"
DEFAULT_CONTROL_GROUP_COLOR <- "#000000"
DEFAULT_EXP_GROUP_PREFIX <- "GrupoExp"
DEFAULT_EXP_GROUP_COLORS <- c(
  "#1f77b4", "#d62728", "#2ca02c", "#9467bd", "#ff7f0e",
  "#8c564b", "#e377c2", "#7f7f7f", "#17becf", "#bcbd22"
)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ===========================================================
# Helpers adicionales para UI intuitiva (detección de columnas)
# ===========================================================

guess_time_col <- function(cols) {
  if (length(cols) == 0) return(NA_character_)
  lc <- tolower(cols)
  idx <- which(
    stringr::str_detect(lc, "fecha") |
      stringr::str_detect(lc, "date")  |
      stringr::str_detect(lc, "hora")  |
      stringr::str_detect(lc, "time")
  )
  if (length(idx) > 0) return(cols[idx[1]])
  NA_character_
}

guess_individual_cols <- function(cols, time_col = NA_character_) {
  if (length(cols) == 0) return(character(0))
  lc <- tolower(cols)
  
  meta_patterns <- c(
    "muestreo", "muestra",
    "hora", "time",
    "fecha", "date",
    "zt",
    "nota", "notas", "comment",
    "promedio", "mean", "avg",
    "grupo", "group",
    "id", "identificador"
  )
  
  is_meta <- rep(FALSE, length(cols))
  for (p in meta_patterns) {
    is_meta <- is_meta | stringr::str_detect(lc, p)
  }
  
  if (!is.na(time_col)) {
    is_meta <- is_meta | cols == time_col
  }
  
  indiv <- cols[!is_meta]
  if (length(indiv) == 0) indiv <- setdiff(cols, time_col)
  indiv
}

# ===========================================================
# Helpers (IO / parsing / validación)
# ===========================================================

read_data_file <- function(file_path) {
  ext <- tools::file_ext(tolower(file_path))
  tryCatch({
    if (ext == "csv") {
      df <- readr::read_csv(
        file_path,
        show_col_types = FALSE,
        col_types = readr::cols(.default = "c"),
        locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
      )
    } else if (ext %in% c("xls", "xlsx")) {
      df <- readxl::read_excel(file_path, col_types = "text")
    } else {
      stop("Formato de archivo no soportado: ", ext, ". Use CSV o Excel (.xlsx/.xls).")
    }
    
    current_names <- names(df)
    new_names <- make.unique(ifelse(is.na(current_names) | current_names == "", "ColumnaSinNombre", current_names))
    names(df) <- new_names
    return(df)
    
  }, error = function(e) {
    stop("Error leyendo archivo: ", e$message)
  })
}

parse_start_datetime <- function(x, tz = "UTC") {
  if (is.null(x) || !nzchar(x)) stop("Fecha de inicio vacía.")
  out <- suppressWarnings(as.POSIXct(x, tz = tz, tryFormats = c(
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%d/%m/%Y %H:%M:%S",
    "%d/%m/%Y %H:%M"
  )))
  if (is.na(out)) stop("Fecha de inicio inválida. Usa 'YYYY-MM-DD HH:MM:SS'.")
  out
}

parse_datetime_col <- function(x, tz = "UTC") {
  if (inherits(x, "POSIXt")) return(lubridate::with_tz(x, tz = tz))
  if (inherits(x, "Date"))   return(as.POSIXct(x, tz = tz))
  
  x_chr <- stringr::str_trim(as.character(x))
  is_num_str <- stringr::str_detect(x_chr, "^[0-9]+(\\.[0-9]+)?$")
  
  out <- rep(as.POSIXct(NA, tz = tz), length(x_chr))
  
  # Excel serial date
  if (any(is_num_str, na.rm = TRUE)) {
    idx_num <- which(is_num_str & !is.na(x_chr))
    if (length(idx_num) > 0) {
      nums <- as.numeric(x_chr[idx_num])
      out[idx_num] <- as.POSIXct(nums * 86400, origin = "1899-12-30", tz = tz)
    }
  }
  
  idx_na <- which(is.na(out) & !is.na(x_chr))
  if (length(idx_na) > 0) {
    parsed <- suppressWarnings(lubridate::parse_date_time(
      x_chr[idx_na],
      orders = c("ymd_HMS","ymd_HM","dmy_HMS","dmy_HM","mdy_HMS","mdy_HM","ymd","dmy"),
      tz = tz,
      exact = FALSE
    ))
    out[idx_na] <- parsed
  }
  
  if (all(is.na(out))) {
    stop("No se pudo detectar formato de fecha en la columna seleccionada. Revisa el archivo.")
  }
  out
}

coerce_numeric_vec <- function(x) {
  if (is.numeric(x)) return(x)
  x_chr <- as.character(x)
  x_chr <- stringr::str_replace_all(x_chr, "\u00A0", "")
  x_chr <- stringr::str_replace_all(x_chr, "\\s+", "")
  
  has_comma <- stringr::str_detect(x_chr, ",")
  has_dot   <- stringr::str_detect(x_chr, "\\.")
  
  x_clean <- dplyr::case_when(
    has_comma & has_dot   ~ stringr::str_replace_all(stringr::str_replace_all(x_chr, "\\.", ""), ",", "."),
    has_comma & !has_dot  ~ stringr::str_replace_all(x_chr, ",", "."),
    TRUE                  ~ x_chr
  )
  
  suppressWarnings(as.numeric(x_clean))
}

add_zt <- function(df, fecha_col = "fecha", zt0, step_minutes = 30L) {
  step_hours <- as.numeric(step_minutes) / 60
  if (step_hours <= 0) stop("Intervalo inválido.")
  delta_h <- as.numeric(difftime(df[[fecha_col]], zt0, units = "secs")) / 3600
  zt <- (delta_h %% 24)
  
  zt <- round(zt / step_hours) * step_hours
  zt[abs(zt - 24) < 1e-10] <- 0
  df$ZT <- zt
  df
}

night_windows_from_zt <- function(min_dt, max_dt, zt0, night_start = 12, night_end = 24) {
  if (!inherits(min_dt, "POSIXt") || !inherits(max_dt, "POSIXt")) stop("min_dt/max_dt deben ser POSIXct.")
  if (is.na(zt0)) stop("zt0 inválido.")
  
  min_h <- as.numeric(difftime(min_dt, zt0, units = "secs")) / 3600
  max_h <- as.numeric(difftime(max_dt, zt0, units = "secs")) / 3600
  
  i_min <- floor(min_h / 24) - 1
  i_max <- ceiling(max_h / 24) + 1
  cycles <- seq(i_min, i_max)
  
  mk <- function(i, from_zt, to_zt) {
    from <- zt0 + lubridate::hours(24 * i + from_zt)
    to   <- zt0 + lubridate::hours(24 * i + to_zt)
    tibble::tibble(from = from, to = to)
  }
  
  if (night_end >= night_start) {
    w <- dplyr::bind_rows(lapply(cycles, mk, from_zt = night_start, to_zt = night_end))
  } else {
    w1 <- dplyr::bind_rows(lapply(cycles, mk, from_zt = night_start, to_zt = 24))
    w2 <- dplyr::bind_rows(lapply(cycles, mk, from_zt = 0, to_zt = night_end))
    w <- dplyr::bind_rows(w1, w2)
  }
  
  w %>%
    dplyr::filter(.data$to >= min_dt, .data$from <= max_dt) %>%
    dplyr::mutate(
      from = pmax(.data$from, min_dt),
      to   = pmin(.data$to, max_dt)
    ) %>%
    dplyr::filter(.data$to > .data$from)
}

# ===========================================================
# NUEVOS HELPERS (mejoras solicitadas)
# ===========================================================

infer_min_interval_minutes <- function(datetime_vec) {
  x <- sort(unique(datetime_vec[!is.na(datetime_vec)]))
  if (length(x) < 2) return(NA_integer_)
  diffs <- as.numeric(diff(x), units = "mins")
  diffs_pos <- diffs[diffs > 0]
  if (length(diffs_pos) == 0) return(NA_integer_)
  as.integer(round(min(diffs_pos)))
}

parse_zt_col <- function(x) {
  z <- coerce_numeric_vec(x)
  if (all(is.na(z))) {
    stop("Columna ZT no se pudo convertir a numérica. Revisa el archivo.")
  }
  z
}

# Trim global por TIEMPO: recorta fechas donde TODAS las variables están NA (en todos los individuos)
trim_data_global <- function(df, fecha_col = "fecha", vars = c("act", "temp")) {
  existing_vars <- intersect(vars, names(df))
  if (length(existing_vars) == 0) return(df)
  if (!fecha_col %in% names(df)) return(df)
  
  df2 <- df %>%
    dplyr::mutate(.has_data_row = rowSums(!is.na(dplyr::across(dplyr::all_of(existing_vars)))) > 0)
  
  by_fecha <- df2 %>%
    dplyr::group_by(.data[[fecha_col]]) %>%
    dplyr::summarise(.has_data_fecha = any(.data$.has_data_row), .groups = "drop") %>%
    dplyr::arrange(.data[[fecha_col]])
  
  if (!any(by_fecha$.has_data_fecha)) return(df)
  
  first_fecha <- by_fecha %>% dplyr::filter(.data$.has_data_fecha) %>% dplyr::slice(1) %>% dplyr::pull(.data[[fecha_col]])
  last_fecha  <- by_fecha %>% dplyr::filter(.data$.has_data_fecha) %>% dplyr::slice(n()) %>% dplyr::pull(.data[[fecha_col]])
  
  df %>%
    dplyr::filter(.data[[fecha_col]] >= first_fecha, .data[[fecha_col]] <= last_fecha)
}

resample_long_data <- function(df,
                               agg_minutes,
                               base_minutes,
                               var_cols = c("act", "temp"),
                               fecha_col = "fecha",
                               zt_col = "ZT",
                               id_col = "original_tratamiento") {
  
  if (is.na(agg_minutes) || is.na(base_minutes) || agg_minutes <= base_minutes) {
    return(df)
  }
  if (agg_minutes %% base_minutes != 0) {
    stop("El intervalo de análisis debe ser múltiplo del intervalo mínimo detectado.")
  }
  if (!fecha_col %in% names(df) || !id_col %in% names(df)) return(df)
  
  var_cols <- intersect(var_cols, names(df))
  if (length(var_cols) == 0) return(df)
  
  if (!zt_col %in% names(df)) df[[zt_col]] <- NA_real_
  
  t0 <- suppressWarnings(min(df[[fecha_col]], na.rm = TRUE))
  if (!is.finite(as.numeric(t0))) return(df)
  
  k <- as.numeric(agg_minutes) * 60
  
  mean_or_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  median_or_na <- function(x) if (all(is.na(x))) NA_real_ else stats::median(x, na.rm = TRUE)
  
  out <- df %>%
    dplyr::mutate(
      .ventana = floor(as.numeric(difftime(.data[[fecha_col]], t0, units = "secs")) / k),
      .fecha_agg = t0 + .data$.ventana * k
    ) %>%
    dplyr::group_by(.data[[id_col]], .data$.fecha_agg) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(var_cols), ~ mean_or_na(.x)),
      .ZT_tmp = median_or_na(.data[[zt_col]]),
      .groups = "drop"
    ) %>%
    dplyr::rename(!!fecha_col := .data$.fecha_agg) %>%
    dplyr::rename(!!zt_col := .data$.ZT_tmp)
  
  out
}

# -----------------------------------------------------------
# LÍNEAS DE PRUEBA (NO rompen la app): activa con:
# Sys.setenv(RUN_HELPER_TESTS="1")
# -----------------------------------------------------------
if (identical(Sys.getenv("RUN_HELPER_TESTS"), "1")) {
  message("[TEST] Ejecutando self-tests de helpers...")
  
  # infer_min_interval_minutes
  dt_test <- as.POSIXct("2022-01-01 00:00:00", tz = "UTC") + c(0, 1800, 3600, 5400)
  stopifnot(identical(infer_min_interval_minutes(dt_test), 30L))
  
  # parse_zt_col
  zt_test <- parse_zt_col(c("0", "0.5", "1", "1.5"))
  stopifnot(is.numeric(zt_test), length(zt_test) == 4)
  
  # trim_data_global (por fecha)
  df_trim <- tibble::tibble(
    fecha = as.POSIXct("2022-01-01 00:00:00", tz = "UTC") + rep(c(0, 60, 120, 180), each = 2),
    original_tratamiento = rep(c("A", "B"), times = 4),
    act = c(NA, NA, NA, NA, 1, NA, NA, NA),
    temp = c(NA, NA, NA, NA, NA, NA, 2, NA)
  )
  df_trim2 <- trim_data_global(df_trim, vars = c("act", "temp"))
  stopifnot(min(df_trim2$fecha) == as.POSIXct("2022-01-01 00:02:00", tz = "UTC"))
  stopifnot(max(df_trim2$fecha) == as.POSIXct("2022-01-01 00:03:00", tz = "UTC"))
  
  # resample_long_data
  df_rs <- tibble::tibble(
    fecha = as.POSIXct("2022-01-01 00:00:00", tz = "UTC") + c(0, 60, 120, 180),
    original_tratamiento = "A",
    ZT = c(0, 0.0167, 0.0333, 0.0500),
    act = c(1, 2, 3, 4),
    temp = c(10, 10, 11, 12)
  )
  df_rs2 <- resample_long_data(df_rs, agg_minutes = 2, base_minutes = 1, var_cols = c("act", "temp"))
  stopifnot(nrow(df_rs2) == 2)
  
  message("[TEST] OK.")
}

# ===========================================================
# Manejo unificado: pasar SIEMPRE a formato largo
# ===========================================================

make_long_dataset <- function(df,
                              dataset_label,
                              format = c("wide", "long"),
                              wide_id_cols = NULL,
                              long_id_col = NULL,
                              long_value_col = NULL) {
  
  format <- match.arg(format)
  
  if (!"ZT" %in% names(df)) df$ZT <- NA_real_
  
  if (format == "wide") {
    if (is.null(wide_id_cols) || length(wide_id_cols) == 0) {
      stop(dataset_label, ": en formato ANCHO debes seleccionar columnas de individuos.")
    }
    missing_cols <- setdiff(wide_id_cols, names(df))
    if (length(missing_cols) > 0) {
      stop(dataset_label, ": columnas no existentes: ", paste(missing_cols, collapse = ", "))
    }
    
    long <- df %>%
      dplyr::select(fecha, ZT, dplyr::all_of(wide_id_cols)) %>%
      dplyr::mutate(across(dplyr::all_of(wide_id_cols), as.character)) %>%
      tidyr::pivot_longer(
        cols      = dplyr::all_of(wide_id_cols),
        names_to  = "original_tratamiento",
        values_to = "value_raw"
      ) %>%
      dplyr::mutate(value = coerce_numeric_vec(.data$value_raw)) %>%
      dplyr::select(fecha, ZT, original_tratamiento, value)
    
    na_rate <- mean(is.na(long$value))
    if (is.nan(na_rate)) na_rate <- 1
    if (na_rate > 0.50) {
      stop(
        dataset_label, ": >50% de valores quedaron NA.\n",
        "Revisa que no hayas seleccionado columnas con texto en lugar de números."
      )
    }
    return(long)
  }
  
  # formato "long"
  if (is.null(long_id_col) || is.null(long_value_col)) {
    stop(dataset_label, ": en formato LARGO debes elegir columna ID y columna valor.")
  }
  if (!long_id_col %in% names(df) || !long_value_col %in% names(df)) {
    stop(dataset_label, ": columnas ID/valor no existen en el dataset.")
  }
  
  long <- df %>%
    dplyr::transmute(
      fecha = .data$fecha,
      ZT    = .data$ZT,
      original_tratamiento = as.character(.data[[long_id_col]]),
      value = coerce_numeric_vec(.data[[long_value_col]])
    )
  
  if (any(is.na(long$original_tratamiento)) || any(!nzchar(long$original_tratamiento))) {
    stop(dataset_label, ": hay IDs vacíos/NA en la columna seleccionada.")
  }
  
  na_rate <- mean(is.na(long$value))
  if (is.nan(na_rate)) na_rate <- 1
  if (na_rate > 0.50) {
    stop(
      dataset_label, ": >50% de valores quedaron NA.\n",
      "Revisa que la columna de valor realmente contenga números."
    )
  }
  
  long
}

process_dataset <- function(raw, dataset_label,
                            format = c("wide","long"),
                            datetime_mode = c("use_col","generate"),
                            datetime_col = NULL,
                            start_dt = NULL,
                            interval_min = 30L,
                            wide_id_cols = NULL,
                            long_id_col = NULL,
                            long_value_col = NULL,
                            zt_col = NULL,            # <-- NUEVO (opcional)
                            tz = "UTC") {
  
  format <- match.arg(format)
  datetime_mode <- match.arg(datetime_mode)
  
  df <- as.data.frame(raw)
  
  # 1) Construir / parsear fecha
  if (datetime_mode == "use_col") {
    if (is.null(datetime_col) || !datetime_col %in% names(df)) {
      stop(dataset_label, ": debes seleccionar una columna de fecha válida.")
    }
    df$fecha <- parse_datetime_col(df[[datetime_col]], tz = tz)
  } else {
    if (is.null(start_dt) || !inherits(start_dt, "POSIXt")) stop("start_dt inválido.")
    df$fecha <- seq(from = start_dt, by = lubridate::minutes(interval_min), length.out = nrow(df))
  }
  
  # 2) Eliminar filas sin fecha válida
  df <- df %>% dplyr::filter(!is.na(fecha))
  
  # 2bis) ZT original (si existe y el usuario seleccionó columna)
  if (!is.null(zt_col) && nzchar(zt_col) && zt_col %in% names(df)) {
    df$ZT <- parse_zt_col(df[[zt_col]])
  } else {
    df$ZT <- NA_real_
  }
  
  # 3) Convertir a formato largo
  long <- make_long_dataset(
    df = df,
    dataset_label = dataset_label,
    format = format,
    wide_id_cols = wide_id_cols,
    long_id_col = long_id_col,
    long_value_col = long_value_col
  )
  
  long
}

# ===========================================================
# Estadística (t-tests)
# ===========================================================
compute_ttests_by_zt <- function(data_individual_means,
                                 control_group_name,
                                 experimental_groups,
                                 variable,
                                 var_prefix) {
  req_cols <- c("user_group", "individuo", "ZT", variable)
  if (!all(req_cols %in% names(data_individual_means))) {
    stop("T-test: faltan columnas requeridas.")
  }
  
  control_data <- data_individual_means %>% dplyr::filter(.data$user_group == control_group_name)
  if (nrow(control_data) == 0) stop("T-test: no hay datos para el grupo control.")
  
  available_zt <- sort(unique(data_individual_means$ZT))
  out <- list()
  
  for (exp_group in experimental_groups) {
    exp_data <- data_individual_means %>% dplyr::filter(.data$user_group == exp_group)
    if (nrow(exp_data) == 0) next
    
    rows <- vector("list", length(available_zt))
    pvals <- rep(NA_real_, length(available_zt))
    
    for (k in seq_along(available_zt)) {
      zt_val <- available_zt[k]
      control_vals <- control_data %>%
        dplyr::filter(.data$ZT == zt_val) %>%
        dplyr::pull(!!rlang::sym(variable)) %>%
        stats::na.omit()
      exp_vals <- exp_data %>%
        dplyr::filter(.data$ZT == zt_val) %>%
        dplyr::pull(!!rlang::sym(variable)) %>%
        stats::na.omit()
      
      mean_control <- mean(control_vals, na.rm = TRUE)
      sd_control   <- stats::sd(control_vals, na.rm = TRUE)
      n_control    <- length(control_vals)
      sem_control  <- if (n_control > 1) sd_control / sqrt(n_control) else NA_real_
      
      mean_exp <- mean(exp_vals, na.rm = TRUE)
      sd_exp   <- stats::sd(exp_vals, na.rm = TRUE)
      n_exp    <- length(exp_vals)
      sem_exp  <- if (n_exp > 1) sd_exp / sqrt(n_exp) else NA_real_
      
      p_value <- NA_real_
      if (n_control >= 2 && n_exp >= 2) {
        tt <- tryCatch(stats::t.test(control_vals, exp_vals), error = function(e) NULL)
        if (!is.null(tt)) p_value <- as.numeric(tt$p.value)
      }
      pvals[k] <- p_value
      
      rows[[k]] <- tibble::tibble(
        ZT = zt_val,
        !!paste0(var_prefix, "_mean_", control_group_name) := mean_control,
        !!paste0(var_prefix, "_sd_",   control_group_name) := sd_control,
        !!paste0(var_prefix, "_n_",    control_group_name) := n_control,
        !!paste0(var_prefix, "_sem_",  control_group_name) := sem_control,
        !!paste0(var_prefix, "_mean_", exp_group) := mean_exp,
        !!paste0(var_prefix, "_sd_",   exp_group) := sd_exp,
        !!paste0(var_prefix, "_n_",    exp_group) := n_exp,
        !!paste0(var_prefix, "_sem_",  exp_group) := sem_exp,
        p_value = p_value
      )
    }
    
    df <- dplyr::bind_rows(rows) %>% dplyr::arrange(.data$ZT)
    
    p_adj <- rep(NA_real_, nrow(df))
    idx <- which(!is.na(df$p_value))
    if (length(idx) > 0) p_adj[idx] <- stats::p.adjust(df$p_value[idx], method = "BH")
    df$p_adj_BH <- p_adj
    
    signif_stars <- function(p) dplyr::case_when(
      is.na(p) ~ "",
      p < 0.0001 ~ "****",
      p < 0.001  ~ "***",
      p < 0.01   ~ "**",
      p < 0.05   ~ "*",
      TRUE ~ ""
    )
    
    df$signif_raw <- signif_stars(df$p_value)
    df$signif_BH  <- signif_stars(df$p_adj_BH)
    
    out[[paste0(exp_group, "_vs_", control_group_name)]] <- df
  }
  
  out
}

# ===========================================================
# Gráficas (perfiles)
# ===========================================================
plot_profile_graph <- function(data_group_means, var_prefix, titulo, etiqueta_y, grupos_config_list) {
  mean_col <- paste0(var_prefix, "_promedio_grupo")
  sem_col  <- paste0(var_prefix, "_sem_grupo")
  
  if (nrow(data_group_means) == 0) return(ggplot() + theme_void() + labs(title="Sin datos"))
  
  group_names_in_data <- unique(data_group_means$user_group)
  valid_configs <- Filter(function(g) g$nombre %in% group_names_in_data, grupos_config_list)
  if (length(valid_configs) == 0) stop("Perfil: configuración de grupos no coincide con datos.")
  
  group_colors <- setNames(
    vapply(valid_configs, `[[`, character(1), "color"),
    vapply(valid_configs, `[[`, character(1), "nombre")
  )
  
  df <- data_group_means %>%
    dplyr::filter(.data$user_group %in% names(group_colors))
  
  if (nrow(df) == 0) return(ggplot() + labs(title = "Sin datos filtrados") + theme_void())
  
  df_plot <- df
  if (any(abs(df_plot$ZT - 0) < 1e-10, na.rm = TRUE)) {
    df_plot <- dplyr::bind_rows(
      df_plot,
      df_plot %>% dplyr::filter(abs(.data$ZT - 0) < 1e-10) %>% dplyr::mutate(ZT = 24)
    )
  }
  
  y_min <- suppressWarnings(min(df_plot[[mean_col]] - df_plot[[sem_col]], na.rm = TRUE))
  y_max <- suppressWarnings(max(df_plot[[mean_col]] + df_plot[[sem_col]], na.rm = TRUE))
  if (!is.finite(y_min)) y_min <- 0
  if (!is.finite(y_max)) y_max <- 1
  if (y_max == y_min) { y_min <- y_min - 0.5; y_max <- y_max + 0.5 }
  
  p <- ggplot(df_plot, aes(x = .data$ZT, colour = .data$user_group, fill = .data$user_group)) +
    geom_rect(aes(xmin = ZT_NIGHT_START, xmax = ZT_NIGHT_END, ymin = -Inf, ymax = Inf),
              fill = "grey80", alpha = 0.25, inherit.aes = FALSE) +
    geom_ribbon(aes(ymin = .data[[mean_col]] - .data[[sem_col]],
                    ymax = .data[[mean_col]] + .data[[sem_col]]),
                alpha = 0.20, colour = NA, show.legend = FALSE) +
    geom_line(aes(y = .data[[mean_col]]), linewidth = 1.1) +
    geom_point(aes(y = .data[[mean_col]]), size = 1.8, show.legend = FALSE) +
    labs(title = titulo, x = "ZT (Zeitgeber Time)", y = etiqueta_y, colour = "Grupo") +
    scale_x_continuous(breaks = seq(0, 24, by = 3), limits = c(0, 24), expand = c(0, 0)) +
    scale_y_continuous(limits = c(y_min, y_max), labels = scales::label_number(accuracy = 0.1)) +
    scale_colour_manual(values = group_colors) +
    scale_fill_manual(values = group_colors, guide = "none") +
    theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, size = rel(1.3), face = "bold", margin = margin(b = 10)),
      legend.position = "top",
      axis.ticks.length = grid::unit(4, "pt"),
      panel.grid.major = element_line(color = "grey92", linewidth = 0.4),
      panel.grid.minor = element_blank()
    )
  
  p
}

# ===========================================================
# Dygraphs
# ===========================================================
create_all_individuals_dygraph <- function(data, variable, zt0,
                                           night_start, night_end,
                                           series_color_map = NULL,
                                           group_id = APP_TITLE) {
  req_cols <- c("fecha", "original_tratamiento", variable)
  if (!all(req_cols %in% names(data))) return(NULL)
  
  df <- data %>%
    dplyr::select(fecha, original_tratamiento, dplyr::all_of(variable)) %>%
    dplyr::filter(!is.na(.data[[variable]])) %>%
    dplyr::arrange(.data$fecha)
  
  if (nrow(df) == 0) return(NULL)
  
  df <- df %>%
    dplyr::group_by(.data$fecha, .data$original_tratamiento) %>%
    dplyr::summarise(value = mean(.data[[variable]], na.rm = TRUE), .groups = "drop")
  
  individuals <- unique(df$original_tratamiento)
  xts_list <- vector("list", length(individuals))
  names(xts_list) <- individuals
  
  for (ind in individuals) {
    dfi <- df %>%
      dplyr::filter(.data$original_tratamiento == ind) %>%
      dplyr::arrange(.data$fecha)
    
    if (any(duplicated(dfi$fecha))) {
      dfi <- dfi %>% dplyr::distinct(fecha, .keep_all = TRUE)
    }
    if (nrow(dfi) > 0) {
      ind_xts <- xts::xts(dfi$value, order.by = dfi$fecha)
      colnames(ind_xts) <- ind
      xts_list[[ind]] <- ind_xts
    }
  }
  
  xts_list <- xts_list[!vapply(xts_list, is.null, logical(1))]
  if (length(xts_list) == 0) return(NULL)
  
  all_xts <- do.call(merge, xts_list)
  
  g <- dygraphs::dygraph(all_xts, main = paste0(stringr::str_to_title(variable), " individual"), group = group_id) %>%
    dygraphs::dyOptions(
      fillGraph = TRUE, fillAlpha = 0.12,
      drawGrid = TRUE,
      strokeWidth = 1.0,
      drawPoints = FALSE,
      connectSeparatedPoints = TRUE,
      useDataTimezone = TRUE
    ) %>%
    dygraphs::dyAxis("x", label = "Fecha y hora (UTC)") %>%
    dygraphs::dyAxis("y", label = stringr::str_to_title(variable)) %>%
    dygraphs::dyRangeSelector(height = 30) %>%
    dygraphs::dyCrosshair(direction = "both") %>%
    dygraphs::dyHighlight(
      highlightCircleSize = 4,
      highlightSeriesBackgroundAlpha = 0.25,
      hideOnMouseOut = TRUE,
      highlightSeriesOpts = list(strokeWidth = 2.5)
    ) %>%
    dygraphs::dyLegend(show = "onmouseover", width = 260, showZeroValues = FALSE)
  
  if (!is.null(series_color_map) && length(series_color_map) > 0) {
    for (s in colnames(all_xts)) {
      if (!is.null(series_color_map[[s]])) {
        g <- g %>% dygraphs::dySeries(s, label = s, color = series_color_map[[s]])
      }
    }
  }
  
  min_dt <- min(df$fecha, na.rm = TRUE)
  max_dt <- max(df$fecha, na.rm = TRUE)
  windows <- night_windows_from_zt(min_dt, max_dt, zt0, night_start = night_start, night_end = night_end)
  if (nrow(windows) > 0) {
    for (i in seq_len(nrow(windows))) {
      g <- g %>% dygraphs::dyShading(from = windows$from[i], to = windows$to[i], color = "#F0F0F0")
    }
  }
  
  g
}

# ===========================================================
# ZIP helper
# ===========================================================
zip_folder <- function(folder, zipfile) {
  files <- list.files(folder, recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) stop("No hay archivos para comprimir.")
  
  if (requireNamespace("zip", quietly = TRUE)) {
    old <- getwd()
    on.exit(setwd(old), add = TRUE)
    setwd(folder)
    rel <- list.files(".", recursive = TRUE, all.files = TRUE, include.dirs = FALSE)
    zip::zip(zipfile = zipfile, files = rel, mode = "cherry-pick")
  } else {
    old <- getwd()
    on.exit(setwd(old), add = TRUE)
    setwd(folder)
    rel <- list.files(".", recursive = TRUE, all.files = TRUE, include.dirs = FALSE)
    utils::zip(zipfile, files = rel)
  }
  zipfile
}

# ===========================================================
# UI
# ===========================================================
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body { background-color: #f4f6f9; font-family: 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; }
      .navbar-default { box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
      .sidebar-container { max-height: 90vh; overflow-y: auto; padding-right: 10px; }
      .step-card {
        background: #ffffff;
        border-radius: 8px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.05);
        padding: 15px;
        margin-bottom: 20px;
        border-left: 5px solid #3c8dbc;
      }
      .step-card h4 {
        margin-top: 0; color: #333; font-weight: 600; font-size: 16px;
        border-bottom: 1px solid #eee; padding-bottom: 8px; margin-bottom: 15px;
      }
      .btn-action { width: 100%; margin-top: 5px; font-weight: bold; }
      .btn-primary { background-color: #3c8dbc; border-color: #367fa9; }
      .btn-primary:hover { background-color: #367fa9; }
      .btn-success { background-color: #00a65a; border-color: #008d4c; }
      .btn-danger { background-color: #d73925; border-color: #ac2925; color: white; }
      .status-msg { margin-top: 10px; font-size: 13px; }
      .main-title { color: #3c8dbc; font-weight: 700; margin-bottom: 20px; }
      .config-help { font-size: 12px; color: #555; margin-bottom: 6px; }
    "))
  ),
  
  div(class="container-fluid",
      h2(APP_TITLE, class="main-title")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      class = "sidebar-container",
      
      # --- PASO 1 ---
      div(class = "step-card",
          h4(icon("upload"), "1. Carga de Datos"),
          fileInput("actividadFile", "Actividad (CSV/XLSX)", accept = c(".csv",".xlsx",".xls")),
          fileInput("temperaturaFile", "Temperatura (CSV/XLSX)", accept = c(".csv",".xlsx",".xls")),
          
          fluidRow(
            column(7, dateInput("dateInputPart", "Fecha Inicio", value = as.Date(DEFAULT_START_DATE))),
            column(5, textInput("timeInputPart", "Hora (UTC)", value = format(as.POSIXct(DEFAULT_START_DATE), "%H:%M:%S")))
          ),
          
          # Se actualizará automáticamente si detectamos intervalo base
          selectInput("intervalMin", "Intervalo de análisis (minutos)", choices = c(5,10,15,30,60), selected = DEFAULT_INTERVAL_MIN),
          
          actionButton("readFiles", "Leer Archivos", icon = icon("file-import"), class = "btn-primary btn-action"),
          div(class="status-msg", uiOutput("readMsg"))
      ),
      
      # --- PASO 2 ---
      conditionalPanel(
        condition = "output.filesReady === true",
        div(class = "step-card",
            h4(icon("table"), "2. Configurar columnas (solo una vez)"),
            
            p(class = "config-help",
              "Tus archivos de Actividad y Temperatura tienen la misma estructura: mismas ratas, mismas columnas. ",
              "Aquí eliges una sola vez cómo interpretarlos, y se aplica a todos los archivos."
            ),
            
            radioButtons(
              "dataFormat",
              "¿Cómo están tus ratas en los archivos?",
              choices = c(
                "Cada columna es un individuo (APS1, CTL3, RPS2...)" = "wide",
                "Tengo una columna 'ID' y otra 'Valor' (formato largo)" = "long"
              ),
              selected = "wide",
              inline = FALSE
            ),
            
            uiOutput("globalConfigUI"),
            
            br(),
            actionButton("processAndJoin", "Procesar y Unir", icon = icon("cogs"), class = "btn-primary btn-action"),
            div(class="status-msg", uiOutput("procMsg"))
        )
      ),
      
      # --- PASO 3 ---
      div(class = "step-card",
          h4(icon("calendar-alt"), "3. Filtro de Fechas"),
          uiOutput("dateRangeUI"),
          conditionalPanel(
            condition = "input.processAndJoin > 0",
            actionButton("applyDates", "Aplicar Filtro", icon = icon("filter"), class = "btn-primary btn-action"),
            div(class="status-msg", uiOutput("dateMsg"))
          )
      ),
      
      # --- PASO 4 ---
      div(class = "step-card",
          h4(icon("users"), "4. Definición de Grupos"),
          strong("Grupo Control"),
          fluidRow(
            column(8, textInput("controlGroupName", NULL, value = DEFAULT_CONTROL_GROUP_NAME, placeholder="Nombre Control")),
            column(4, colourInput("controlGroupColor", NULL, value = DEFAULT_CONTROL_GROUP_COLOR))
          ),
          uiOutput("controlGroupSelectUI"),
          hr(style="margin: 10px 0;"),
          strong("Grupos Experimentales"),
          numericInput("numExpGrupos", "Cantidad:", value = 2, min = 1, max = 10),
          uiOutput("expGroupsConfigUI"),
          uiOutput("expGroupsSelectUI"),
          actionButton("validateGroups", "Validar Grupos", icon = icon("check-circle"), class = "btn-primary btn-action"),
          div(class="status-msg", uiOutput("groupMsg"))
      ),
      
      # --- PASO 5 ---
      div(class = "step-card", style = "border-left-color: #00a65a;",
          h4(icon("chart-line"), "5. Ejecución"),
          actionButton("generateResults", "Generar Resultados", icon = icon("rocket"), class = "btn-success btn-action"),
          div(class="status-msg", uiOutput("genMsg")),
          hr(),
          downloadButton("downloadZip", "Descargar Todo (.zip)", class = "btn-default btn-action")
      ),
      
      hr(),
      actionButton(
        "quitApp", "Cerrar Aplicación", icon = icon("power-off"),
        class = "btn-danger btn-action",
        onclick = "setTimeout(function(){window.close();}, 500);"
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "mainTabs",
        type = "tabs",
        tabPanel("Resumen", icon = icon("list-alt"), br(), tableOutput("groupSummary")),
        tabPanel("Perfil Actividad", icon = icon("chart-area"), br(), plotOutput("perfilActividad", height = "550px")),
        tabPanel("Perfil Temperatura", icon = icon("thermometer-half"), br(), plotOutput("perfilTemperatura", height = "550px")),
        tabPanel("T-Test Actividad", icon = icon("calculator"), br(), DT::dataTableOutput("ttAct")),
        tabPanel("T-Test Temperatura", icon = icon("calculator"), br(), DT::dataTableOutput("ttTemp")),
        tabPanel("Dygraph Actividad", icon = icon("chart-bar"), br(), dygraphOutput("dyAct", height = "600px")),
        tabPanel("Dygraph Temperatura", icon = icon("chart-bar"), br(), dygraphOutput("dyTemp", height = "600px"))
      )
    )
  )
)

# ===========================================================
# Server
# ===========================================================
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    raw_act = NULL,
    raw_temp = NULL,
    combined = NULL,
    filtered = NULL,
    individuals = character(0),
    validated_sig = NULL,
    bundle_dir = NULL,
    bundle_zip = NULL,
    last_results = NULL,
    
    # NUEVO: intervalo base detectado y referencia zt0 para sombreado
    base_interval_min = NA_integer_,
    zt0_ref = NULL
  )
  
  output$filesReady <- reactive({ !is.null(rv$raw_act) || !is.null(rv$raw_temp) })
  outputOptions(output, "filesReady", suspendWhenHidden = FALSE)
  
  observeEvent(input$quitApp, {
    showNotification("Cerrando aplicación y deteniendo proceso de R...", type = "error", duration = 2)
    Sys.sleep(0.5)
    stopApp()
  })
  
  # -----------------------------------------
  # Paso 1: leer
  # -----------------------------------------
  observeEvent(input$readFiles, {
    output$readMsg <- renderUI(tagList(icon("spinner", class="fa-spin"), " Leyendo..."))
    rv$raw_act <- NULL
    rv$raw_temp <- NULL
    rv$combined <- NULL
    rv$filtered <- NULL
    rv$bundle_dir <- NULL
    rv$bundle_zip <- NULL
    rv$last_results <- NULL
    rv$validated_sig <- NULL
    rv$base_interval_min <- NA_integer_
    rv$zt0_ref <- NULL
    
    tryCatch({
      if (!is.null(input$actividadFile)) rv$raw_act <- read_data_file(input$actividadFile$datapath)
      if (!is.null(input$temperaturaFile)) rv$raw_temp <- read_data_file(input$temperaturaFile$datapath)
      if (is.null(rv$raw_act) && is.null(rv$raw_temp)) stop("Carga al menos un archivo (actividad y/o temperatura).")
      
      # Intento 1 (temprano): detectar intervalo base usando una columna de fecha “probable”
      dfs <- list()
      if (!is.null(rv$raw_act))  dfs$Actividad <- as.data.frame(rv$raw_act)
      if (!is.null(rv$raw_temp)) dfs$Temperatura <- as.data.frame(rv$raw_temp)
      
      common_cols <- Reduce(intersect, lapply(dfs, names))
      if (length(common_cols) == 0) common_cols <- unique(unlist(lapply(dfs, names)))
      
      guessed_dt_col <- guess_time_col(common_cols)
      base_int <- NA_integer_
      
      if (!is.na(guessed_dt_col)) {
        df_ref <- if (!is.null(rv$raw_act)) as.data.frame(rv$raw_act) else as.data.frame(rv$raw_temp)
        if (guessed_dt_col %in% names(df_ref)) {
          base_int <- tryCatch({
            fechas_tmp <- parse_datetime_col(df_ref[[guessed_dt_col]], tz = "UTC")
            infer_min_interval_minutes(fechas_tmp)
          }, error = function(e) NA_integer_)
        }
      }
      
      if (!is.na(base_int) && base_int > 0) {
        rv$base_interval_min <- base_int
        choices <- sort(unique(c(base_int, seq(base_int, 240, by = base_int))))
        updateSelectInput(session, "intervalMin", choices = choices, selected = base_int)
      }
      
      output$readMsg <- renderUI(tagList(icon("check", style="color:green;"), " Archivos leídos. Configura columnas y procesa."))
    }, error = function(e) {
      output$readMsg <- renderUI(tagList(icon("times", style="color:red;"), " Error: ", e$message))
    })
  })
  
  # -----------------------------------------
  # UI dinámica: configuración global de columnas
  # -----------------------------------------
  output$globalConfigUI <- renderUI({
    dfs <- list()
    if (!is.null(rv$raw_act))  dfs$Actividad   <- as.data.frame(rv$raw_act)
    if (!is.null(rv$raw_temp)) dfs$Temperatura <- as.data.frame(rv$raw_temp)
    
    req(length(dfs) > 0)
    
    all_cols_list <- lapply(dfs, names)
    common_cols <- Reduce(intersect, all_cols_list)
    if (length(common_cols) == 0) {
      common_cols <- unique(unlist(all_cols_list))
    }
    
    fecha_default <- guess_time_col(common_cols)
    indiv_guess   <- guess_individual_cols(common_cols, time_col = fecha_default)
    
    choices_wide <- indiv_guess
    if (length(choices_wide) == 0) {
      choices_wide <- setdiff(common_cols, fecha_default)
    }
    
    zt_default <- common_cols[stringr::str_detect(tolower(common_cols), "zt")][1]
    if (is.na(zt_default)) zt_default <- ""
    
    zt_choices <- c("(No hay ZT; calcular con Inicio+Intervalo)" = "", common_cols)
    
    tagList(
      p(class = "config-help",
        "1) Elige una vez de dónde saldrá la fecha/hora para TODOS los archivos."
      ),
      radioButtons(
        "datetimeMode", NULL,
        choices = c(
          "Usar una columna de fecha/hora que ya viene en los archivos" = "use_col",
          "No hay columna de tiempo: generar con 'Fecha Inicio' + intervalo" = "generate"
        ),
        selected = if (!is.na(fecha_default)) "use_col" else "generate",
        inline = FALSE
      ),
      
      conditionalPanel(
        condition = "input.datetimeMode === 'use_col'",
        selectInput(
          "datetimeCol",
          "Columna de fecha/hora (debe existir en TODOS los archivos):",
          choices = common_cols,
          selected = fecha_default
        )
      ),
      
      p(class = "config-help",
        "ZT: si tus archivos ya traen columna ZT, selecciónala aquí para NO recalcularla."
      ),
      selectInput(
        "ztCol",
        "Columna ZT (Zeitgeber Time) en TODOS los archivos:",
        choices = zt_choices,
        selected = zt_default
      ),
      
      conditionalPanel(
        condition = "input.dataFormat === 'wide'",
        p(class = "config-help",
          "2) Marca qué columnas son individuos (APS, RPS, CTL...). ",
          "Aquí ya filtramos columnas obvias de meta-datos (Muestreo, ZT, Notas, Promedio...)."
        ),
        checkboxGroupInput(
          "wideCols",
          "Columnas que corresponden a individuos:",
          choices  = choices_wide,
          selected = choices_wide
        )
      ),
      
      conditionalPanel(
        condition = "input.dataFormat === 'long'",
        tagList(
          p(class = "config-help",
            "2) Formato largo: todos los archivos comparten la misma columna ID de individuo, ",
            "pero cada uno tiene su propia columna de valor (Actividad vs Temperatura)."
          ),
          selectInput(
            "idCol",
            "Columna ID del individuo (común a TODOS los archivos):",
            choices = common_cols,
            selected = if (length(common_cols) > 0) common_cols[1] else NULL
          ),
          if (!is.null(rv$raw_act)) {
            selectInput(
              "actValueCol",
              "Columna VALOR de ACTIVIDAD:",
              choices = names(as.data.frame(rv$raw_act)),
              selected = tail(names(as.data.frame(rv$raw_act)), 1)
            )
          },
          if (!is.null(rv$raw_temp)) {
            selectInput(
              "tempValueCol",
              "Columna VALOR de TEMPERATURA:",
              choices = names(as.data.frame(rv$raw_temp)),
              selected = tail(names(as.data.frame(rv$raw_temp)), 1)
            )
          }
        )
      )
    )
  })
  
  # -----------------------------------------
  # Re-detectar intervalo base cuando el usuario elige datetimeCol (use_col)
  # y actualizar intervalMin a múltiplos hasta 4h.
  # -----------------------------------------
  observeEvent(list(input$datetimeMode, input$datetimeCol), {
    if (is.null(rv$raw_act) && is.null(rv$raw_temp)) return()
    if (!identical(input$datetimeMode, "use_col")) return()
    if (is.null(input$datetimeCol) || !nzchar(input$datetimeCol)) return()
    
    df_ref <- if (!is.null(rv$raw_act)) as.data.frame(rv$raw_act) else as.data.frame(rv$raw_temp)
    if (!input$datetimeCol %in% names(df_ref)) return()
    
    base_int <- tryCatch({
      fechas_tmp <- parse_datetime_col(df_ref[[input$datetimeCol]], tz = "UTC")
      infer_min_interval_minutes(fechas_tmp)
    }, error = function(e) NA_integer_)
    
    if (!is.na(base_int) && base_int > 0) {
      rv$base_interval_min <- base_int
      choices <- sort(unique(c(base_int, seq(base_int, 240, by = base_int))))
      current_sel <- suppressWarnings(as.integer(isolate(input$intervalMin)))
      new_sel <- if (!is.na(current_sel) && current_sel %in% choices) current_sel else base_int
      updateSelectInput(session, "intervalMin", choices = choices, selected = new_sel)
    }
  }, ignoreInit = TRUE)
  
  # -----------------------------------------
  # Paso 2: procesar y unir
  # -----------------------------------------
  observeEvent(input$processAndJoin, {
    req(input$dateInputPart, input$timeInputPart, input$intervalMin)
    output$procMsg <- renderUI(tagList(icon("spinner", class="fa-spin"), " Procesando..."))
    
    tryCatch({
      full_date_str <- paste(format(input$dateInputPart, "%Y-%m-%d"), input$timeInputPart)
      start_dt <- parse_start_datetime(full_date_str, tz = "UTC")
      
      interval_min <- as.integer(input$intervalMin) # intervalo de ANÁLISIS (puede ser > base)
      base_interval <- if (!is.na(rv$base_interval_min) && rv$base_interval_min > 0) rv$base_interval_min else interval_min
      
      zt_col_selected <- input$ztCol %||% ""
      if (!nzchar(zt_col_selected)) zt_col_selected <- NULL
      
      act_long <- NULL
      temp_long <- NULL
      
      withProgress(message = "Procesando datasets", value = 0, {
        
        if (!is.null(rv$raw_act)) {
          incProgress(0.3, detail = "Actividad")
          act_long <- process_dataset(
            raw = rv$raw_act,
            dataset_label = "Actividad",
            format = input$dataFormat,
            datetime_mode = input$datetimeMode,
            datetime_col = if (identical(input$datetimeMode, "use_col")) input$datetimeCol else NULL,
            start_dt = start_dt,
            interval_min = base_interval, # base si se genera; si use_col no afecta
            wide_id_cols = if (identical(input$dataFormat, "wide")) input$wideCols else NULL,
            long_id_col = if (identical(input$dataFormat, "long")) input$idCol else NULL,
            long_value_col = if (identical(input$dataFormat, "long")) input$actValueCol else NULL,
            zt_col = zt_col_selected,
            tz = "UTC"
          ) %>% dplyr::rename(act = value)
        }
        
        if (!is.null(rv$raw_temp)) {
          incProgress(0.3, detail = "Temperatura")
          temp_long <- process_dataset(
            raw = rv$raw_temp,
            dataset_label = "Temperatura",
            format = input$dataFormat,
            datetime_mode = input$datetimeMode,
            datetime_col = if (identical(input$datetimeMode, "use_col")) input$datetimeCol else NULL,
            start_dt = start_dt,
            interval_min = base_interval,
            wide_id_cols = if (identical(input$dataFormat, "wide")) input$wideCols else NULL,
            long_id_col = if (identical(input$dataFormat, "long")) input$idCol else NULL,
            long_value_col = if (identical(input$dataFormat, "long")) input$tempValueCol else NULL,
            zt_col = zt_col_selected,
            tz = "UTC"
          ) %>% dplyr::rename(temp = value)
        }
        
        incProgress(0.2, detail = "Uniendo")
        
        # Join robusto: por fecha + individuo; luego unificamos ZT (coalesce)
        if (!is.null(act_long) && !is.null(temp_long)) {
          combined <- dplyr::full_join(
            act_long, temp_long,
            by = c("fecha", "original_tratamiento"),
            suffix = c("_act", "_temp")
          )
          
          # Coalesce de ZT si viene de ambos lados
          if ("ZT_act" %in% names(combined) && "ZT_temp" %in% names(combined)) {
            combined <- combined %>%
              dplyr::mutate(ZT = dplyr::coalesce(.data$ZT_act, .data$ZT_temp)) %>%
              dplyr::select(-.data$ZT_act, -.data$ZT_temp)
          }
        } else if (!is.null(act_long)) {
          combined <- act_long
          combined$temp <- NA_real_
          if (!"ZT" %in% names(combined)) combined$ZT <- NA_real_
        } else if (!is.null(temp_long)) {
          combined <- temp_long
          combined$act <- NA_real_
          if (!"ZT" %in% names(combined)) combined$ZT <- NA_real_
        } else {
          stop("No hay datos procesados.")
        }
        
        incProgress(0.2, detail = "Normalizando")
        
        mean_or_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
        
        combined <- combined %>%
          dplyr::group_by(.data$fecha, .data$ZT, .data$original_tratamiento) %>%
          dplyr::summarise(
            act  = mean_or_na(.data$act),
            temp = mean_or_na(.data$temp),
            .groups = "drop"
          ) %>%
          dplyr::arrange(.data$fecha, .data$original_tratamiento)
        
        # Si NO hay ZT (o hay huecos), calculamos SOLO para NA (fallback)
        if (all(is.na(combined$ZT))) {
          combined <- add_zt(combined, fecha_col = "fecha", zt0 = start_dt, step_minutes = base_interval)
        } else if (any(is.na(combined$ZT))) {
          tmp <- add_zt(combined, fecha_col = "fecha", zt0 = start_dt, step_minutes = base_interval)
          combined$ZT <- dplyr::coalesce(combined$ZT, tmp$ZT)
        }
        
        combined <- combined %>%
          dplyr::mutate(individuo = .data$original_tratamiento)
        
        if (nrow(combined) == 0) stop("Datos combinados vacíos.")
        
        # Trim global de bordes (por fecha) manteniendo NAs intermedios
        combined <- trim_data_global(combined, fecha_col = "fecha", vars = c("act", "temp"))
        
        # Resampling opcional: si intervalo de análisis > intervalo base detectado
        combined <- resample_long_data(
          df = combined,
          agg_minutes = interval_min,
          base_minutes = base_interval,
          var_cols = c("act", "temp"),
          fecha_col = "fecha",
          zt_col = "ZT",
          id_col = "original_tratamiento"
        ) %>%
          dplyr::mutate(individuo = .data$original_tratamiento)
        
        # Referencia zt0 para sombreado dygraphs (si ZT vino de archivo)
        if (any(!is.na(combined$ZT))) {
          zt0_candidates <- combined$fecha[!is.na(combined$ZT)] - lubridate::hours(combined$ZT[!is.na(combined$ZT)])
          rv$zt0_ref <- suppressWarnings(min(zt0_candidates, na.rm = TRUE))
        } else {
          rv$zt0_ref <- start_dt
        }
        
        rv$combined <- combined
      })
      
      # Filtro de fechas
      min_day <- as.Date(min(rv$combined$fecha, na.rm = TRUE))
      max_day <- as.Date(max(rv$combined$fecha, na.rm = TRUE))
      
      picker_max <- max_day
      if (min_day == max_day) {
        picker_max <- max_day + 1
      }
      
      output$dateRangeUI <- renderUI({
        dateRangeInput(
          "date_range", "Rango de fechas (UTC):",
          start = min_day,
          end = picker_max,
          min = min_day,
          max = picker_max,
          format = "yyyy-mm-dd", separator = " a "
        )
      })
      
      output$procMsg <- renderUI(tagList(icon("check", style="color:green;"), " OK. Filtra por fecha y configura grupos."))
    }, error = function(e) {
      rv$combined <- NULL
      rv$filtered <- NULL
      rv$validated_sig <- NULL
      rv$zt0_ref <- NULL
      output$procMsg <- renderUI(tagList(icon("times", style="color:red;"), " Error: ", e$message))
      output$dateRangeUI <- renderUI(NULL)
    })
  })
  
  # -----------------------------------------
  # Paso 3: aplicar filtro
  # -----------------------------------------
  observeEvent(input$applyDates, {
    req(rv$combined, input$date_range)
    output$dateMsg <- renderUI(tagList(icon("spinner", class="fa-spin"), " Filtrando..."))
    
    tryCatch({
      start_day <- input$date_range[1]
      end_day   <- input$date_range[2]
      
      if (is.na(start_day) || is.na(end_day)) stop("Selecciona un rango válido.")
      
      start_dt <- as.POSIXct(paste0(start_day, " 00:00:00"), tz = "UTC")
      end_dt   <- as.POSIXct(paste0(end_day,   " 23:59:59"), tz = "UTC")
      
      filt <- rv$combined %>%
        dplyr::filter(.data$fecha >= start_dt, .data$fecha <= end_dt)
      if (nrow(filt) == 0) stop("No hay datos en el rango seleccionado.")
      
      rv$filtered <- filt
      rv$individuals <- sort(unique(filt$original_tratamiento))
      
      output$controlGroupSelectUI <- renderUI({
        selectizeInput(
          "controlGroupIndividuals", "Individuos del control:",
          choices = rv$individuals, multiple = TRUE,
          options = list(placeholder = "Selecciona...", plugins = list("remove_button"))
        )
      })
      
      output$expGroupsConfigUI <- renderUI({
        req(input$numExpGrupos >= 1)
        lapply(seq_len(input$numExpGrupos), function(i) {
          fluidRow(
            column(6, textInput(paste0("expGroupName", i), paste("Nombre Grp.", i),
                                value = paste0(DEFAULT_EXP_GROUP_PREFIX, i))),
            column(6, colourInput(
              paste0("expGroupColor", i), paste("Color Grp.", i),
              value = DEFAULT_EXP_GROUP_COLORS[((i - 1) %% length(DEFAULT_EXP_GROUP_COLORS)) + 1]
            ))
          )
        })
      })
      
      output$expGroupsSelectUI <- renderUI({
        req(input$numExpGrupos >= 1)
        lapply(seq_len(input$numExpGrupos), function(i) {
          lab <- input[[paste0("expGroupName", i)]] %||% paste0(DEFAULT_EXP_GROUP_PREFIX, i)
          selectizeInput(
            paste0("expGroupIndividuals", i),
            paste("Individuos para", lab, ":"),
            choices = rv$individuals, multiple = TRUE,
            options = list(placeholder = "Selecciona...", plugins = list("remove_button"))
          )
        })
      })
      
      rv$validated_sig <- NULL
      output$dateMsg <- renderUI(tagList(icon("check", style="color:green;"), " Rango aplicado."))
    }, error = function(e) {
      rv$filtered <- NULL
      rv$individuals <- character(0)
      rv$validated_sig <- NULL
      output$dateMsg <- renderUI(tagList(icon("times", style="color:red;"), " Error: ", e$message))
      output$controlGroupSelectUI <- renderUI(NULL)
      output$expGroupsConfigUI <- renderUI(NULL)
      output$expGroupsSelectUI <- renderUI(NULL)
    })
  })
  
  # -----------------------------------------
  # Validación de grupos
  # -----------------------------------------
  current_group_sig <- reactive({
    if (is.null(rv$filtered)) return(NA_character_)
    num <- input$numExpGrupos %||% 0
    
    control_inds <- sort(input$controlGroupIndividuals %||% character(0))
    exp_bits <- character(0)
    if (num > 0) {
      exp_bits <- vapply(seq_len(num), function(i) {
        nm <- trimws(input[[paste0("expGroupName", i)]] %||% "")
        co <- input[[paste0("expGroupColor", i)]] %||% ""
        inds <- sort(input[[paste0("expGroupIndividuals", i)]] %||% character(0))
        paste(nm, co, paste(inds, collapse = ","), sep = "|")
      }, character(1))
    }
    
    paste(
      trimws(input$controlGroupName %||% ""),
      (input$controlGroupColor %||% ""),
      paste(control_inds, collapse = ","),
      num,
      paste(exp_bits, collapse = ";;"),
      sep = "||"
    )
  })
  
  groups_are_validated <- reactive({
    !is.null(rv$validated_sig) && !is.na(current_group_sig()) && identical(rv$validated_sig, current_group_sig())
  })
  
  observeEvent(current_group_sig(), {
    if (!is.null(rv$validated_sig) && !identical(rv$validated_sig, current_group_sig())) {
      output$groupMsg <- renderUI(tagList(
        icon("exclamation-triangle", style="color:orange;"),
        " Cambiaste la configuración; valida de nuevo."
      ))
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$validateGroups, {
    req(rv$filtered, input$numExpGrupos >= 1)
    output$groupMsg <- renderUI(tagList(icon("spinner", class="fa-spin"), " Validando..."))
    
    tryCatch({
      control_name <- trimws(input$controlGroupName %||% "")
      control_inds <- input$controlGroupIndividuals %||% character(0)
      if (!nzchar(control_name)) stop("El grupo control necesita nombre.")
      if (length(control_inds) == 0) stop("El grupo control debe tener al menos 1 individuo.")
      
      all_selected <- control_inds
      
      for (i in seq_len(input$numExpGrupos)) {
        nm <- trimws(input[[paste0("expGroupName", i)]] %||% "")
        inds <- input[[paste0("expGroupIndividuals", i)]] %||% character(0)
        if (!nzchar(nm)) stop("Grupo experimental ", i, " necesita nombre.")
        if (length(inds) == 0) stop("Grupo experimental '", nm, "' debe tener al menos 1 individuo.")
        
        dup <- inds[inds %in% all_selected]
        if (length(dup) > 0) stop("Individuo(s) duplicados en más de un grupo: ", paste(unique(dup), collapse = ", "))
        
        all_selected <- c(all_selected, inds)
      }
      
      unassigned <- setdiff(rv$individuals, all_selected)
      rv$validated_sig <- current_group_sig()
      
      if (length(unassigned) > 0) {
        output$groupMsg <- renderUI(tagList(
          icon("check", style="color:green;"), " Validación OK.",
          br(),
          icon("info-circle", style="color:steelblue;"),
          " ", length(unassigned), " individuo(s) no asignados (se ignorarán)."
        ))
      } else {
        output$groupMsg <- renderUI(tagList(icon("check", style="color:green;"), " Validación OK."))
      }
    }, error = function(e) {
      rv$validated_sig <- NULL
      output$groupMsg <- renderUI(tagList(icon("times", style="color:red;"), " Error: ", e$message))
    })
  })
  
  # -----------------------------------------
  # Paso 5: Generar
  # -----------------------------------------
  results <- eventReactive(input$generateResults, {
    req(rv$filtered)
    validate(need(groups_are_validated(), "Primero valida los grupos (y no cambies inputs después)."))
    
    full_date_str <- paste(format(input$dateInputPart, "%Y-%m-%d"), input$timeInputPart)
    start_dt <- parse_start_datetime(full_date_str, tz = "UTC")
    
    # Si se detectó zt0 desde ZT original, úsalo para sombreado
    zt0_for_dy <- rv$zt0_ref
    if (is.null(zt0_for_dy) || is.na(zt0_for_dy)) zt0_for_dy <- start_dt
    
    withProgress(message = "Generando resultados", value = 0, {
      incProgress(0.05, detail = "Mapeando grupos")
      
      control_config <- list(
        nombre = trimws(input$controlGroupName),
        color = input$controlGroupColor,
        individuos = input$controlGroupIndividuals
      )
      
      exp_groups_config <- lapply(seq_len(input$numExpGrupos), function(i) {
        list(
          nombre = trimws(input[[paste0("expGroupName", i)]]),
          color = input[[paste0("expGroupColor", i)]],
          individuos = input[[paste0("expGroupIndividuals", i)]]
        )
      })
      
      all_groups_config <- c(list(control_config), exp_groups_config)
      
      map_df <- tibble::tibble(
        original_tratamiento = c(control_config$individuos, unlist(lapply(exp_groups_config, `[[`, "individuos"))),
        user_group = c(
          rep(control_config$nombre, length(control_config$individuos)),
          unlist(lapply(exp_groups_config, function(g) rep(g$nombre, length(g$individuos))))
        )
      )
      
      incProgress(0.10, detail = "Preparando datos análisis")
      
      datos_analisis <- rv$filtered %>%
        dplyr::inner_join(map_df, by = "original_tratamiento")
      
      validate(need(nrow(datos_analisis) > 0, "No quedaron datos tras asignar grupos."))
      
      incProgress(0.15, detail = "Promedios por individuo y ZT")
      
      datos_individual_means <- datos_analisis %>%
        dplyr::group_by(.data$user_group, .data$individuo, .data$ZT) %>%
        dplyr::summarise(
          act_promedio_ind  = mean(.data$act, na.rm = TRUE),
          temp_promedio_ind = mean(.data$temp, na.rm = TRUE),
          .groups = "drop"
        )
      
      incProgress(0.15, detail = "Promedios+SEM por grupo y ZT")
      
      datos_grupo_means_sem <- datos_individual_means %>%
        dplyr::group_by(.data$user_group, .data$ZT) %>%
        dplyr::summarise(
          act_promedio_grupo  = mean(.data$act_promedio_ind, na.rm = TRUE),
          act_n_grupo         = dplyr::n(),
          act_sd_grupo        = sd(.data$act_promedio_ind, na.rm = TRUE),
          act_sem_grupo       = if (act_n_grupo > 1) act_sd_grupo / sqrt(act_n_grupo) else NA_real_,
          
          temp_promedio_grupo = mean(.data$temp_promedio_ind, na.rm = TRUE),
          temp_n_grupo        = dplyr::n(),
          temp_sd_grupo       = sd(.data$temp_promedio_ind, na.rm = TRUE),
          temp_sem_grupo      = if (temp_n_grupo > 1) temp_sd_grupo / sqrt(temp_n_grupo) else NA_real_,
          .groups = "drop"
        )
      
      incProgress(0.10, detail = "Perfiles (ggplot)")
      
      p_act <- plot_profile_graph(
        datos_grupo_means_sem, "act",
        "Actividad promedio por grupo (±SEM)", "Actividad (u.)",
        all_groups_config
      )
      
      p_temp <- plot_profile_graph(
        datos_grupo_means_sem, "temp",
        "Temperatura promedio por grupo (±SEM)", "Temperatura (°C)",
        all_groups_config
      )
      
      incProgress(0.15, detail = "T-tests + BH/FDR")
      
      exp_names <- vapply(exp_groups_config, `[[`, character(1), "nombre")
      
      ttests_act <- compute_ttests_by_zt(
        datos_individual_means,
        control_group_name = control_config$nombre,
        experimental_groups = exp_names,
        variable = "act_promedio_ind",
        var_prefix = "act"
      )
      
      ttests_temp <- compute_ttests_by_zt(
        datos_individual_means,
        control_group_name = control_config$nombre,
        experimental_groups = exp_names,
        variable = "temp_promedio_ind",
        var_prefix = "temp"
      )
      
      incProgress(0.10, detail = "Dygraphs")
      
      group_color <- setNames(
        vapply(all_groups_config, `[[`, character(1), "color"),
        vapply(all_groups_config, `[[`, character(1), "nombre")
      )
      id_to_group <- setNames(map_df$user_group, map_df$original_tratamiento)
      series_color_map <- lapply(names(id_to_group), function(id) group_color[[id_to_group[[id]]]] %||% "#777777")
      names(series_color_map) <- names(id_to_group)
      
      dy_act <- create_all_individuals_dygraph(
        data = rv$filtered,
        variable = "act",
        zt0 = zt0_for_dy,
        night_start = ZT_NIGHT_START,
        night_end = ZT_NIGHT_END,
        series_color_map = series_color_map,
        group_id = APP_TITLE
      )
      
      dy_temp <- create_all_individuals_dygraph(
        data = rv$filtered,
        variable = "temp",
        zt0 = zt0_for_dy,
        night_start = ZT_NIGHT_START,
        night_end = ZT_NIGHT_END,
        series_color_map = series_color_map,
        group_id = APP_TITLE
      )
      
      incProgress(0.10, detail = "Guardando a carpeta temporal + ZIP")
      
      run_dir <- file.path(tempdir(), paste0("results_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", session$token))
      dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
      
      readr::write_csv(rv$combined, file.path(run_dir, "processed_combined_all.csv"))
      readr::write_csv(rv$filtered, file.path(run_dir, "processed_combined_filtered.csv"))
      readr::write_csv(datos_individual_means, file.path(run_dir, "analysis_individual_means_by_ZT.csv"))
      readr::write_csv(datos_grupo_means_sem, file.path(run_dir, "analysis_group_means_sem_by_ZT.csv"))
      
      if (length(ttests_act) > 0) {
        for (nm in names(ttests_act)) {
          readr::write_csv(ttests_act[[nm]], file.path(run_dir, paste0("ttest_act_", nm, ".csv")))
        }
      }
      if (length(ttests_temp) > 0) {
        for (nm in names(ttests_temp)) {
          readr::write_csv(ttests_temp[[nm]], file.path(run_dir, paste0("ttest_temp_", nm, ".csv")))
        }
      }
      
      ggplot2::ggsave(file.path(run_dir, "profile_actividad.png"), plot = p_act, width = 10, height = 6, dpi = 300)
      ggplot2::ggsave(file.path(run_dir, "profile_temperatura.png"), plot = p_temp, width = 10, height = 6, dpi = 300)
      
      if (!is.null(dy_act))  htmlwidgets::saveWidget(dy_act,  file.path(run_dir, "dygraph_act.html"),  selfcontained = TRUE)
      if (!is.null(dy_temp)) htmlwidgets::saveWidget(dy_temp, file.path(run_dir, "dygraph_temp.html"), selfcontained = TRUE)
      
      resumen <- datos_individual_means %>%
        dplyr::group_by(.data$user_group) %>%
        dplyr::summarise(N_Individuos = dplyr::n_distinct(.data$individuo), .groups = "drop") %>%
        dplyr::arrange(.data$user_group)
      readr::write_csv(resumen, file.path(run_dir, "group_summary.csv"))
      
      zip_path <- file.path(tempdir(), paste0("resultados_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", session$token, ".zip"))
      zip_folder(run_dir, zip_path)
      
      rv$bundle_dir <- run_dir
      rv$bundle_zip <- zip_path
      
      list(
        datos_individual_means = datos_individual_means,
        datos_grupo_means_sem = datos_grupo_means_sem,
        ttests_act = ttests_act,
        ttests_temp = ttests_temp,
        plot_act = p_act,
        plot_temp = p_temp,
        dy_act = dy_act,
        dy_temp = dy_temp,
        resumen = resumen
      )
    })
  })
  
  observeEvent(results(), {
    rv$last_results <- results()
    output$genMsg <- renderUI(tagList(icon("check", style="color:green;"),
                                      " Listo. Ya puedes descargar el ZIP."))
    updateTabsetPanel(session, "mainTabs", selected = "Resumen")
  }, ignoreInit = TRUE)
  
  output$groupSummary <- renderTable({
    req(rv$last_results)
    rv$last_results$resumen
  })
  
  output$perfilActividad <- renderPlot({
    req(rv$last_results)
    rv$last_results$plot_act
  })
  
  output$perfilTemperatura <- renderPlot({
    req(rv$last_results)
    rv$last_results$plot_temp
  })
  
  output$ttAct <- DT::renderDataTable({
    req(rv$last_results)
    lst <- rv$last_results$ttests_act
    if (length(lst) == 0) return(NULL)
    df <- dplyr::bind_rows(lst, .id = "Comparacion")
    DT::datatable(df, options = list(pageLength = 24, scrollX = TRUE), rownames = FALSE)
  })
  
  output$ttTemp <- DT::renderDataTable({
    req(rv$last_results)
    lst <- rv$last_results$ttests_temp
    if (length(lst) == 0) return(NULL)
    df <- dplyr::bind_rows(lst, .id = "Comparacion")
    DT::datatable(df, options = list(pageLength = 24, scrollX = TRUE), rownames = FALSE)
  })
  
  output$dyAct <- renderDygraph({
    req(rv$last_results)
    rv$last_results$dy_act
  })
  
  output$dyTemp <- renderDygraph({
    req(rv$last_results)
    rv$last_results$dy_temp
  })
  
  output$downloadZip <- downloadHandler(
    filename = function() {
      paste0("resultados_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      req(rv$bundle_zip)
      file.copy(rv$bundle_zip, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)
