#' Fatorial completo com caderno de campo e mapa de parcelas
#'
#' Gera um experimento fatorial completo (CRD/DIC ou RCBD/DBC) a partir da
#' combinação de fatores e níveis, com opção de tratamentos adicionais,
#' layout serpentina, geração de identificadores únicos reprodutíveis e
#' retorno do caderno de campo consolidado e mapas de campo (ggplot) por
#' localidade.
#'
#' @param factors `character`. Vetor com os nomes dos fatores (ex.: `c("A","B")`).
#' @param nlevels `integer`. Número de níveis para cada fator, na mesma ordem de
#'   `factors` (ex.: `c(2, 3)`).
#' @param levels `character`. Vetor com os rótulos de todos os níveis, na
#'   sequência dos fatores repetidos por seus respectivos níveis
#'   (comprimento deve ser `sum(nlevels)` quando expandido como em `rep(factors, nlevels)`).
#' @param add_trats `character` ou `NULL`. Nomes de tratamentos adicionais
#'   (por exemplo, testemunhas) a serem incluídos além das combinações fatoriais.
#' @param reps `integer` (>= 1). Número de repetições (blocos para RCBD; linhas
#'   “pilhas” para CRD).
#' @param design `character`. Delineamento: `"CRD"` (DIC) ou `"RCBD"` (DBC).
#' @param seed `integer`. Semente para reprodutibilidade; é
#'   incremental por localidade (`seed + índice_da_localidade`).
#' @param serpentine `logical`. Se `TRUE`, aplica ordem “serpentina” por linha no
#'   layout final.
#' @param exp_year `character`. Ano do experimento (exibido no caderno de campo).
#' @param exp_name `character`. Nome do experimento. Pode ser de comprimento 1
#'   (reciclado para todas as localidades) ou de comprimento igual a `locations`.
#' @param fill_color `logical`. Se `TRUE`, o mapa colore por `TRT_COMB`; caso
#'   contrário, usa preenchimento cinza.
#' @param text_size `numeric` (> 0). Tamanho do texto dos rótulos de parcela no mapa.
#' @param layout `character`. `"default"` (matriz `reps x ntrats`),
#'   `"custom"` (definido por `layout_allocation`).
#' @param layout_allocation `numeric(2)` ou `NULL`. Quando `layout = "custom"`,
#'   vetor `c(nrow, ncol)` com o número de linhas e colunas do layout
#'   por repetição. Deve comportar `ntrats_total = prod(nlevels) + length(add_trats)`.
#' @param namespace `character`. UUID namespace para gerar IDs estáveis via
#'   `uuid::UUIDfromName()`.
#' @param locations `integer` (>= 1). Número de localidades (experimentos
#'   independentes) a serem geradas.
#'
#' @details
#' A função é um invólucro de conveniência em torno de
#' `FielDHub::full_factorial()`, adicionando:
#' * Inclusão opcional de tratamentos extras (`add_trats`) com
#'   reamostragem adequada por delineamento;
#' * Geração de um layout (padrão ou customizado) com ou sem serpentina;
#' * Criação de identificadores únicos e reprodutíveis por parcela
#'   (`UNIQUE_ID`) considerando `ROW`, `COL`, `YEAR`,
#'   `LOCATION`, `PLOT`, `REP` e `TRT_COMB`;
#' * Produção de um *fieldbook* consolidado (todas as localidades) e
#'   de mapas de campo (`ggplot`) por localidade.
#'
#' Regras e validações principais:
#' * `FACTORS <- rep(factors, nlevels)` deve ter `length(levels)`
#'   correspondente; caso contrário, a função aborta com mensagem do pacote `cli`.
#' * Para `layout = "custom"`, `nrow * ncol` deve ser >= `ntrats_total`.
#' * Em `design = "RCBD"`, a alocação é feita por bloco (`REP`).
#'
#' @return Uma `list` com dois componentes:
#' * `fieldbook`: `data.frame/tibble` com as colunas de
#'   identificação (`UNIQUE_ID`, `YEAR`, `LOCATION`, `ROW`,
#'   `COL`, `PLOT`, `REP`, `TRT_COMB`, `FACTOR_*`, etc.).
#' * `fieldmap`: `list` de objetos `ggplot` (um por
#'   localidade) representando o mapa de campo.
#'   
#'   
full_factorial <- function(factors,
                           nlevels,
                           levels,
                           add_trats = NULL,
                           reps = 4,
                           design = c("CRD", "RCBD"),
                           seed = 123,
                           serpentine = TRUE,
                           exp_year = format(Sys.Date(), "%Y"),
                           exp_name = "Experiment",
                           fill_color = TRUE,
                           text_size = 3,
                           layout = c("default", "custom"),
                           layout_allocation = NULL,
                           namespace = "7ac1a295-ef95-4f7c-8a35-3eed97cb256d",
                           locations = 1) {
  # Função auxiliar
  check_and_load <- function(pkg){
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
  
  # Lista de pacotes
  pkgs <- c("dplyr", "FielDHub", "qrlabelr", "uuid", "cli")
  
  # Aplicar a função a cada pacote
  invisible(lapply(pkgs, check_and_load))
  
  
  cli::cli_h1("Iniciando experimento fatorial completo")
  
  layout <- match.arg(layout)
  design <- match.arg(design)
  
  # Ajusta exp_name se necessário
  if (length(exp_name) == 1) {
    exp_name <- rep(exp_name, locations)
  } else if (length(exp_name) != locations) {
    cli::cli_abort("O argumento {.arg exp_name} deve ter comprimento 1 ou igual ao número de localidades.")
  }
  
  if (!is.numeric(reps) || reps < 1 || length(reps) != 1) {
    cli::cli_abort("O argumento {.arg reps} deve ser um número inteiro positivo.")
  }
  if (!is.logical(serpentine) || length(serpentine) != 1) {
    cli::cli_abort("O argumento {.arg serpentine} deve ser TRUE ou FALSE.")
  }
  if (!is.logical(fill_color) || length(fill_color) != 1) {
    cli::cli_abort("O argumento {.arg fill_color} deve ser TRUE ou FALSE.")
  }
  if (!is.numeric(text_size) || text_size <= 0) {
    cli::cli_abort("O argumento {.arg text_size} deve ser um número positivo.")
  }
  
  FACTORS <- rep(factors, nlevels)
  if (length(levels) != length(FACTORS)) {
    cli::cli_abort(c("O produto do número de níveis {.val {nlevels}} não bate.",
                     "i" = "Número de níveis: {length(FACTORS)}",
                     "x" = "Níveis fornecidos: {length(levels)}"))
  }
  
  get_design <- function(loc, loc_name){
    data_factorial <- data.frame(FACTOR = FACTORS, LEVEL = levels)
    expdes <- switch(design, CRD = 1, RCBD = 2)
    
    fullFact2 <- FielDHub::full_factorial(
      setfactors = NULL,
      reps = reps,
      type = expdes,
      seed = seed + loc,
      data = data_factorial,
      locationNames = loc_name
    )
    
    fieldbook <- 
      fullFact2$fieldBook |> 
      dplyr::mutate(REP = as.integer(as.character(REP)))
    
    # Adiciona tratamentos extras
    if (!is.null(add_trats)) {
      cli_progress_step("Adicionando tratamentos adicionais...")
      ntrats <- prod(nlevels) + length(add_trats)
      factor_cols <- grep("^FACTOR_", names(fieldbook), value = TRUE)
      
      add_rows <- purrr::map_dfr(add_trats, function(trat) {
        tibble_row <- as.list(setNames(rep(trat, length(factor_cols)), factor_cols))
        tibble_row$TRT_COMB <- trat
        tibble_row$REP <- seq_len(reps)
        tibble_row <- as.data.frame(tibble_row)
        tibble_row[rep(seq_len(nrow(tibble_row)), each = 1), ]
      })
      
      base_rows <- fieldbook[1:(length(add_trats) * reps), ]
      add_rows <- dplyr::bind_cols(
        base_rows[, !names(base_rows) %in% c(factor_cols, "TRT_COMB", "REP")],
        add_rows
      )
      
      if (expdes == 1) {
        fieldbook <- dplyr::bind_rows(fieldbook, add_rows) |>
          dplyr::ungroup() |>
          dplyr::slice_sample(n = nrow(fieldbook) + nrow(add_rows)) |>
          dplyr::mutate(PLOT = dplyr::row_number())
      } else {
        fieldbook <- dplyr::bind_rows(fieldbook, add_rows) |>
          dplyr::arrange(REP) |>
          dplyr::group_by(REP) |>
          dplyr::slice_sample(n = prod(nlevels) + length(add_trats)) |>
          dplyr::ungroup() |>
          dplyr::mutate(PLOT = dplyr::row_number())
      }
    }
    
    ntrats_total <- prod(nlevels) + if (!is.null(add_trats)) length(add_trats) else 0
    
    generate_plot <- function(layout_df) {
      fieldbook_layout <- 
        fieldbook |>
        dplyr::mutate(
          ROW = layout_df$ROW,
          COL = layout_df$COL,
          YEAR = exp_year,
          LOCATION = loc_name,
          .before = 1
        ) |>
        unite("FACTORS", ROW, COL, YEAR, LOCATION, PLOT, REP, TRT_COMB, remove = FALSE) |> 
        mutate(UNIQUE_ID = uuid::UUIDfromName(namespace, FACTORS), .before = 1) |> 
        dplyr::select(-c(ID, FACTORS))
      
      if (serpentine) {
        cli::cli_progress_step("Transformando o layout em serpentina...")
        fieldbook_layout <- fieldbook_layout |>
          dplyr::group_by(LOCATION, ROW) |>
          dplyr::arrange(ifelse(ROW %% 2 == 0, dplyr::desc(COL), COL), .by_group = TRUE) |>
          dplyr::ungroup() |>
          dplyr::mutate(PLOT = dplyr::row_number())
      }
      
      row_max <- max(fieldbook_layout$ROW)
      block_sep <- switch(layout,
                          default = seq(1.5, row_max - 0.5, by = 1),
                          custom  = seq(max(fieldbook_layout$ROW) / reps + 0.5, row_max - 0.5, by = max(fieldbook_layout$ROW) / reps),
                          auto    = {
                            block_height <- max(table(fieldbook_layout$ROW))
                            seq(block_height + 0.5, row_max - 0.5, by = block_height)
                          })
      
      caption_text <- glue::glue("Local: {loc_name}. Delineamento: {design}. Layout: {layout}. Tratamentos: {ntrats_total}. Repetições: {reps}.")
      cli_progress_step("Gerando o caderno de campo...")
      plot <- ggplot2::ggplot(fieldbook_layout, ggplot2::aes(COL, ROW)) +
        {if (fill_color) ggplot2::geom_tile(ggplot2::aes(fill = TRT_COMB), color = "black")
          else ggplot2::geom_tile(fill = "gray", color = "black")} +
        ggplot2::geom_text(ggplot2::aes(label = TRT_COMB), size = text_size) +
        ggplot2::geom_hline(yintercept = block_sep, linewidth = 1.2, color = "black") +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(0), position = "top") +
        ggplot2::scale_y_reverse(expand = ggplot2::expansion(0)) +
        ggplot2::labs(title = loc_name, x = "Coluna", y = "Linha", caption = caption_text) +
        ggplot2::theme_minimal()
      
      list(fieldbook = fieldbook_layout, fieldmap = plot)
    }
    
    if (layout == "custom") {
      if (is.null(layout_allocation) || length(layout_allocation) != 2) {
        cli::cli_abort("Você deve fornecer um vetor de dois elementos para {.arg layout_allocation}.")
      }
      nrow <- layout_allocation[1]
      ncol <- layout_allocation[2]
      ntrats_custom <- nrow * ncol
      if (ntrats_total > ntrats_custom) {
        cli::cli_abort("O número de tratamentos ({.val {ntrats_total}}) é maior que o layout customizado ({.val {ntrats_custom}}).")
      }
      cli_progress_step("Customizando o layout...")
      tratnumb <- c(1:ntrats_total, rep(NA, nrow * ncol - ntrats_total))
      idx <- matrix(tratnumb, nrow = nrow, ncol = ncol, byrow = TRUE)
      if (serpentine && nrow > 1) {
        for (i in seq(2, nrow, by = 2)) idx[i, ] <- rev(idx[i, ])
      }
      layout_df_base <- data.frame(
        ROW = rep(seq_len(nrow), each = ncol),
        COL = rep(seq_len(ncol), times = nrow)
      )
      layout_df_base <- layout_df_base[!is.na(as.vector(t(idx))), , drop = FALSE]
      layout_df <- dplyr::bind_rows(
        purrr::map(seq_len(reps), function(r) {
          df <- layout_df_base
          df$ROW <- df$ROW + (r - 1) * nrow
          df
        })
      )
    } else {
      layout_df <- data.frame(
        ROW = rep(seq_len(reps), each = ntrats_total),
        COL = rep(seq_len(ntrats_total), times = reps)
      )
    }
    
    generate_plot(layout_df)
  }
  
  # Geração para todas as localidades
  all_results <- purrr::map2(seq_len(locations), exp_name, get_design)
  
  fieldbook_all <- dplyr::bind_rows(purrr::map(all_results, "fieldbook"))
  fieldmaps_all <- purrr::map(all_results, "fieldmap")
  
  return(list(fieldbook = fieldbook_all, fieldmap = fieldmaps_all))
}




#' Parcelas subdivididas com caderno de campo e mapa de parcelas
#'
#' Gera um experimento em parcelas subdivididas (split-plot) nos delineamentos
#' CRD (DIC) ou RCBD (DBC), permitindo múltiplas localidades, layout serpentina,
#' e exportação do caderno de campo consolidado e mapas de campo (`ggplot`).
#'
#' @param wholeplot `character`. Vetor com os níveis do fator da parcela principal.
#' @param subplot `character`. Vetor com os níveis do fator da subparcela.
#' @param reps `integer` (>= 1). Número de repetições (blocos para RCBD).
#' @param design `character`. Delineamento: `"CRD"` (DIC) ou `"RCBD"` (DBC).
#' @param seed `integer`. Semente para reprodutibilidade (incrementada por localidade).
#' @param serpentine `logical`. Se `TRUE`, aplica ordem serpentina por linha no layout.
#' @param exp_year `character`. Ano do experimento (exibido no caderno de campo).
#' @param exp_name `character`. Nome do experimento. Pode ser único ou de comprimento igual a `locations`.
#' @param namespace `character`. UUID namespace para gerar identificadores únicos com `uuid::UUIDfromName()`.
#' @param fill_color `logical`. Se `TRUE`, colore o mapa de campo pelo fator da parcela principal (`WHOLE_PLOT`).
#' @param text_size `numeric` (> 0). Tamanho do texto no mapa de campo.
#' @param locations `integer` (>= 1). Número de localidades (experimentos independentes).
#'
#' @details
#' A função é um invólucro de conveniência em torno de `FielDHub::split_plot()`, com:
#' * Definição automática do delineamento a partir de `design`;
#' * Alocação de parcelas principais (`WHOLE_PLOT`) e subparcelas (`SUB_PLOT`);
#' * Criação de identificadores únicos e reprodutíveis (`UNIQUE_ID`) considerando
#'   `YEAR`, `LOCATION`, `ROW`, `COL`, `PLOT`, `REP`, `TRT_COMB`;
#' * Geração de mapas (`ggplot`) com rótulos `WHOLE_PLOT` e `SUB_PLOT`;
#' * Opção de layout serpentina por linha.
#'
#' Regras principais:
#' * `reps` deve ser um número inteiro positivo;
#' * `exp_name` deve ter comprimento 1 ou ser igual a `locations`;
#' * Para `design = "RCBD"`, a alocação considera blocos (`REP`).
#'
#' @return Uma `list` com:
#' * `fieldbook`: `data.frame/tibble` com informações do experimento, incluindo
#'   `WHOLE_PLOT`, `SUB_PLOT`, `TRT_COMB`, identificadores e coordenadas (`ROW`, `COL`);
#' * `fieldmap`: lista de objetos `ggplot` (um por localidade), com o mapa de campo.
#'
split_plot <- function(wholeplot,
                       subplot,
                       reps = 4,
                       design = c("CRD", "RCBD"),
                       seed = 123,
                       serpentine = TRUE,
                       exp_year = format(Sys.Date(), "%Y"),
                       exp_name = "Experiment",
                       namespace = "7ac1a295-ef95-4f7c-8a35-3eed97cb256d",
                       fill_color = TRUE,
                       text_size = 3,
                       locations = 1) {
  # Função auxiliar
  check_and_load <- function(pkg){
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
  
  # Lista de pacotes
  pkgs <- c("dplyr", "FielDHub", "qrlabelr", "uuid", "cli")
  
  # Aplicar a função a cada pacote
  invisible(lapply(pkgs, check_and_load))
  
  cli::cli_h1("Iniciando experimento em parcelas subdivididas")
  
  design <- match.arg(design)
  
  if (length(exp_name) == 1) {
    exp_name <- rep(exp_name, locations)
  } else if (length(exp_name) != locations) {
    cli::cli_abort("O argumento {.arg exp_name} deve ter comprimento 1 ou igual ao número de localidades.")
  }
  
  if (!is.numeric(reps) || length(reps) != 1 || reps < 1) {
    cli::cli_abort("O argumento {.arg reps} deve ser um número inteiro positivo.")
  }
  if (!is.logical(serpentine)) {
    cli::cli_abort("O argumento {.arg serpentine} deve ser TRUE ou FALSE.")
  }
  if (!is.logical(fill_color)) {
    cli::cli_abort("O argumento {.arg fill_color} deve ser TRUE ou FALSE.")
  }
  if (!is.numeric(text_size) || text_size <= 0) {
    cli::cli_abort("O argumento {.arg text_size} deve ser um número positivo.")
  }
  
  get_design <- function(loc, loc_name) {
    ntrats <- length(subplot) * length(wholeplot)
    
    sp <- subplot
    wp <- wholeplot
    
    if (length(wp) > length(sp)) {
      sp <- c(sp, rep(NA, length(wp) - length(sp)))
    } else if (length(wp) < length(sp)) {
      wp <- c(wp, rep(NA, length(sp) - length(wp)))
    }
    
    datasp <- data.frame(WHOLE_PLOT = wp, SUB_PLOT = sp)
    expdes <- switch(design, CRD = 1, RCBD = 2)
    
    cli::cli_progress_step("Criando delineamento split-plot para {.val {loc_name}}...")
    
    fullFact2 <- FielDHub::split_plot(
      reps = reps,
      type = expdes,
      seed = seed + loc,
      data = datasp,
      locationNames = loc_name
    )
    
    fieldbook <- fullFact2$fieldBook |>
      dplyr::mutate(REP = as.integer(as.character(REP)))
    
    REPVAL <- if (design == "CRD") rep(seq_len(reps), each = ntrats) else fieldbook$REP
    
    fieldbook <- 
      fieldbook |>
      dplyr::mutate(
        ROW = as.numeric(REPVAL),
        COL = rep(seq_len(nrow(fieldbook) / reps), reps),
        YEAR = exp_year,
        LOCATION = loc_name,
        .before = 1
      ) |> 
      unite("FACTORS", YEAR, LOCATION, ROW, COL, PLOT, REP, TRT_COMB, remove = FALSE) |> 
      mutate(UNIQUE_ID = uuid::UUIDfromName(namespace, FACTORS), .before = 1) |> 
      dplyr::select(-c(ID, FACTORS))
    
    if (serpentine) {
      cli::cli_progress_step("Aplicando layout serpentina...")
      fieldbook <- fieldbook |>
        dplyr::group_by(LOCATION, ROW) |>
        dplyr::arrange(ifelse(ROW %% 2 == 0, dplyr::desc(COL), COL), .by_group = TRUE) |>
        dplyr::ungroup()
    }
    
    row_max <- max(fieldbook$ROW)
    block_sep <- seq(1.5, row_max - 0.5, by = 1)
    
    cli::cli_progress_step("Gerando mapa do campo para {.val {loc_name}}...")
    
    p <- ggplot2::ggplot(fieldbook, aes(COL, ROW)) +
      {if(fill_color) ggplot2::geom_tile(aes(fill = WHOLE_PLOT), color = "black")
        else ggplot2::geom_tile(fill = "gray", color = "black")} +
      ggplot2::geom_text(aes(label = paste0(WHOLE_PLOT, "\n", SUB_PLOT)), size = text_size) +
      ggplot2::geom_hline(yintercept = block_sep, linewidth = 1.2, color = "black") +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(0), breaks = 1:max(fieldbook$COL),
                                  position = "top") +
      ggplot2::scale_y_reverse(expand = ggplot2::expansion(0), breaks = 1:reps) +
      ggplot2::labs(title = loc_name, x = "Coluna", y = "Linha") +
      ggplot2::theme_minimal()
    
    list(fieldbook = fieldbook, fieldmap = p)
  }
  
  # Gera resultados para cada localidade
  all_results <- purrr::map2(seq_len(locations), exp_name, get_design)
  
  fieldbook_all <- dplyr::bind_rows(purrr::map(all_results, "fieldbook"))
  fieldmaps_all <- purrr::map(all_results, "fieldmap")
  
  cli::cli_h2("Experimento concluído com sucesso.")
  return(list(fieldbook = fieldbook_all, fieldmap = fieldmaps_all))
}






#' Experimento unifatorial com caderno de campo e mapa de parcelas
#'
#' Gera um experimento unifatorial (CRD/DIC ou RCBD/DBC) com múltiplas localidades,
#' opção de layout serpentina, croqui (mapa em `ggplot`) e caderno de campo consolidado.
#'
#' @param trats `character`. Vetor com os tratamentos a serem testados.
#' @param reps `integer` (>= 1). Número de repetições.
#' @param design `character`. Delineamento: `"CRD"` (DIC) ou `"RCBD"` (DBC).
#' @param seed `integer`. Semente para reprodutibilidade (incrementada por localidade).
#' @param serpentine `logical`. Se `TRUE`, aplica ordem serpentina por linha no layout.
#' @param exp_name `character`. Nome do experimento. Pode ser único ou de comprimento igual a `locations`.
#' @param exp_year `character`. Ano do experimento (usado nas colunas do caderno de campo).
#' @param fill_color `logical`. Se `TRUE`, colore o croqui pelos tratamentos.
#' @param text_size `numeric` (> 0). Tamanho do texto exibido no croqui.
#' @param layout `character`. `"default"` (matriz `reps x trats`) ou `"custom"` (definido por `layout_allocation`).
#' @param namespace `character`. UUID namespace para gerar identificadores únicos com `uuid::UUIDfromName()`.
#' @param layout_allocation `numeric(2)` ou `NULL`. Quando `layout = "custom"`, define `c(nrow, ncol)` do croqui.
#' @param locations `integer` (>= 1). Número de localidades (experimentos independentes).
#'
#' @details
#' A função é um invólucro em torno de `FielDHub::CRD()` e `FielDHub::RCBD()`, adicionando:
#' * Identificadores únicos (`UNIQUE_ID`) por parcela;
#' * Layout serpentina opcional;
#' * Layout customizado (`nrow x ncol`) quando `layout = "custom"`;
#' * Croqui em `ggplot` com cores e rótulos configuráveis;
#' * Caderno de campo consolidado para todas as localidades.
#'
#' Regras e validações principais:
#' * `reps` deve ser um inteiro positivo;
#' * `exp_name` deve ter comprimento 1 ou igual a `locations`;
#' * Para `layout = "custom"`, `nrow * ncol` deve ser >= número de tratamentos.
#'
#' @return Uma `list` com:
#' * `fieldbook`: `data.frame/tibble` com identificadores, tratamentos e coordenadas (`ROW`, `COL`, etc.);
#' * `fieldmap`: lista de objetos `ggplot` (um por localidade) representando o croqui do campo.
#'
#'
unifatorial <- function(trats,
                        reps = 4,
                        design = c("CRD", "RCBD"),
                        seed = 123,
                        serpentine = TRUE,
                        exp_name = "Experiment",
                        exp_year = format(Sys.Date(), "%Y"),
                        fill_color = TRUE,
                        text_size = 3,
                        layout = c("default", "custom"),
                        namespace = "7ac1a295-ef95-4f7c-8a35-3eed97cb256d",
                        layout_allocation = NULL,
                        locations = 1) {
  # Função auxiliar
  check_and_load <- function(pkg){
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
  
  # Lista de pacotes
  pkgs <- c("dplyr", "FielDHub", "qrlabelr", "uuid", "cli")
  
  # Aplicar a função a cada pacote
  invisible(lapply(pkgs, check_and_load))
  
  cli::cli_h1("Iniciando experimento unifatorial")
  
  design <- match.arg(design)
  layout <- match.arg(layout)
  
  if (length(exp_name) == 1) {
    exp_name <- rep(exp_name, locations)
  } else if (length(exp_name) != locations) {
    cli::cli_abort("O argumento {.arg exp_name} deve ter comprimento 1 ou igual ao número de localidades.")
  }
  
  if (!is.numeric(reps) || reps < 1 || length(reps) != 1) {
    cli::cli_abort("O argumento {.arg reps} deve ser um número inteiro positivo.")
  }
  
  if (!is.logical(serpentine) || length(serpentine) != 1) {
    cli::cli_abort("O argumento {.arg serpentine} deve ser TRUE ou FALSE.")
  }
  
  if (!is.logical(fill_color) || length(fill_color) != 1) {
    cli::cli_abort("O argumento {.arg fill_color} deve ser TRUE ou FALSE.")
  }
  
  if (!is.numeric(text_size) || text_size <= 0) {
    cli::cli_abort("O argumento {.arg text_size} deve ser um número positivo.")
  }
  
  get_design <- function(loc, loc_name) {
    # cli::cli_alert_info("Criando delineamento {.val {design}} para {.val {loc_name}} com {.val {reps}} repetições...")
    cli::cli_progress_step("Criando delineamento {.val {design}} para {.val {loc_name}} com {.val {reps}} repetições...")
    
    fieldbook <- if (design == "CRD") {
      FielDHub::CRD(t = trats, reps = reps, seed = seed + loc, locationName = loc_name)$fieldBook
    } else {
      FielDHub::RCBD(t = trats, reps = reps, seed = seed + loc, locationNames = loc_name)$fieldBook
    } |> 
      dplyr::mutate(REP = as.integer(as.character(REP)))
    
    ntrats <- length(trats)
    
    generate_plot <- function(layout_df) {
      fieldbook_layout <- 
        fieldbook |>
        dplyr::mutate(
          ROW = layout_df$ROW,
          COL = layout_df$COL,
          YEAR = exp_year,
          LOCATION = loc_name,
          .before = 1
        ) |> 
        dplyr::ungroup() |>
        dplyr::mutate(PLOT = dplyr::row_number()) |> 
        unite("FACTORS", YEAR, LOCATION, ROW, COL, PLOT, REP, TREATMENT, remove = FALSE) |> 
        mutate(UNIQUE_ID = uuid::UUIDfromName(namespace, FACTORS), .before = 1) |> 
        dplyr::select(-c(ID, FACTORS))
      
      if (serpentine) {
        cli::cli_progress_step("Aplicando layout serpentina...")
        fieldbook_layout <- 
          fieldbook_layout |>
          dplyr::group_by(LOCATION, ROW) |>
          dplyr::arrange(ifelse(ROW %% 2 == 0, dplyr::desc(COL), COL), .by_group = TRUE) |>
          dplyr::ungroup() |>
          dplyr::mutate(PLOT = dplyr::row_number())
      }
      
      row_max <- max(fieldbook_layout$ROW)
      block_sep <- switch(layout,
                          default = seq(1.5, row_max - 0.5, by = 1),
                          custom  = seq(layout_allocation[1] + 0.5, row_max - 0.5, by = layout_allocation[1]))
      cli::cli_progress_step("Gerando o croqui...")
      caption_text <- glue::glue("Local: {loc_name}. Delineamento: {design}. Layout: {layout}. Tratamentos: {ntrats}. Repetições: {reps}.")
      
      p <- ggplot2::ggplot(fieldbook_layout, ggplot2::aes(COL, ROW)) +
        {if (fill_color)
          ggplot2::geom_tile(ggplot2::aes(fill = TREATMENT), color = "black")
          else
            ggplot2::geom_tile(fill = "gray", color = "black")} +
        ggplot2::geom_text(ggplot2::aes(label = TREATMENT), size = text_size) +
        ggplot2::geom_hline(yintercept = block_sep, linewidth = 1.2, color = "black") +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(0), position = "top", breaks = 1:max(fieldbook_layout$COL)) +
        ggplot2::scale_y_reverse(expand = ggplot2::expansion(0), breaks = 1:max(fieldbook_layout$ROW)) +
        ggplot2::labs(title = loc_name, x = "Coluna", y = "Linha",  caption = caption_text) +
        ggplot2::theme_minimal()
      
      cli::cli_alert_success("Experimento {.val {loc_name}} concluído com sucesso.")
      list(fieldbook = fieldbook_layout, fieldmap = p)
    }
    
    if (layout == "custom") {
      if (is.null(layout_allocation) || length(layout_allocation) != 2) {
        cli::cli_abort("Você deve fornecer um vetor de dois elementos para {.arg layout_allocation}.")
      }
      nrow <- layout_allocation[1]
      ncol <- layout_allocation[2]
      nparc <- nrow * ncol
      
      ntrats_total <- length(trats)
      if (ntrats_total > nparc) {
        cli::cli_abort("O número de tratamentos ({.val {ntrats_total}}) é maior que o número de parcelas no layout ({.val {nparc}}).")
      }
      cli::cli_progress_step("Customizando o layout...")
      tratnumb <- c(1:ntrats_total, rep(NA, nparc - ntrats_total))
      idx <- matrix(tratnumb, nrow = nrow, ncol = ncol, byrow = TRUE)
      
      if (serpentine && nrow > 1) {
        for (i in seq(2, nrow, by = 2)) {
          idx[i, ] <- rev(idx[i, ])
        }
      }
      
      layout_df_base <- data.frame(
        ROW = rep(seq_len(nrow), each = ncol),
        COL = rep(seq_len(ncol), times = nrow)
      )
      
      layout_df_base <- layout_df_base[!is.na(as.vector(t(idx))), , drop = FALSE]
      n_layout_rows <- nrow
      
      layout_df <- dplyr::bind_rows(
        purrr::map(seq_len(reps), function(r) {
          df <- layout_df_base
          df$ROW <- df$ROW + (r - 1) * n_layout_rows
          df
        })
      )
      
      return(generate_plot(layout_df))
    } else {
      layout_df <- data.frame(
        ROW = rep(seq_len(reps), each = length(trats)),
        COL = rep(seq_len(length(trats)), times = reps)
      )
      return(generate_plot(layout_df))
    }
  }
  
  all_results <- purrr::map2(seq_len(locations), exp_name, get_design)
  fieldbook_all <- dplyr::bind_rows(purrr::map(all_results, "fieldbook"))
  fieldmaps_all <- purrr::map(all_results, "fieldmap")
  
  return(list(fieldbook = fieldbook_all, fieldmap = fieldmaps_all))
}









#' Delineamento em blocos aumentados com caderno de campo e croqui
#'
#' Gera um experimento em blocos aumentados (Augmented RCBD), permitindo múltiplas
#' localidades, layout serpentina, croqui (mapa em `ggplot`) e caderno de campo
#' consolidado.
#'
#' @param lines `character`. Vetor com os tratamentos candidatos (linhagens/novas entradas).
#' @param checks `character`. Vetor com os tratamentos de checagem (testemunhas).
#' @param blocks `integer` (>= 1). Número de blocos.
#' @param seed `integer`. Semente para reprodutibilidade (incrementada por localidade).
#' @param serpentine `logical`. Se `TRUE`, aplica ordem serpentina por linha no layout.
#' @param exp_year `character`. Ano do experimento.
#' @param exp_name `character`. Nome do experimento. Pode ser único ou de comprimento igual a `locations`.
#' @param namespace `character`. UUID namespace para gerar identificadores únicos com `uuid::UUIDfromName()`.
#' @param fill_color `logical`. Se `TRUE`, colore o croqui por fator (linha do bloco).
#' @param text_size `numeric` (> 0). Tamanho do texto exibido no croqui.
#' @param layout `character`. `"default"` (blocos em sequência) ou `"custom"` (definido por `layout_allocation`).
#' @param layout_allocation `numeric(2)` ou `NULL`. Quando `layout = "custom"`, define `c(nrow, ncol)` do croqui.
#' @param locations `integer` (>= 1). Número de localidades (experimentos independentes).
#'
#' @details
#' A função é um invólucro em torno de `FielDHub::RCBD_augmented()`, adicionando:
#' * Identificadores únicos (`UNIQUE_ID`) por parcela;
#' * Layout serpentina opcional;
#' * Layout customizado (`nrow x ncol`) quando `layout = "custom"`;
#' * Croqui em `ggplot` com cores e rótulos configuráveis;
#' * Caderno de campo consolidado para todas as localidades.
#'
#' Regras e validações principais:
#' * `blocks` deve ser um inteiro positivo;
#' * `exp_name` deve ter comprimento 1 ou igual a `locations`;
#' * Para `layout = "custom"`, `nrow * ncol` deve ser >= número de parcelas por bloco.
#'

augmented <- function(lines,
                      checks,
                      blocks = 4,
                      seed = 123,
                      serpentine = TRUE,
                      exp_year = format(Sys.Date(), "%Y"),
                      exp_name = "Experiment",
                      namespace = "7ac1a295-ef95-4f7c-8a35-3eed97cb256d",
                      fill_color = TRUE,
                      text_size = 3,
                      layout = c("default", "custom"),
                      layout_allocation = NULL,
                      locations = 1) {
  
  # Função auxiliar
  check_and_load <- function(pkg){
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
  
  # Lista de pacotes
  pkgs <- c("dplyr", "FielDHub", "qrlabelr", "uuid", "cli")
  
  # Aplicar a função a cada pacote
  invisible(lapply(pkgs, check_and_load))
  
  cli::cli_h1("Iniciando experimento em blocos aumentados")
  layout <- match.arg(layout)
  
  if (!is.numeric(blocks) || blocks < 1 || length(blocks) != 1) {
    cli::cli_abort("O argumento {.arg blocks} deve ser um número inteiro positivo.")
  }
  if (!is.logical(serpentine)) {
    cli::cli_abort("O argumento {.arg serpentine} deve ser TRUE ou FALSE.")
  }
  if (!is.logical(fill_color)) {
    cli::cli_abort("O argumento {.arg fill_color} deve ser TRUE ou FALSE.")
  }
  if (!is.numeric(text_size) || text_size <= 0) {
    cli::cli_abort("O argumento {.arg text_size} deve ser um número positivo.")
  }
  if (length(exp_name) == 1) {
    exp_name <- rep(exp_name, locations)
  } else if (length(exp_name) != locations) {
    cli::cli_abort("O argumento {.arg exp_name} deve ter comprimento 1 ou igual ao número de localidades.")
  }
  
  get_design <- function(loc, loc_name) {
    cli::cli_progress_step("Gerando delineamento para {.val {loc_name}}...")
    
    trats <- c(checks, lines)
    treatment_list <- data.frame(list(ENTRY = 1:length(trats), NAME = trats))
    
    ARCBD2 <- RCBD_augmented(
      lines = length(lines),
      checks = length(checks),
      b = blocks,
      seed = seed + loc,
      locationNames = loc_name,
      data = treatment_list
    )
    
    info <- ARCBD2$infoDesign
    fieldbook <- 
      ARCBD2$fieldBook |>
      dplyr::select(-c(ID, COLUMN, EXPT, YEAR))
    
    generate_plot <- function(layout_df) {
      fieldbook_layout <- 
        fieldbook |>
        dplyr::mutate(
          UNIQUE_ID = uuid::UUIDgenerate(n = nrow(fieldbook)),
          ROW = layout_df$ROW,
          COL = layout_df$COL,
          YEAR = exp_year,
          LOCATION = loc_name,
          .before = 1
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(PLOT = dplyr::row_number()) |> 
        unite("FACTORS", YEAR, LOCATION, ROW, COL, YEAR, PLOT, CHECKS, TREATMENT, remove = FALSE) |> 
        mutate(UNIQUE_ID = uuid::UUIDfromName(namespace, FACTORS), .before = 1) |> 
        dplyr::select(-FACTORS) |> 
        dplyr::relocate(LOCATION, ROW, COL, .before = 2)
      
      if (serpentine) {
        cli::cli_progress_step("Aplicando layout serpentina...")
        fieldbook_layout <- 
          fieldbook_layout |>
          dplyr::group_by(LOCATION, ROW) |>
          dplyr::arrange(ifelse(ROW %% 2 == 0, dplyr::desc(COL), COL), .by_group = TRUE) |>
          dplyr::ungroup() |>
          dplyr::mutate(PLOT = dplyr::row_number())
      }
      
      row_max <- max(fieldbook_layout$ROW)
      block_sep <- switch(layout,
                          default = seq(1.5, row_max - 0.5, by = 1),
                          custom  = seq(layout_allocation[1] + 0.5, row_max - 0.5, by = layout_allocation[1])
      )
      caption_text <- glue::glue("Local: {loc_name}. Layout: {layout}. Lines: {length(lines)}. Checks: {length(checks)}.")
      cli::cli_progress_step("Gerando o croqui...")
      p <- ggplot2::ggplot(fieldbook_layout, ggplot2::aes(COL, ROW)) +
        {if (fill_color)
          ggplot2::geom_tile(ggplot2::aes(fill = factor(ROW)), color = "black")
          else
            ggplot2::geom_tile(fill = "gray", color = "black")} +
        ggplot2::geom_text(ggplot2::aes(label = TREATMENT), size = text_size) +
        ggplot2::geom_hline(yintercept = block_sep, linewidth = 1.2, color = "black") +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(0), position = "top",
                                    breaks = 1:max(fieldbook_layout$COL)) +
        ggplot2::scale_y_reverse(expand = ggplot2::expansion(0), breaks = 1:max(fieldbook_layout$ROW)) +
        ggplot2::labs(title = loc_name, x = "Coluna", y = "Linha", caption = caption_text) +
        ggplot2::theme_minimal()
      
      list(fieldbook = fieldbook_layout, fieldmap = p)
    }
    
    if (layout == "custom") {
      if (is.null(layout_allocation) || length(layout_allocation) != 2) {
        cli::cli_abort("Você deve fornecer um vetor de dois elementos para {.arg layout_allocation}.")
      }
      nrow <- layout_allocation[1]
      ncol <- layout_allocation[2]
      nparc <- nrow * ncol
      ntrats_total <- ARCBD2$infoDesign$columns_within_blocks
      
      if (ntrats_total > nparc) {
        cli::cli_abort("O número de tratamentos ({.val {ntrats_total}}) é maior que o número de parcelas no layout ({.val {nparc}}).")
      }
      cli::cli_progress_step("Customizando o layout...")
      tratnumb <- c(1:ntrats_total, rep(NA, nparc - ntrats_total))
      idx <- matrix(tratnumb, nrow = nrow, ncol = ncol, byrow = TRUE)
      
      if (serpentine && nrow > 1) {
        for (i in seq(2, nrow, by = 2)) {
          idx[i, ] <- rev(idx[i, ])
        }
      }
      
      layout_df_base <- data.frame(
        ROW = rep(seq_len(nrow), each = ncol),
        COL = rep(seq_len(ncol), times = nrow)
      )
      
      layout_df_base <- layout_df_base[!is.na(as.vector(t(idx))), , drop = FALSE]
      n_layout_rows <- nrow
      layout_df <- dplyr::bind_rows(
        purrr::map(seq_len(blocks), function(r) {
          df <- layout_df_base
          df$ROW <- df$ROW + (r - 1) * n_layout_rows
          df
        })
      )
    } else {
      ntrats <- nrow(fieldbook) / blocks
      layout_df <- data.frame(
        ROW = rep(seq_len(blocks), each = ntrats),
        COL = rep(seq_len(ntrats), times = blocks)
      )
    }
    
    generate_plot(layout_df)
  }
  
  all_results <- purrr::map2(seq_len(locations), exp_name, get_design)
  fieldbook_all <- dplyr::bind_rows(purrr::map(all_results, "fieldbook"))
  fieldmaps_all <- purrr::map(all_results, "fieldmap")
  
  return(list(fieldbook = fieldbook_all, fieldmap = fieldmaps_all))
}
