#' ui
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
create_formula <- function(fixed_effects, varying_effects) {
  n_fe <- length(fixed_effects)
  n_ve <- length(varying_effects)

  fixed_effect_only <- c("sex")
  mean_structure <- NULL

  if(n_ve == 0 & n_fe == 0) {
    valid <- FALSE
    formula <- "Please include at least one predictor."

  } else {
    if(length(intersect(varying_effects, fixed_effect_only)) > 0) {
      valid <- FALSE
      formula <- paste0(fixed_effect_only, collapse = ", ") |> paste0(" cannot be varying effects.")
    } else {
      init <- "positive | vint(total) + vreal(sens, spec) ~ 1"
      fixed <- if(n_fe > 0) paste0(" + ", fixed_effects) |> paste0(collapse = "") else ""
      varying <- if(n_ve > 0) paste0(" + (1 | ", varying_effects, ")") |> paste0(collapse = "") else ""

      valid <- TRUE
      mean_structure <- paste0("1", fixed, varying)
      formula <- paste0(init, fixed, varying)
    }
  }

  return(list(formula, mean_structure, valid))
}

check_iter_chain <- function(n_iter, n_iter_range, n_chains, n_chains_range) {
  within_range <- TRUE
  msg <- ""
  if(n_iter < n_iter_range[1] | n_iter > n_iter_range[2]) {
    msg <- paste0(msg, "The number of iterations must be between ", n_iter_range[1], " and ", n_iter_range[2], ". ")
    within_range <- FALSE
  }

  if(n_chains < n_chains_range[1] | n_chains > n_chains_range[2]) {
    msg <- paste0(msg, "The number of chains must be between ", n_chains_range[1], " and ", n_chains_range[2], ".")
    within_range <- FALSE
  }

  return(list(within_range, msg))
}


create_text_box <- function(title, content) {
  tags$div(
    class = "panel panel-default",
    tags$div(
      class = "panel-heading",
      title
    ),
    tags$div(
      class = "panel-body",
      content
    )
  )
}

create_drag_item <- function(s) {
  HTML(paste0("<p><span class='glyphicon glyphicon-move'></span> <strong>", s, "</strong></p>"))
}

waiter_ui <- function(type = "") {
  if(type == "fit") {
    tagList(
      waiter::spin_loaders(2, color = "black"),
      tags$h4("Fitting model...", style = "color: black")
    )
  } else if(type == "loo") {
    tagList(
      waiter::spin_loaders(2, color = "black"),
      tags$h4("Running diagnostics...", style = "color: black")
    )
  } else if (type == "wait") {
    tagList(
      waiter::spin_loaders(15, color = "black"),
      tags$h4("Please wait...", style = "color: black")
    )
  } else {
    waiter::spin_1()
  }
}

show_alert <- function(message, session) {
  showModal(
    modalDialog(
      title = tagList(icon("triangle-exclamation", "fa"), "Warning"),
      message
    ),
    session = session
  )
}

show_notif <- function(message, session) {
  showModal(
    modalDialog(
      title = tagList(icon("bell", "fa"), "Notification"),
      message
    ),
    session = session
  )
}
