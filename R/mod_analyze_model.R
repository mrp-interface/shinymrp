#' Model Fitting Module UI Function
#'
#' @description Creates the user interface for Bayesian model specification, fitting,
#' and comparison in the MRP application. Provides a sidebar layout with accordion
#' panels for model specification (effects selection, priors, sampling options) and
#' model upload. The main panel includes tabs for model comparison with LOO-CV
#' analysis and posterior predictive checks.
#'
#' @param id Character string. The module's namespace identifier.
#'
#' @return A `bslib::layout_sidebar` containing the model fitting interface with:
#' \itemize{
#'   \item Sidebar with model specification controls and file upload
#'   \item Virtual select inputs for fixed/varying effects and interactions
#'   \item Prior specification interface and sampling parameter controls
#'   \item Main panel with model comparison tabs and diagnostic plots
#' }
#'
#' @noRd
#'
#' @importFrom shiny NS tagList conditionalPanel fileInput actionButton uiOutput selectizeInput actionLink numericInput selectInput fluidRow column tags
mod_analyze_model_ui <- function(id) {
  ns <- NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 375,
      bslib::accordion(
        id = ns("accordion"),
        multiple = FALSE,
        bslib::accordion_panel(
          title = "Model Specification",
          value = "model_spec",
          tags$div(class = "d-flex justify-content-between align-items-start",
            tags$p(tags$strong("Step 1: Select main effects and"), tags$br(), tags$strong("interactions")),
            tags$div(style = "margin-top: 10px",
              bslib::tooltip(
                actionButton(
                  inputId = ns("effect_warning_btn"),
                  label = NULL,
                  icon = icon("info", class = "fa align-top"),
                  class = "btn btn-sm btn-secondary rounded-circle",
                  style = "width: 24px; height: 24px;"
                ),
                "Some effects are omitted (Click for details)",
                id = ns("effect_warning_tooltip"),
                placement = "left",
                options = list(trigger = "manual")
              )
            )
          ),
          shinyWidgets::virtualSelectInput(ns("fixed"), "Fixed Effects", choices = NULL, multiple = TRUE),
          shinyWidgets::virtualSelectInput(ns("varying"), "Varying Effects (Partial Pooling)", choices = NULL, multiple = TRUE),
          shinyWidgets::virtualSelectInput(ns("interaction"), "Interactions", choices = NULL, multiple = TRUE),
          tags$hr(),

          tags$p(tags$strong("Step 2: Specify Priors")),
          p("All effects have default priors, which can be customized. See the ",
            actionLink(ns("show_priors"), "list"), " of available priors.", class = "small"),
          uiOutput(ns("prior_spec_ui")),
          actionButton(ns("add_prior"), "Add Prior", class = "btn-sm btn-secondary w-100"),
          tags$hr(),

          tags$p(tags$strong("Step 3: Sampling Options")),
          selectInput(ns("iter_select"), "Iterations",
                      choices = c("100 (Test)", "500 (Low)", "2000 (Medium)", "5000 (High)", "Custom"),
                      selected = "2000 (Medium)"),
          conditionalPanel(ns = ns,
            condition = "input.iter_select == 'Custom'",
            numericInput(ns("iter_kb"), "Custom iterations", value = 1000, min = 100, max = 5000)
          ),
          numericInput(ns("chain_select"), "Chains", value = 4, min = 1, max = 8),
          numericInput(ns("seed_select"), "Seed", value = 123, min = 1, max = 100000),

          conditionalPanel(
            condition = "output.special_case == 'covid'",
            fluidRow(
              column(6, numericInput(ns("spec_kb"), "Specificity", value = 0.999, min = 0, max = 1, step = 0.01)),
              column(6, numericInput(ns("sens_kb"), "Sensitivity", value = 0.7, min = 0, max = 1, step = 0.01))
            )
          ),

          layout_column_wrap(width = "50%",
            actionButton(ns("reset_btn"), "Reset Fields", icon("rotate"), class = "w-100"),
            actionButton(ns("add_model"), "Fit Model", icon("chart-line"), class = "w-100")
          ),

          tags$p(class = "small mt-3",
            "For details about model specification and fitting, check the",
            actionLink(ns("show_fit_guide"), "User Guide."))
        ),

        bslib::accordion_panel(
          title = "Estimation Result Upload",
          value = "model_upload",
          fileInput(ns("fit_upload"), "Select .qs file with model estimation", accept = ".qs"),
          uiOutput(ns("model_feedback")),
          tags$p("Or use example result", class = "mt-2 mb-1"),
          actionButton(ns("use_example"), "Example Estimation Result", icon("table"), class = "w-100")
        )
      )
    ),
    bslib::navset_underline(id = ns("navbar_model"),
      bslib::nav_panel("Model Comparison", value = "nav_compare",
        tags$div(class = "d-flex justify-content-between align-items-center mt-4",
          uiOutput(ns("model_select_ui")),
          bslib::tooltip(
            actionButton(
              inputId = ns("loo_diagnos_btn"),
              label = NULL,
              icon = icon("sliders-h", "fa"),
              class = "btn btn-sm btn-secondary"
            ),
            "Please check LOO-CV diagnostics",
            id = ns("loo_diagnos_tooltip"),
            placement = "left",
            options = list(trigger = "manual")
          )
        ),
        tags$h4("Leave-one-out Cross-validation (LOO-CV)", class = "mt-4"),
        tags$hr(class = "break_line"),
        uiOutput(ns("loo_ui")),
        tags$h4("Posterior Predictive Check", class = "mt-4"),
        tags$hr(class = "break_line"),
        uiOutput(ns("ppc_plots"))
      )
    )
  )
}

#' Model Fitting Module Server Function
#'
#' @description Server logic for the model fitting module. Handles Bayesian model
#' specification, MCMC sampling, model comparison, and diagnostics. Manages effect
#' selection, prior specification, model fitting with CmdStan, and provides
#' comprehensive model evaluation including LOO-CV and posterior predictive checks.
#' Supports model upload/download and dynamic tab management for multiple models.
#'
#' @param id Character string. The module's namespace identifier.
#' @param global Reactive values object containing global application state
#'
#' @return Server function for the model fitting module. Creates reactive values
#' for model management, handles MCMC fitting, generates model comparison tables
#' and diagnostic plots, and manages model tabs dynamically.
#'
#' @noRd
#'
#' @importFrom shiny moduleServer reactiveVal reactive outputOptions observeEvent updateSelectInput renderUI renderTable renderPlot req isolate showModal modalDialog modalButton
#' @importFrom shinyWidgets virtualSelectInput
#' @importFrom loo loo loo_compare pareto_k_table
#' @importFrom shinyjs reset toggle show hide delay addClass disable click
#' @importFrom qs qread qsave
#' @importFrom purrr map map_chr
#' @importFrom dplyr select mutate
#' @importFrom rlang .data
mod_analyze_model_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    prior_buffer <- reactiveVal(list())
    model_buffer <- reactiveVal()
    pareto_k_dfs <- reactiveVal()
    model_uploaded <- reactiveVal()
    show_effect_warning <- reactiveVal(FALSE)

    observeEvent(input$show_priors, {
      .show_guide("model_spec")
    })

    observeEvent(input$show_fit_guide, {
      .show_guide("model_fit")
    })


    #--------------------------------------------------------------------------
    # Set display flag for tooltip if some effects are omitted
    #--------------------------------------------------------------------------
    observeEvent(global$mrp_ver, {
      req(global$workflow, global$workflow$mrp_data())

      show_effect_warning(!is.null(unlist(global$workflow$mrp_data()$vars$omit)))
        
    })
  
    observeEvent(global$input$navbar_analyze, {
      if(global$input$navbar_analyze == "nav_analyze_model") {
        if (show_effect_warning()) {
          bslib::toggle_tooltip("effect_warning_tooltip", show = TRUE)
          show_effect_warning(FALSE)
        }
      }
    })

    #--------------------------------------------------------------------------
    # Show modal diaglog for omitted effects
    #--------------------------------------------------------------------------
    observeEvent(input$effect_warning_btn, {
      bslib::toggle_tooltip("effect_warning_tooltip", show = FALSE)

      showModal(modalDialog(
        title = "Omitted Effects",
        tags$p("The following effects are omitted from the list because the corresponding data only has one level:"),
        if (length(global$workflow$mrp_data()$vars$omit$one_level) == 0) {
          tags$p("None", class = "fst-italic ml-2")
        } else {
          tags$ul(
            purrr::map(global$workflow$mrp_data()$vars$omit$one_level, ~ tags$li(.x))
          )
        },
        tags$p("The following interactions are omitted from the list because their main effects are nested:"),
        if (length(global$workflow$mrp_data()$vars$omit$nested) == 0) {
          tags$p("None", class = "fst-italic ml-2")
        } else {
          tags$ul(
            purrr::map(global$workflow$mrp_data()$vars$omit$nested, ~ tags$li(.x))
          )
        },
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })

    #--------------------------------------------------------------------------
    # Update select inputs for fixed and varying effects and prevent a variable
    # from being included as both a fixed effect and a varying effect
    #--------------------------------------------------------------------------
    purrr::map(c("fixed", "varying"), function(id) {
      observeEvent(input[[paste0(id, "_open")]], {
        if(input[[paste0(id, "_open")]]) {
          other_id <- setdiff(c("fixed", "varying"), id)
          choices <- global$workflow$mrp_data()$vars[[id]] %>%
            purrr::map(function(l) as.list(setdiff(l, input[[other_id]])))
          selected = setdiff(input[[id]], input[[other_id]])
          
          shinyWidgets::updateVirtualSelect(
            inputId = id,
            choices = choices,
            selected = selected
          )
        }
      })
    })

    #--------------------------------------------------------------------------
    # Update select input for interactions
    #--------------------------------------------------------------------------
    observeEvent(input$interaction_open, {
      if(input$interaction_open) {
        choices <- .create_interactions(
          input$fixed,
          input$varying,
          global$workflow$mrp_data()$input
        ) %>% 
        .pair_setdiff(global$workflow$mrp_data()$vars$omit$nested)

        shinyWidgets::updateVirtualSelect(
          inputId = "interaction",
          choices = choices,
          selected = input$interaction
        )
      }
    })

    #--------------------------------------------------------------------------
    # Handler for adding priors
    #--------------------------------------------------------------------------
    observeEvent(input$add_prior, {
        holder <- purrr::map(1:(length(prior_buffer()) + 1), ~ list(
          dist = input[[paste0("prior_dist_", .x)]],
          eff = input[[paste0("prior_eff_", .x)]]
        ))

        prior_buffer(holder)
        holder[[length(holder) + 1]] <- list(dist = "", eff = NULL)

        for(i in 1:length(holder)) {
          shinyWidgets::updateVirtualSelect(
            inputId = paste0("prior_eff_", i),
            choices = list(
              "Intercept" = stats::setNames(c("intercept_intercept"), c("Intercept")),
              "Fixed Effect" = if(length(input$fixed) > 0)  stats::setNames(paste0("fixed_", input$fixed), input$fixed),
              "Varying Effect" = if(length(input$varying) > 0)  stats::setNames(paste0("varying_", input$varying), input$varying),
              "Interaction" = if(length(input$interaction) > 0)  stats::setNames(paste0("interaction_", input$interaction), input$interaction)
            ),
            selected = holder[[i]]$eff
          )
        }
      })


    #--------------------------------------------------------------------------
    # Update select inputs for prior assignment
    #--------------------------------------------------------------------------
    observe({
      purrr::map(1:(length(prior_buffer()) + 1), function(i) {
        dist_id <- paste0("prior_dist_", i)
        eff_id <- paste0("prior_eff_", i)
        eff_id_open <- paste0("prior_eff_", i, "_open")
        
        # update select inputs for prior assignment
        observeEvent(input[[eff_id_open]], {
          if(input[[eff_id_open]]) {
            intercept <- stats::setNames(c("intercept_intercept"), c("Intercept"))
            fixed_effects <- if(length(input$fixed) > 0) stats::setNames(paste0("fixed_", input$fixed), input$fixed)
            varying_effects <- if(length(input$varying) > 0) stats::setNames(paste0("varying_", input$varying), input$varying)
            interactions <- if(length(input$interaction) > 0) stats::setNames(paste0("interaction_", input$interaction), input$interaction)
            
            # filter effects for structred prior
            if(input[[dist_id]] == "structured") {
              intercept <- list()
              fixed_effects <- list()
              varying_effects <- list()
              interactions <- list()
              
              if(length(input$interaction) > 0) {
                interactions <- .filter_interactions(input$interaction, input$fixed, global$workflow$mrp_data()$input)
                interactions <- stats::setNames(paste0("interaction_", interactions), interactions)
              }
            }

            
            shinyWidgets::updateVirtualSelect(
              inputId = eff_id,
              choices = list(
                "Intercept" = intercept,
                "Fixed Effect" = fixed_effects,
                "Varying Effect" = varying_effects,
                "Interaction" = interactions
              ),
              selected = input[[eff_id]]
            )
          }
        })
        
      })
    })
   

    #--------------------------------------------------------------------------
    # Render prior specification UI
    #--------------------------------------------------------------------------
    output$prior_spec_ui <- renderUI({
      holder <- prior_buffer()
      holder[[length(holder) + 1]] <- list(dist = "", eff = NULL)

      purrr::map(seq_along(holder), ~ fluidRow(
        column(width = 6,
          textInput(
            inputId = ns(paste0("prior_dist_", .x)),
            label = NULL,
            value = holder[[.x]]$dist,
            placeholder = "default"
          )
        ),
        column(width = 6,
          shinyWidgets::virtualSelectInput(
            inputId = ns(paste0("prior_eff_", .x)),
            label = NULL,
            choices = list(
              "Intercept" = NULL,
              "Fixed Effects" = NULL,
              "Varying Effects" = NULL,
              "Interaction" = NULL
            ),
            showValueAsTags = TRUE,
            search = TRUE,
            multiple = TRUE,
            hideClearButton = FALSE,
            selected = holder[[.x]]$eff
          )
        )
      ))
    })

    #--------------------------------------------------------------------------
    # Render model selection UI
    #--------------------------------------------------------------------------
    output$model_select_ui <- renderUI({
      model_names <- purrr::map_chr(global$models, ~ .x$name())
      model_ids <- purrr::map_chr(global$models, ~ .x$get_id())

      tags$div(class = "d-flex align-items-end gap-2",
        selectizeInput(
          inputId = ns("model_select"),
          label = "Select one or more models",
          choices = stats::setNames(model_ids, model_names),
          multiple = TRUE
        ),
        tags$div(style = "margin-bottom: 18px",
          actionButton(
            inputId = ns("compare_btn"),
            label = "Compare",
            class = "btn btn-sm"
          ) 
        )
      )
    })

    #-----------------------------------------------------------------------
    # Render LOO-CV UI
    #-----------------------------------------------------------------------
    output$loo_ui <- renderUI({
      req(global$models)
      global$workflow
      input$compare_btn

      selected_ids <- isolate(input$model_select)

      ui <- if(length(selected_ids) == 0) {
        NULL
      } else if(length(selected_ids) == 1) {
        tags$p("*Two or more models are required")
      } else {
        tagList(
          bslib::card(
            bslib::card_header(tags$b("Note")),
            bslib::card_body(
              tags$p(
                "Generally, a small ",
                tags$a("elpd_diff", href = "https://mc-stan.org/loo/articles/online-only/faq.html#elpd_interpretation", target = "_blank"),
                " difference (e.g., less than 4) indicates a small difference in the predictive power between models. For a large ",
                tags$a("elpd_diff", href = "https://mc-stan.org/loo/articles/online-only/faq.html#elpd_interpretation", target = "_blank"),
                " difference (e.g., greater than 4), ",
                tags$a("se_diff", href = "https://mc-stan.org/loo/articles/online-only/faq.html#elpd_interpretation", target = "_blank"),
                ", the standard error of ",
                tags$a("elpd_diff", href = "https://mc-stan.org/loo/articles/online-only/faq.html#elpd_interpretation", target = "_blank"),
                ", measures the uncertainty in the difference."
              )
            )
          ),
          tableOutput(outputId = ns("loo_table"))
        )
      }

      return(ui)
    })

    #-----------------------------------------------------------------------
    # Render posterior predictive check UI
    #-----------------------------------------------------------------------
    output$ppc_plots <- renderUI({
      global$workflow
      input$compare_btn

      selected_ids <- isolate(input$model_select)
      models <- isolate(global$models)

      if(length(selected_ids) > 0 && !is.null(models)) {
        model_names <- purrr::map_chr(models[selected_ids], ~ .x$name())
        model_formulas <- purrr::map_chr(models[selected_ids], ~ .x$formula())

        tagList(
          bslib::card(
            bslib::card_header(tags$b("Note")),
            bslib::card_body(tags$p("The plots show outcome averages computed from the observed data and 10 sets of replicated data."))
          ),
          purrr::map(seq_along(model_names), ~ list(
            HTML(paste0("<h4 class='formula'><u>", model_names[.x], "</u>", ": ", model_formulas[.x], "</h4>")),
            plotOutput(ns(paste0("compare_ppc", .x)))
          ))
        )
      }
    })

    #-----------------------------------------------------------------------
    # Render LOO table and posterior predictive check plots
    #-----------------------------------------------------------------------
    observeEvent(input$compare_btn, {
      req(input$model_select, global$models)

      selected_ids <- input$model_select

      if(length(selected_ids) >= 1) {
        waiter::waiter_show(
          html = .waiter_ui("loo"),
          color = waiter::transparent(0.9)
        )
      
        ### PPC plots
        selected_models <- global$models[selected_ids]

        purrr::map(seq_along(selected_models), function(i) {
          output[[paste0("compare_ppc", i)]] <- renderPlot({
            global$workflow$pp_check(selected_models[[i]])
          })
        })


        ### LOO table
        if (length(selected_ids) >= 2) {
          out <- NULL
          
          tryCatch({
            # Use model names instead of IDs
            selected_models <- global$models[selected_ids]
            names(selected_models) <- purrr::map_chr(selected_models, ~ .x$name())

            out <- global$workflow$compare_models(selected_models)

          }, error = function(e) {
            message(paste0("Error during LOO-CV: ", e$message))
            .show_alert("An error occured during leave-one-out cross-validation. Please check whether the models being compared were generated from the same dataset.", global$session)
          })

          if (!is.null(out)) {
            # render LOO table
            output$loo_table <- renderTable(out$compare_df, rownames = TRUE, width = "300px")

            # store pareto k tables
            pareto_k_dfs(out$pareto_k_dfs)

            # show tooltip if any Pareto k values are problematic
            bslib::toggle_tooltip("loo_diagnos_tooltip", show = out$flag)
          }

          
        }

        waiter::waiter_hide()
      }
    })


    #-----------------------------------------------------------------------
    # Show LOO-CV diagnostics
    #-----------------------------------------------------------------------
    observeEvent(input$loo_diagnos_btn, {
      bslib::toggle_tooltip("loo_diagnos_tooltip", show = FALSE)

      showModal(modalDialog(
        title = "LOO-CV Diagnostics",
        tags$div(class = "mt-0 mb-5",
          withMathJax(
            "We provide a summary of the estimated Pareto shape parameter \\(\\kappa\\) values, which estimates how far an individual leave-one-out distribution is from the full distribution. High \\(\\kappa\\) values often indicate model misspecification, outliers or mistakes in data processing, resulting in an unreliable importance sampling estimate and an unreliable approximation of LOO-CV. See the ",
            tags$a("LOO FAQ", href = "https://mc-stan.org/loo/articles/online-only/faq.html#pareto_shape_parameter_k", target = "_blank"),
            " for more details."
          )
        ),
        if (length(pareto_k_dfs()) == 0) {
          tags$p("No models selected", class = "fst-italic")
        } else {
          purrr::map(seq_along(pareto_k_dfs()), function(i) {
            tagList(
              tags$h5(names(pareto_k_dfs())[i]),
              tableOutput(ns(paste0("pareto_k_table", i)))
            )
          })
        },
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))

      purrr::map(seq_along(pareto_k_dfs()), function(i) {
        output[[paste0("pareto_k_table", i)]] <- renderTable(
          pareto_k_dfs()[[i]] %>%
            as.data.frame() %>%
            mutate(Count = as.integer(.data$Count)) %>%
            select(.data$Count, .data$Proportion),
          rownames = TRUE
        )
      })
    })

    #-----------------------------------------------------------------------
    # Render feedbacl for model upload
    #-----------------------------------------------------------------------
    output$model_feedback <- renderUI({
      if(model_uploaded()) {
        model <- isolate(model_buffer())
        valid <- checkmate::check_class(
          model,
          classes = "ShinyMRPModel",
          null.ok = FALSE
        )

        if(valid) {
          tags$div(
            tagList(icon("circle-check", "fa"), "Success"),
            tags$p("Estimation result loaded successfully.", class = "small")
          )
        } else {
          tags$div(
            tagList(icon("circle-xmark", "fa"), "Error"),
            tags$p("Invalid. Object must be of class ShinyMRPModel.", class = "small")
          )
        }
      }
    })

    #-----------------------------------------------------------------------
    # Reset input fields
    #-----------------------------------------------------------------------
    observeEvent(input$reset_btn, {
      prior_buffer(list())

      .reset_inputs(global$workflow$mrp_data()$vars)
    })

    #-----------------------------------------------------------------------
    # Add model
    #-----------------------------------------------------------------------
    observeEvent(input$add_model, {
      # Check if CmdStan is installed
      if(is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
        msg <- if(.get_config("demo")) {
          tags$p("This functionality is currently not available for the web version of the MRP interface. Try the example model estimation provided under ", tags$b("Upload Estimation Results"), ".")
        } else {
          tags$p("CmdStan is not installed to compile user-defined models. Try the example model estimation provided under ", tags$b("Upload Estimation Results"), ".")
        }
        .show_alert(msg, global$session)
        return()
      }
      
      # Validate iteration and chain parameters
      n_iter <- if(input$iter_select == "Custom") {
        input$iter_kb
      } else {
        as.integer(strsplit(input$iter_select, " ")[[1]][1])
      }
      n_chains <- input$chain_select
      seed <- input$seed_select
      
      check <- .check_iter_chain(
        n_iter, .const()$ui$model$iter_range,
        n_chains, .const()$ui$model$chain_range,
        seed
      )
      
      if(!check$valid) {
        .show_alert(
          tagList(
            tags$ul(
              purrr::map(check$msg, ~ tags$li(.x))
            )
          ),
          global$session
        )
        return()
      }
      
      # Check if maximum number of models is reached
      if(length(global$models) + 1 > .const()$ui$model$max_models) {
        .show_alert("Maximum number of models reached. Please remove existing models to add more.", global$session)
        return()
      }
      
      # Check if the user selected any predictors
      if(length(c(input$fixed, input$varying, input$interaction)) == 0) {
        .show_alert("No predictor has been selected. Please include at least one.", global$session)
        return()
      }
      
      # Check if prior syntax is correct
      valid_priors <- purrr::map(1:(length(prior_buffer()) + 1), function(i) {
        input[[paste0("prior_dist_", i)]] %>%
          .clean_prior_syntax() %>%
          .check_prior_syntax()
      }) %>% unlist()
      
      if(!all(valid_priors)) {
        .show_alert("Invalid prior provided. Please check the User Guide for the list of available priors.", global$session)
        return()
      }
      
      # All validation passed, show waiter and proceed
      waiter::waiter_show(
        html = .waiter_ui("fit"),
        color = waiter::transparent(0.9)
      )
      
      # Try to fit the model
      tryCatch({
        # assign default priors to all selected effects
        model_spec <- list(intercept = list(intercept = .const()$default_priors$intercept))
        for(type in .const()$args$effect_types) {
          for(v in input[[type]]) {
            model_spec[[type]][[v]] <- .const()$default_priors[[type]]
          }
        }
    
        # assign user-specified priors
        for(i in 1:(length(prior_buffer()) + 1)) {
          dist <- input[[paste0("prior_dist_", i)]] %>% .clean_prior_syntax()
          eff <- input[[paste0("prior_eff_", i)]]

          if(!is.null(.nullify(dist))) {
            for(s in eff) {
              ss <- strsplit(s, split = "_")[[1]]
              model_spec[[ss[1]]][[ss[2]]] <- dist
            }
          }
        }

        # include sensitivity and specificity for COVID data
        extra <- if (!is.null(global$workflow$metadata()$special_case) &&
            global$workflow$metadata()$special_case == "covid") {
          list(
            sens = input$sens_kb,
            spec = input$spec_kb
          )
        } else {
          list(
            sens = 1,
            spec = 1
          )
        }

        # create model object
        model <- global$workflow$create_model(
          intercept_prior = model_spec$intercept$intercept,
          fixed = model_spec$fixed,
          varying = model_spec$varying,
          interaction = model_spec$interaction,
          sens = extra$sens,
          spec = extra$spec
        )

        # run MCMC
        model$fit(
          n_iter = n_iter,
          n_chains = n_chains,
          seed = seed
        )

        model_buffer(model)

        global$trigger_model_change()

      }, error = function(e) {
        message(paste0("Error fitting model: ", e$message))
        .show_alert("An error occurred during model fitting. Please report this as an issue on our GitHub page and we will resolve as soon as possible. Thank you for your patience.", global$session)
        waiter::waiter_hide()
      })
    })

    observeEvent(input$fit_upload, {
      waiter::waiter_show(
        html = .waiter_ui(),
        color = waiter::transparent(0.9)
      )

      model_buffer(
        qs::qread(input$fit_upload$datapath)
      )

      model_uploaded(TRUE)
      shinyjs::delay(5000, { model_uploaded(FALSE) })

      global$trigger_model_change()

    })

    observeEvent(input$use_example, {
      waiter::waiter_show(
        html = .waiter_ui(),
        color = waiter::transparent(0.9)
      )

      file_name <- .create_example_filename(
        global$workflow$metadata(),
        suffix = "fit",
        ext = ".qs"
      )

      model_buffer(
        .fetch_data(file_name, subdir = "example/fit")
      )

      global$trigger_model_change()
    })
    
    # create new model tab
    observeEvent(global$model_ver, {
      req(model_buffer())

      model <- model_buffer()
      model$assign_id()
      model_id <- model$get_id()

      model$summary()
      model$log_lik()
      model$ppc()
   
      # create new model tab
      tab_ids <- purrr::map_chr(global$models, function(m) m$get_id("tab"))
      last_tab_id <- if(length(tab_ids) > 0) {
        as.character(tab_ids[length(tab_ids)])
      } else {
        "nav_compare"
      }
      
      bslib::nav_insert(
        id = "navbar_model",
        target = last_tab_id,
        position = "after",
        select = TRUE,
        nav = .create_model_tab(ns, model, last_tab_id)
      )

      # changeable tab title
      model_name <- paste0("Model ", length(global$models) + 1)
      output[[model$get_id("title")]] <- renderText(model_name)
      model$set_name(model_name)

      # tab removal
      observeEvent(input[[model$get_id("rm_btn")]], {
        # remove model object and tab
        bslib::nav_remove("navbar_model", global$models[[model_id]]$get_id("tab"), session)
        global$models[[model_id]] <- NULL

        # rename tabs
        new_names <- if(length(global$models) > 0) paste0("Model ", seq_along(global$models)) else c()
        purrr::map(seq_along(new_names), function(i) {
          output[[global$models[[i]]$get_id("title")]] <- renderText(new_names[i])
          global$models[[i]]$set_name(new_names[i])
        })
      })

      # show sampler diagnostics
      observeEvent(input[[model$get_id("diagnos_btn")]], {
        bslib::toggle_tooltip(model$get_id("diagnos_tooltip"), show = FALSE)

        showModal(modalDialog(
          title = "Sampler Diagnostics",
          tags$p("Below is a summary of problems encountered during sampling for the current model. Ideally, the top two quantities are close to 0 and E-BFMI value are above 0.3. For details, see ",
            tags$a("https://mc-stan.org/misc/warnings", href = "https://mc-stan.org/misc/warnings", target = "_blank"), "."),
          tableOutput(ns(model$get_id("diagnos_tbl"))),
          easyClose = TRUE,
          size = "l",
          footer = modalButton("Close")
        ))
      })

      # render sampling diagnostics table
      output[[model$get_id("diagnos_tbl")]] <- renderTable(
        model$diagnostics(),
        colnames = FALSE
      )

      # render fixed and varying effect tables
      output[[model$get_id("fixed_tbl")]] <- renderTable(
        model$summary()$fixed,
        rownames = TRUE,
        na = ""
      )
      output[[model$get_id("varying_tbl")]] <- renderTable(
        model$summary()$varying,
        rownames = TRUE,
        na = ""
      )
      output[[model$get_id("other_tbl")]] <- renderTable(
        model$summary()$other,
        rownames = TRUE,
        na = ""
      )

      # render ppc plot
      output[[model$get_id("ppc_plot")]] <- renderPlot({
        global$workflow$pp_check(model)
      })

      # postprocessing
      observeEvent(input[[model$get_id("postprocess_btn")]], {

        waiter::waiter_show(
          html = .waiter_ui("pstrat"),
          color = waiter::transparent(0.9)
        )

        global$models[[model_id]]$poststratify()

        waiter::waiter_hide()
        
        # download fit result after postprocessing
        output[[model$get_id("save_fit_btn")]] <- downloadHandler(
          filename = function() { 
            paste0("estimation_w_postprocess_", format(Sys.Date(), "%Y%m%d"), ".qs")
          },
          content = function(file) {
            waiter::waiter_show(
              html = .waiter_ui(),
              color = waiter::transparent(0.9)
            )

            model$save(file)
            
            waiter::waiter_hide()
          }
        )
        
        # change fit result download button color to indicate inclusion of postprocessing results
        shinyjs::addClass(model$get_id("save_fit_btn"), "btn btn-primary")

        # disable postprocessing button
        shinyjs::disable(model$get_id("postprocess_btn"))
      })
      
      # download fit result before postprocessing
      output[[model$get_id("save_fit_btn")]] <- downloadHandler(
        filename = function() {
          paste0("estimation_wo_postprocess_", format(Sys.Date(), "%Y%m%d"), ".qs")
        },
        content = function(file) {
          waiter::waiter_show(
            html = .waiter_ui(),
            color = waiter::transparent(0.9)
          )

          model$save(file)

          waiter::waiter_hide()
        }
      )

      # download Stan code
      output[[model$get_id("save_code_btn")]] <- downloadHandler(
        filename = function() { "model.stan" },
        content = function(file) {
          writeLines(model$stan_code(), file)
        }
      )

      # trigger diagnostics tooltip
      if(.check_divergence(model$diagnostics(summarize = FALSE))) {
        bslib::toggle_tooltip(
          model$get_id("diagnos_tooltip"),
          show = TRUE
        )
      }

      # if object contains poststratification results
      if(model$check_estimate_exists()) {
        shinyjs::delay(
          100,
          shinyjs::click(model$get_id("postprocess_btn"))
        )
      }

      # add to model list
      global$models[[model_id]] <- model

      waiter::waiter_hide()
    })


    #------------------------------------------------------------------
    # reset everything when new data is uploaded,
    # when poststratification data changes,
    # or when a new workflow is created
    #------------------------------------------------------------------
    observeEvent(
      eventExpr = list(
        global$workflow,
        global$prep_ver,
        global$mrp_ver
      ), 
      handlerExpr = {
        # reset input fields
        prior_buffer(list())
        model_buffer(NULL)
        pareto_k_dfs(NULL)
        model_uploaded(FALSE)

        .reset_inputs(vars = list(
          fixed = list(),
          varying = list()
        ))

        # delete all model tabs
        tab_ids <- purrr::map_chr(global$models, function(m) m$get_id("tab"))
        purrr::map(tab_ids, function(id) {
          bslib::nav_remove("navbar_model", id, session)
        })

        bslib::nav_select(
          id = "navbar_model",
          selected = "nav_compare",
          session = session
        )

        # clear model object list
        global$models <- NULL

        # reset the accordion to show the model specification panel
        bslib::accordion_panel_open(
          id = "accordion",
          values = "model_spec",
          session = session
        )
      }
    )
  })
}
