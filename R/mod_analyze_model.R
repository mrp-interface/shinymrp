#' analyze_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinyWidgets
#' @import zeallot
mod_analyze_model_ui <- function(id){
  ns <- NS(id)
  tags$div(class = "pad_top",
    sidebarLayout(
      sidebarPanel(width = 3,
        tagList(
          HTML("<details open='true'><summary class='summary'>Model Specification</summary>"),
          tags$div(style = "margin: 15px 10px",
            tags$p(tags$u("Step 1"), ": Select main effects and interactions"),
            virtualSelectInput(
              ns("fixed"),
              label = "Fixed Effects",
              choices = NULL,
              showValueAsTags = TRUE,
              search = TRUE,
              multiple = TRUE,
              hideClearButton = FALSE
            ),
            virtualSelectInput(
              ns("varying"),
              label = "Varying Effects (Partial Pooling)",
              choices = NULL,
              showValueAsTags = TRUE,
              search = TRUE,
              multiple = TRUE,
              hideClearButton = FALSE
            ),
            virtualSelectInput(
              ns("interaction"),
              label = "Interactions",
              choices = NULL,
              showValueAsTags = TRUE,
              search = TRUE,
              multiple = TRUE,
              hideClearButton = FALSE
            ),
            tags$p(tags$u("Step 2"), ": Specify priors"),
            tags$p(class = "ref",
              "All effects are automatically assigned default priors and can be changed below. Check this",
              actionLink(
                inputId = ns("show_priors"),
                label = "list",
                class = "action_link"
              ),
              "of available priors."
            ),
            uiOutput(ns("prior_spec_ui")),
            actionButton(
              inputId = ns("add_prior"),
              label = "Add prior",
              class = "btn btn-sm"
            ),
            tags$div(class = "pad_top"),
            tags$p(tags$u("Step 3"), ": Set sampling options"),
            selectInput(
              inputId = ns("iter_select"),
              label = "Select the number of iterations",
              choices = c("100 (Test)", "500 (Low)", "2000 (Medium)", "5000 (High)", "Custom"),
              selected = "2000 (Medium)"
            ),
            conditionalPanel(ns = ns,
              condition = "input.iter_select == 'Custom'",
              numericInput(
                inputId = ns("iter_kb"),
                label = "Enter the number of iterations",
                min = 100, max = 5000, step = 100,
                value = 1000
              )
            ),
            numericInput(
              inputId = ns("chain_select"),
              label = "Select the number of chains",
              min = 1, max = 8, step = 1,
              value = 4
            ),
            numericInput(
              inputId = ns("seed_select"),
              label = "Set seed",
              min = 1, max = 100000, step = 1,
              value = 123
            ),
            conditionalPanel(
              condition = "output.data_format == 'temporal_covid'",
              fluidRow(
                column(width = 6,
                  numericInput(
                    inputId = ns("spec_kb"),
                    label = "Specificity",
                    min = 0, max = 1, step = 0.01,
                    value = 0.999
                  )
                ),
                column(width = 6,
                  numericInput(
                    inputId = ns("sens_kb"),
                    label = "Sensitivity",
                    min = 0, max = 1, step = 0.01,
                    value = 0.7
                  )
                )
              )
            ),
            tags$div(class = "justify pad_bottom",
              actionButton(
                inputId = ns("reset_btn"),
                label = "Reset fields",
                icon = icon("arrow-rotate-right", lib = "font-awesome", class = "button_icon"),
                width = "49.5%"
              ),
              actionButton(
                inputId = ns("add_model"),
                label = "Fit model",
                icon = icon("chart-line", lib = "font-awesome", class = "button_icon"),
                width = "49.5%"
              )
            ),
            tags$p(class = "ref",
              "For details about the model fitting process, open",
              actionLink(
                inputId = ns("show_fit_guide"),
                label = "Guide.",
                class = "action_link"
              )
            )
          ),
          HTML("</details>"),
          HTML("<details><summary class='summary'>Upload Estimation Results</summary>"),
          tags$div(style = "margin: 15px 10px",
            fileInput(
              inputId = ns("fit_upload"),
              label = "Select a RDS file containing a model estimation",
              accept = ".RDS"
            ),
            tags$p("Or use example estimation result", class = "custom_label"),
            actionButton(
              inputId = ns("use_example"),
              label = "Example estimation result",
              icon = icon("table", lib = "font-awesome", class = "button_icon")
            )
          ),
          HTML("</details>")
        )
      ),
      mainPanel(width = 9,
        tabsetPanel(id = ns("navbar_model"),
          tabPanel("Model Comparison",
            value = "nav_compare",
            tags$div(class = "pad_top",
              uiOutput(outputId = ns("model_select_ui")),
              tags$h4("Leave-one-out Cross-validation", class = "break_title"),
              tags$hr(class = "break_line"),
              uiOutput(outputId = ns("loo_ui")),
              tags$h4("Posterior Predictive Check", class = "break_title"),
              tags$hr(class = "break_line"),
              uiOutput(outputId = ns("ppc_plots"))
            )
          )
        )
      )
    )
  )
}

#' analyze_model Server Functions
#'
#' @noRd
mod_analyze_model_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    prior_buffer <- reactiveVal(list())
    model_buffer <- reactiveVal()
    
    observeEvent(global$input$navbar_analyze, {
      if(global$input$navbar_analyze == "nav_analyze_model") {
        if(is.null(global$mrp)) {
          showModal(
            modalDialog(
              title = tagList(icon("triangle-exclamation", "fa"), "Warning"),
              "Invalid input data.",
              footer = actionButton(
                inputId = ns("to_upload"),
                label = "Go to data upload page"
              )
            ),
            session = global$session
          )
        }
      }
    })

    observeEvent(input$to_upload, {
      updateTabsetPanel(global$session,
                        inputId = "navbar_analyze",
                        selected = "nav_analyze_upload")

      removeModal(global$session)
    })

    observeEvent(input$show_priors, {
      show_guide("model_select", session)
    })

    observeEvent(input$show_fit_guide, {
      show_guide("model_fit", session)
    })

   observeEvent(input$add_prior, {
      holder <- purrr::map(1:(length(prior_buffer()) + 1), ~ list(
        dist = input[[paste0("prior_dist_", .x)]],
        eff = input[[paste0("prior_eff_", .x)]]
      ))

      prior_buffer(holder)
      holder[[length(holder) + 1]] <- list(dist = "", eff = NULL)

      for(i in 1:length(holder)) {
        updateVirtualSelect(
          inputId = paste0("prior_eff_", i),
          choices = list(
            "Intercept" = setNames(c("Intercept_Intercept"), c("Intercept")),
            "Fixed Effect" = if(length(input$fixed) > 0)  setNames(paste0("fixed_", input$fixed), input$fixed),
            "Varying Effect" = if(length(input$varying) > 0)  setNames(paste0("varying_", input$varying), input$varying),
            "Interaction" = if(length(input$interaction) > 0)  setNames(paste0("interaction_", input$interaction), input$interaction)
          ),
          selected = holder[[i]]$eff
        )
      }
    })
   
   # create interaction and populate select input
   observeEvent(input$interaction_open, {
     if(input$interaction_open) {
       updateVirtualSelect(
         inputId = "interaction",
         choices = create_interactions(
           input$fixed,
           input$varying,
           global$mrp$input
         ),
         selected = input$interaction
       )
     }
   })

   # prevent a variable from being included as
   # both a fixed effect and a varying effect
   purrr::map(c("fixed", "varying"), function(id) {
     observeEvent(input[[paste0(id, "_open")]], {
       if(input[[paste0(id, "_open")]]) {
         other_id <- setdiff(c("fixed", "varying"), id)
         choices <- global$mrp$vars[[id]] |>
           purrr::map(function(l) as.list(setdiff(l, input[[other_id]])))
         selected = setdiff(input[[id]], input[[other_id]])
         
         updateVirtualSelect(
           inputId = id,
           choices = choices,
           selected = selected
         )
       }
     })
   })



   observe({
     purrr::map(1:(length(prior_buffer()) + 1), function(i) {
       dist_id <- paste0("prior_dist_", i)
       eff_id <- paste0("prior_eff_", i)
       eff_id_open <- paste0("prior_eff_", i, "_open")
       
       # update select inputs for prior assignment
       observeEvent(input[[eff_id_open]], {
         if(input[[eff_id_open]]) {
           intercept <- setNames(c("Intercept_Intercept"), c("Intercept"))
           fixed_effects <- if(length(input$fixed) > 0) setNames(paste0("fixed_", input$fixed), input$fixed)
           varying_effects <- if(length(input$varying) > 0) setNames(paste0("varying_", input$varying), input$varying)
           interactions <- if(length(input$interaction) > 0) setNames(paste0("interaction_", input$interaction), input$interaction)
           
           # filter effects for structred prior
           if(input[[dist_id]] == "structured") {
             intercept <- list()
             fixed_effects <- list()
             varying_effects <- list()
             interactions <- list()
             
             if(length(input$interaction) > 0) {
               interactions <- filter_interactions(input$interaction, input$fixed, global$mrp$input)
               interactions <- setNames(paste0("interaction_", interactions), interactions)
             }
           }

           
           updateVirtualSelect(
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
   



   output$prior_spec_ui <- renderUI({
      holder <- prior_buffer()
      holder[[length(holder) + 1]] <- list(dist = "", eff = NULL)

      purrr::map(1:length(holder), ~ fluidRow(
        column(width = 6,
          textInput(
            inputId = ns(paste0("prior_dist_", .x)),
            label = NULL,
            value = holder[[.x]]$dist,
            placeholder = "default"
          )
        ),
        column(width = 6,
          virtualSelectInput(
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

    # Model select input
    output$model_select_ui <- renderUI({
      
      tags$div(style = "display: flex; align-items: self-end; gap: 10px;",
        selectizeInput(
          inputId = ns("model_select"),
          label = "Select one or more models",
          choices = names(global$models),
          multiple = TRUE
        ),
        tags$div(style = "margin-bottom: 15px",
          actionButton(
            inputId = ns("diagnos_btn"),
            label = "Compare",
            class = "btn btn-sm"
          ) 
        )

      )
    })

    output$loo_ui <- renderUI({
      global$data_format
      input$diagnos_btn
      selected_names <- isolate(input$model_select)

      ui <- NULL
      if(!is.null(isolate(global$models))) {
        if(length(selected_names) == 0) {
          ui <- NULL
        } else if(length(selected_names) == 1) {
          ui <- tags$p("*Two or more models are required")
        } else {
          ui <- tagList(
            create_text_box(
              title = tags$b("Note"),
              tags$p("Generally, a small ", tags$code("elpd_diff"), "difference (e.g., less than 4) indicates a small difference in the predictive power between models. For a large ", tags$code("elpd_diff"), " difference (e.g., greater than 4), ", tags$code("se_diff"), ", the standard error of ", tags$code("elpd_diff"), ", measures the uncertainty in the difference. Find more details about how to inteprete these terms ", tags$a("here", href = "https://mc-stan.org/loo/articles/online-only/faq.html#elpd_interpretation", target = "_blank"), ".")
            ),
            tableOutput(outputId = ns("loo_table"))
          )

          output$loo_table <- renderTable({
            waiter::waiter_show(
              html = waiter_ui("loo"),
              color = waiter::transparent(0.9)
            )

            df <- isolate(global$models[selected_names]) |>
              purrr::map(function(m) loo::loo(m$fit$loo$draws("log_lik"))) |>
              loo::loo_compare() |>
              as.data.frame() |>
              select(elpd_diff, se_diff)

            waiter::waiter_hide()

            return(df)
          }, rownames = TRUE)
        }
      }

      return(ui)
    })

    # PPC plots
    output$ppc_plots <- renderUI({
      global$data_format
      input$diagnos_btn

      selected_names <- isolate(input$model_select)

      if(length(selected_names) > 0 & !is.null(isolate(global$models))) {
        formulas <- purrr::map(isolate(global$models[selected_names]), function(m) m$formula)

        tagList(
          create_text_box(
            title = tags$b("Note"),
            if(global$data_format == "temporal_covid") {
              tags$p("The plots show the weekly postive response rates computed from the observed data and 10 sets of replicated data.")
            } else {
              tags$p("The plots show the proportion of positive responses computed from the observed data and 10 sets of replicated data.")
            }
          ),
          purrr::map(1:length(formulas), ~ list(
            HTML(paste0("<h4><u>", selected_names[.x], "</u>", ": ", formulas[[.x]], "</h4>")),
            plotOutput(ns(paste0("compare_ppc", .x)))
          ))
        )
      }
    })

    # run model comparison
    observeEvent(input$diagnos_btn, {

      selected_names <- input$model_select

      if(length(selected_names) > 0) {

        yreps <- purrr::map(global$models[selected_names], function(m) m$yrep)

        purrr::map(1:length(yreps), function(i) {
          output[[paste0("compare_ppc", i)]] <- renderPlot({
            if(!is.null(isolate(global$models))) {
              if(global$data_format == "temporal_covid" | global$data_format == "temporal_other") {
                plot_ppc_covid_subset(
                  yreps[[i]],
                  global$mrp$input,
                  global$plotdata$dates
                )
              } else {
                plot_ppc_poll(
                  yreps[[i]],
                  global$mrp$input
                )
              }
            }
          })
        })
      }

    })


    # reset input fields
    observeEvent(input$reset_btn, {
      prior_buffer(list())

      reset_inputs(global$mpr$vars)
    })

    # add model
    observeEvent(input$add_model, {
      if(is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
        if(global$web_version) {
          show_alert(tags$p("This functionality is currently not available for the web version of the MRP interface. Try the example model estimation provided under ", tags$b("Upload Estimation Results"), "."), global$session)
        } else {
          show_alert(tags$p("CmdStan is not installed to compile user-defined models. Try the example model estimation provided under ", tags$b("Upload Estimation Results"), "."), global$session)
        }
      } else {
        n_iter <- if(input$iter_select == "Custom") input$iter_kb else as.integer(strsplit(input$iter_select, " ")[[1]][1])
        n_chains <- input$chain_select
        seed <- input$seed_select

        # check if number of iterations and number of chains are within defined range
        c(valid, errors) %<-% check_iter_chain(
          n_iter, GLOBAL$ui$iter_range,
          n_chains, GLOBAL$ui$chain_range,
          seed
        )

        if(valid) {
          # check if maximum number of models is reached
          if(length(global$models) <= GLOBAL$ui$max_model) {
            # check if the user selects nothing
            if(length(c(input$fixed, input$varying, input$interaction)) > 0) {
              # check if prior syntax are correct
              valid_priors <- purrr::map(1:(length(prior_buffer()) + 1), function(i) {
                check_prior_syntax(input[[paste0("prior_dist_", i)]])
              }) |> unlist()

              if(all(valid_priors)) {
                waiter::waiter_show(
                  html = waiter_ui("fit"),
                  color = waiter::transparent(0.9)
                )

                # assign default priors to all selected effects
                all_priors <- list(Intercept = list(Intercept = GLOBAL$default_priors$Intercept))
                for(type in c("fixed", "varying", "interaction")) {
                  for(v in input[[type]]) {
                    all_priors[[type]][[v]] <- GLOBAL$default_priors[[type]]
                  }
                }
  
                # assign user-specified priors
                for(i in 1:(length(prior_buffer()) + 1)) {
                  dist <- input[[paste0("prior_dist_", i)]]
                  eff <- input[[paste0("prior_eff_", i)]]
                  if(dist != "") {
                    for(s in eff) {
                      ss <- strsplit(s, split = "_")[[1]]
                      all_priors[[ss[1]]][[ss[2]]] <- dist
                    }
                  }
                }

                # classify effects
                all_priors <- all_priors |>
                  group_effects(global$mrp$input) |>
                  ungroup_effects()

                # create a list to store model info
                model <- list()
                model$data_format <- global$data_format
                model$effects <- all_priors
                model$formula <- create_formula(all_priors)
                model$n_iter <- n_iter
                model$n_chains <- n_chains
                model$gq_data <- list(
                  subgroups = intersect(GLOBAL$vars$subgroups, names(global$mrp$new)),
                  temporal = "time" %in% names(global$mrp$input)
                )

                # run MCMC
                c(model$fit, model$stan_data, model$stan_code) %<-% run_mcmc(
                  input_data = stan_factor(global$mrp$input, GLOBAL$vars$ignore),
                  new_data = stan_factor(global$mrp$new, GLOBAL$vars$ignore),
                  effects = model$effects,
                  gq_data = model$gq_data,
                  n_iter = model$n_iter,
                  n_chains = model$n_chains,
                  seed = input$seed_select,
                  sens = if(global$data_format == "temporal_covid") input$sens_kb else 1,
                  spec = if(global$data_format == "temporal_covid") input$spec_kb else 1
                )
                
                model_buffer(model)
  
              } else {
                show_alert("Invalid prior provided. Please check Guide (bottom right corner) for the list of available priors.", global$session)
              }
            } else {
              show_alert("No predictor has been selected. Please include at least one.", global$session)
            }
          } else {
            show_alert("Maximum number of models reached. Please removed existing models to add more.", global$session)
          }
        } else {
          show_alert(
            tagList(
              tags$ul(
                purrr::map(errors, ~ tags$li(.x))
              )
            ),
            global$session
          )
        }
      }

    })

    observeEvent(input$fit_upload, {
      waiter::waiter_show(
        html = waiter_ui("wait"),
        color = waiter::transparent(0.9)
      )
      
      model <- qs::qread(input$fit_upload$datapath)
      
      if(!("data_format" %in% names(model))) {
        show_alert("The uploaded RDS file does not contain a model estimation.", global$session)
      } else if(model$data_format != global$data_format) {
        if(global$data_format == "temporal_covid") {
          show_alert(paste0("The uploaded RDS file contains model estimation for cross-sectional data instead of spatio-temporal data."), global$session)
        } else {
          show_alert(paste0("The uploaded RDS file contains model estimation for spatio-temporal data instead of cross-sectional data."), global$session)
        }
      } else {
        model_buffer(model)
      }

    })

    observeEvent(input$use_example, {
      waiter::waiter_show(
        html = waiter_ui("wait"),
        color = waiter::transparent(0.9)
      )
      
      if(global$data_format == "temporal_covid") {
        model <- qs::qread(app_sys("extdata/fit_st.RDS"))
      } else {
        model <- qs::qread(app_sys("extdata/fit_cs.RDS"))
      }

      model_buffer(model)
    })
    
    # create new model tab
    observeEvent(model_buffer(), {
      model <- model_buffer()
      
      # extract posterior summary of coefficients
      if (is.null(model$fixed) || is.null(model$varying)) {
        c(model$fixed, model$varying) %<-% extract_parameters(model$fit$mcmc, model$effects)
      }
      
      # run standalone generated quantities for LOO
      if (is.null(model$fit$loo)) {
        model$fit$loo %<-% run_gq(
          fit_mcmc = model$fit$mcmc,
          stan_code = model$stan_code$loo,
          stan_data = model$stan_data,
          n_chains = model$n_chains
        )
      }
      
      # run standalone generated quantities for PPC
      if (is.null(model$fit$ppc)) {
        model$fit$ppc %<-% run_gq(
          fit_mcmc = model$fit$mcmc,
          stan_code = model$stan_code$ppc,
          stan_data = model$stan_data,
          n_chains = model$n_chains
        )
      }
      
      # data for PPC plots
      if (is.null(model$yrep)) {
        model$yrep <- extract_yrep(
          model$fit$ppc,
          global$mrp$input,
          model$gq_data
        )
      }
 

      waiter::waiter_hide()
      
      model_name <- paste0("Model ", length(global$models) + 1)
      
      # UI element IDs
      model$IDs <- list(
        fixed = paste0("fixed", global$model_count),
        varying = paste0("varying", global$model_count),
        ppc = paste0("ppc", global$model_count),
        tab = paste0("tab", global$model_count),
        title = paste0("title", global$model_count),
        rm_btn = paste0("rm_btn", global$model_count),
        save_fit_btn = paste0("save_fit_btn", global$model_count),
        save_code_btn = paste0("save_code_btn", global$model_count),
        postprocess_btn = paste0("postprocess_btn", global$model_count)
      )
      
      # create new model tab
      create_model_tab(ns, model)

      # changeable tab title
      output[[model$IDs$title]] <- renderText(model_name)
      
      # tab removal
      observeEvent(input[[model$IDs$rm_btn]], {
        # remove model object and tab
        global$models[[model_name]] <- NULL
        removeTab("navbar_model", model$IDs$tab, session)
        
        # re-index model objects and tabs
        names(global$models) <- if(length(global$models) > 0) paste0("Model ", 1:length(global$models)) else character()
        purrr::map(names(global$models), function(name) {
          output[[global$models[[name]]$IDs$title]] <- renderText(name)
        })
      })
      
      # render fixed and varying effect tables
      output[[model$IDs$fixed]] <- renderTable(model$fixed, rownames = TRUE)
      output[[model$IDs$varying]] <- renderTable(model$varying, rownames = TRUE)
      
      # render ppc plot
      output[[model$IDs$ppc]] <- renderPlot({
        req(model$yrep)
        
        if(global$data_format == "temporal_covid" | global$data_format == "temporal_other") {
          plot_ppc_covid_subset(
            model$yrep,
            global$mrp$input,
            global$plotdata$dates
          )
        } else {
          plot_ppc_poll(
            model$yrep,
            global$mrp$input
          )
        }
      })
      
      # postprocessing
      observeEvent(input[[model$IDs$postprocess_btn]], {

        if(is.null(global$models[[model_name]]$fit$pstrat)) {
          waiter::waiter_show(
            html = waiter_ui("pstrat"),
            color = waiter::transparent(0.9)
          )
          
          model <- global$models[[model_name]]
          
          model$fit$pstrat %<-% run_gq(
            fit_mcmc = model$fit$mcmc,
            stan_code = model$stan_code$pstrat,
            stan_data = model$stan_data,
            n_chains = model$n_chains
          )
          
          model$est <- extract_est(
            model$fit$pstrat,
            global$mrp$new,
            model$gq_data
          )

          # update reactiveValues
          global$models[[model_name]] <- model
          
          waiter::waiter_hide()
        }
        
        # download fit result after postprocessing
        output[[model$IDs$save_fit_btn]] <- downloadHandler(
          filename = function() { "model_fit_w_postprocess.RDS" },
          content = function(file) {
            waiter::waiter_show(
              html = waiter_ui("wait"),
              color = waiter::transparent(0.9)
            )
            
            model$fit$mcmc$draws()
            qs::qsave(model, file)
            
            waiter::waiter_hide()
          }
        )
        
        # change fit result download button color to indicate inclusion of postprocessing results
        shinyjs::addClass(model$IDs$save_fit_btn, "success")

        # disable postprocessing button
        shinyjs::disable(model$IDs$postprocess_btn)
      })
      
      # download fit result before postprocessing
      output[[model$IDs$save_fit_btn]] <- downloadHandler(
        filename = function() { "model_fit_wo_postprocess.RDS" },
        content = function(file) {
          waiter::waiter_show(
            html = waiter_ui("wait"),
            color = waiter::transparent(0.9)
          )
          
          model$fit$mcmc$draws()
          qs::qsave(model, file)
          
          waiter::waiter_hide()
        }
      )
      
      # download Stan code
      output[[model$IDs$save_code_btn]] <- downloadHandler(
        filename = function() { "model.stan" },
        content = function(file) {
          writeLines(model$stan_code$mcmc, file)
        }
      )
      
      # show whole page if object contains postprocessing results
      if(!is.null(model$fit$pstrat)) {
        shinyjs::delay(100, shinyjs::click(model$IDs$postprocess_btn))
      }
      
      # add to model list
      global$models[[model_name]] <- model
      global$model_count <- global$model_count + 1
    })

    # reset everything when new data is uploaded or
    # when user switch interface
    reset_flag <- reactive({
      global$data
      global$data_format
      global$link_data
    })
    
    observeEvent(reset_flag(), {
      # reset input fields
      prior_buffer(list())

      reset_inputs(vars = list(
        fixed = list(),
        varying = list()
      ))

      # delete all model tabs
      purrr::map(purrr::map(global$models, function(m) m$IDs$tab), function(id) {
        removeTab("navbar_model", id, session)
      })

      updateTabsetPanel(session,
                        inputId = "navbar_model",
                        selected = "nav_compare")

      # clear model object list
      global$models <- NULL
      global$poststratified_models <- NULL

    })
  })
}
