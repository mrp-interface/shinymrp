#' analyze_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import sortable
#' @import zeallot
mod_analyze_model_ui <- function(id){
  ns <- NS(id)
  tags$div(class = "pad_top",
    sidebarLayout(
      sidebarPanel(width = 3,
        uiOutput(outputId = ns("model_spec"))
      ),
      mainPanel(width = 9,
        tabsetPanel(id = ns("navbar_model"),
          tabPanel("Model Comparison",
            value = "nav_compare",
            tags$div(class = "pad_top",
              uiOutput(outputId = ns("model_select_ui")),
              actionButton(
                inputId = ns("diagnos_btn"),
                label = "Run diagnostics"
              ),
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

    observeEvent(global$input$navbar_analyze, {
      if(global$input$navbar_analyze == "nav_analyze_model") {
        if(is.null(global$mrp_input)) {
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


    # Model Specification Panel
    output$model_spec <- renderUI({
      spec_sens_ui <- fluidRow()
      if(global$covid) {
        spec_sens_ui <- fluidRow(
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
      }

      tagList(
        tags$div(
          class = "justify pad_bottom",
          tags$div(id = "model_spec_title",
            tags$h4("Model Specification", class = "inline"),
            icon("info-sign", lib = "glyphicon", id = "model_spec_icon")
          ),
          tags$div(
            class = "model_spec_buttons",
            actionButton(
              inputId = ns("reset_btn"),
              label = icon("repeat", lib = "glyphicon"),
            ),
            actionButton(
              inputId = ns("add_btn"),
              label = icon("plus", "fa")
            )
          ),
          shinyBS::bsTooltip("model_spec_icon", "Specify models by dragging elements from boxes on the left to ones on the right. Drag individual elements out of the \"Effect\" boxes to remove them or use the \"Reset\" button to start over. Press the \"Arrow\" button to fit and save a model."),
          shinyBS::bsTooltip(ns("reset_btn"), "Reset fields", placement = "top"),
          shinyBS::bsTooltip(ns("add_btn"), "Add model", placement = "top")
        ),
        tags$div(
          id = ns("remove_panel"),
          fluidRow(
            column(
              width = 6,
              selectInput(
                inputId = ns("predictor_select"),
                label = NULL,
                choices = names(global$mrp_input$vars)
              ),
              conditionalPanel(ns = ns,
                condition = paste0("input.predictor_select == 'Individual-level Predictor'"),
                tags$div(
                  class = "panel panel-success predictor-panel",
                  tags$div(
                    class = "panel-body",
                    id = ns("pred_indiv"),
                    isolate(global$mrp_input$vars[["Individual-level Predictor"]]) |> lapply(create_drag_item)
                  )
                )
              ),
              conditionalPanel(ns = ns,
                condition = paste0("input.predictor_select == 'Geographic Indicator'"),
                tags$div(
                  class = "panel panel-success",
                  tags$div(
                    class = "panel-body",
                    id = ns("pred_geo_indi"),
                    isolate(global$mrp_input$vars[["Geographic Indicator"]]) |> create_drag_item()
                  )
                )
              ),
              conditionalPanel(ns = ns,
                condition = paste0("input.predictor_select == 'Geographic Predictor'"),
                tags$div(
                  class = "panel panel-success",
                  tags$div(
                    class = "panel-body",
                    id = ns("pred_geo"),
                    isolate(global$mrp_input$vars[["Geographic Predictor"]]) |> lapply(create_drag_item)
                  )
                )
              ),
              conditionalPanel(ns = ns,
                condition = paste0("input.predictor_select == 'Interaction'"),
                tags$div(
                  class = "panel panel-success",
                  tags$div(
                    class = "panel-body",
                    id = ns("pred_interact"),
                    isolate(global$mrp_input$vars[["Interaction"]]) |> lapply(create_drag_item)
                  )
                )
              )
            ),
            column(
              width = 6,
              tags$div(  # fixed effect box
                class = "panel panel-primary",
                tags$div(
                  class = "panel-heading",
                  "Fixed Effect"
                ),
                tags$div(
                  class = "panel-body effect-box",
                  id = ns("effect_fixed")
                )
              ),
              tags$div(  # varying effect box
                class = "panel panel-primary",
                tags$div(
                  class = "panel-heading",
                  "Varying Effect"
                ),
                tags$div(
                  class = "panel-body effect-box",
                  id = ns("effect_varying")
                )
              )
            )
          ),
          sortable_js(
            ns("pred_indiv"),
            options = sortable_options(
              group = list(
                name = "indiv",
                pull = "clone",
                put = FALSE
              ),
              sort = FALSE
            )
          ),
          sortable_js(
            ns("pred_geo_indi"),
            options = sortable_options(
              group = list(
                name = "geo_indi",
                pull = "clone",
                put = FALSE
              ),
              sort = FALSE
            )
          ),
          sortable_js(
            ns("pred_geo"),
            options = sortable_options(
              group = list(
                name = "geo_pred",
                pull = "clone",
                put = FALSE
              ),
              sort = FALSE
            )
          ),
          sortable_js(
            ns("pred_interact"),
            options = sortable_options(
              group = list(
                name = "interact",
                pull = "clone",
                put = FALSE
              ),
              sort = FALSE
            )
          ),
          sortable_js(
            ns("effect_fixed"),
            options = sortable_options(
              group = list(
                name = "fixed",
                pull = TRUE,
                put = c("indiv", "geo_indi", "geo_pred", "interact")
              ),
              onSort = sortable_js_capture_input(ns("fixed_effects"))
            )
          ),
          sortable_js(
            ns("effect_varying"),
            options = sortable_options(
              group = list(
                name = "varying",
                pull = TRUE,
                put = c("indiv", "geo_indi", "interact")
              ),
              onSort = sortable_js_capture_input(ns("varying_effects"))
            )
          ),
          sortable_js(
            ns("remove_panel"),
            options = sortable_options(
              group = list(
                name = "remove",
                pull = FALSE,
                put = c("fixed", "varying")
              ),
              onAdd = htmlwidgets::JS("function (evt) { this.el.removeChild(evt.item); }"),
              animation = 0
            )
          )
        ),
        selectInput(
          inputId = ns("iter_select"),
          label = "Select the number of iterations",
          choices = c("100 (Test)", "500 (Low)", "2000 (Medium)", "5000 (High)", "Custom"),
          selected = "2000 (Medium)"
        ),
        conditionalPanel(ns = ns,
          condition = paste0("input.iter_select == 'Custom'"),
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
        spec_sens_ui,
        tags$p(
          "For details about the model fitting process, go to ",
          actionLink(
            inputId = ns("to_mrp"),
            label = "Interface",
            class = "action_link"
          ),
          "."
        )
      )
    })

    observeEvent(input$to_mrp, {
      updateTabsetPanel(global$session,
                        inputId = "navbar",
                        selected = "nav_learn_interface")

      removeModal(global$session)
    })


    # Model select input
    output$model_select_ui <- renderUI({
      selectizeInput(
        inputId = ns("model_select"),
        label = "Select one or more models",
        choices = names(global$models),
        multiple = TRUE
      )
    })

    output$loo_ui <- renderUI({
      req(global$models)
      global$covid
      input$diagnos_btn

      selected_names <- isolate(input$model_select)

      if(length(selected_names) == 0) {
        ui <- NULL
      } else if(length(selected_names) == 1) {
        ui <- tags$p("*Two or more models are required")
      } else {
        ui <- tagList(
          create_text_box(
            title = tags$b("Note"),
            tags$p("Generally, ", tags$code("elpd_diff"), "beng less than 4 indicates small difference in the predictive power between models. For values of ", tags$code("elpd_diff"), "greater than 4, ", tags$code("se_diff"), ", the standard error of ", tags$code("elpd_diff"), "can account for the uncertainty in the difference. Find more details about how to inteprete these terms ", tags$a("here", href = "https://mc-stan.org/loo/articles/online-only/faq.html#elpd_interpretation", target = "_blank"), ".")
          ),
          tableOutput(outputId = ns("loo_table"))
        )

        output$loo_table <- renderTable({
          waiter::waiter_show(
            html = waiter_ui("loo"),
            color = waiter::transparent(0.9)
          )

          res <- isolate(global$models[selected_names]) |>
            purrr::map(function(m) m$fit) |>
            unname() %>%
            do.call(brms::loo, .)

          rownames(res$diffs) <- selected_names
          res$diffs <- res$diffs |> as.data.frame() |> select(elpd_diff, se_diff)

          waiter::waiter_hide()

          return(res$diffs)
        })
      }

      return(ui)
    })

    # PPC plots
    output$ppc_plots <- renderUI({
      req(global$models)
      global$covid
      input$diagnos_btn

      selected_names <- isolate(input$model_select)

      if(length(selected_names) > 0) {
        structs <- purrr::map(isolate(global$models[selected_names]), function(m) m$mean_structure)

        tagList(
          create_text_box(
            title = tags$b("Note"),
            if(global$covid) {
              tags$p("The plots show the weekly prevalence rates computed from the observed data and multiple sets of replicated data.")
            } else {
              tags$p("The plots show the percentage of positive response computed from the observed data and multiple sets of replicated data.")
            }
          ),
          purrr::map(1:length(structs), ~ list(
            HTML(paste0("<h4><u>Model ", .x, "</u>", ": ", structs[[.x]], "</h4>")),
            plotOutput(ns(paste0("compare_ppc", .x)))
          ))
        )
      }
    })


    observeEvent(input$diagnos_btn, {
      selected_names <- input$model_select

      if(length(selected_names) > 0) {

        yreps <- purrr::map(global$models[selected_names], function(m) m$yrep)

        purrr::map(1:length(yreps), function(i) {
          req(global$models)

          output[[paste0("compare_ppc", i)]] <- renderPlot({

            if(global$covid) {
              plot_ppc_covid_subset(
                yreps[[i]],
                global$mrp_input$brms_input,
                global$plotdata$dates
              )
            } else {
              plot_support(
                yreps[[i]],
                global$mrp_input$brms_input
              )
            }

          })
        })
      }

    })


    # reset input fields
    observeEvent(input$reset_btn, {
      shinyjs::runjs("$('.effect-box').empty();")
      shinyjs::reset("predictor_select")
      shinyjs::reset("iter_select")
      shinyjs::reset("iter_kb")
      shinyjs::reset("chain_select")
      shinyjs::runjs(sprintf("Shiny.setInputValue('%s', null);", ns("fixed_effects")))
      shinyjs::runjs(sprintf("Shiny.setInputValue('%s', null);", ns("varying_effects")))
    })

    # add model
    observeEvent(input$add_btn, {
      n_iter <- if(input$iter_select == "Custom") input$iter_kb else as.integer(strsplit(input$iter_select, " ")[[1]][1])
      n_chains <- input$chain_select

      if(is.null(global$models)) {
        global$models <- list()
      }

      # check if number of iterations and number of chains are within defined range
      c(within_range, msg_range) %<-% check_iter_chain(
        n_iter, global$static$ui$iter_range,
        n_chains, global$static$ui$chain_range
      )

      if(within_range) {
        if(length(global$models) <= global$static$ui$max_model) {

          # check if the formula is valid
          c(formula, mean_structure, valid) %<-% create_formula(
            fixed_effects = input$fixed_effects |> unique() |> sort(),
            varying_effects = input$varying_effects |> unique() |> sort()
          )

          if(valid) {
            # check if model has been created
            if(!(paste0(mean_structure, n_iter) %in% purrr::map(global$models, function(m) m$sig))) {
              waiter::waiter_show(
                html = waiter_ui("fit"),
                color = waiter::transparent(0.9)
              )

              # create a list to store model info
              model_name <- paste0("Model ", length(global$models) + 1)
              model <- list()
              model$sig <- paste0(mean_structure, n_iter)
              model$mean_structure <- mean_structure

              # fit model
              c(model$fit, pred_mat, yrep_mat) %<-% run_brms(
                formula,
                global$mrp_input$brms_input,
                global$mrp_input$brms_new,
                n_iter = n_iter,
                n_chains = n_chains,
                spec = if(global$covid) input$spec_kb else 1,
                sens = if(global$covid) input$sens_kb else 1
              )

              # process brms outputs for plotting
              for(v in names(global$mrp_input$levels)) {
                model[[v]] <- global$mrp_input$brms_new |>
                  mutate(factor = global$mrp_input$brms_new[[v]]) |>
                  process_pred(pred_mat, global$covid)
              }

              model$overall <- global$mrp_input$brms_new |>
                mutate(factor = 1) |>
                process_pred(pred_mat, global$covid)

              model$yrep <- process_yrep(
                yrep_mat,
                global$mrp_input$brms_input,
                global$covid,
                10
              )

              model_summary <- summary(model$fit)

              # UI element IDs
              model$IDs <- list(
                fixed = paste0("fixed", global$model_count),
                varying = paste0("varying", global$model_count),
                ppc = paste0("ppc", global$model_count),
                tab = paste0("tab", global$model_count),
                title = paste0("title", global$model_count),
                button = paste0("rm_btn", global$model_count)
              )

              # create new tab
              tab_header <- tags$div(
                class = "model_tab_header",
                textOutput(
                  outputId = ns(model$IDs$title),
                  inline = TRUE
                ),
                actionButton(
                  inputId = ns(model$IDs$button),
                  label = NULL,
                  icon = icon("remove", lib = "glyphicon"),
                  class = "btn-xs remove_model"
                )
              )

              appendTab("navbar_model",
                select = TRUE,
                tabPanel(title = tab_header,
                  value = model$IDs$tab,
                  tags$div(class = "pad_top",
                    HTML(paste0("<h4>", "Formula: ", mean_structure, "</h4>")),
                    tags$h5(paste0("A binomial model with a logit function of the prevalence. ",
                                   "Samples are generated using ", model_summary$chains, " chains with ", model_summary$iter - model_summary$warmup, " post-warmup iterations each.")),
                    create_text_box(
                      title = tags$b("Note"),
                      tags$ul(
                        tags$li("Values for ", tags$code("Convergence"), " that are greater than 1.1 indicates the chains have not yet converged and it is necessary to run more iterations and/or set stronger priors."),
                        tags$li("Low values for ", tags$code("Bulk-ESS"), " and ", tags$code("Tail-ESS"), " (ESS stands for Effective Sample Size) also suggest that more iterations are required.")
                      )
                    ),
                    tags$h4("Fixed Effects", class = "break_title"),
                    tags$hr(class = "break_line"),
                    tableOutput(ns(model$IDs$fixed)),
                    tags$h4("Varying Effects", class = "break_title"),
                    tags$hr(class = "break_line"),
                    purrr::map(names(model_summary$random),  ~ list(
                      tags$em(tags$h4(.x)),
                      gsub(':', '_', .x) %>%
                        paste0(model$IDs$varying, '_', .) |>
                        ns() |>
                        tableOutput()
                    )),
                    tags$h4("Posterior Predictive Check", class = "break_title"),
                    tags$hr(class = "break_line"),
                    create_text_box(
                      title = tags$b("Note"),
                      if(global$covid) {
                        tags$p("The plot shows the weekly prevalence rates computed from the observed data and multiple sets of replicated data.")
                      } else {
                        tags$p("The plot shows the percentage of positive response computed from the observed data and multiple sets of replicated data.")
                      }
                    ),
                    plotOutput(outputId = ns(model$IDs$ppc))
                  )
                )
              )

              # changeable tab title
              output[[model$IDs$title]] <- renderText(model_name)

              # render fixed effect table
              output[[model$IDs$fixed]] <- renderTable({
                model_summary$fixed |>
                  rename("Convergence" = "Rhat") |>
                  mutate(
                    Bulk_ESS = as.integer(Bulk_ESS),
                    Tail_ESS = as.integer(Tail_ESS)
                  )
              }, rownames = TRUE)


              # render varying effect tables
              purrr::map(names(model_summary$random), function(s) {
                id <- gsub(':', '_', s) %>% paste0(model$IDs$varying, '_', .)
                output[[id]] <- renderTable(
                  model_summary$random[[s]] |>
                    rename("Convergence" = "Rhat") |>
                    mutate(
                      Bulk_ESS = as.integer(Bulk_ESS),
                      Tail_ESS = as.integer(Tail_ESS)
                    ),
                  rownames = TRUE
                )
              })

              # render ppc plot
              output[[model$IDs$ppc]] <- renderPlot(
                if(global$covid) {
                  plot_ppc_covid_subset(
                    model$yrep,
                    global$mrp_input$brms_input,
                    global$plotdata$dates
                  )
                } else {
                  plot_ppc_poll(
                    model$yrep,
                    global$mrp_input$brms_input
                  )
                }
              )

              observeEvent(input[[model$IDs$button]], {
                # remove model object and tab
                global$models[[model_name]] <- NULL
                removeTab("navbar_model", model$IDs$tab, session)

                # re-index model objects and tabs
                names(global$models) <- if(length(global$models) > 0) paste0("Model ", 1:length(global$models)) else character()
                purrr::map(names(global$models), function(name) {
                  output[[global$models[[name]]$IDs$title]] <- renderText(name)
                })
              })

              global$models[[model_name]] <- model
              global$model_count <- global$model_count + 1
              waiter::waiter_hide()

            } else {
              showModal(
                modalDialog(
                  title = tagList(icon("triangle-exclamation", "fa"), "Warning"),
                  "This model has already been added."
                ),
                session = global$session
              )
            }
          } else {
            showModal(
              modalDialog(
                title = tagList(icon("triangle-exclamation", "fa"), "Warning"),
                formula
              ),
              session = global$session
            )
          }
        } else {
          showModal(
            modalDialog(
              title = tagList(icon("triangle-exclamation", "fa"), "Warning"),
              "Maximum number of models reached. Please removed existing models to add more."
            ),
            session = global$session
          )
        }
      } else {
        showModal(
          modalDialog(
            title = tagList(icon("triangle-exclamation", "fa"), "Warning"),
            msg_range
          ),
          session = global$session
        )
      }
    })

    # reset everything when new data is uploaded
    observeEvent(global$data, {
      # reset input fields
      shinyjs::runjs("$('.effect-box').empty();")
      shinyjs::reset("predictor_select")
      shinyjs::reset("iter_select")
      shinyjs::reset("iter_kb")
      shinyjs::reset("chain_select")
      shinyjs::runjs(sprintf("Shiny.setInputValue('%s', null);", ns("fixed_effects")))
      shinyjs::runjs(sprintf("Shiny.setInputValue('%s', null);", ns("varying_effects")))

      # delete all model tabs
      purrr::map(purrr::map(global$models, function(m) m$IDs$tab), function(id) {
        removeTab("navbar_model", id, session)
      })

      updateTabsetPanel(session,
                        inputId = "navbar_model",
                        selected = "nav_compare")

      # clear model object list
      global$models <- NULL

    })

    # reset everything when user switch interface
    observeEvent(global$covid, {
      # reset input fields
      shinyjs::runjs("$('.effect-box').empty();")
      shinyjs::reset("predictor_select")
      shinyjs::reset("iter_select")
      shinyjs::reset("iter_kb")
      shinyjs::reset("chain_select")
      shinyjs::runjs(sprintf("Shiny.setInputValue('%s', null);", ns("fixed_effects")))
      shinyjs::runjs(sprintf("Shiny.setInputValue('%s', null);", ns("varying_effects")))

      # delete all model tabs
      purrr::map(purrr::map(global$models, function(m) m$IDs$tab), function(id) {
        removeTab("navbar_model", id, session)
      })

      updateTabsetPanel(session,
                        inputId = "navbar_model",
                        selected = "nav_compare")

      # clear model object list
      global$models <- NULL

    })
  })
}
