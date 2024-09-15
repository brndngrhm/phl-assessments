source("helpers.R")


ui <- page_sidebar(
  title = "Philly Property Assessment Explorer",
  sidebar = sidebar(
    shiny::h5("Start Here"),
    textInput(
      inputId = "address_input",
      label = "Step 1 - Enter Address",
      value = "",
      placeholder = "ex: 123 Market St"
    ),
    shiny::actionButton(
      "goButton",
      "Lookup Address"
    ),
    selectInput(
      inputId = "address_select",
      label = "Step 2 - Confirm Address",
      choices = " "
    ),
    shiny::p("Step 3 - Find Matches"),
    shiny::actionButton(
      "get_matches",
      "Find Matches"
    ),
    shiny::conditionalPanel(
      condition = "input.get_matches > 0",
      htmltools::browsable(
        tagList(
          tags$button(
            tagList(fontawesome::fa("download"), "Download Table (csv)"),
            onclick = "Reactable.downloadDataCSV('matches-table', 'property_comps.csv')"
          ),
        )
      )
    ),
    shiny::hr(),
    shiny::h5("Learn More"),
    actionButton(
      "about_button",
      "About"
    ),
    actionButton(
      "methods_and_data",
      "Methods & Data"
    ),
    shiny::hr(),
    shiny::h5("Disclaimer"),
    shiny::p("This app is not affiliated in any way with the Phila Office of Property Assessmment or OpenDataPhilly. There is no guarantee any of the resulting 'matched' properties were used in determining property's market value."),
  ),
  layout_columns(
    col_widths = c(6, 6),
    row_heights = c(2, 1.5),
    card(
      card_header(
        "Matched Properties",
        tooltip(
          bs_icon("info-circle"),
          "Map showing the input property (red circle) and matched properties located in adjacent census tracts (blue circles)"
        )
      ),
      full_screen = TRUE,
      fill = TRUE,
      leafletOutput("map")
    ),
    card(
      full_screen = TRUE,
      fill = TRUE,
      card_header(
        "Matches Detail",
        tooltip(
          bs_icon("info-circle"),
          "Detailed information on the matched properties identifed by the algorithm. Note the Input Property will be in the first row."
        )
      ),
      reactableOutput("matches2")
    ),
    card(
      full_screen = TRUE,
      card_header(
        "Assessment Comparison",
        tooltip(
          bs_icon("info-circle"),
          "Compares the property assessment of the input propery (red) relative to the matches (blue)"
        )
      ),
      plotly::plotlyOutput("assessment_compare_plot")
    ),
    card(
      full_screen = TRUE,
      fill = TRUE,
      card_header(
        "Assessment Trend",
        tooltip(
          bs_icon("info-circle"),
          "Property assessment over time"
        )
      ),
      highchartOutput("assessment_plot")
    )
  ),
)

server <- function(input, output) {
  observeEvent(input$goButton, {
    address_result <- get_loc_names(input$address_input)
    updateSelectizeInput(getDefaultReactiveDomain(), "address_select",
      label = "Step 2 - Confirm Address",
      choices = address_result %>% pull(location),
      selected = head(address_result, 1)
    )
  })

  index_property <-
    eventReactive(input$get_matches, {
      get_index_property(input$address_select) |>
        select_matching_params(params)
    })

  matching_tracts <-
    reactive(
      get_touching_tracts(index_property(), get_phl_tracts())
    )

  match_universe <-
    reactive(
      get_match_universe(
        index_property = index_property(),
        matching_tracts =
          paste0(
            c(index_property()$census_tract, matching_tracts()),
            collapse = ","
          )
      )
    )

  matches <-
    reactive(
      find_matching_properties(
        match_universe = match_universe(),
        index_property = index_property(),
        matching_tracts = matching_tracts()
      )
    )

  matches_shared <-
    reactive(
      crosstalk::SharedData$new(matches(), ~location)
    )

  output$matches2 <-
    renderReactable({
      matches_shared() %>%
        # select(-c(match_ind, census_tract, lattitude, longitude, weights, subclass, parcel_number)) %>%
        # select(type, match_num, distance, everything()) %>%
        # mutate(
        #   type = ifelse(type == "Input", "Input Property", "Match"),
        #   match_num = ifelse(match_num == 0, NA_real_, match_num),
        #   distance = round(distance, 4)
        # ) %>%
        # rename(
        #   Type = type,
        #   `Match ID` = match_num,
        #   `Closeness Score` = distance,
        #   Address = location,
        #   Assessment = market_value,
        #   `Central Air` = central_air,
        #   `Exterior Condition` = exterior_condition,
        #   `Interior Condition` = interior_condition,
        #   `Bedrooms` = number_of_bedrooms,
        #   `Stories` = number_stories,
        #   `Quality Grade` = quality_grade,
        #   `Total Area` = total_area,
        #   `Livable Area` = total_livable_area,
        #   `View` = view_type,
        #   `Year Built` = year_built
        # ) %>%
        # select(-c(`Match ID`, Type)) |>
        # select(Address, everything()) |>
        # mutate(across(where(is.numeric), ~ as.character(.x))) |>
        # pivot_longer(cols = `Closeness Score`:`Year Built`) |>
        # pivot_wider(names_from = Address, values_from = value) |>
        # rename(
        #   Features = name,
        #   `Input Proprty` = 2
        # ) %>%
        reactable(.,
          searchable = TRUE,
          filterable = TRUE,
          resizable = TRUE,
          highlight = TRUE,
          striped = TRUE,
          defaultPageSize = 30,
          elementId = "matches-table",
          selection = "multiple",
          onClick = "select",
          rowStyle = list(cursor = "pointer"),
          # columns = list(
          #   Features = colDef(
          #     sticky = "left",
          #     # Add a right border style to visually distinguish the sticky column
          #     style = list(borderRight = "1px solid #eee"),
          #     headerStyle = list(borderRight = "1px solid #eee")
          #   ),
          #   `Input Proprty` = colDef(
          #     sticky = "left",
          #     # Add a right border style to visually distinguish the sticky column
          #     style = list(borderRight = "1px solid #eee"),
          #     headerStyle = list(borderRight = "1px solid #eee")
          #   )
          # )
        )
    })

  output$map <-
    renderLeaflet({
      leaflet(matches_shared()) |>
        addTiles() %>%
        addCircleMarkers(
          lng = ~longitude,
          lat = ~lattitude,
          # opacity = .8,
          stroke = FALSE,
          fillOpacity = 0.75,
          radius = ~ ifelse(type == "Match", 8, 12),
          color = ~ ifelse(type != "Match", "tomato", "dodgerblue"),
          popup = ~ paste(
            location, "</br>",
            scales::dollar(market_value), "</br>",
            glue::glue('<a href="https://www.google.com/maps/place/{location},+Philadelphia,+PA">Google Maps</a>')
          )
        )
    })

  output$assessment_plot <-
    renderHighchart({
      get_prop_asssessment(index_property()$parcel_number) %>%
        get_prop_assessment_plot(., location = input$address_select)
    })

  output$assessment_compare_plot <-
    plotly::renderPlotly({
      get_assessment_compare_plotly(matches_shared())
    })

  # About & Help modals
  observeEvent(input$about_button, {
    showModal(
      modalDialog(
        tags$h3("About"),
        tags$p("The purpose of this app is to make the residential property assessments appeals process more efficient."),
        tags$p("The app can save you time by finding up to 25 comps automatically using a matching algorithm, and returns some relevant information."),
        tags$p("You can review the characteristics of the matched properties and how their assessments compare to yours.
          If you choose to appeal, the matched properties, if appropriate, can be used as comparisons."),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })

  observeEvent(input$methods_and_data, {
    showModal(
      modalDialog(
        HTML(
          "<h3>Methods</h3>
          <p>
            Once a user enters a property into the search field and clicks 'Find Matches', the underlying data is filtered to only include properties in the surrounding census tracts. This reduced list of properties forms a more relevant 'universe' of potential matches.
          </p>
          <p>
            Then, to find comprable properties within this reduced list, a matching algorithm is implemented using <a href='https://github.com/kosukeimai/MatchIt'>MatchIt</a>. A 'nearest neighbors' approach finds comprable properties by simultaneously analyzing multiple variables in the OPA dataset:
              <ul>
                <li>Exterior Condition</li>
                <li>Interior Condition</li>
                <li>Bedrooms</li>
                <li>Stories Built</li>
                <li>Quality Grade Built</li>
                <li>Total Area</li>
                <li>Total Livable Area</li>
                <li>Year Built</li>
              </ul>
            A full list if variables and column definitions can be found at the <a href='https://metadata.phila.gov/#home/datasetdetails/5543865f20583086178c4ee5/representationdetails/55d624fdad35c7e854cb21a4/'>OPA properties metadata catalogue.</a>
          </p>
          <h3>Data</h3>
          <p>Property assessment and geospatial data is sourced from the
          <a href='https://opendataphilly.org/datasets/philadelphia-properties-and-assessment-history/#:~:text=Philadelphia%20Properties%20and%20Assessment%20History.%20Some%20of,to%20contact%20OPA%20to%20report%20the%20issue.'>Philadelphia Properties and Assessment History</a>
            which is maintained by OpenDataPhilly. Additional census tract data is obtained from
          <a href='https://github.com/walkerke/tigris'>Tigris</a>.</p>
        "
        ),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
}

shinyApp(ui, server)
