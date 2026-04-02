
box::use(
  shiny[moduleServer, NS, reactive, renderUI],
  reactable.extras[reactable_extras_ui, reactable_extras_server],
  reactable,
  htmltools,
  reactable[colDef],
  reactablefmtr[data_bars],
)

# UI module part
ui <- function(id) {
  ns <- NS(id)
  reactable_extras_ui(ns("reactable_table"))
}

# Server module part
server <- function(id, dt) {
  moduleServer(id, function(input, output, session) {
    reactable_extras_server("reactable_table", dt,
                            resizable = TRUE,
                            striped = TRUE,
                            wrap = FALSE,
                            highlight = TRUE,
                            outlined = TRUE,
                            defaultColDef = colDef(
                                header = function(value) gsub("_", " ", value, fixed = TRUE),
                                align = "center",
                                sortNALast = TRUE
                              ),
                            total_pages = ceiling(nrow(dt) / 20)
                            # coverage_depth = colDef(
                            #   cell = data_bars(
                            #     data = dt,
                            #     fill_color = c('#FFF2D9','#FFE1A6','#FFCB66','#FFB627'),
                            #     fill_gradient = TRUE,
                            #     background = 'transparent',
                            #     number_fmt = scales::comma_format(accuracy = 0.1)
                            #   )
                            # )
                            # total_pages = 7
                          )
  })
}




## valid params:
# Must be a subset of {
# 'data','columns','columnGroups','rownames','groupBy','sortable','resizable','filterable','searchable','searchMethod',
# 'defaultColDef','defaultColGroup','defaultSortOrder','defaultSorted','pagination','defaultPageSize','showPageSizeOptions',
# 'pageSizeOptions','paginationType','showPagination','showPageInfo','minRows','paginateSubRows','details','defaultExpanded',
# 'selection','defaultSelected','onClick','highlight','outlined','bordered','borderless','striped','compact','wrap',
# 'showSortIcon','showSortable','class','style','rowClass','rowStyle','fullWidth','width','height','theme','language',
# 'meta','elementId','static','selectionId'
# }