Span <- function(class=12, ..., style=""){
  if (!length(class[-1]))
    class_str=paste('span', class, sep="")
  else
    class_str=paste('span', class[1], ' ', class[-1], sep='')
  
  div(class=class_str, style=style, ...)
}

Row <- function(...){
  div(class='row-fluid', ...)
}

textOutputAdv <- function (outputId, class=12) 
{
  Row(Span(class=class, div(id = outputId, class = "shiny-text-output")))
}

numericInputRow<-function (inputId, label, value, min = NA, max = NA, step = NA, ssize_in=3, ssize_lab=9) 
{
  inputTag <- tags$input(id = inputId, type = "number", value = value, class="span12")
  if (!is.na(min)) 
    inputTag$attribs$min = min
  if (!is.na(max)) 
    inputTag$attribs$max = max
  if (!is.na(step)) 
    inputTag$attribs$step = step
  #tagList(tags$label(label, `for` = inputId), inputTag)
  Row(Span(class=ssize_in, inputTag),
      Span(class=ssize_lab, tags$label(label, `for` = inputId))
    )
}

actionButtonAdv <- function(inputId, label, class="")
{
  tags$button(id = inputId, type = "button", class = paste("btn action-button", class), 
              label)
}

Panel <- function(class="primary", head, body){
  
  div(class=paste("panel panel-",class, sep=""), 
      div(class="panel-heading", head), 
      div(class="panel-body", body)
  )
  
}

# 
# plotOutputVert <- function (outputId, width = "100%", height = "400px", clickId = NULL, 
#           hoverId = NULL, hoverDelay = 300, hoverDelayType = c("debounce", 
#                                                                "throttle")) 
# {
#   if (is.null(clickId) && is.null(hoverId)) {
#     hoverDelay <- NULL
#     hoverDelayType <- NULL
#   }
#   else {
#     hoverDelayType <- match.arg(hoverDelayType)[[1]]
#   }
#   style <- paste("width:", validateCssUnit(width), ";", "height:", 
#                  validateCssUnit(height))
#   
#   scr<-tags$script(paste("$('#",outputId,"Vert').on('mousemove', null, [$('#vertical')],function(e){
#   e.data[0].css('left', e.offsetX==undefined?e.originalEvent.layerX:e.offsetX);
# });
# $('#",outputId,"Vert').on('mouseenter', null, [ $('#vertical')], function(e){
#     e.data[0].show();
# }).on('mouseleave', null, [$('#vertical')], function(e){
#         e.data[0].hide();
#     }
# );", sep=""))
#   div(id=paste(outputId,"Vert", sep=""),style="overflow:hidden;display:inline-block;position:relative",
#     div(id="vertical",  style="width:1px;height:100%;background-color:black;position:absolute"),
#     div(id = outputId, class = "shiny-plot-output", style = style, 
#       `data-click-id` = clickId, `data-hover-id` = hoverId, 
#       `data-hover-delay` = hoverDelay, `data-hover-delay-type` = hoverDelayType), scr)
# }


accordionsetPanel <- function (..., id) 
{
  Items <- list(...)
  accordionContent <- tags$div(class = "accordion", id=id)
  accordionId <- 1
  for (divTag in Items) {
    thisId <- paste("Acc", id, accordionId, sep = "-")
    divTag$children[[1]]$children[[1]]$attribs$href <- paste("#",thisId, sep="")
    divTag$children[[1]]$children[[1]]$attribs$'data-parent' <- paste("#",id, sep="")
    divTag$children[[2]]$attribs$id <- thisId
    accordionId <- accordionId + 1
    
    accordionContent <- tagAppendChild(accordionContent, divTag)
    }
  
  return(accordionContent)
}

accordionPanel <- function(head, ..., collapsed=TRUE){
  
  head_tag <- div(class="accordion-heading alert-info", a(class="accordion-toggle", "data-toggle"="collapse", head))
  body_class <- "accordion-body collapse"
  if (!collapsed)
    body_class <- paste(body_class, "in")
  body_tag <- div(class=body_class, div(class="accordion-inner", ...))
  
  return(div(class="accordion-group", head_tag, body_tag))
  
}

modal <- function(id, label, linkLabel,  ..., accept_button, linkClass=''){
  
  link_tag <- a(href=paste('#', id, sep=''),  class=linkClass, "data-toggle"='modal', linkLabel)
  
  label_id = paste(id, "label", sep='-')
  
  modal_tag <- div(id=id, 
                   class="modal hide fade", 
                   "aria-hidden"=FALSE, 
                   "aria-labelledby"=label_id, 
                   "role"="dialog", 
                   "tabindex"="-1", 
                   style="left:5%;width:90%;margin:0;top:5%;max-height:90%")
  
  header_tag <- div(class="modal-header",
                    tags$button(class="close",
                                "aria-hidden"=TRUE,
                                "data-dismiss"="modal",
                                type="button", "x"),
                    h3(id=label_id, label))
  
  body_tag <- div(class="modal-body",
                  style="max-height:100%",
                  ...)
  
  footer_tag <- div(class="modal-footer",
                    tags$button(class="btn",
                                "data-dismiss"="modal",
                                "Close"),
                    accept_button)
  
  modal_tag <- tagAppendChildren(modal_tag, header_tag, body_tag, footer_tag)
  
  return(tagList(link_tag, modal_tag))
}


modalBusy <- function(id, title,  ...){
  
  label_id = paste(id, "label", sep='-')
  
  modal_tag <- div(id=id, 
                   class="modal hide fade", 
                   "aria-hidden"=FALSE, 
                   "aria-labelledby"=label_id, 
                   "role"="dialog", 
                   "tabindex"="-1",
                   "data-keyboard"=FALSE,
                   "data-backdrop"="static")
  
  header_tag <- div(class="modal-header",
                    h3(id=label_id, title))
  
  body_tag <- div(class="modal-body",
                  ...)
  
  footer_tag <- div(class="modal-footer")
  
  modal_tag <- tagAppendChildren(modal_tag, header_tag, body_tag, footer_tag)
  
  return(modal_tag)
}