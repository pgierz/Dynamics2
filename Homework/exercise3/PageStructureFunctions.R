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

numericInputSolo <- function(inputId, value, min = NA, max = NA, step = NA, class = NA){
  inputTag <- tags$input(id = inputId, type = "number", value = value)
  if (!is.na(class)) 
    inputTag$attribs$class = class
  if (!is.na(min)) 
    inputTag$attribs$min = min
  if (!is.na(max)) 
    inputTag$attribs$max = max
  if (!is.na(step)) 
    inputTag$attribs$step = step
  inputTag
}

actionButtonAdv <- function(inputId, label, class="")
{
  tags$button(id = inputId, type = "button", class = paste("btn action-button", class), 
              label)
}

Panel <- function(class="primary", head=NULL, body=NULL, footer=NULL){
  
  panelCont = tagList()
  
  if (!is.null(head))
    panelCont <- tagList(panelCont, div(class="panel-heading", head))
  if (!is.null(body))
    panelCont <- tagList(panelCont, div(class="panel-body", body))
  if (!is.null(footer))
    panelCont <- tagList(panelCont, div(class="panel-footer", footer))
  
  if (length(panelCont) > 0)
    return(div(class=paste("panel panel-",class, sep=""), panelCont))
  else
    return(NULL)
  
}

accordionsetPanel <- function (..., id) 
{
  Items <- list(...)
  accordionContent <- tags$div(class = "accordion", id=id)
  accordionId <- 1
  styleTag <- tagList()
  for (divTag in Items) {
    thisId <- paste("Acc", id, accordionId, sep = "-")
    styleTag <- tagList(styleTag,
                        tags$head(tags$script(type="text/javascript",
                                              paste("$(document).ready( function(){$('#",thisId,"').on('show.bs.collapse', function(){
                                                      $(this).parent().find('.caret-right').removeClass('caret-right').addClass('caret-down');
                                                    }).on('hide.bs.collapse', function(){
                                                      $(this).parent().find('.caret-down').removeClass('caret-down').addClass('caret-right');
                                                     });});"
                                                    ,sep=""))))
    divTag$children[[1]]$children[[1]]$attribs$href <- paste("#",thisId, sep="")
    divTag$children[[1]]$children[[1]]$attribs$'data-parent' <- paste("#",id, sep="")
    divTag$children[[2]]$attribs$id <- thisId
    accordionId <- accordionId + 1
    
    accordionContent <- tagAppendChild(accordionContent, divTag)
    }
  
  styleTag <- tagList(styleTag, 
                      singleton(tags$head(tags$style(type="text/css", 
                                                  ".caret-right{
                                                          display:inline-block;
                                                          width:0;
                                                          height:0;
                                                          vertical-align:middle;
                                                          border-left:4px solid #000;
                                                          border-bottom:4px solid transparent;
                                                          border-top:4px solid transparent;
                                                          content:''}
                                                  .caret-down{
                                                          display:inline-block;
                                                          width:0;
                                                          height:0;
                                                          vertical-align:middle;
                                                          border-top:4px solid #000;
                                                          border-left:4px solid transparent;
                                                          border-right:4px solid transparent;
                                                          content:''}"
                                             ))))
  
  return(tagList(styleTag,accordionContent))
}

accordionPanel <- function(head, ..., collapsed=TRUE){
  body_class <- "accordion-body collapse"
  if (!collapsed){
    body_class <- paste(body_class, "in")
    caret="caret-down"
  }
  else
    caret="caret-right"
  
  head_tag <- div(class="accordion-heading alert-info", a(class="accordion-toggle", "data-toggle"="collapse", span(class=caret),head))
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


customJsMsg <- function(){
  singleton(tags$head(tags$script('Shiny.addCustomMessageHandler("jsCode",
                                                function(message) {
                                                  console.log(message)
                                                  eval(message.code);
                                                });'
                                  )
                      )
            )
}


modalBusy <- function(id, title,  ...){
  
  msgHandler = customJsMsg()
  
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
                  Row(...))
  
#   body_tag <- div(class="modal-body",
#                   Row(Span(class=2,
#                            div(class="progress progress-striped active",
#                                div(class="bar", style="width: 100%;")
#                                )),
#                       Span(class=10, ...)))
  
  footer_tag <- div(class="modal-footer")
  
  modal_tag <- tagAppendChildren(modal_tag, header_tag, body_tag, footer_tag)
  
  tagList(msgHandler, modal_tag)
}

progressBar <- function(id, min=0, max=100, value=0, class=NULL){
  
  msgHandler=singleton(
    tags$head(
      tags$script(
        'Shiny.addCustomMessageHandler("updateProgress",
                                      function(data) {

                                        var call;
                                        call = "$(\'#" + data.id + "\').prop(\'style\',\'width:" + data.progress * 100 + "%\');";
                                        eval(call);
                                        call = "$(\'#" + data.id + "\').prop(\'innerHTML\',\'" + data.message  + "\');";
                                        eval(call);
                                      }
        );'
      )
    )
  )
  
  pbClass = paste("progress", class)
  
  ProgBar=div(class=pbClass,
      div(class="progress-bar bar", 
          id=id,
          role="progressbar", 
          "aria-valuenow"=as.character(value), 
          "aria-valuemin"=as.character(min), 
          "aria-valuemax"=as.character(max), 
          style=paste("width: ",value,"%", sep=""),
          paste(value,"%", sep="")
          )
      )
  
  tagList(msgHandler, ProgBar)
  
}


showModal <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').modal('show')"
                                             ,sep="")))
}

hideModal <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').modal('hide')"
                                             ,sep="")))
}

hideElement <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('style','display: none;')"
                                             ,sep="")))
}

showElement <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('style','display: block;')"
                                             ,sep="")))
}

disableButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('disabled',true)"
                                             ,sep="")))
}

enableButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('disabled',false)"
                                             ,sep="")))
}

setProp <- function(id,session,prop,value) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('", prop,"', '", value,"')"
                                             ,sep="")))
}