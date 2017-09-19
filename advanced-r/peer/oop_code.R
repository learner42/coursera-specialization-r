library(methods)
library(magrittr)
library(dplyr)
library(tidyr)

#' Declare the usual generics to be extended
setGeneric("print")
setGeneric("summary")

#' Declare the new generics for this analysis
setGeneric("subject", function(x, ...){standardGeneric("subject")})
setGeneric("visit", function(x, ...){standardGeneric("visit")})
setGeneric("room", function(x, ...){standardGeneric("room")})

setClass("LongitudinalData",slots = list(data = "data.frame"))

#' Define the print method for LongitudinalData
setMethod("print", c(x = "LongitudinalData"),
          function(x){
              paste("Longitudinal dataset with",
                    length(unique((x@data)$id)),
                    "subjects")
          }
          )

#' Convert a dataframe to a LongitudinalData object
make_LD <- function(data) { new("LongitudinalData", data = data) }

#' Define the Subject class
setClass("Subject", slots = list(id = "numeric", data = "data.frame"))
#' Define the SubjectSummary class
setClass("SubjectSummary", slots = list(id = "numeric", data = "data.frame"))
#' Define the Visit class
setClass("Visit", slots = list(id = "numeric", visit = "numeric", data = "data.frame"))
#' Define the Room class
setClass("Room", slots = list(id = "numeric", visit = "numeric", room = "character", data = "data.frame"))

#' Define the RoomSummary class
setClass("RoomSummary", slots = list(id = "numeric", visit = "numeric", room = "character", data = "table"))


#' Define the print method for Subject
setMethod("print", c(x = "Subject"),
          function(x){
              paste("Subject ID: ", x@id)
          }
          )

setMethod("print", c(x = "SubjectSummary"),
          function(x){
              cat("ID: ", x@id, "\n")
              print(x@data)
          }
          )

#' Define the print method for Visit
setMethod("print", c(x = "Visit"),
          function(x){
              cat("Subject ID: ", x@id, "\nVisit: ", x@visit )
          }
          )

#' Define the print method for Room
setMethod("print", c(x = "Room"),
          function(x){
              cat("Subject ID: ", x@id, "\nVisit: ", x@visit, "\nRoom: ", x@room )
          }
          )

setMethod("print", c(x = "RoomSummary"),
          function(x){
              cat("ID: ", x@id)
              print(x@data)
          }
          )


#' Define the summary method for Subject
setMethod("summary", c(object = "Subject"),
          function(object){
              new("SubjectSummary",
                  id = object@id,
                  data = object@data %>%
                      select(c("visit", "room", "value")) %>%
                      group_by(visit, room) %>%
                      summarize(avg = mean(value)) %>%
                      spread(room, avg) %>%
                      as.data.frame
                  )
          }
          )

#' Define the summary method for Subject
setMethod("summary", c(object = "Room"),
          function(object){
              new("RoomSummary",
                  id = object@id,
                  visit = object@visit,
                  room = object@room,
                  data = object@data %>%
                      select(c("value")) %>%
                      summary
                  )
          }
          )

#' Search for a subject by its ID
#' @param x A LongitudinalData object
#' @param id_ref The ID of the subject to be found
#' @return The subject with the specified ID, or NULL if nothing is found
setMethod("subject",
          c(x = "LongitudinalData"),
          function(x, id_ref){
              if (id_ref %in% x@data$id) {
                  new("Subject",
                      id = id_ref,
                      data = (x@data %>% filter(id == id_ref)))
              }
              else {
                  NULL
              }
          })


setMethod("visit",
          c(x = "Subject"),
          function(x, visit_ref){
              if (visit_ref %in% x@data$visit) {
                  new("Visit",
                      id = x@id,
                      visit = visit_ref,
                      data = (x@data %>% filter(visit == visit_ref)))
              }
              else {
                  NULL
              }
          })

setMethod("room",
          c(x = "Visit"),
          function(x, room_ref){
              if (room_ref %in% x@data$room) {
                  new("Room",
                      id = x@id,
                      visit = x@visit,
                      room = room_ref,
                      data = (x@data %>% filter(room == room_ref)))
              }
              else {
                  NULL
              }
          })
