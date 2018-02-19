(ns hiposfer.kamal.libs.time
  (:import (clojure.lang IPersistentMap)))

; The API is designed to be type-safe where reasonable in the main high-level
; API. Thus, there are separate classes for the distinct concepts of date,
; time and date-time, plus variants for offset and time-zone. This can seem
; like a lot of classes, but most applications can begin with just five
; date/time types.
;
;Instant - a timestamp
;LocalDate - a date without a time, or any reference to an offset or time-zone
;LocalTime - a time without a date, or any reference to an offset or time-zone
;LocalDateTime - combines date and time, but still without any offset or time-zone
;ZonedDateTime - a "full" date-time with time-zone and resolved offset from UTC/Greenwich


; The API has a relatively large surface area in terms of number of methods.
; This is made manageable through the use of consistent method prefixes.
;
;of - static factory method
;parse - static factory method focussed on parsing
;get - gets the value of something
;is - checks if something is true
;with - the immutable equivalent of a setter
;plus - adds an amount to an object
;minus - subtracts an amount from an object
;to - converts this object to another type
;at - combines this object with another, such as date.atTime(time)


(defprotocol Computable
  (plus [o])
  (minus [o]))

(deftype Data [instance])
  ; implement interfaces for
  ; Map Interface for assoc, dissoc -> date, time, datetime, zonedDateTime, offsetDateTime
  ; Set contains -> interval, duration, period
  ;

(defn data
  [value]
  (->Data value))
;; wrapping the value inside a type allows us to both implement all java.time and clojure.lang interfaces
;; however, this comes with a price. Inner complexity !! It would be a giant monolithic implementation
;; containing everything inside. It might be possible to mitigate this by reusing the inner java.time classes

(defn now []) ; -> instant

(defn with []) ; -> clock, zoneId,
