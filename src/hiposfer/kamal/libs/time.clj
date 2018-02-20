(ns hiposfer.kamal.libs.time)

;; ----------------------------------------------------------
;; FOOD FOR THOUGHT
;;  After some experimentation with the Clojure Java Time I have come
;; to actually hate having a wrapper around the Java Api. It seems to me
;; that the Java API has a great design and that wrapping it in functions
;; just for the sake of it is a mistake !
;; I think that a proper wrapper of such a library such use Clojure's
;; protocols and avoid re-designing the date/time library, while leveraging
;; Clojure Polymorphism and dynamic typing
;; A way to achieve that might be
;; (time/data {:month 12 :day 3 :offset 3}) => ZonedDate
;; (:day (time/data {:year 1970 :month 12 :offset {:hours 3})) => throws Exception
;; (time/between instant interval) => interval
;; (time/between instant instant) => interval
;; (time/amount local-date local-time) => duration
;; (time/reverse interval) => swap start-end
;; (time/reverse duration) => length negated
;; (time/data {:hour 3 :second 2}) => local-time
;; (:year (native {:hour 3 :second 2}) => throws Exception
;; -
;; As you can see from the previous examples, it is possible to represent most of
;; the concepts of Java Time with Clojure data structures. That however requires a
;; careful interconnection between Clojure's protocol and Java's API. I believe
;; such interconnection is not only possible but IDEAL.
;; If that were to be defined it would only look as a couple of functions
;; - data => wrap a Java Time instance on a Clojure Type to allow its syntax
;; - between => creates an interval based on two temporals
;; - amount => creates a duration based on two temporals
;; - reverse => reverse the interval/duration
;; - lookUp => returns the requested key of a native Java Time (keyword lookup)
;; - minus, plus, multiply, divide => polymorphic methods to add dates and times

;; --------------------------------------------------------------
;; NOTES from the Author of Java Time

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

; ......

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

;; ----------------------------------------------------
;; Temporal Interface ---------------------------------
; (defn supported? [d]) ;; => not needed
;                          -> (keys d)
(defn minus ([d [value key]]) ;; (minus local-time [1000 :seconds])
            ([d amount])) ;; needed in case you already have a duration instance
(defn plus ([d [value key]])
           ([d amount]))
;(defn until [d1 d2 key]) ;; => not needed
;                            -> (amount d1 d2)
;(defprotocol at [d value key]) ; => not needed
;                                 -> (assoc d :days 10 :month 12)

;; Temporal Accessor -----------------------------------
; (defn get [d]) ;; => not needed
;                   -> (:day d), (:month d)
; (defn isSupported []) ;; => not needed
;                          -> (keys d), (:day d) ; might return nil
(defn query [d query]) ;; => TODO
(defn range [d field]) ;; => TODO

;; Temporal Amount -------------------------------------
;(defn addTo [d amount]) ;; => not needed
;                           -> assoc, update, plus
;(defn get [d unit]) ;; => not needed
;                       -> (:hours d)
;(defn getUnits [d unit]) ;; => not needed
;                            -> (keys d)
;(defn substractFrom [d amount]) ;; => not needed
;                                   -> assoc, update, minus

;; Temporal Adjuster ------------------------------------
(defn adjustInto [adjuster d]) ;; => (into adjuster data)
;; => adjuster needs to be wrapped in (data value)
;; otherwise it would be too difficult to figure out how to handle it.
;; an auto-wrapping could be implemented for non-temporal values such as
;; Clojure's Maps

;; Time Chrono --------------------- PACKAGE --------------------------

(defprotocol before? [d1 d2])
(defprotocol after? [d1 d2])
(defn before? [d1 d2 & more])
(defn after? [d1 d2 & more])

; these functions are not needed since you can use Clojure's (min-key :days)
; and it would be more readable
; min
; max

(deftype Data [instance])
  ; implement interfaces for
  ; Map Interface for assoc, dissoc
  ; Map Interface for keys, vals
  ; Seq Interface for getting [key val] pairs
  ; Set interface for contains -> interval, duration, period
  ; Object interface for string, equals, hashCode


(defn data
  [value]
  (->Data value))
;; wrapping the value inside a type allows us to both implement all java.time and clojure.lang interfaces
;; however, this comes with a price. Inner complexity !! It would be a giant monolithic implementation
;; containing everything inside. It might be possible to mitigate this by reusing the inner java.time classes

;; NOTE:
;; a way to avoid typos when defining date/times instead of Amounts would be to
;; invert the key-value relation. For example
;; (time/data {:year 1970 :month 2 :day 3}) => LocalDate
;; (time/data {2 :years 3 :months 7 :days}) => Period

;TODO: what is the best way to query for dayOfWeek, dayOfYear
;      -> (:month/day data), (:year/day data)
