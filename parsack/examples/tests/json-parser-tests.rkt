#lang racket
(require "../../parsack.rkt")
(require "../../tests/test-utils.rkt")
(require "../json-parser.rkt")
(require rackunit)


(check-parse (parse $p_number "-3.14") -3.14)
(check-parse (parse $p_bool "true") #t)
(check-parse (parse $value "null") (JNull))
(check-parse (parse $p_text "[-3.14, true, null, \"a string\"]")
             (JArray (list (JNumber -3.14)
                           (JBool #t)
                           (JNull)
                           (JString "a string"))))
(check-parse (parse $p_text "{\"numbers\": [1,2,3,4,5], \"useful\": false}")
             (JObject (list (cons "numbers" (JArray (list (JNumber 1)
                                                          (JNumber 2)
                                                          (JNumber 3)
                                                          (JNumber 4)
                                                          (JNumber 5))))
                            (cons "useful" (JBool #f)))))
  
(check-parse
 (parse 
  $p_text 
  "{
    \"firstName\": \"John\",
    \"lastName\": \"Smith\",
    \"age\": 25,
    \"address\": {
        \"streetAddress\": \"21 2nd Street\",
        \"city\": \"New York\",
        \"state\": \"NY\",
        \"postalCode\": 10021
    },
    \"phoneNumbers\": [
        {
            \"type\": \"home\",
            \"number\": \"212 555-1234\"
        },
        {
            \"type\": \"fax\",
            \"number\": \"646 555-4567\"
        }
    ]
}")
 (JObject
   (list
    (cons "firstName" (JString "John"))
    (cons "lastName" (JString "Smith"))
    (cons "age" (JNumber 25))
    (cons
     "address"
     (JObject
      (list
       (cons "streetAddress" (JString "21 2nd Street"))
       (cons "city" (JString "New York"))
       (cons "state" (JString "NY"))
       (cons "postalCode" (JNumber 10021)))))
    (cons
     "phoneNumbers"
     (JArray
      (list
       (JObject (list (cons "type" (JString "home")) (cons "number" (JString "212 555-1234"))))
       (JObject (list (cons "type" (JString "fax")) (cons "number" (JString "646 555-4567"))))))))))