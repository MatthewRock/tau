;;;; tau.asd

(asdf:defsystem #:tau
  :description "Describe tau here"
  :author "Your Name <your.name@example.com>"
  :license "MIT"
  :depends-on (#:hash-set #:cl-ppcre #:command-line-arguments)
  :serial t
  :components ((:file "tau")))
;; Wersja dla 100 iteracji, 3 par słów chodzi ponad 20 minut dla nieprzyspieszonej wersji.
;; Dla przyspieszonej jest to 1/1000 sekundy...
