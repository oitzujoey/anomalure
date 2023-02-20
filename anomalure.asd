(asdf:defsystem #:anomalure
  :description "A minimal scripting language"
  :author "Joey Herguth <oitzujoey@gmail.com>"
  :license "MIT License"
  :version "0.1.0"
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "anomalure")))
