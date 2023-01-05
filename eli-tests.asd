(defsystem "eli-tests"
  :description "ELI tests"
  :licence "GNU GPL v3"
  :depends-on (:asdf :eli)
  :components ((:file "t/parser/read-next-word")))
