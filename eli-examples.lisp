(require :asdf)

(asdf:load-asd (merge-pathnames "eli.asd" (uiop:getcwd)))
(asdf:load-asd (merge-pathnames "eli-examples.asd" (uiop:getcwd)))

(asdf:load-system :eli)
(asdf:load-system :eli-examples)
