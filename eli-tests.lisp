(require :asdf)

(asdf:load-asd (merge-pathnames "eli.asd" (uiop:getcwd)))
(asdf:load-asd (merge-pathnames "eli-tests.asd" (uiop:getcwd)))

(asdf:load-system :eli)
(asdf:load-system :eli-tests)
