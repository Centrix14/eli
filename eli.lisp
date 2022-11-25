(require :asdf)

(asdf:load-asd (merge-pathnames "eli.asd" (uiop:getcwd)))
(asdf:load-system :eli)
