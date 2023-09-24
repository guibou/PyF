; extends
(quasiquote
  (quoter) @_name
  (#eq? @_name "fmt")
  ((quasiquote_body) @injection.content)
  (#set! injection.language "pyf"))

(quasiquote
  (quoter) @_name
  (#eq? @_name "fmtTrim")
  ((quasiquote_body) @injection.content)
  (#set! injection.language "pyf"))
