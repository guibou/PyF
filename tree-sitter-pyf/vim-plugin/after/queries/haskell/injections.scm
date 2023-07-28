; extends
(quasiquote
  ((quoter) @_quoter (#any-of? @_quoter "fmt" "fmtTrim"))
  (quasiquote_body) @pyf
)
