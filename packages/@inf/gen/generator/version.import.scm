(module version (version-generate)
  (import scheme chicken data-structures tools signatures)
  (require-extension srfi-13 srfi-1)

  (define (emit vnum)
    (list 
      "// this file has been automatically generated, do not modify\n"
      "\nexport default '" (inexact->exact vnum) "';"))

  (define (version-generate vnum) 
    (smoosh (emit vnum)))
)

