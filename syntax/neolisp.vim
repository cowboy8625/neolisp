if exists("b:current_syntax")
  finish
endif

let b:current_syntax = "nl"
runtime! syntax/lisp.vim

" Highlight numbers
syntax match nlNumber /\v[+-]?\d+(\.\d+)?([eE][+-]?\d+)?/
highlight link nlNumber Number

" Highlight identifiers (any symbol after `(` but excluding `();`)
syntax match nlIdentifier /\v(\()[ \t]*\zs[A-Za-z0-9!$%&*+\-./:<=>?@^_~]+/
highlight link nlIdentifier Identifier

" Highlight function names after 'fn'
syntax match nlFunction "\(fn\_s\+\)\@<=\<[A-z0-9!\"#$%&'*+,-./:;<=>?@\[\\\]^_`{|}~]\+\>"
highlight link nlFunction Function


" Highlight strings (allow escaped quotes inside strings)
syntax region nlString start=+"+ skip=+\\\"+ end=+"+
highlight link nlString String

" Highlight true and false as keywords
syntax keyword nlBoolean true false nil
highlight link nlBoolean Boolean

" Highlight line comments starting with ";"
syntax match nlComment /;.*/ contains=nlTodo
highlight link nlComment Comment

" Add TODO or NOTE inside comments (optional)
syntax match nlTodo /TODO:\|NOTE:/ containedin=nlComment
highlight link nlTodo Todo

" Highlight keywords (variables starting with :)
syntax match nlKeyword /\v :[A-Za-z0-9_!$%&*+<=>?\/^~.-]+/
highlight link nlKeyword Keyword
