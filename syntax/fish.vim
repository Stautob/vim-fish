if exists('b:current_syntax')
  finish
endif

function! s:CreatePrivateKeyword (name, keyword, parent, hiGroup)
  execute "syn keyword " . a:name  . " " . a:keyword . " contained containedin=" . a:parent
  execute "hi link " . a:name . " " . a:hiGroup
  execute "syn cluster c_private add=" . a:name
endfunction

function! s:CreatePrivateKeywordWithError (name, keyword, parent, hiGroup)
  execute "syn keyword err_" . a:name . " " . a:keyword
  execute "hi default link err_" . a:name . " Error"
  call s:CreatePrivateKeyword(a:name, a:keyword, a:parent, a:hiGroup)
endfunction

function! s:CreatePrivateMatch (name, pattern, parent, hiGroup)
  execute "syn match " . a:name  . " \"" . a:pattern . "\" contained containedin=" . a:parent
  execute "hi link " . a:name . " " . a:hiGroup
  execute "syn cluster c_private add=" . a:name
endfunction

function! s:CreatePrivateMatchWithError (name, pattern, parent, hiGroup)
  execute "syn match err_" . a:name . " \"" . a:pattern . "\""
  execute "hi default link err_" . a:name . " Error"
  call s:CreatePrivateMatch(a:name, a:pattern, a:parent, a:hiGroup)
endfunction
syn case match

"syn cluster fishCommand contains=NONE
"syn match fishLineStart "\%(\\\@1<!\%(\\\\\)*\\\n\)\@<!\_^" nextgroup=@fishCommand skipwhite


"syn match fishFunctionName "\v(\d|\w)+[^/]{-}(\n|;)" contained

"syn match fishAliasName "\v(\d|\w)+[^/]" nextgroup=

"syn keyword fishAliasDef alias nextgroup=fishAliasName skipwhite

syn match m_path "\v\/?\S+\/(\/|\S*)+"
highlight link m_path Directory


" FISH OPT IN/OPT OUT FEATURE AS ERROR
" TODO



" STANDALONE ENDS ARE ERRORS
syn keyword k_standaloneEnd end
hi default link k_standaloneEnd Error


" KEYWORDS FROM BASH
syn keyword err_k_bash do done then fi export local
hi default link err_k_bash Error


" LOOPS
syn region r_forLoop matchgroup=Repeat start="\<for\>" end="\<end\>" keepend extend fold transparent contains=ALLBUT,@c_private
syn region r_whileLoop matchgroup=Repeat start="\<while\>" end="\<end\>" keepend extend fold transparent contains=ALLBUT,@c_private


" CONDITIONALS
call s:CreatePrivateMatchWithError("m_if", '\v<if>', "r_ifStmt", "Conditional")
call s:CreatePrivateMatchWithError("m_else", '\v<else>(if)@!', "r_ifStmt", "Conditional")
call s:CreatePrivateMatchWithError("m_elseIf", '\v<else\s+if>', "r_ifStmt", "Conditional")
syn region r_ifStmt start="\<if\>" matchgroup=Conditional end="\<end\>" keepend extend fold transparent contains=ALLBUT,@c_private



" SWITCH
call s:CreatePrivateKeywordWithError("k_case", "case", "r_switchStmt", "Conditional")
syn region r_switchStmt matchgroup=Conditional start="\<switch\>" end="\<end\>" keepend extend fold transparent contains=ALLBUT,@c_private



" FUNCTIONS

" matches valid function names
syn cluster c_funName add=m_funName
syn cluster c_private add=m_funName
syn match m_funName :\v(function\s+)@<=[^-/][^/]{-}($|;|\s+)@=: contained containedin=m_funSignature
syn match m_funName :\v(function\s+)@<="[^-/][^/]*": contained containedin=m_funSignature
syn match m_funName :\v(function\s+)@<='[^-/][^/]*': contained containedin=m_funSignature
hi default link m_funName Identifier

" matches invalid function names
syn cluster c_funName add=err_m_funName
syn cluster c_private add=err_m_funName
syn match err_m_funName :\v(function\s+)@<=(-.{-}|/=\S{-}/\S{-})($|;|\s+)@=: contained containedin=m_funSignature
syn match err_m_funName :\v(function\s+)@<="(-.{-}|/=.{-}/.{-})": contained containedin=m_funSignature
syn match err_m_funName :\v(function\s+)@<='(-.{-}|/=.{-}/.{-})': contained containedin=m_funSignature
hi default link err_m_funName Error

syn cluster c_private add=m_funDescription
syn match m_funDescription :\v(\s+)@<=(-d|--description=)\s+(".{-}"|'.{-}'): contained containedin=m_funSignature contains=r_string nextgroup=m_funArgument
hi default link m_funDescription Type

syn cluster c_argName add=m_argName
syn cluster c_private add=m_argName
syn match m_argName :\v(\s+)@<=[a-zA-Z0-9_]*: contained containedin=m_funArgument
hi default link m_argName Identifier

syn match m_funArgument :\v(\s+)@<=(-a|--argument-names=\s+)([a-zA-Z0-9_]*\s+)+: contained containedin=m_funSignature contains=m_argName nextgroup=m_funDescription
hi default link m_funArgument Type

" matches function signature
syn cluster c_private add=m_funSignature
syn match m_funSignature '\v<function>.{-}($|;)@='he=s+8 contained contains=c_funName,m_funArgument,m_funDescription containedin=r_functionDef nextgroup=@c_funName
hi link m_funSignature Function

" matches whole function definition
syn region r_functionDef start="\v<function>" matchgroup=Function end="\<end\>" keepend extend fold transparent contains=ALLBUT,@c_private



" STRINGS

syn region r_string start='"' end='"' extend fold contains=m_doubleQuoteEscape,m_variable
syn region r_string start="'" end="'" extend fold contains=m_singleQuoteEscape
hi default link r_string String

" matches string escapes
syn match m_singleQuoteEscape "\\'" contained
hi link m_singleQuoteEscape Special
syn match m_doubleQuoteEscape '\\"' contained
hi link m_doubleQuoteEscape Special


" ARRAY INDEX
" matches [-22..33] [+2..34] [12]
syn match m_arrayIndex "\[[\+-]=\d+(..[\+-]=\d+)=\]" contained
hi default link m_arrayIndex Operator

" GUARDED VAR DEREF
" matches {$foo} {$foo[-1]} {$foo[+1..2]}
syn match m_guardedVar "\v\{\$+\w*(\[[\+-]=\d+(..[\+-]=\d+)=\])=\}" contains=m_arrayIndex
hi default link m_guardedVar Identifier

" VAR DEREF
" matches $foo $foo[-1] $$foo[+1..2]
syn match m_variable "\v\$+\w*(\[[\+-]=\d+(..[\+-]=\d+)=\])=" contains=m_arrayIndex
hi default link m_variable Identifier


" COMMENTS
syn keyword k_todos contained FIXME XXX TODO FIXME: XXX: TODO:
hi default link k_todos Todo
syn match m_comment "\v^\s*\#.*$" contains=k_todos
hi default link m_comment Comment

" matches shebang
syn match m_bang "\v#\!" nextgroup=m_path
hi default link m_bang Macro


" ARGUMENT
syn cluster c_argument contains=r_string

syn match m_varDerefError "\$[-#@*$?!]"
syn region err_r_varDeref start="\${" end="}"
syn region err_r_varDeref start="\$(" end=")"
hi default link err_r_varDeref Error

"syn match fishVarDeref "\$\+\w\+" " NB: $$foo is allowed: multiple deref
syn region fishVarDeref start="\$\+\w\+\[" end="]" excludenl end="$" contains=fishSubst,fishVarDeref,@fishEscapeSeqs

syn match m_redirect "\v\d=(\>\>=|\<)(\&\d)="
hi default link m_redirect Operator





"syn keyword fishKeyword contained
"            \ contains_seq delete-or-exit down-or-search
"            \ fish_default_key_bindings grep la ll ls man nextd-or-forward-word
"            \ N_ prevd-or-backward-word prompt_pwd seq setenv sgrep up-or-search
"syn keyword fishKeyword contained
"            \ begin bg bind block break breakpoint builtin cd
"            \ commandline complete \contains continue count dirh dirs echo emit
"            \ eval exec exit fg fish fish_config fish_indent fish_pager
"            \ fish_prompt fish_right_prompt fish_update_completions fishd funced
"            \ funcsave functions help history isatty jobs math mimedb
"            \ nextd not open popd prevd psub pushd pwd random read return
"            \ set_color source status trap type ulimit umask vared
"syn keyword fishFunctionDefs contained
"            \ abbr alias
"syn keyword fishKeyword command contained nextgroup=@fishCommand skipwhite
"syn match fishKeyword "\.\ze\%(\s\|$\)" contained
"syn cluster fishCommand add=fishKeyword
"syn keyword fishKeywordError do done then fi export local contained
"syn cluster fishCommand add=fishKeywordError
"
"syn keyword fishConditional if else switch or and not contained nextgroup=@fishCommand skipwhite
"syn cluster fishCommand add=fishConditional
"syn keyword fishRepeat while contained nextgroup=@fishCommand skipwhite
"syn keyword fishRepeat for contained nextgroup=fishRepeatForVar skipwhite
"syn cluster fishCommand add=fishRepeat
"syn region fishRepeatForVar start="\S" end="\ze\%(\s\|;\|$\)" contained contains=@fishValues,@fishEscapeSeqs nextgroup=fishRepeatIn skipwhite
"syn keyword fishRepeatIn in contained
"syn keyword fishLabel case contained
"syn cluster fishCommand add=fishLabel
"
"syn match fishOperator "[*?]"
"syn match fishOperator "[;&]" nextgroup=@fishCommand skipwhite
"
"syn region fishSubst matchgroup=fishOperator start="(" end=")" excludenl end="$" contains=TOP
"syn match fishSubstStart "\ze." contained containedin=fishSubst nextgroup=@fishCommand skipwhite
"syn region fishBrace matchgroup=fishOperator start="{" end="}" excludenl end="$" contains=TOP
"syn match fishPipe "\(\d>\)\=|" nextgroup=@fishCommand skipwhite skipnl
"
"syn match fishComment excludenl "#.*$" contains=fishTodo
"syn match fishComment "#.*\\\@1<!\%(\\\\\)*\\$" contains=fishTodo,fishCommentEscape nextgroup=@fishCommand skipwhite
"syn match fishSpecial "\\[abefnrtv$\\]" "these must be escaped in and out of quoted strings
"syn match fishEscape ,\\[{}[\]()&;| *?~%#<>^"'\n], "these are not escaped in strings
"syn match fishNumEscape "\\\(\d\d\d\|[xX]\x\x\|u\x\x\x\x\(\x\x\x\x\)\?\|c\a\)"
"syn cluster fishEscapeSeqs contains=fishSpecial,fishEscape,fishNumEscape
"
"syn match fishSet "\<set\>\ze\%(\s\|;\|$\)" contained nextgroup=fishSetOpt,fishSetIdentifier skipwhite
"syn cluster fishCommand add=fishSet
"syn region fishSetIdentifier start="\S" end="\ze\%(\s\|;\|$\)" contained contains=@fishValues,@fishEscapeSeqs
"syn match fishSetOpt contained "-[eglLnquUx]\+\ze\%(\s\|;\|$\)" nextgroup=fishSetOpt,fishSetIdentifier skipwhite
"syn match fishSetOpt contained "--\(local\|global\|universal\|names\|\(un\)\=export\|erase\|query\|long\)\ze\%(\s\|;\|$\)" nextgroup=fishSetOpt,fishSetIdentifier skipwhite
"syn match fishSetOpt contained "--\ze\%(\s\|;\|$\)" nextgroup=fishSetIdentifier skipwhite
"
"syn match fishVarDerefError "\$[-#@*$?!]" " special variables
"syn region fishVarDerefError start="\${" end="}" " safe dereferencing
"syn region fishVarDerefError start="\$(" end=")" " var substitution
"syn match fishVarDeref "\$\+\w\+" " NB: $$foo is allowed: multiple deref
"syn region fishVarDeref start="\$\+\w\+\[" end="]" excludenl end="$" contains=fishSubst,fishVarDeref,@fishEscapeSeqs
"syn region fishString matchgroup=fishOperator start=/'/ end=/'/ contains=fishSpecial,fishSingleQuoteEscape
"syn match fishSingleQuoteEscape "\\'" contained
"syn region fishString matchgroup=fishOperator start=/"/ end=/"/ contains=fishVarDeref,fishSpecial,fishDoubleQuoteEscape
"syn match fishDoubleQuoteEscape '\\"' contained
"syn match fishNumber "\<[-+]\=\d\+\>"
"syn match fishHex "\<[-+]\=[0-9a-fA-F]\+\>"
"syn cluster fishValues contains=fishVarDeref,fishString,fishNumber,fishHex,fishVarDerefError
"
"syn region fishTest matchgroup=fishOperator start="\[" end="\]" end="\ze[;#]" excludenl end="$" contained contains=@fishTestContents
"syn region fishTest matchgroup=fishKeyword start="\<test\>" end="\ze[;#]" excludenl end="$" contained contains=@fishTestContents
"syn cluster fishCommand add=fishTest
"syn match fishTestOp contained "\s\@1<=-[a-hnoprstuwxzLS]\>"
"syn match fishTestOp contained "\s\@1<=-\%(eq\|ne\|ge\|gt\|le\|lt\)\>"
"syn match fishTestOp contained excludenl "\s\@1<=\%(!=\|!\|=\)\%($\|\s\@=\)"
"syn cluster fishTestContents contains=fishTestOp,fishSubst,fishOpError,fishTestOpError,@fishEscapeSeqs,@fishValues
"syn match fishTestOpError contained excludenl "\s\@1<=\%(==\|>\|<\)\%($\|\s\@=\)"
"
"" Some sequences used in Bourne-like shells, but not fish
"syn match fishOpError "==\|&&\|||\|!!\|\[\[\|]]" "syn
"
"hi default link fishKeyword Keyword
"hi default link fishConditional Conditional
"hi default link fishRepeat Repeat
"hi default link fishRepeatForVar fishSetIdentifier
"hi default link fishRepeatIn Repeat
"hi default link fishLabel Label
"
"hi default link fishFunctionDef Function
"
"hi default link fishComment Comment
"hi default link fishTodo Todo
"
"hi default link fishEscape Special
"hi default link fishSpecial fishEscape
"hi default link fishNumEscape fishEscape
"hi default link fishCommentEscape fishEscape
"
"hi default link fishSet Keyword
"hi default link fishSetOpt Operator
"hi default link fishSetIdentifier Identifier
"hi default link fishVarDeref Identifier
"hi default link fishString String
"hi default link fishSingleQuoteEscape fishEscape
"hi default link fishDoubleQuoteEscape fishEscape
"hi default link fishNumber Number
"hi default link fishHex Number
"
"
"hi default link fishOperator Operator
"hi default link fishPipe fishOperator
"
"hi default link fishTestOp Operator
"
"hi default link fishError Error
"hi default link fishKeywordError fishError
"hi default link fishOpError fishError
"hi default link fishVarDerefError fishError

syn sync minlines=50
syn sync maxlines=500

let b:current_syn = 'fish'
