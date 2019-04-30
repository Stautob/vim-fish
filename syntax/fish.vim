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

syn match m_bang "\v#\!" nextgroup=m_path
highlight link m_bang Macro

" STANDALONE ENDS ARE ERRORS
syn keyword k_standaloneEnd end
hi default link k_standaloneEnd Error

" LOOPS
syn cluster c_loops contains=r_forLoop,r_whileLoop
syn region r_forLoop matchgroup=c_loops start="\<for\>" end="\<end\>" keepend extend fold transparent contains=ALLBUT,@c_private
syn region r_whileLoop matchgroup=c_loops start="\<while\>" end="\<end\>" keepend extend fold transparent contains=ALLBUT,@c_private

highlight link c_loops Repeat


" CONDITIONALS
call s:CreatePrivateMatchWithError("m_else", '\v<else\s+(if)@!>', "r_ifStmt", 'Conditional')
call s:CreatePrivateMatchWithError("m_elseIf", '\v<else\s+if>', "r_ifStmt", "Conditional")
syn region r_ifStmt matchgroup=Conditional start="\<if\>" end="\<end\>" keepend extend fold transparent contains=ALLBUT,@c_private,err_m_else,err_m_elseIf

" SWITCH
call s:CreatePrivateKeywordWithError("k_case", "case", "r_switchStmt", "Conditional")
syn region r_switchStmt matchgroup=Conditional start="\<switch\>" end="\<end\>" keepend extend fold transparent contains=ALLBUT,@c_private



" FUNCTIONS
call s:CreatePrivateMatchWithError('m_function', '\<function\>', 'r_functionDef', "Function")
syn region r_functionDef start="\<function\>" matchgroup=c_function end="\<end\>" keepend extend fold transparent contains=ALLBUT,@c_private

highlight link c_function Function


syn cluster c_strings contains=r_string
syn region r_string start='"' end='"' contains=m_doubleQuoteEscape
syn region r_string start="'" end="'" contains=m_singleQuoteEscape
"hi default link r_string String

syn cluster c_specialKey contains=m_singleQuoteEscape,m_doubleQuoteEscape
syn match m_singleQuoteEscape "\\'" contained
syn match m_doubleQuoteEscape '\\"' contained

hi default link c_specialKey Special

hi default link c_strings String


syn keyword todos contained FIXME XXX TODO FIXME: XXX: TODO:
hi default link todos Todo
syn match comment "\v^\s*\%.*$" contains=todos
hi default link comment Comment




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
"syn match fishRedirect "\d\=\(>>\?\|<\|\^\^\?\)\(&\(-\|\d\)\)\="
"syn match fishPipe "\(\d>\)\=|" nextgroup=@fishCommand skipwhite skipnl
"
"syn match fishComment excludenl "#.*$" contains=fishTodo
"syn match fishComment "#.*\\\@1<!\%(\\\\\)*\\$" contains=fishTodo,fishCommentEscape nextgroup=@fishCommand skipwhite
"syn keyword fishTodo contained TODO FIXME
"syn match fishCommentEscape "\\\n" contained
"
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
"hi default link fishRedirect fishOperator
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
