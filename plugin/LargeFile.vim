" LargeFile: Sets up an autocmd to make editing large files work with celerity
"   Author:     Charles E. Campbell, Jr.
"   Date:       May 18, 2011
"   Version:    5i ASTRO-ONLY
"   Copyright:  see :help LargeFile-copyright
" GetLatestVimScripts: 1506 1 :AutoInstall: LargeFile.vim

" ---------------------------------------------------------------------
" Load Once: {{{1
if exists("g:loaded_LargeFile") || &cp
  finish
endif
let g:loaded_LargeFile = "v5i"
let s:keepcpo          = &cpo
set cpo&vim

" ---------------------------------------------------------------------
" Commands: {{{1
com! Unlarge            call s:Unlarge()
com! -bang Large        call s:LargeFile(<bang>0,expand("%"))

" ---------------------------------------------------------------------
"  Options: {{{1
if !exists("g:LargeFile")
  let g:LargeFile = 20
endif

if !exists("g:LargeFile_patterns")
  let g:LargeFile_patterns = '*'
endif

let s:vim_options = {
      \   '&l:swapfile':   0,
      \   '&l:bufhidden':  'unload',
      \   '&l:foldmethod': 'manual',
      \   '&l:foldenable': 0,
      \   '&l:complete':   '-wbuU',
      \   '&undolevels':   -1,
      \   '&eventignore':  'FileType'
      \ }

" ---------------------------------------------------------------------
"  LargeFile Autocmd: {{{1
" for large files: turns undo, syntax highlighting, undo off etc
" (based on vimtip#611)
augroup LargeFile
  au!
  execute 'autocmd LargeFile BufReadPre ' .
        \ g:LargeFile_patterns .
        \ ' call <SID>LargeFile(0, expand("<afile>"))'
  execute 'autocmd LargeFile BufReadPost ' .
        \ g:LargeFile_patterns .
        \ ' call <SID>LargeFilePost()'
augroup END

" ---------------------------------------------------------------------
" s:LargeFile: {{{2
fun! s:LargeFile(force,fname)
  "  call Dfunc("s:LargeFile(force=".a:force." fname<".a:fname.">) g:LargeFile=".g:LargeFile)
  if a:force || s:IsLarge(a:fname)
    sil! call s:ParenMatchOff()
    syn clear
    let b:LargeFile_mode = 1
    "   call Decho("turning  b:LargeFile_mode to ".b:LargeFile_mode)

    let b:LargeFile_store = copy(s:vim_options)
    for [key, new_value] in items(b:LargeFile_store)
      let b:LargeFile_store[key] = getbufvar('%', key)
      if type(new_value) == type('') && new_value[0] =~ '\v(\+|-)'
        execute "setlocal " . matchstr(key, '\v\l+$') . new_value[0] . '=' . new_value[1:-1]
      else
        let b:[key] = new_value
      endif
    endfor

    autocmd LargeFile BufEnter <buffer> call s:LargeBufEnter()
    autocmd LargeFile BufLeave <buffer> call s:LargeBufLeave()
    autocmd LargeFile BufUnload <buffer> call s:LargeBufUnload()
    echomsg "***note*** handling a large file"
  endif
  "  call Dret("s:LargeFile")
endfun

" ---------------------------------------------------------------------
" s:LargeFilePost: {{{2
fun! s:LargeFilePost()
  if get(b:, 'LargeFile_mode', 1) == 0 && s:IsLarge(line2byte(line("$")+1))
    call s:LargeFile(1, expand("<afile>"))
  endif
  " call Dret("s:LargeFilePost")
endfun

" ---------------------------------------------------------------------
" s:LargeBufEnter: {{{2
fun! s:LargeBufEnter()
  setlocal undolevels=-1
endfun

" ---------------------------------------------------------------------
" s:LargeBufLeave: {{{2
fun! s:LargeBufLeave()
  let &l:undolevels  = b:LargeFile_store['&undolevels']
  let &l:eventignore = string(b:LargeFile_store['&eventignore'])
endfun

" ---------------------------------------------------------------------
" s:LargeBufUnload: {{{2
fun! s:LargeBufUnload()
  autocmd! LargeFile * <buffer>
endfun

" ---------------------------------------------------------------------
" s:IsLarge: {{{2
fun! s:IsLarge(fname_or_bytes)
  let bytes = type(a:fname_or_bytes) == type('') ?
        \ getfsize(a:fname_or_bytes) :
        \ a:fname_or_bytes
  " call Dfunc("s:IsLarge(fname_or_bytes=" . a:fname_or_bytes . ") g:LargeFile=" . g:LargeFile . " b:LargeFile_mode=" . get(b:, 'LargeFile_mode', '(not set)'))
  return bytes >= g:LargeFile * get(g:, 'LargeFile_size_unit', 1024 * 1024) || bytes <= -2
endfun

" ---------------------------------------------------------------------
" s:ParenMatchOff: {{{2
fun! s:ParenMatchOff()
  "  call Dfunc("s:ParenMatchOff()")
  redir => matchparen_enabled
  com NoMatchParen
  redir END
  if matchparen_enabled =~ 'g:loaded_matchparen'
    let b:LF_nmpkeep= 1
    NoMatchParen
  endif
  "  call Dret("s:ParenMatchOff")
endfun

" ---------------------------------------------------------------------
" s:Unlarge: this function will undo what the LargeFile autocmd does {{{2
fun! s:Unlarge()
  "  call Dfunc("s:Unlarge()")
  let b:LargeFile_mode= 0
  "  call Decho("turning  b:LargeFile_mode to ".b:LargeFile_mode)

  for [key, old_value] in items(b:LargeFile_store)
    execute 'let ' . key . ' = ' . string(old_value)
  endfor
  unlet! b:LargeFile_store

  if exists("b:LF_nmpkeep")
    DoMatchParen
    unlet b:LF_nmpkeep
  endif
  syn on
  doau FileType
  echomsg "***note*** stopped large-file handling"
  "  call Dret("s:Unlarge")
endfun

" ---------------------------------------------------------------------
"  Restore: {{{1
let &cpo= s:keepcpo
unlet s:keepcpo

" modeline {{{1
" vim: ts=4 shiftwidth=2 fdm=marker expandtab
