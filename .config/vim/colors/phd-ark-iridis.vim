" Name: phd-ark-iridis.vim

set background=dark
hi clear

if exists('syntax on')
    syntax reset
endif

let g:colors_name='phd-ark-iridis'
set t_Co=256

let s:magenta = "#FF0087"
let s:pink = "#E83A82"
let s:ruby = "#D7005F"
let s:crimson = "#D70000"
let s:red = "#FF6D6B"
let s:tiger = "#FF5F00"
let s:orange = "#F69927"
let s:sand = "#FDB760"
let s:yellow = "#FFD787"
let s:green = "#44BC84"
let s:grass = "#3DAA77"
let s:emerald = "#00AF5F"
let s:viridis = "#00AF87"
let s:teal = "#4DB5BD"
let s:ocean = "#1F5582"
let s:cyan = "#46D9FF"
let s:blue = "#5F8AF7"
let s:indigo = "#5F5FFF"
let s:amethyst = "#3723B7"
let s:lilac = "#875FFF"
let s:purple = "#8787FF"
let s:violet = "#A9A1E1"
let s:white = "#F6F9FE"
let s:subtext0 = "#EDF3FE"
let s:subtext1 = "#DDE3EE"
let s:text = "#BDC3CE"
let s:overlay2 = "#8A8A8A"
let s:overlay1 = "#464858"
let s:overlay0 = "#5B6268"
let s:surface2 = "#515C66"
let s:surface1 = "#404850"
let s:surface0 = "#3A3C3F"
let s:base = "#303033"
let s:mantle = "#2B2B2F"
let s:crust = "#181E26"


function! s:hi(group, guisp, guifg, guibg, gui, cterm)
  let cmd = ""
  if a:guisp != ""
    let cmd = cmd . " guisp=" . a:guisp
  endif
  if a:guifg != ""
    let cmd = cmd . " guifg=" . a:guifg
  endif
  if a:guibg != ""
    let cmd = cmd . " guibg=" . a:guibg
  endif
  if a:gui != ""
    let cmd = cmd . " gui=" . a:gui
  endif
  if a:cterm != ""
    let cmd = cmd . " cterm=" . a:cterm
  endif
  if cmd != ""
    exec "hi " . a:group . cmd
  endif
endfunction


call s:hi("Normal", "NONE", s:text, s:base, "NONE", "NONE")
call s:hi("Visual", "NONE", "NONE", s:blue, "NONE", "NONE")
call s:hi("Conceal", "NONE", s:pink, "NONE", "NONE", "NONE")
call s:hi("ColorColumn", "NONE", "NONE", s:surface0, "NONE", "NONE")
call s:hi("Cursor", "NONE", s:base, s:blue, "NONE", "NONE")
call s:hi("lCursor", "NONE", s:base, s:blue, "NONE", "NONE")
call s:hi("CursorIM", "NONE", s:base, s:blue, "NONE", "NONE")
call s:hi("CursorColumn", "NONE", "NONE", s:mantle, "NONE", "NONE")
call s:hi("CursorLine", "NONE", "NONE", s:overlay1, "NONE", "NONE")
call s:hi("Directory", "NONE", s:indigo, "NONE", "NONE", "NONE")
call s:hi("DiffAdd", "NONE", s:base, s:viridis, "NONE", "NONE")
call s:hi("DiffChange", "NONE", s:base, s:sand, "NONE", "NONE")
call s:hi("DiffDelete", "NONE", s:base, s:ruby, "NONE", "NONE")
call s:hi("DiffText", "NONE", s:base, s:teal, "NONE", "NONE")
call s:hi("EndOfBuffer", "NONE", "NONE", "NONE", "NONE", "NONE")
call s:hi("ErrorMsg", "NONE", s:red, "NONE", "NONE", "NONE")
call s:hi("VertSplit", "NONE", s:crust, "NONE", "NONE", "NONE")
call s:hi("Folded", "NONE", s:blue, s:surface1, "NONE", "NONE")
call s:hi("FoldColumn", "NONE", s:overlay0, s:base, "NONE", "NONE")
call s:hi("SignColumn", "NONE", s:surface1, s:base, "NONE", "NONE")
call s:hi("IncSearch", "NONE", s:surface1, s:pink, "NONE", "NONE")
call s:hi("CursorLineNR", "NONE", s:teal, "NONE", "NONE", "NONE")
call s:hi("LineNr", "NONE", s:surface1, "NONE", "NONE", "NONE")
call s:hi("MatchParen", "NONE", s:base, s:blue, "NONE", "NONE")
call s:hi("ModeMsg", "NONE", s:subtext0, "NONE", "NONE", "NONE")
call s:hi("MoreMsg", "NONE", s:teal, "NONE", "NONE", "NONE")
call s:hi("NonText", "NONE", s:teal, "NONE", "NONE", "NONE")
call s:hi("Pmenu", "NONE", s:teal, s:overlay1, "NONE", "NONE")
call s:hi("PmenuSel", "NONE", s:pink, s:surface2, "NONE", "NONE")
call s:hi("PmenuSbar", "NONE", "NONE", s:surface1, "NONE", "NONE")
call s:hi("PmenuThumb", "NONE", "NONE", s:overlay2, "NONE", "NONE")
call s:hi("Question", "NONE", s:blue, "NONE", "NONE", "NONE")
call s:hi("QuickFixLine", "NONE", "NONE", s:surface1, "NONE", "NONE")
call s:hi("Search", "NONE", s:pink, s:surface1, "NONE", "NONE")
call s:hi("SpecialKey", "NONE", s:teal, "NONE", "NONE", "NONE")
call s:hi("SpellBad", s:red, "NONE", "NONE", "underline", "underline")
call s:hi("SpellCap", s:yellow, "NONE", "NONE", "underline", "underline")
call s:hi("SpellLocal", s:blue, "NONE", "NONE", "underline", "underline")
call s:hi("SpellRare", s:green, "NONE", "NONE", "underline", "underline")
call s:hi("StatusLine", "NONE", s:text, s:mantle, "NONE", "NONE")
call s:hi("StatusLineNC", "NONE", s:surface1, s:mantle, "NONE", "NONE")
call s:hi("TabLine", "NONE", s:surface1, s:mantle, "NONE", "NONE")
call s:hi("TabLineFill", "NONE", "NONE", s:mantle, "NONE", "NONE")
call s:hi("TabLineSel", "NONE", s:green, s:surface1, "NONE", "NONE")
call s:hi("Title", "NONE", s:blue, "NONE", "NONE", "NONE")
call s:hi("VisualNOS", "NONE", "NONE", s:surface1, "NONE", "NONE")
call s:hi("WarningMsg", "NONE", s:sand, "NONE", "NONE", "NONE")
call s:hi("WildMenu", "NONE", "NONE", s:overlay0, "NONE", "NONE")

call s:hi("Comment", "NONE", s:overlay2, "NONE", "NONE", "NONE")
call s:hi("Constant", "NONE", s:purple, "NONE", "NONE", "NONE")
call s:hi("Identifier", "NONE", s:sand, "NONE", "NONE", "NONE")
call s:hi("Statement", "NONE", s:pink, "NONE", "NONE", "NONE")
call s:hi("PreProc", "NONE", s:cyan, "NONE", "NONE", "NONE")
call s:hi("Type", "NONE", s:cyan, "NONE", "NONE", "NONE")
call s:hi("Special", "NONE", s:violet, "NONE", "NONE", "NONE")
call s:hi("Underlined", "NONE", s:text, s:base, "underline", "underline")
call s:hi("Error", "NONE", s:red, "NONE", "NONE", "NONE")
call s:hi("Todo", "NONE", s:overlay0, s:base, "NONE", "NONE")

call s:hi("String", "NONE", s:green, "NONE", "NONE", "NONE")
call s:hi("Character", "NONE", s:yellow, "NONE", "NONE", "NONE")
call s:hi("Number", "NONE", s:subtext0, "NONE", "NONE", "NONE")
call s:hi("Boolean", "NONE", s:purple, "NONE", "NONE", "NONE")
call s:hi("Float", "NONE", s:white, "NONE", "NONE", "NONE")
call s:hi("Function", "NONE", s:blue, "NONE", "NONE", "NONE")
call s:hi("Conditional", "NONE", s:pink, "NONE", "NONE", "NONE")
call s:hi("Repeat", "NONE", s:pink, "NONE", "NONE", "NONE")
call s:hi("Label", "NONE", s:pink, "NONE", "NONE", "NONE")
call s:hi("Operator", "NONE", s:text, "NONE", "NONE", "NONE")
call s:hi("Keyword", "NONE", s:pink, "NONE", "NONE", "NONE")
call s:hi("Include", "NONE", s:pink, "NONE", "NONE", "NONE")
call s:hi("StorageClass", "NONE", s:violet, "NONE", "NONE", "NONE")
call s:hi("Structure", "NONE", s:yellow, "NONE", "NONE", "NONE")
call s:hi("Typedef", "NONE", s:cyan, "NONE", "NONE", "NONE")
call s:hi("debugPC", "NONE", "NONE", s:crust, "NONE", "NONE")
call s:hi("debugBreakpoint", "NONE", s:viridis, s:base, "NONE", "NONE")

hi link Define PreProc
hi link Macro PreProc
hi link PreCondit PreProc
hi link SpecialChar Special
hi link Tag Special
hi link Delimiter Special
hi link SpecialComment Special
hi link Debug Special
hi link Exception Error
hi link StatusLineTerm StatusLine
hi link StatusLineTermNC StatusLineNC
hi link Terminal Normal
hi link Ignore Comment
hi link shShellVariables shVariable
hi link bashSpecialVariables shVariable
