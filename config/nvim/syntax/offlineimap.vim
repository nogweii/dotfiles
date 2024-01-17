" Vim syntax file for offlineimap

if exists("b:current_syntax")
  finish
endif

syn match offlineimapComment /#.*$/ contains=@Spell
syn match offlineimapGeneral /\[\(general\|mbnames\)\]/
syn match offlineimapAccount /\[\(Account\).*\]/
syn match offlineimapRepository /\[\(Repository\).*\]/

syn match offlineimapOption /\<\w\+\>\s\+=/

syn match offlineimapNumber /\<\(\d\+$\)/
syn match offlineimapBool /\<\(true\|false\)\>/
syn match offlineimapActivate /\<\(yes\|no\)\>/

highlight default link offlineimapComment Comment
highlight default link offlineimapGeneral Function
highlight default link offlineimapAccount Function
highlight default link offlineimapRepository Function
highlight default link offlineimapOption Type
highlight default link offlineimapNumber Number
highlight default link offlineimapBool Boolean
highlight default link offlineimapActivate Boolean

let b:current_syntax = "offlineimap"
