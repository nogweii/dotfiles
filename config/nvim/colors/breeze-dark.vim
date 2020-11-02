" breeze-dark.vim -- Vim color scheme.
" Author:      Nogweii (hello@nogweii.one)
" Webpage:     https://nogweii.one/projects/breeze-dark-vim.html
" Description: A Vim color scheme using the KDE Breeze colors, with dark intentions and full support for plugins & semantic highlighting.
" Last Change: 2020-10-12


hi clear

if exists("syntax_on")
  syntax reset
endif

let colors_name = "breeze-dark"

hi Normal guibg=#232629 guifg=#fcfcfc gui=NONE
hi Comment  guifg=#7f8c8d gui=NONE
hi NonText  guifg=#4d4d4d gui=NONE
hi link EndOfBuffer NonText
hi link Whitespace NonText
hi Constant  guifg=#f47750 gui=NONE
hi String  guifg=#2ecc71 gui=NONE
hi Character  guifg=#e74c30 gui=NONE
hi Number  guifg=#da4453 gui=NONE
hi Boolean  guifg=#fdbc4b gui=NONE
hi link Float Number
hi Identifier  guifg=fg gui=NONE
hi Function  guifg=#9b59b6 gui=NONE
hi Statement  guifg=#1d99f3 gui=NONE
hi Conditional  guifg=#1d99f3 gui=NONE
hi Repeat  guifg=#93cee9 gui=NONE
hi Label  guifg=#da4453 gui=NONE
hi Operator  guifg=#27ae60 gui=NONE
hi Keyword  guifg=#2980b9 gui=NONE
hi Exception  guifg=#e74c30 gui=NONE
hi link Noise Delimiter
hi PreProc  guifg=#c9ce3b gui=NONE
hi Include  guifg=#11d116 gui=NONE
hi Define  guifg=#3daee9 gui=NONE
hi Macro  guifg=#3daee9 gui=NONE
hi PreCondit  guifg=#c9ce3b gui=NONE
hi Type  guifg=#3daee9 gui=NONE
hi StorageClass  guifg=#f47750 gui=NONE
hi Structure  guifg=#3daee9 gui=NONE
hi Typedef  guifg=#3daee9 gui=NONE
hi Special  guifg=#e74c30 gui=NONE
hi SpecialChar  guifg=#e74c30 gui=NONE
hi link SpecialKey Character
hi link Tag Underlined
hi Delimiter  guifg=#eff0f1 gui=NONE
hi SpecialComment  guifg=#7f8c8d gui=NONE
hi link Debug WarningMsg
hi Underlined  guifg=#93cee9 gui=NONE
hi Ignore  guifg=#7f8c8d gui=NONE
hi Error guibg=#ed1515 guifg=#eff0f1 gui=NONE
hi Todo  guifg=#fdbc4b gui=NONE
hi link helpHyperTextJump Underlined
hi link helpNormal Normal
hi link helpLeadBlank Normal
hi Hint guibg=#e74c30 guifg=#31363b gui=NONE
hi Info guibg=#da4453 guifg=#31363b gui=NONE
hi Warning guibg=#d35400 guifg=#31363b gui=NONE
hi StatusLine guibg=#4d4d4d guifg=#11d116 gui=NONE
hi StatusLineNC guibg=#4d4d4d guifg=#7f8c8d gui=NONE
hi link StatusLineTerm StatusLine
hi link StatusLineTermNC StatusLineNC
hi VertSplit  guifg=#4d4d4d gui=NONE
hi TabLine guibg=#4d4d4d guifg=fg gui=NONE
hi TabLineFill  guifg=fg gui=NONE
hi TabLineSel guibg=#4d4d4d guifg=fg gui=NONE
hi Title   gui=NONE
hi Conceal guibg=NONE guifg=NONE gui=NONE
hi CursorLine guibg=#31363b  gui=NONE
hi CursorLineNr guibg=#31363b guifg=#da4453 gui=NONE
hi link debugBreakpoint ErrorMsg
hi link debugPC ColorColumn
hi LineNr  guifg=#7f8c8d gui=NONE
hi QuickFixLine guibg=#4d4d4d  gui=NONE
hi Visual   gui=NONE
hi VisualNOS guibg=#4d4d4d  gui=NONE
hi Pmenu guibg=#31363b guifg=fg gui=NONE
hi PmenuSbar guibg=#31363b  gui=NONE
hi PmenuSel  guifg=fg gui=NONE
hi PmenuThumb guibg=#eff0f1  gui=NONE
hi WildMenu   gui=NONE
hi FoldColumn guibg=#4d4d4d  gui=NONE
hi Folded guibg=#c039db guifg=#31363b gui=NONE
hi DiffAdd  guifg=#27ae60 gui=NONE
hi DiffChange  guifg=#fdbc4b gui=NONE
hi DiffDelete  guifg=#ed1515 gui=NONE
hi DiffText   gui=NONE
hi IncSearch   gui=NONE
hi Search   gui=NONE
hi MatchParen  guifg=#2ecc71 gui=NONE
hi SpellBad guibg=#ed1515  gui=NONE
hi SpellCap guibg=#2980b9  gui=NONE
hi SpellLocal guibg=#da4453  gui=NONE
hi SpellRare guibg=#da4453  gui=NONE
hi ColorColumn   gui=NONE
hi SignColumn   gui=NONE
hi ErrorMsg  guifg=#ed1515 gui=NONE
hi HintMsg  guifg=#e74c30 gui=NONE
hi InfoMsg  guifg=#da4453 gui=NONE
hi ModeMsg  guifg=#fdbc4b gui=NONE
hi link MoreMsg ModeMsg
hi Question  guifg=#f47750 gui=NONE
hi WarningMsg  guifg=#d35400 gui=NONE
hi link LspDiagnosticsError Error
hi link LspDiagnosticsErrorFloating ErrorMsg
hi link LspDiagnosticsErrorSign ErrorMsg
hi link LspDiagnosticsWarning Warning
hi link LspDiagnosticsWarningFloating WarningMsg
hi link LspDiagnosticsWarningSign WarningMsg
hi link LspDiagnosticsHint Hint
hi link LspDiagnosticsHintFloating HintMsg
hi link LspDiagnosticsHintSign HintMsg
hi link LspDiagnosticsInformation Info
hi link LspDiagnosticsInformationFloating InfoMsg
hi link LspDiagnosticsInformationSign InfoMsg
hi LspDiagnosticsUnderline   gui=NONE
hi link LspDiagnosticsUnderlineError CocErrorHighlight
hi link LspDiagnosticsUnderlineHint CocHintHighlight
hi link LspDiagnosticsUnderlineInfo CocInfoHighlight
hi link LspDiagnosticsUnderlineWarning CocWarningHighlight
hi Cursor   gui=NONE
hi link CursorIM Cursor
hi CursorColumn guibg=#31363b  gui=NONE
hi Directory  guifg=#1d99f3 gui=NONE
hi link cConstant Constant
hi link cCustomClass Type
hi link cppSTLexception Exception
hi link cppSTLnamespace String
hi link csBraces Delimiter
hi link csClass Structure
hi link csClassType Type
hi link csContextualStatement Conditional
hi link csEndColon Delimiter
hi link csGeneric Typedef
hi link csInterpolation Include
hi link csInterpolationDelimiter SpecialChar
hi link csLogicSymbols Operator
hi link csModifier Keyword
hi link csNew Operator
hi link csNewType Type
hi link csParens Delimiter
hi link csPreCondit PreProc
hi link csRepeat Repeat
hi link csStorage StorageClass
hi link csUnspecifiedStatement Statement
hi link csXmlTag Define
hi link csXmlTagName Define
hi link cssBraces Delimiter
hi link cssProp Keyword
hi link cssSelectorOp Operator
hi link cssTagName htmlTagName
hi link scssAmpersand Special
hi link scssAttribute Label
hi link scssBoolean Boolean
hi link scssDefault Keyword
hi link scssElse PreCondit
hi link scssIf PreCondit
hi link scssInclude Include
hi link scssSelectorChar Operator
hi link scssSelectorName Identifier
hi link scssVariable Define
hi link scssVariableAssignment Operator
hi link dartLibrary Statement
hi link dotKeyChar Character
hi link dotType Type
hi link goBlock Delimiter
hi link goBoolean Boolean
hi link goBuiltins Operator
hi link goField Identifier
hi link goFloat Float
hi link goFormatSpecifier Character
hi link goFunction Function
hi link goFunctionCall goFunction
hi goFunctionReturn   gui=NONE
hi link goMethodCall goFunctionCall
hi link goParamType goReceiverType
hi link goPointerOperator SpecialChar
hi link goPredefinedIdentifiers Constant
hi link goReceiver goBlock
hi link goReceiverType goTypeName
hi link goSimpleParams goBlock
hi link goType Type
hi link goTypeConstructor goFunction
hi link goTypeName Type
hi link goVarAssign Identifier
hi link goVarDefs goVarAssign
hi link htmlArg Label
hi htmlBold  guifg=#bdc3c7 gui=NONE
hi link htmlTitle htmlBold
hi link htmlEndTag htmlTag
hi link htmlH1 markdownH1
hi link htmlH2 markdownH2
hi link htmlH3 markdownH3
hi link htmlH4 markdownH4
hi link htmlH5 markdownH5
hi link htmlH6 markdownH6
hi htmlItalic   gui=NONE
hi link htmlSpecialTagName Keyword
hi link htmlTag Special
hi link htmlTagN Typedef
hi link htmlTagName Type
hi link javaClassDecl Structure
hi link jsFuncBlock Function
hi link jsObjectKey Type
hi link jsReturn Keyword
hi link jsVariableDef Identifier
hi link jsonBraces luaBraces
hi link jsonKeywordMatch Operator
hi link jsonNull Constant
hi link jsonQuote Delimiter
hi link jsonString String
hi link jsonStringSQError Exception
hi link luaBraces Structure
hi link luaBrackets Delimiter
hi link luaBuiltin Keyword
hi link luaComma Delimiter
hi link luaFuncArgName Identifier
hi link luaFuncCall Function
hi link luaFuncId luaNoise
hi link luaFuncKeyword Type
hi link luaFuncName Function
hi link luaFuncParens Delimiter
hi link luaFuncTable Structure
hi link luaLocal Type
hi link luaNoise Operator
hi link luaParens Delimiter
hi link luaSpecialTable StorageClass
hi link luaSpecialValue Function
hi link makeCommands Statment
hi link makeSpecTarget Type
hi markdownH1  guifg=#ed1515 gui=NONE
hi markdownH2  guifg=#f67400 gui=NONE
hi markdownH3  guifg=#fdbc4b gui=NONE
hi markdownH4  guifg=#27ae60 gui=NONE
hi markdownH5  guifg=#3daee9 gui=NONE
hi markdownH6  guifg=#c039db gui=NONE
hi link mkdBold SpecialComment
hi link mkdCode Keyword
hi link mkdCodeDelimiter mkdBold
hi link mkdCodeStart mkdCodeDelimiter
hi link mkdCodeEnd mkdCodeStart
hi link mkdHeading Delimiter
hi link mkdItalic mkdBold
hi link mkdListItem Special
hi link mkdRule Underlined
hi link texMathMatcher Number
hi link texMathZoneX Number
hi link texMathZoneY Number
hi link pythonBrackets Delimiter
hi link pythonBuiltinFunc Operator
hi link pythonBuiltinObj Type
hi link pythonBuiltinType Type
hi link pythonClass Structure
hi link pythonClassParameters pythonParameters
hi link pythonDecorator PreProc
hi link pythonDottedName Identifier
hi link pythonError Error
hi link pythonException Exception
hi link pythonInclude Include
hi link pythonIndentError pythonError
hi link pythonLambdaExpr pythonOperator
hi link pythonOperator Operator
hi link pythonParam Identifier
hi link pythonParameters Delimiter
hi link pythonSelf Statement
hi link pythonSpaceError pythonError
hi link pythonStatement Statement
hi link rubyClass Structure
hi link rubyDefine Define
hi link rubyInterpolationDelimiter Delimiter
hi link rustKeyword Keyword
hi link rustModPath Include
hi link rustScopeDecl Delimiter
hi link rustTrait StorageClass
hi link scalaKeyword Keyword
hi link scalaNameDefinition Identifier
hi link shDerefSimple SpecialChar
hi link shFunctionKey Function
hi link shLoop Repeat
hi link shParen Delimiter
hi link shQuote Delimiter
hi link shSet Statement
hi link shTestOpr Debug
hi link solBuiltinType Type
hi link solContract Typedef
hi link solContractName Function
hi link tomlComment Comment
hi link tomlKey Label
hi link tomlTable StorageClass
hi link helpSpecial Special
hi link vimFgBgAttrib Constant
hi link vimHiCterm Label
hi link vimHiCtermFgBg vimHiCterm
hi link vimHiGroup Typedef
hi link vimHiGui vimHiCterm
hi link vimHiGuiFgBg vimHiGui
hi link vimHiKeyList Operator
hi link vimOption Define
hi link vimSetEqual Operator
hi link xmlAttrib htmlArg
hi link xmlEndTag xmlTag
hi link xmlEqual Operator
hi link xmlTag htmlTag
hi link xmlTagName htmlTagName
hi link sqlKeyword Keyword
hi link sqlParen Delimiter
hi link sqlSpecial Constant
hi link sqlStatement Statement
hi link sqlParenFunc Function
hi link dosiniHeader Title
hi link crontabDay StorageClass
hi link crontabDow String
hi link crontabHr Number
hi link crontabMin Float
hi link crontabMnth Structure
hi plantumlColonLine   gui=NONE
hi link ALEErrorSign ErrorMsg
hi link ALEWarningSign WarningMsg
hi CocErrorHighlight   gui=NONE
hi CocHintHighlight   gui=NONE
hi CocInfoHighlight   gui=NONE
hi CocWarningHighlight   gui=NONE
hi link CocErrorSign ALEErrorSign
hi link CocHintSign HintMsg
hi link CocInfoSign InfoMsg
hi link CocWarningSign ALEWarningSign
hi link EasyMotion IncSearch
hi link JumpMotion EasyMotion
hi GitGutterAdd  guifg=#2ecc71 gui=NONE
hi GitGutterChange  guifg=#fdbc4b gui=NONE
hi GitGutterDelete  guifg=#ed1515 gui=NONE
hi GitGutterChangeDelete  guifg=#f67400 gui=NONE
hi link SignifySignAdd GitGutterAdd
hi link SignifySignChange GitGutterChange
hi link SignifySignDelete GitGutterDelete
hi link SignifySignChangeDelete GitGutterChangeDelete
hi IndentGuidesOdd guibg=#4d4d4d  gui=NONE
hi IndentGuidesEven guibg=#31363b  gui=NONE
hi link NERDTreeCWD Label
hi link NERDTreeUp Operator
hi link NERDTreeDir Directory
hi link NERDTreeDirSlash Delimiter
hi link NERDTreeOpenable NERDTreeDir
hi link NERDTreeClosable NERDTreeOpenable
hi link NERDTreeExecFile Function
hi link NERDTreeLinkTarget Tag
hi link TSConstBuiltin Constant
hi link TSConstructor Typedef
hi link TSFuncBuiltin Function
hi link TSStringEscape Character
hi link TSStringRegex SpecialChar
hi link TSURI Tag
hi link TSVariableBuiltin Identifier

" Generated with eRuby, inspired by RNB & Highlite theme engines
