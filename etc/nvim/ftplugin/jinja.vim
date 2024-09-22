if !get(b:, 'jinja_syntax_autocmd_loaded', v:false)
    " Turn on syntax highlighting for files that have jinja in their filetype,
    " even if there is a treesitter highlighter loaded
    if luaeval("vim.treesitter.language.get_lang('jinja')") == v:null
        autocmd FileType <buffer> if !empty(&ft) | setlocal syntax=on | endif
    endif

    " Only do this once per buffer
    let b:jinja_syntax_autocmd_loaded = v:true
endif
