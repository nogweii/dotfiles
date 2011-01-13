" Name: NERD_tree Project
" Author: Janlay Wu <janlay@gmail.com>
" Version: 0.1
"
" Description:
"   This plugin works together with NERD_tree (http://www.vim.org/scripts/script.php?script_id=1658).
"   It tries to find out root project directory for current file,
"   and calls NERD_tree in that directory.
"   If no project found, then calls NERD_tree in current directory.
"
" Install:
"   First, make sure NERD_tree is installed. Then put NERD_tree_project.vim
"   in ~/.vim/plugin (*nix) or $HOME/vimfiles/plugin (DOS)
"
" Usage:
"   Type in normal mode:
"       ToggleNERDTree<CR>,
"   or map shortcut in your .vimrc file:
"       map <F8> :ToggleNERDTree<CR>
"
" Customize:
"   Make NERD_tree Project recognize more project, such as scons:
"       let g:NTPNames = add(g:NTPNames, 'SConstruct')
"   or add more file types:
"       extend(g:NTPNames, ['*.sln', '*.csproj'])
"
" License: {{{
"   Software License Agreement (BSD License)
"
"   Copyright (c) 2002 - 2009
"   All rights reserved.
"
"   Redistribution and use of this software in source and binary forms, with
"   or without modification, are permitted provided that the following
"   conditions are met:
"
"   * Redistributions of source code must retain the above
"     copyright notice, this list of conditions and the
"     following disclaimer.
"
"   * Redistributions in binary form must reproduce the above
"     copyright notice, this list of conditions and the
"     following disclaimer in the documentation and/or other
"     materials provided with the distribution.
"
"   * Neither the name of Gergely Kontra or Eric Van Dewoestine nor the names
"   of its contributors may be used to endorse or promote products derived
"   from this software without specific prior written permission of Gergely
"   Kontra or Eric Van Dewoestine.
"
"   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
"   IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
"   THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
"   PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
"   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
"   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
"   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
"   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
"   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
"   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
"   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
" }}}

if exists('loaded_nerdtreeproject') || version < 700
	finish
endif
let loaded_nerdtreeproject = 1

" set default filename for local vimrc
if !exists("g:NTPNames")
  let g:NTPNames = ['build.xml', 'Makefile', '.project', '.lvimrc']
endif

"Function: s:loadLocalVimrc() {{{3
"upwards search project file
function! s:findProject()
	for filename in g:NTPNames
		let file = findfile(filename, expand("%:p:h") . ';')
		if filereadable(file)
			let b:ProjectRoot = fnamemodify(file, ':p:h')
			break
		endif
	endfor
endfunction

"Function: s:toggle() {{{3
"call NERD_tree in different ways
"
"Args:
"name: the full path for the root node (is only used if the NERD tree is being
"initialized.
function! s:toggle(name)
	let dir = expand("%:p:h")
	let cmd = 'NERDTreeToggle '
	if a:name !=# ''
		" load the passed position
		let cmd .= a:name
	elseif exists('b:ProjectRoot') && stridx(dir, b:ProjectRoot, 0) == 0
		" found in project directory
		let cmd .= b:ProjectRoot
	else
		" then load current directory
		let cmd .= dir
	endif
	execute cmd
endfunction

" create new command for this plugin
command! -n=? -complete=dir -bar ToggleNERDTree :call s:toggle('<args>')
" find project directory when creating or reading file
autocmd BufNewFile,BufRead * call s:findProject()

