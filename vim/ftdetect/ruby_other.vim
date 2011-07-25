" Other ruby files
au BufNewFile,BufRead Rakefile set filetype=ruby
au BufNewFile,BufRead *.ru set filetype=ruby

" Rubygem's RC file is really YAML
au BufNewFile,BufRead .gemrc set filetype=yaml
au BufNewFile,BufRead gemrc set filetype=yaml
