" buffer_redir.vim
"
" Inspired by the TabMessage function/command combo found
" at <http://www.jukie.net/~bart/conf/vimrc>.
" And many thanks to Bill Odom from StackOverflow
" https://stackoverflow.com/a/2573758

" Captures the output generated by executing a:msgcmd, then places this
" output in the current buffer.
"
" Examples:
"
"   " Insert the output of :registers into the current buffer.
"   call BufferRedir('registers')
"
function! BufferRedir(msgcmd)
    " Redirect messages to a variable.
    redir => message

    " Execute the specified Ex command, capturing any messages
    " that it generates into the message variable.
    silent execute a:msgcmd

    " Turn off redirection.
    redir END

    " Place the messages in the destination buffer.
    silent put=message
endfunction

" Create commands to make BufferRedir() easier to use interactively.
" Here are some examples of their use:
"
"   :BufferRedir messages
"   :BufferRedir registers
"
command! -nargs=+ -complete=command BufferRedir call BufferRedir(<q-args>)
