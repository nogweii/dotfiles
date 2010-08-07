
let g:WinWalker_debug = ''	"jumplist,events,help_extract

" Header {{{
"------------------------------------------------------------------------------
"                     WinWalker.vim : Vim windows navigator/manager {{{
"
"
"
" Author:		Eric Arnold ( eric_p_arnold@yahoo.com )
"							  ^^^^^^^^^^^^^^^^^^^^^^^
"							  Comments, bugs, feedback welcome.
" Created:		Apr, 2006
" Updated:		Wed May 24, 05/24/2006 6:00:03 PM
" Requirements:	Vim 7
" Dependencies:	Char_menu.vim
"
" Version:		1.0		Wed Apr 19, 04/19/2006 4:25:30 AM
"						-	Initial release
"				1.1		Fri Apr 21, 04/21/2006 1:33:54 PM
"						-	Added help extraction to standard help
"						-	Tweaked to handle Taglist.vim
"				1.2		Mon Apr 24, 04/24/2006 3:47:09 AM
"						-	handle copen and exchange/push
"						-	fixed searching by bufnr bug
"						-	switch to Unix file format
"						-	added session quick-save
"						-	revamped help and help folding
"				1.2.1	Mon Apr 24, 04/24/2006 8:38:24 AM
"						-	Restore accidentally deleted function
"				2.0		Tue Apr 25, 04/25/2006 12:52:39 AM
"						-	Constrained size and placement.  Windows can be
"							'attached' to walls or other windows.
"						-	Added key feedkeys() macros
"						-	Sessions remember WinWalker constraints
"				2.1		Tue May 16, 05/16/2006 9:15:05 AM
"						-	added 'follow' option
"						-	'c' preset now sets the sticky auto-resize
"							line/column values
"						-	windows and buffers auto-resize separately
"						-	small cmdheight bug
"						-	smarter handling of 'previous' window
"						-	various sizing fixes
"				2.2		Wed May 24, 05/24/2006 5:59:28 PM
"						-	Revamped window jump list ( ^I and ^O ); better
"							tracking, cleanup, and display.
"						-	Fixed a few problems with the find command and
"							menu handler returning early
"						-	Separated startup and menu timeoutlen[s].
"						-	Updated help file.
"
"
" }}}
" Help Start:
"*WinWalker.txt* version2.2 : Vim windows navigator/manager 
"
"	Table Of Contents: ~ {{{
"
"	|WW_Features|
"	|WW_Useful_Examples|
"	|WW_Initial_Set_Up|
"	|WW_Commands|
"		|WW_Starting|
"		|WW_Main_Menu|
"			|WW_window_next_prev|
"			|WW_movement|
"			|WW_rotate|
"			|WW_only_clone|
"			|WW_Jump_list|
"			|WW_grow|  |WW_resize|
"			|WW_New_edit_etc|
"			|WW_equalize|
"			|WW_ex_passthrough|
"			|WW_edit_passthrough|
"			|WW_find_window|
"			|WW_Quitting|
"		|WW_Misc_Menu|
"			|WW_grow_toggle|
"			|WW_screen_wrap_type|
"			|WW_menu_verbose_level|
"			|WW_highlight_current_window|
"			|WW_empty_files|
"			|WW_timeoutlen|
"			|WW_mtimeoutlen|
"			|WW_Jump_list_display|
"			|WW_key_remap|
"			|WW_Dropout_keys|
"			|WW_Macro_keys|
"		|WW_Tab_Menu|
"		|WW_Save_Menu|
"		|WW_Attach_Constrain_Menu|
"		|WW_Customizing|
"	|WW_Notes|
"	|WW_Other|
"	|WW_Caveats|
"	|WW_Bugs|
"
"
"	|Turn_folding_on|
"	|Turn_folding_off|
" }}}
"
"
"  Features:                                            *WW_Features* {{{~
"
"	-	Navigate and manage windows and layouts with blinding speed!
"
"	-	Multiple window operations are faster and easier because it creates
"		a window navigation and management mode where all window commands
"		are familiar single chars,  i.e.  h,H,^H  and j,J,^J  etc.  If you
"		weren't using Vim in full-screen mode, you'll start considering it.
"
"	-	More added functionality:
"
"		-	Better [im]movability:
"
"			-	Enhanced push/drag for windows and buffers.
"
"			-	Push/drag/exchange buffers between windows, and windows
"				between tabs.  Use adjacent tabs as if they were extensions
"				of the current visible screen.
"			
"			-	Constrained size and placement.  Windows can be 'attached'
"				to walls or other windows.  Windows will resize to preset
"				values as you move into them.  
"
"			-	Windows can follow focus into new tabs.
"
"		-	Explore buffer list, and quick search/jump to any window in any
"			tab by giving the first unique char(s).  It jumps to a buffer's
"			window, instead of the default action of editing buffers in the
"			current window.
"
"
"		-	More intuitive for many operations:
"			
"			-	Incremental resizing is easier, using the [HJKL] and
"				[^h^j^k^l].
"
"			-	Resize in the direction specified, unlike  wincmd + - < > 
"
"			-	Windows move/push past each other more like you'd expect.
"				They can push 'between', instead of just 'maximize at far
"				side'.
"
"		-	Other usability stuff:
"
"			-	A window-level jump list for ^I/^O  movement.
"
"			-	O/^O  as a tab-capable alternative to ^Wo .
"
"			-	Various shortcuts to help keep you from breaking your
"				stride, i.e. to  :edit,  :ex  mode, sessions, macros, remapping
"
"	-	All the commands are presented as a prompt menu in the command line
"		so forgetting stuff isn't a problem.
"
"	-	Many of the commands are the same as the default Vim window  ^W  and
"		'wincmd' for easy transition, others are shifted around to be more
"		consistent.
"
"	-	Different invocation modes:  |WW_timeoutlen| for menu, key
"		pass-through.  The goal is to have the menu mode there when you
"		want it, and keep quiet otherwise.  Experiment with the different
"		methods to find what works best for you.  Particularly useful for
"		^W^P   ^W^O  ^W^I  ^W/   etc.
"
"
" }}}
"
"
"  Useful Examples:                                            *WW_Useful_Examples* {{{~
"
"	... Or, 'How is this going to help me?'
"
"
"	Ergonomics:
"
"		Overall speed dealing with multiple windows.  Try it out:
"
"		-	Start the WinWalker menu, ^W
"		-	Open a new tab with  'N'
"		-	Open a few windows with  'n' and 'v'
"		-	Jump around a little with  '[hljk]'
"		-	Put some files in them with 'e'
"		-	Push some around with '[HLJK]'
"		-	Smack them all down with 'q'
"
"		By now you should have a feel for the difference it makes to have
"		single keys to deal with the window system.
"
"
"	AutoResizing:
"
"		-	Your screen/terminal/GUI size will no longer cramp your style.
"			Create windows that are always big enough.
"
"			Example:  You have several windows open, vertical and
"			horizontal, which you set to 80 columns, but you only have
"			120 or whatever.
"
"			-	Quickly visit each window, set the dimensions with 'c'.
"				See |WW_Misc_Menu| => |WW_presets| if you want to change
"				the values.
"			
"			-	For more individualized sizing,  'grow' the window to
"				whatever size makes sense, and use the
"				|WW_Attach_Constrain_Menu| -> lc 'lock columns' (and/or
"				'll') command to cause the window to resize itself to the
"				current size each time it is entered.
"
"			Now all those windows will resize themselves each time they are
"			entered.
"
"	Templates:
"
"	-	Store a tab to a session with the windows arranged how you
"		want.
"
"	-	Create a macro key to retrieve the template like:
"
"		<F2>	srsessname^M
"
"				s				save menu
"				 r				restore
"				  sessname		name of the session (without Session. or .vim )
"						  ^M	literal carriage return, with ^V^M or ^Q^M
"					
"
"	Keeping Track Of Buffers:
"
"		If you're using TabLineSet.vim with the verbose option, you'll be
"		able to see all open buffers in the tabline (especially if you have
"		the 'Tabline wrapping patch' installed).
"
"		The find, '/', command will show you all loaded buffers, or '?' for
"		all buffers.
"
"   See Also:  |WW_Other|
"
"
"	To do :  Better tab-to-tab navigation. (^I, ^O, wrapping, /find )
"	To do :  Creating Layouts with attachments and locked sizes.
"
"
"
" }}}
"
"	Initial Set Up:                                 *WW_Initial_Set_Up* {{{~
"
"		-	Unzip the distribution file above your plugin to extract:
">
"				plugin/WinWalker.vim
"				plugin/Char_menu.vim
"				doc/WinWalker.txt
"<
"
"		-	Set up a key map.  I.e.
">
"				nmap <silent> <Leader>w :call WinWalkerMenu()<CR>
"<
"					Default.  I like it mapped to ^W, since I'm used to
"					hitting ^W for windows stuff, and WinWalker is mostly a
"					superset of the standard ^W/wincmd commands.
"
"		-	Look through the |WW_Misc_Menu| and |WW_Customizing| for other
"			options to change.
"
"
"}}}
"
"
"
"                             Commands:                   *WW_Commands*  {{{~
"
"
"	Starting:                                          *WW_Starting*   {{{~
"
"				Assuming you decide to map ^W :
">
"		nnoremap <silent> <C-W> :call WinWalkerMenu()<CR>
"<
"			<C-W>
"
"				-	Type <C-W> and note the delay (~1/2 sec) before the menu
"					starts.  
"
"				-	Type <C-W>j (or whatever) fast together, and it
"					should move down one window without starting the menu.  
"
"				-	Decide whether you want more or less time to type the
"					second char before the menu starts.  
"
"				-	Change the timeoutlen in {m}isc
"					menu -> {t}imeoutlent ( |WW_timeoutlen| ).
"
"			<C-W><SPACE>
"
"				This starts the menu immediately without the initial
"				timeoutlen.  (For us impatient people :-)  Set timeoutlen to 0
"				for a similar effect.
"
"			See also |WW_Customizing|
"
"
"}}}
"
"
"	Main Menu:										 *WW_Main_Menu* {{{~
"
"
"		{w}/{W}/{<C-W>}indow=>Nxt/Prv				*WW_window_next_prev*
"
"				Move through the window list, same as default behavior.
"
"
"		{k}/{K}/{<C-K>}=up							*WW_movement*
"		{j}/{J}/{<C-J>}=down
"		{h}/{H}/{<C-H>}=left
"		{l}/{L}/{<C-L>}=right
"
"				There are three general kinds of move operations with Vim
"				windows that are handled here:  
"				1)	moving a cursor from one window into another, 
"				2)	moving a buffer from one window to another, without
"					creating or destroying windows , 
"				3)	moving a window (and its buffer and cursor) to a new
"					location, causing the layout to rearrange.
"				
"				*	See |WW_grow| for the alternation actions for these
"					keys when 'grow' mode is on.
"
"				The above key commands are set up along those lines:
"
"			-	Lowercase='move cursor to another window'
"			-	Uppercase='push the current window up/down/left/right'
"			-	Control='exchange buffers, keep focus with buffer'
"				(when exchanging to an adjacent tab, it relocates the
"				buffer into a new window, since it's more confusing
"				otherwise).
"			-	Arrow keys are mapped to analogous [jkhl] commands, i.e.
"				<S-UP> is mapped to 'K'.
"
"
"		{r}/{R}=rotate wins/buffers					*WW_rotate*
"
"			{r}	Rotates the current window according to new/vnew.  It isn't
"				smart about detecting the orientation of a window it
"				hasn't seen before, so you might have to hit "r" twice to
"				get it started.
"
"			{R}	Rotate all the buffers around the windows in the screen
"				without changing the window layout.
"
"		{o}/{O}='only'/clone					*WW_only_clone*
"
"			{o}	The default 'wincmd' behavior where all windows are closed
"				but the current.  
"				
"			{O}	Clone the current buffer in it's own new tab.  This is for
"				when you want to temporarily maximize the current window
"				without changing any others.  If <C-O> ( <C-W><C-O> from Vim
"				Normal ) is the first command used to leave, it will close the
"				window, and return to the origination.
"			
"												*WW_Jump_list*
"		{<C-I>}/{<C-O>}=jump forward/back
"
"				The jump list is a history of windows visited.  It
"				traverses WinWalker's window-level jump list.  (As opposed
"				to the Vim Normal mode ^I/^O, which traverses a
"				buffer-level jump list.)
"
"				^I moves the jump list index forward toward a higher, more
"				recent jump number.
"
"				^O moves the jump list index backward toward a lower, older
"				jump number.
"				
"				If you move to a different window when the jump list index
"				isn't at the end, the new window jump point will be
"				inserted into the list.  The list manager will prune
"				invalid jump points when they are encountered by ^I or ^O.
"
"				See |WW_Misc_Menu| -> |WW_Jump_list_display| to inspect the
"				jump list, or go directly to a specific jump number.
"
"		{t}ab menu
"			
"				Submenu for handling tabs.  See |WW_Tab_Menu|
"
"		Window {a}ttach/Constrain menu
"
"				Submenu for setting windows attachments, fixed sizing, etc.
"				See |WW_Attach_Constrain_Menu|
"
"		{m}isc menu
"				
"				Submenu for setting options.  See |WW_Misc_Menu|
"
"		{s}ave/restore menu
"
"				Submenu for saving sessions.  See |WW_Save_Menu|
"				( ^S and ^R macros are set by default to
"				quicksave/restore.  See |WW_Macro_keys| .)
"
"
"		{g}row        *WW_grow*  *WW_resize*
"
"				Toggles 'grow' mode where the shift- and control- of [jkhl]
"				keys are changed to do window resizing.  This toggle in the
"				|WW_Misc_Menu|, but is also at the top level for convenience.
"
"			[JKHL]
"				Grow the window one line/col in the given direction.  I.e
"				(remember, 'l' is Vim for 'right', and 'h' is Vim for
"				'left' ) for win1 'L' will increase the width by 1, and 'H'
"				will decrease by 1, and the opposite will happen for win2:
">
"				---------------
"				|     L|->    |
"				|win1  |      |
"				|    <-|H     |
"				---------------
"				---------------
"				|     L|->    |
"				|      |  win2|
"				|    <-|H     |
"				---------------
"<
"				The [JKHL] and arrow keys attempt to 'grow' the window in
"				that direction, not just shrink or expand like wincmd + - <
"				>  .  Hopefully others will find this more intuitive, as I
"				do.  The exception is for windows between other windows,
"				where the directional keys become ambiguous, in which case a
"				default action is done.
"
"			^J^K^H^L
"				Grow the window more quickly (I picked a couple values
"				which seemed about right...not real scientific).
"				
"
"				This option is reset each time WinWalkerMenu() is called
"				from Vim Normal mode via ^W or whatever.
"				
"												*WW_New_edit_etc*
"		{n}ew win
"		{v}ert new
"
"				Same as  :new  and  :vnew  .
"
"		{=}equalize windows						*WW_equalize*
"
"				Same as Vim default.
"
"		{:}ex									*WW_ex_passthrough*
"
"				Prompts for a command and transfers it to  :ex  to execute.
"				Saves time exiting and re-entering the menu.
"
"		{e}dit									*WW_edit_passthrough*
"				A shortcut to |:edit| without exiting the menu first.
"
"		{/}{?}find win							*WW_find_window*
"
"				Submenu of available/loaded buffers.  Typing the first one or
"				so unique leading characters jumps to the matched window.
"				Buffer name or number can be selected. {?} includes hidden
"				buffer names.
"
"		{c}olumn preset ( lines, cols, actually)	|WW_presets|
"
"				A quick set option to resize the current window to user's
"				preferred working dimensions, I.e. 80 columns, and 15 lines.
"
"													*WW_Quitting*
"		:{q}uit/:{Q}uit!/{<C-Q>}=tabclose!/{Z}Z
"				
"				Sends various quit commands.
"
"		{<SPACE>}/{<ESC>}=>Exit
"
"				Exit the WinWalker subsytem.  <C-C> and <BS> also work.  If
"				<C-W><SPACE>, <C-W><C-SPACE>, <C-W><CR>, <C-W><C-CR> are
"				entered before the timeoutlen (before the menu starts), it
"				the reverse effect, and bypasses the timeoutlen, and starts
"				the menu immediately.
"
"		Any other keys
"
"				... are delivered to Normal mode, and the menu exits.  So,
"				typing ^U, ^D, ^F, ^B  exits WinWalker, and scroll the
"				window.
"
" }}}
"
"
"	Attach/Constrain Menu:                        *WW_Attach_Constrain_Menu*   {{{~
"
"		lines/{c}olumns preset		*WW_presets*
"
"				The preset is used create a standard workable size for a
"				window.  It is most useful when creating and moving around
"				a layout with multiple windows.  This will prompt for
"				{lines,cols}.  It is triggered by the 'c' command at the
"				top level.
"
"		{ll}/{lc} lock-lines/cols
"				
"				Lock the height or width at the current value.
"				
"		{a}ttach
"				
"				Attach the window to walls or other windows.
"
"				An attachment tells the window to seek a position next to a
"				wall or another window, but doesn't create any bond with
"				other window, meaning the attachment won't 'pull' the other
"				window.
"
"				The best way to decrease conflicts is to set the minimum
"				number of constraints possible.  Vim windows can have
"				constraints of their own which can interferWinWalkerWinWalker
"				e with WinWalkers (i.e. if you have two windows, one atop
"				the other, Vim causes them to share the same width, so
"				different WinWalker 'locked' column values won't result in
"				windows with different widths).
"
"				The placement routine is fairly simplistic, and will have
"				trouble with many windows and constraints, particularly
"				because Vim's native placement can fight back.
"
"				There can be cases when there are no direct conflicts, but
"				there are a lot of windows open, and doing the auto-resize
"				for locked line/col values will fight with the Vim default
"				window size minimums.
"
"				There are some cases where the auto-resizing won't work
"				immediately, i.e. a window is trying to resize itself to a
"				locked value, and it is adjacent to the current/cursor
"				window, then the minimum Vim value for the current window
"				will take precedence.  Typing '=' in some other window can
"				give all windows a chance to update properly.
"
"				There are some cases that aren't handled:
"
"				-	A window attached to walls up/down or left/right will
"					not necessarily allow windows to 'push' (JKHL)
"					past it.  
"
"					The exchange (^J^K^H^L) command is the right choice for
"					this, and similar move-past situations.
"				
"				-	If win1 is attached upwards to the top wall, and you're
"					trying to 'push' win2 which is adjacent to win1, it can
"					fail depending on the rotation of win2 (which causes it
"					to always try to push win2 over win1).
"
"
"
" }}}
"
"
"	Misc Menu:                                        *WW_Misc_Menu* {{{~
"
"
"		{g}row						*WW_grow_toggle*
"
"			Toggle whether the [jkhl] uppercase and control keys are used
"			to resize windows.
"
"		{w}rap						*WW_screen_wrap_type*
"
"			Set wrap behavior:
"				None :	default Vim
"				Win :	wrap at top/bottom/left/right sides of screen
"				tabs :	wrap into adjacent tabs at R/L sides of screen
"
"		{me}nu							*WW_menu_verbose_level*
"
"			Set whether full prompt is shown for the main menu.
"
"		{h}ighlight current win 		*WW_highlight_current_window*
"
"			Toggle highlighting of the current window.  The highlighting is
"			done to help distinguish which window is current, since the
"			cursor won't be visible.
"
"		{e}mpty files					*WW_empty_files*
"
"			Same as highlighting, above, but add some text to empty files
"			to help distinguish as current window.
"
"		{t}imeoutlen  					*WW_timeoutlen*
"
"			Set the timeoutlen used for the wait for the key press after ^W
"			(or whatever) in Vim Normal mode.  If the second key is
"			received before the timeoutlen, the command is executed and
"			control returns immediately to Vim Normal.  If timeoutlen is
"			reached, the main menu starts and subsequent commands require
"			only one key.  Control is kept by WinWalker until the menu is
"			exited.
"
"			It is analogous to the Vim |timeoutlen|, but it is an
"			independent internal value.
"
"			This also affects the timeoutlen for ambiguous matches in the
"			{/}find command (using the Char_menu function).
"
"		{mt}imeoutlen  					*WW_mtimeoutlen*
"
"			Set the timeoutlen used for menu entries which match partial
"			entries, i.e. if the find menu had {win} and {WinWalker}, and
"			you typed 'win', it will return 'win' unless you type 'walker'
"			before mtimeoutlen milliseconds.
"
"		{j}ump list						*WW_Jump_list_display*
"
"			Show the window-level jump list (like |:jumps| ).
">
"                Jump  Tab   Win   WinId Buf   Name
"                5     5     1     60    27    pablo.vim
"                4     4     3     3     3     gui.c
"                3     4     2     14    1     normal.c
"                2     5     1     60    27    pablo.vim
"                1     3     1     1     35    WinWalker.vim
"              > 0     3     2     77    116   WinWalker.txt
"<
"		{k}ey remap						*WW_key_remap*
"
"			Remap single keys.  No nesting.
"
"		{d}ropout keys					*WW_Dropout_keys*
"
"			Change keys in the list which causes the main menu to exit
"			after execution.  This is most useful for when |WW_timeoutlen|
"			is set to 0.  See |WW_Customizing|
"
"		{ma}cro keys      				*WW_Macro_keys*
"			
"			Use this to create single key mappings to key sequence macros
"			(like the Vim 'normal' command') that are given to:
">
"				call feedkeys( sequence, 't' )
"<
"			At the moment, if you want to enter a special/control char, use
"			^V or ^Q to escape it.
"
"			Examples :
">
"				1		vemyfile^Mc
"						Bring up your favorite file in a new window:
"							vertical new, edit 'myfile', resize to preset
"
"				2		/WinWalker^M^C
"						Jump to your favorite window from anywhere.  In this
"						example 'W' matches 'WinWalker' since it's the only
"						buffer starting with 'w' in my current buffer list,
"						and 'inWalker' is discarded (^M,^C,<ESC>,' '
"						delimit the discard end).  ^C quits (or ' ', etc.)
"						and leaves the 'WinWalker' window/buffer open.
"
"				<F2>	Nsrmysession^M 
"						Bring up your favorite screen/tab layout:
"						new tab, save menu, restore 'mysession'
"
"
"
"		Note: the g:WinWalker_opts.rc_file is updated when the Misc menu is
"		exited.
"
"}}}
"
"
"	Tab Menu:                                         *WW_Tab_Menu*   {{{~
"
"				Note:  The prev/next commands for tabs are down in a
"				submenu because I found that, in general, it's more
"				consistent to move through tabs using the window left/right
"				when wrap=tabs.
"
"		{h}/{l}
"				Go to prev/next tab.
"
"		{H}/{L}
"				Move/rotate tab.  Changes the order of the tab in the tab
"				line list.
"
"		{n}ew tab
"
"				See :help tabpage  ;-)
"
"		{t}ab table
"				
"				Run  |:tabs|
"
"		{q}/{C-Q>=:tabclose/:tabclose!
"
" }}}
"
"
"	Save Menu:                                         *WW_Save_Menu*   {{{~
"
"		Save {d}irectory
"				
"				Directory to hold the Session files.  Default is |$HOME|.
"
"		{s}ave session
"				
"				Save the current session with 'tabpages' session option to the
"				file prompted for, using the above directory as the starting
"				point.  Any filename can be used, but the prefix 'Session.' is
"				used as a default to help filter the completion list more
"				quickly.  If the completion key (^D or <TAB>) is pressed when
"				only a single file can be completed, a summary is show, per
"				the {l}ist sessions command format, below.
"
"		{t}ab save session
"
"				Same as above, but without the 'tabpages' session option, so
"				saves only the current tab.
"
"		{r}estore session
"
"				Restore a session file, with the usual defaults and prefixes.
"
"		{l}ist sessions
"			
"				Lists the Session.* files in the current and default
"				directories.  It shows a summary of the buffers, and their tabs,
"				stored in the session files.
"
" }}}
"
"
"	Customizing:                                     *WW_Customizing* {{{~
"
"		-	g:WinWalker_opts.rc_file  sets the filename for the options
"			settings.  Default is  $HOME/.WInWalker.vim .  Most of the
"			global option variables are handled here, so they don't need to
"			go into your .vimrc.  The  g:WinWalker_opts.rc_file  file is
"			sourced once only, when the plugin is first read when Vim
"			starts up.
"		
"		-	Highlighting overrides can be done by copying the  s:Hi_init()
"			function into your  .vimrc , and making changes there. 
"
"		-	You can set certain keys to drop out of (bypass) the menu
"			automatically and return to Vim Normal mode with this var:
">				
"				let g:WinWalker_dropout_keys = [ "\<C-W>", "\<C-P>" ]
"<
"
"
"			This is most useful for when timeoutlen is set to 0.  For example:
"
"			With timeoutlen > 0, typing ^W^W fast from Vim Normal mode will
"			move to the next window without starting the menu.
"
"			With timeoutlen = 0, the first  ^W  will start the menu, and the
"			second  ^W  will move to the next window, and then leave you back
"			in the menu, regardless of how fast you type them.
"
"			With  ^W  set in the dropout keys list,  ^W^W  from Vim Normal
"			mode will behave the same regardless of timeoutlen.  If you set
"			it up this way, the following are handy for moving around
"			outside the menu mode:
">
"				nnoremap <silent> <tab> :call Tab_jump_or_win_move()<CR>
"				nmap <silent> <S-Tab> <C-W><C-P>
"				nnoremap <silent> <c-tab> :tabnext<CR>
"				nnoremap <silent> <s-c-tab> :tabprev<CR>
"<
"
"		-	Single keys can be remapped inside WinWalker via the
"			g:WinWalker_remap  dict var.  You can put it into your .vimrc,
"			but it probably easier to use it through the |WW_Misc_Menu|
"	
" }}}
"
" End Commands fold }}}
"
"
"
"{{{	*WW_Notes_Other_Bugs*
"
"
"	Notes:     *WW_Notes* ~
"
"	-	Since this uses Vim7 tabs extensively, see  TabLineSet.vim  for
"		better visual information in the tabs themselves.
"
"	-	Don't map a key that also has other operator-pending maps, i.e.
"		trying to map  ^W  while there were still other mappings like
"		custom  ^W^O  will cause it to wait (forever, possibly) for the
"		second key, which keeps the main menu from starting.
"
"	-	Removing a tab, or re-arranging windows can invalidate parts of the
"		jumplist, which will become somewhat unpredictable until it clears
"		all the bad jumps as they are encountered.
"
"
"	Other:     *WW_Other*  ~
"
"		-	Make some test tabs:
">
"			map ,t <c-w>Nnvnvnv
"
"			map ,tt <C-W>N:Tlist<CR>hn:Explore<CR><CR>:copen<CR>H:help WinWalker<CR>Lnesomefile<CR>=c<SPACE>asome stuff<ESC>
"
"				<C-W>				calls WinWalkerMenu function
"				N					opens a new tab
"				:Tlist<CR>			opens Taglist using the  :  pass-through
"				h					move left one window (return into Taglist)
"				n					open a new window horizontally
"				:Explore<CR><CR>	Run Explore in the new window, don't
"									know why it needs two <CR>s
"				:copen<CR>			open the quickfix window
"				H					push it left in line vertically with
"									Tlist and Explore
"				:help WinWalker<CR>	open a help window
"				L					push it right
"				n					open a new window below the help
"				etmpfile<CR>		open 'tmpfile' in it using 'e' quick
"									edit
"				=					equalize all the windows
"				c					resize 'tmpfile' window to preset
"				<SPACE>				exit WinWalker
"				asome stuff<ESC>	put some stuff into 'tmpfile'
"<
"
"		-	Mappings to resize with shift-arrows from Normal mode, using the
"			pass-through of the default  wincmd  resizing commands:
">
"			nmap <c-up> <c-w>+
"			nmap <c-down> <c-w>-
"			nmap <c-left> <c-w><
"			nmap <c-right> <c-w>>
"<
"			Mappings which use the modified resize commands:
">
"			nmap <c-up> <c-w>g<c-k>
"			nmap <c-down> <c-w>g<c-j>
"			nmap <c-left> <c-w>g<c-h>
"			nmap <c-right> <c-w>g<c-l>
"<
"			If timeoutlen is 0, these maps will have to explicitly exit the
"			menu to be used from Vim Normal mode directly.
"
">
"			nmap <expr> <s-up> ( g:WinWalker_opts.timeoutlen ? '<c-w>gK' : '<c-w>gK<space>' )
"			nmap <expr> <s-down> ( g:WinWalker_opts.timeoutlen ? '<c-w>gJ' : '<c-w>gJ<space>' )
"			nmap <expr> <s-left> ( g:WinWalker_opts.timeoutlen ? '<c-w>gH' : '<c-w>gH<space>' )
"			nmap <expr> <s-right> ( g:WinWalker_opts.timeoutlen ? '<c-w>gL' : '<c-w>gL<space>' )
"
"			nmap <expr> <c-up> ( g:WinWalker_opts.timeoutlen ? '<c-w>g<c-k>' : '<c-w>g<c-k><space>' )
"			nmap <expr> <c-down> ( g:WinWalker_opts.timeoutlen ? '<c-w>g<c-j>' : '<c-w>g<c-j><space>' )
"			nmap <expr> <c-left> ( g:WinWalker_opts.timeoutlen ? '<c-w>g<c-h>' : '<c-w>g<c-h><space>' )
"			nmap <expr> <c-right> ( g:WinWalker_opts.timeoutlen ? '<c-w>g<c-l>' : '<c-w>g<c-l><space>' )
"<
"
"	-	Some apps like 'Taglist' and 'quickfix window' set 'bufhidden',
"		which interferes with window moving/exchanging.  This is mostly
"		dealt with, but other app.s like 'Project.vim' reset the window
"		every time the buffer is entered, which isn't handled (probably
"		can't be without changing the application itself).
"
"
"	Caveats:  *WW_Caveats*  ~
"		
"		Here will be listed things to keep in mind if problems should ever
"		crop up.
"
"	-	The tag jump keys are remapped for Help file types to go through the
"		WinWalker_help_tag_trap().  This should in general be an improvement,
"		but side any effects aren't known yet as of v1.3.
"
"
"	Bugs Etc:  *WW_Bugs*  ~
"
"	-	The cmdheight isn't always restored correctly at this time.  As of
"		Vim70c, a bug exists for setting/resetting the value in different
"		tabs.  It seems to be better with 70d, but not all cmdheight issues
"		are resolved.
"
"	-	New windows are inheriting set_fixed_lines/cols.
"
"	-	As of Vim70d, there is a problem with using <expr>, so this form:
"
"			nnoremap <expr> <c-w> WinWalkerMenu()
"
"		does not allow subsequent mappings like:
"
"			nmap <s-up> <c-w>g<c-k>
"
"		use instead,
"
"			nnoremap <silent> <c-w> :call WinWalkerMenu()<CR>
"	
"
"}}} End Notes fold
"
"
"vim7:ft=help:norl:ts=4:sw=4
" Help End:
"
"
"	To Do: {{{
"
"	-	Trapping enough events with eventignore?
"
"	-	Starting Taglist from the ':' passthrough freaks out with 'ml_get'
"		errors.
"
"	-	This used to work, but it broke, probably when I changed <SPACE> in
"		unhuman mode to start the menu.
"
"			map ,t <c-w>tn<space>nvnvnv
"		
"		however, this doesn't work either, sooo
"
"			map ,t <c-w>tn<ESC>nvnvnv
"
"	-	Fix Tab_menu for unhuman input
"
" }}}
"}}}




" ----------------------------------------------------------------------
" Globals, config opts                                               {{{



if v:version < 700
	echomsg "WinWalker requires Vim 7"
	finish
endif

let s:version = '2.2'		" must be a string for searches to work


nnoremap <silent> <leader>w :call WinWalkerMenu()<CR>



" --------------------
" These will be overriden by the _rc file:
let g:WinWalker_opts = {}

let g:WinWalker_opts.rc_file = $HOME . '/.WinWalker_rc.vim'
if exists('g:WinWalker_rc')		" backward compatibiity
	let g:WinWalker_opts.rc_file = g:WinWalker_rc
endif

let g:WinWalker_opts.sess_dir		= $HOME
let g:WinWalker_opts.timeoutlen		= 600		" in millisec
let g:CMu_timeoutlen				= 1000		" menu multi-key timeout
let g:WinWalker_opts.wrap			= 'tabs'	" tabs, win, none
let g:WinWalker_opts.menu			= 'FULL'	" FULL, SHORT
let g:WinWalker_opts.hi_curr		= 'ON'		" ON, OFF
let g:WinWalker_opts.empty			= 'LABEL'	" LABEL, NOLABEL
let g:WinWalker_opts.preset_lines	= 15
let g:WinWalker_opts.preset_cols	= 80






let g:WinWalker_remap = {}
if !exists( 'g:WinWalker_macros' )
	let g:WinWalker_macros = {
			\ "\<C-S>" : "stquicksave\<CR> ",
			\ "\<C-R>" : "srquicksave\<CR> ",
			\ }
endif


let g:WinWalker_dropout_keys = [ "\<C-W>", "\<C-P>" ]
"let g:WinWalker_dropout_keys = [ "\<C-W>", "\<C-P>"
"			\ , "\<S-UP>",  "\<S-DOWN>",  "\<S-LEFT>",  "\<S-RIGHT>" ]

" --------------------


" Put a copy of this function in your .vimrc to override the highlighting:
"
function! s:Hi_init()
	hi clear WinWalker_sel
	if g:WinWalker_opts.hi_curr ==? 'ON'
		"hi! WinWalker_sel guibg=DarkGrey ctermfg=DarkGrey
		hi! WinWalker_sel gui=underline cterm=underline
		"hi! WinWalker_sel gui=reverse cterm=reverse
	endif

	" Char_menu settings:
	let g:CMu_menu_hl_text		= 'Directory'
	let g:CMu_menu_hl_standout	= 'WarningMsg'
	let g:CMu_menu_hl_selection	= 'WildMenu'
	let g:CMu_menu_hl_error		= 'Error'
endfunction






let s:Arrow_key_trans = {
			\ "\<UP>"		:	'k',
			\ "\<DOWN>"		:	'j',
			\ "\<LEFT>"		:	'h',
			\ "\<RIGHT>"	:	'l',
			\ "\<S-UP>"		:	'K',
			\ "\<S-DOWN>"	:	'J',
			\ "\<S-LEFT>"	:	'H',
			\ "\<S-RIGHT>"	:	'L',
			\ "\<C-UP>"		:	"\<C-K>",
			\ "\<C-DOWN>"	:	"\<C-J>",
			\ "\<C-LEFT>"	:	"\<C-H>",
			\ "\<C-RIGHT>"	:	"\<C-L>",
			\ "\<M-UP>"		:	'k',
			\ "\<M-DOWN>"	:	'j',
			\ "\<M-LEFT>"	:	'h',
			\ "\<M-RIGHT>"	:	'l'
			\ }



let s:This_script_fname = expand("<sfile>:p")
" fnamemodify( ... ,":t")


" End Globals, config opts                                           }}}
" ----------------------------------------------------------------------




" ----------------------------------------------------------------------
" WinWalkerMenu main func                                           {{{

let s:Win_orient = {}

"let s:empty_label = "{__E__M__P__T__Y__}"
let s:empty_label = "{  W i n W a l k e r -- E M P T Y  }"

let s:last_start_time = 0
let s:prev_winnrs = [ winnr() ]
let s:Fast_pass = 0
let s:errmsg = ''
let s:infomsg = ''
let s:Unhuman_input = 0
let s:buf_check = 'loaded'

function! WinWalkerMenu( ... )
	" This must be here for getchar(1) to wake up, for 70c only?:
	"exe "normal! :   <CR>"

	call s:Hi_init()

	let v:errmsg = ''

	" Reset grow mode upon entry so macros will work, i.e.  <c-w>gK
	let s:grow_mode = 0

	let l:inp = ''
	let l:last_inp = ''
	let s:last_inp_time = localtime()
	let ms_sleep = 0
	let test = 0
	let s:save_cmdheight = &cmdheight
	let s:save_switchbuf = &switchbuf
	let &switchbuf = ''
	" Perhaps should start using 'switchbuf=usetab', but I'm not sure of the
	" side effects on other areas.
	let s:Unhuman_input = 0

	set eventignore-=BufEnter
	set eventignore-=BufLeave
	set eventignore-=WinEnter
	set eventignore-=WinLeave

	let save_lazyredraw = &lazyredraw
	set lazyredraw


	while 1

		" As an alternative to creating lots of 'old_bufnr' type vars 
		let s:this_bufnr = bufnr("%")

		if s:prev_winnrs[ -1 ] != winnr()
			call add( s:prev_winnrs, winnr() )
		endif
		while len( s:prev_winnrs ) > 5
			call remove( s:prev_winnrs, 0 )
		endwhile
		"echomsg 's:prev_winnrs ' . string( s:prev_winnrs )

		if !exists( 'Menu_started' )
			if s:Unhuman_input
				" If we've already seen a Unhuman_input event, then subsequent
				" chars must be coming from mappings,etc. instead of a user,
				" so turn the timeout down, so we won't see a timeout after
				" getting the last char that is part of the key map being
				" deliverd.
				" Note:  it actually requires a timeout of 100, even though
				" it's supposed to be delivered internally.  Maybe it's
				" emulating a user by adding a delay?
				let s:Unhuman_input = Peek_char_timeout_wait( 100 )
				if !s:Unhuman_input  | return | endif
			else
				" The first check for Unhuman_input is done with the longer
				" g:WinWalker_opts.timeoutlen  variable.  Subsequent passes use
				" shorter, as above.
				let s:Unhuman_input = Peek_char_timeout_wait( g:WinWalker_opts.timeoutlen )
			endif
		else
			let s:Unhuman_input = 0
		endif

		if s:Unhuman_input
			let l:inp = getchar()
			if nr2char( l:inp ) != "" | let l:inp = nr2char( l:inp ) | endif
		else

			let Menu_started = 1

			let l = []

			if s:errmsg != ''
				call Char_menu_wrapAdd( l, '%#Error#' . s:errmsg . '%##' )
				let s:errmsg = ''
			endif
			"let s:infomsg .= ' ign(' . &eventignore . ')'
			if s:infomsg != ''
				call Char_menu_wrapAdd( l, '%#Special#' . s:infomsg . '%##' )
				let s:infomsg = ''
			endif

			if g:WinWalker_opts.menu ==? 'FULL'

				call Char_menu_wrapAdd( l, "{?}help" )
				call Char_menu_wrapAdd( l, "{w}/{W}/{<C-W>}indow=>Nxt/Prv" )
				call Char_menu_wrapAdd( l, ( s:grow_mode 
							\ ? '(Move/%#DiffChange#grow-one/grow-more%##)==>'
							\ : "(jump/push/exchange)==>" ) )
				call Char_menu_wrapAdd( l, "{k}/{K}/{<C-K>}=up" )
				call Char_menu_wrapAdd( l, "{j}/{J}/{<C-J>}=down" )
				call Char_menu_wrapAdd( l, "{h}/{H}/{<C-H>}=left" )
				call Char_menu_wrapAdd( l, "{l}/{L}/{<C-L>}=right" )
				call Char_menu_wrapAdd( l, "{r}/{R}=rotate wins/buffers" )
				call Char_menu_wrapAdd( l, "{o}/{O}='only'/clone-in-tab" )
				call Char_menu_wrapAdd( l, "{<C-I>/{<C-O>}=jump forward/back" )
				call Char_menu_wrapAdd( l, "{=}equalize" )

				"call Char_menu_wrapAdd( l, "lines/{c}olumns preset("
				call Char_menu_wrapAdd( l, "{c}=>preset("
							\ . "%#Tag#" . g:WinWalker_opts.preset_lines . "%##"
							\ . "," 
							\ . "%#Tag#" . g:WinWalker_opts.preset_cols . "%##"
							\ . ")" )

				call Char_menu_wrapAdd( l, "{a}ttach/constrain menu" )
				call Char_menu_wrapAdd( l, "{t}ab menu" )
				call Char_menu_wrapAdd( l, "{s}=>save/restore menu" )
				call Char_menu_wrapAdd( l, "[ {m}isc menu--> ", ", " )
				call Char_menu_wrapAdd( l, "{g}row is " 
							\ . ( s:grow_mode ? '%#DiffChange#ON%##' : 'OFF' ) 
							\ , "" )
				call Char_menu_wrapAdd( l, "wrap is " 
							\ . ( g:WinWalker_opts.wrap ==? 'none' 
							\ ? 'OFF' : '%#DiffChange#' . g:WinWalker_opts.wrap . '%##' ) )
				call Char_menu_wrapAdd( l, " ]", "" ) 

				call Char_menu_wrapAdd( l, "{p}/{<C-P>}rev" )
				call Char_menu_wrapAdd( l, "{n}/{N}ew=>win/tab" )
				call Char_menu_wrapAdd( l, "{v}ert new" )
				call Char_menu_wrapAdd( l, "quick {e}dit" )
				call Char_menu_wrapAdd( l, "{:}ex" )
				call Char_menu_wrapAdd( l, "{/}find win" )
				call Char_menu_wrapAdd( l, s:Buffer_status_string() )

				call Char_menu_wrapAdd( l, "send :{q}uit/:{Q}uit!/{<C-Q>}=tabclose!/{Z}Z" )

			endif " if g:WinWalker_opts.menu ==? 'FULL'


			if g:WinWalker_opts.menu ==? 'FULL'
				call Char_menu_wrapAdd( l, "{<SPACE>}/{<ESC>}=>Exit:  " )
			else
				"call Char_menu_wrapAdd( l, "WinWalker {<ESC>}=>Exit:  " )
				" Need to give Char_menu() at least one {} field for it to work
				" properly.
				"call Char_menu_wrapAdd( l, "WinWalker Main, {m}=>menu opts, {<SPACE>}=>Exit:  " )
				call Char_menu_wrapAdd( l, 'WinWalker' 
							\ . ( s:grow_mode ? ' %#DiffChange#grow mode%##' : '' ) 
							\ . s:Buffer_status_string()
							\ . '{ }: ' )
			endif

			" put this here so if it won't draw the menu if the user is
			" typing too fast.
			let s:Fast_pass = ( localtime() - s:last_start_time ) < 1
			let s:last_start_time = localtime()

			if ms_sleep > 300
				let s:Fast_pass = 0
			endif

			if getchar(1)
				let ms_sleep = 0
			else
				let ms_sleep += 100
				sleep 100ms
			endif



			"if s:Fast_pass && getchar(1)
			if getchar(1)
				let l:inp = getchar()
				if nr2char( l:inp ) != ""
					let l:inp = nr2char( l:inp )
				endif
			else
				match WinWalker_sel /./
				call s:Empty_fill_fake()
				call s:Cause_window_update()
				redraw

				let l:inp = Char_menu( join( l, '' )
							\ , "{<ESC>} {<C-C>} {<DEL>} {<BS>} {;}"
							\ . "{<UP>} {<DOWN>} {<LEFT>} {<RIGHT}} "
							\ . "{<S-UP>} {<S-DOWN>} {<S-LEFT>} {<S-RIGHT}} "
							\ . "{<C-UP>} {<S-DOWN>} {<C-LEFT>} {<C-RIGHT}} "
							\ . "{<M-UP>} {<M-DOWN>} {<M-LEFT>} {<M-RIGHT}} "
							\ , l:last_inp )

				match
				call s:Empty_empty()

			endif " end Fast_pass check


		endif	" end Unhuman_input check


		let l:last_inp = l:inp

		if exists( 's:Arrow_key_trans[ l:inp ]' )
			let l:inp = s:Arrow_key_trans[ l:inp ]
		endif

		if exists( 'g:WinWalker_remap[ l:inp ]' )
			let l:inp = g:WinWalker_remap[ l:inp ]
		endif

		" ------------------------------------------------------------
		"  Begin command execution:
		"


		if has_key( g:WinWalker_macros, l:inp )
			"echomsg 'key ' . l:inp . ' is ' . g:WinWalker_macros[ l:inp ]
			call feedkeys( g:WinWalker_macros[ l:inp ], 't' )

		elseif l:inp == '?' || l:inp == "\<F1>"
			"
			" ----------------------------------------
			" Help
			"
			help WinWalker
			" Probably should do some cleanup ops before exiting:
			call Clear_cmd_window()
			return


			" ----------------------------------------
			" Quick :ex command passthrough {{{
			"
		elseif l:inp =~ '[e;:]'

			"			call feedkeys( ':', 't' )
			"
			"			while 1
			"				let inp = getchar()
			"				if inp == "\<ESC>" | break | endif
			"				call feedkeys( inp, 't' )
			"			endwhile
			"
			"			continue
			"
			"			cnoremap <silent> <buffer> <expr> <c-f> s:Hist_to_inp() . "\<c-f>"
			"			function! s:Hist_to_inp()
			"				for i in range( -5, -1 )
			"					let s = histget( 'cmd', i )
			"					call histadd( 'input', s )
			"				endfor
			"				return ''
			"			endfunction

			let fake_prompt = '->:'
			let pre_enter = ''
			let compete_opt = ''

			if l:inp == 'e'
				let pre_enter = 'e '
				let compete_opt = 'file'
			elseif l:inp =~ '[;:]'
				"let compete_opt = 'command'
				let compete_opt = 'command'
			endif

			" compete_opt only works for the line as a whole, not
			" individual words.   Crap.
			"
			" Adding a default text will cripple the compete_opt, because
			" the default text is considered to be part of any compete_opt
			" the user attemps.
			"
			try
				let g:redir_tmp = ''
				let ex_input = input( fake_prompt . pre_enter, "", compete_opt )
				if ex_input != ''
					redir => g:redir_tmp
					exe pre_enter . ex_input
					redir END
				endif
				call histadd( ':', pre_enter . ex_input )
			catch
				" getting wrong v:errmsg for failures to execute ex_inpt
				let s:errmsg .= 'Caught: ' . v:errmsg 
							\ . ', exception:' . v:exception
			finally
				if strlen( g:redir_tmp ) > 70
					call getchar()
				else
					let g:redir_tmp = strtrans( g:redir_tmp )
					let s:infomsg .= '['.substitute( g:redir_tmp,
								\ '<\d\d>', ' ', 'g' ) . ']'
				endif
				"cunmap <buffer> <c-f>
				if l:inp == 'e'
					call WinWalker_WinEnter()
				endif
			endtry

			" Completion behavior				*:command" -completion*
			" -complete=augroup	autocmd groups
			" -complete=buffer	buffer names
			" -complete=command	Ex command (and arguments)
			" -complete=dir		directory names
			" -complete=environment	environment variable names
			" -complete=event		autocommand events
			" -complete=expression	Vim expression
			" -complete=file		file and directory names
			" -complete=shellcmd	Shell command
			" -complete=function	function name
			" -complete=help		help subjects
			" -complete=highlight	highlight groups
			" -complete=mapping	mapping name
			" -complete=menu		menus
			" -complete=option	options
			" -complete=tag		tags
			" -complete=tag_listfiles	tags, file names are shown when CTRL-D is hit
			" -complete=var		user variables
			" -complete=custom,{func} custom completion, defined via {func}
			" -complete=customlist,{func} custom completion, defined via {func}
			"
			"  }}}



			" ----------------------------------------
			"  Other menus,  Misc, Tab, Save {{{
			"
		elseif l:inp ==# 'a'
			call s:Constraints_menu()

		elseif l:inp ==# 'm'
			call s:Misc_menu()


		elseif l:inp ==# 't'
			call s:Tab_menu()


		elseif l:inp ==# 's'
			call s:Save_menu()

			"  End Other menus,  Misc, Tab, Save }}}
			" ----------------------------------------


			" ----------------------------------------
			" Find/jump to a window {{{
			"
		elseif l:inp =~# '[/]'

			let matched = {}
			let matched = s:Pick_buf( )
			let found_wins = []
			let listnr = 0
			if len( matched ) > 0

				for t in range( 1, tabpagenr("$") )
					for b in tabpagebuflist( t )
						if b == matched.bufnr
							let wininfo = {}
							let wininfo.tabnr = t
							" not in correct window for bufwinnr() to work:
							"let wininfo.winnr = bufwinnr( b )
							let wininfo.bufnr = b
							let wininfo.bufname = matched.bufname
							"let wininfo.listnr = listnr
							call add( found_wins, wininfo )
							let listnr += 1
						endif
					endfor
				endfor

				if len( found_wins ) == 0
					if s:buf_check ==? 'loaded'
						let s:infomsg .= ' No window found for selection. '
						continue
					else
						let s:infomsg .= ' No window found, loading '
									\ . matched.bufname
									\ . ', b#' . matched.bufnr . ' in current. '
						exe 'buf '. matched.bufnr
						continue
					endif
				endif

				let picked = found_wins[0]

				if len( found_wins ) > 1
					let l = []
					let i = 1
					for elem in found_wins
						call Char_menu_wrapAdd( l, '{'. i .'}'
									\ . ' = ( '
									\ . fnamemodify( elem.bufname, ':t' ) . ' = b#' . b
									\ . '(tab ' . elem.tabnr
									\ . ' )' )
						"\ . ',win ' . elem['winnr'] 
						let i += 1
					endfor

					if s:Unhuman_input
						let inp = s:Recheck_unhuman()
						if inp == 0 | return | endif
					else
						let inp = Char_menu( join( l, '' )
									\ , "{<ESC>} {<C-C>} {<DEL>} {;}"
									\ , last_inp )
						let last_inp = inp

						if exists( 'found_wins[ inp - 1 ]' )
							let picked = found_wins[ inp - 1 ]
						else
							let s:errmsg .= ' Invalid selection.'
							continue
						endif
					endif

				endif	" if len( found_wins ) > 1

				let s:infomsg .= ' Selected (' . picked.bufname . ')'
				let s:Keep_jumps = 1
				exe 'tabnext ' . picked.tabnr
				let s:Keep_jumps = 0
				exe '' . bufwinnr( picked.bufnr ) . ' wincmd w'

			else
				" Redundant with message in s:Pick_buf()
				"let s:errmsg .= ' No match found. '
			endif

			" End:  Find/jump to a window
			" ---------------------------------------- }}}



			" ----------------------------------------
			"  Only and tabnew clone {{{
			"
		elseif l:inp ==# 'o'
			"
			" ----------------------------------------
			" 'only' window command
			"
			echohl StatusLine
			let yesno = input( "Close all other windows? " )
			if yesno =~? '^y'
				wincmd o
			endif


		elseif l:inp ==# 'O'
			"
			" ----------------------------------------
			" Clone window in new tab
			"
			tabnew
			exe 'buf ' . s:this_bufnr

			" Set the new window to be deleted if the next window 
			" motion is ^O
			let s:O_temp_win = {}
			let s:O_temp_win.tabnr = tabpagenr()
			let s:O_temp_win.winnr = winnr()

			"}}}



			" ----------------------------------------
			" Jump commands:  ^I ^O   {{{
			"
		elseif l:inp ==# "\<C-I>"
			call s:Jump_list_forward()


		elseif l:inp ==# "\<C-O>"
			if exists( 's:O_temp_win' )
				if			s:O_temp_win.tabnr == tabpagenr()
							\ &&	s:O_temp_win.winnr == winnr()
							\ && !&modified
					hide
				endif
				" 'hide' will trigger 'BufLeave' which will unlet it:
				"unlet s:O_temp_win
			endif
			call s:Jump_list_backward()

			" End jump commands
			" ---------------------------------------- }}}


			" ----------------------------------------
			" Grow or shrink {{{
			"
		elseif l:inp ==# 'g'
			"
			" ----------------------------------------
			"  Grow toggle, see also Misc_menu
			if s:grow_mode 
				let s:grow_mode = 0 
			else 
				let s:grow_mode = 1 
			endif

		elseif s:grow_mode && ( l:inp =~# '[JKHL]'
					\ || l:inp =~ "\\(\<C-J>\\|\<C-K>\\|\<C-H>\\|\<C-L>\\)" )
			let dir_keymap = { 
						\ 'K' : 'up', 'J' : 'down', 'H' : 'left', 'L' : 'right'
						\ , "\<C-K>" : 'up', "\<C-J>" : 'down'
						\ , "\<C-H>" : 'left', "\<C-L>" : 'right'
						\ }
			let dir = dir_keymap[ l:inp ]

			let grow_or_shrink = GetWinAdjacent( dir ) > 0 ? '+' : '-'

			if  dir =~?  '\(up\|down\)'
				if GetWinAdjacent( 'up' ) > 0 && GetWinAdjacent( 'down' ) > 0
							\ && dir ==? 'down'
					let grow_or_shrink = '-'
				endif
			else
				if GetWinAdjacent( 'left' ) > 0 && GetWinAdjacent( 'right' ) > 0
							\ && dir ==? 'left'
					let grow_or_shrink = '-'
				endif
			endif

			if  dir =~?  '\(up\|down\)'
				let do_vert = ''
				let incr = 5
			else
				let do_vert = 'vert ' " non-intuitive
				let incr = 15
			endif

			if l:inp =~# '[JKHL]'
				let incr = 1
			else
				" Default to the control keys
			endif

			exe do_vert . 'resize ' . grow_or_shrink . incr

			if  dir =~?  '\(up\|down\)'
				if exists('b:set_fixed_lines')
					let b:set_fixed_lines = winheight(winnr())
				endif
				if exists('w:set_fixed_lines')
					let w:set_fixed_lines = winheight(winnr())
				endif
			else
				if exists('b:set_fixed_cols')
					let b:set_fixed_cols = winwidth(winnr())
				endif
				if exists('w:set_fixed_cols')
					let w:set_fixed_cols = winwidth(winnr())
				endif
			endif

			" End:  Grow or shrink
			" ----------------------------------------}}}


			" ----------------------------------------
			" WinWalker_push_window( ) {{{
			"
		elseif l:inp =~# '[JKHL]'


			call WinWalker_push_window( l:inp )


			" End:  Window push commands
			" End: WinWalker_push_window( )
			" ----------------------------------------}}}


			" ----------------------------------------
			"  Rotate window {{{
			"
		elseif l:inp ==# 'r'

			if &modified
				let s:errmsg = "Cannot rotate modified buffer."
				continue
			endif

			let save_bufnr = s:this_bufnr
			let save_bufhidden = getbufvar( save_bufnr, '&bufhidden' )
			call setbufvar( save_bufnr, '&bufhidden', 'hide' )

			let tabwin = tabpagenr() . ',' . winnr()
			if !exists( 's:Win_orient[ tabwin ]' ) 
						\ || s:Win_orient[ tabwin ] ==? "horizontal"
				" If wrong, will correct with another key press
				quit
				call s:New_window( 'vnew' )
			else
				quit
				call s:New_window( 'new' )
			endif

			exe 'buf ' . s:this_bufnr
			redraw

			wincmd =

			call setbufvar( save_bufnr, '&bufhidden', save_bufhidden )
			let s:Need_window_update = 1

			"  End: Rotate window
			" ---------------------------------------- }}}


			" ----------------------------------------
			"  Rotate view  {{{
			"
		elseif l:inp ==# 'R'

			let w_restore = winnr()
			let w = 1
			while w <= winnr("$")
				exe 'silent! ' . w . 'wincmd w'
				if &modified
					let s:errmsg = "Cannot rotate, some buffer is modified."
					break
				endif
				let w += 1
			endwhile
			if w > winnr("$")
				let w = 1
				let b_first = winbufnr( 1 )
				1wincmd w
				while w < winnr("$")
					let w_next = w + 1
					let b_next = winbufnr( w_next )
					exe 'silent! buf ' . b_next
					wincmd w
					let w += 1
				endwhile
				exe 'silent! buf ' . b_first
				exe "silent!" . w_restore . 'wincmd w'
			endif
			" End:  Rotate view
			" ---------------------------------------- }}}


			" ----------------------------------------
			"  Rotate tab {{{
			"
		elseif l:inp == "\<C-R>"
			let pg = tabpagenr()
			if pg == tabpagenr("$")
				let new_pg = 0
			else
				let new_pg = pg
			endif
			exe 'tabmove ' . new_pg
			"}}}



			" ----------------------------------------
			" Misc commands that don't require switched window highlighting: {{{
			"
		elseif l:inp =~# '[=x+<>-]'
			exe "silent! wincmd " . l:inp
			if l:inp == '='
				let s:Need_window_update = 1
			endif
			"}}}



			" ----------------------------------------
			" [jkhlNtTnvwWtbp]    \\(\<C-W>\\|\<C-P>\\) {{{
			" Misc ommands that require switched window highlighting:
			"
		elseif   l:inp =~# '[jkhlNtTnvwWtbp]'
					\ || l:inp =~# "\\(\<C-W>\\|\<C-P>\\)"


			if l:inp ==# 't'
				tabnext
			elseif l:inp ==# 'v'
				call s:New_window( 'vnew' )
			elseif l:inp ==# 'T'
				tabprev
			elseif l:inp ==# 'N'
				tabnew
			elseif l:inp ==# 'k'
				if GetWinAdjacent( "up" )   > 0 
					silent wincmd k 
				elseif g:WinWalker_opts.wrap !=? 'none'
					let s:Keep_jumps = 1
					silent 99 wincmd j 
					let s:Keep_jumps = 0
					call WinWalker_BufEnter()
				endif
			elseif l:inp ==# 'j'
				if GetWinAdjacent( "down" ) > 0 
					silent wincmd j 
				elseif g:WinWalker_opts.wrap !=? 'none'
					let s:Keep_jumps = 1
					silent 99 wincmd k 
					let s:Keep_jumps = 0
					call WinWalker_BufEnter()
				endif
			elseif l:inp ==# 'h'
				if GetWinAdjacent( "left" ) > 0 
					silent wincmd h 
				else
					call WinWalker_BufLeave()
					let s:Keep_jumps = 1
					if g:WinWalker_opts.wrap ==? 'win'
						99wincmd l
					elseif g:WinWalker_opts.wrap ==? 'tabs'
						tabprev
						99wincmd l
					endif
					let s:Keep_jumps = 0
					call WinWalker_BufEnter()
				endif
			elseif l:inp ==# 'l'
				if GetWinAdjacent( "right" ) > 0 
					silent! wincmd l 
				else
					call WinWalker_BufLeave()
					let s:Keep_jumps = 1
					if g:WinWalker_opts.wrap ==? 'win'
						99wincmd h
					elseif g:WinWalker_opts.wrap ==? 'tabs'
						tabnext
						99wincmd h
					endif
					let s:Keep_jumps = 0
					call WinWalker_BufEnter()
				endif

			elseif l:inp == "\<C-W>"
				exe "silent! wincmd w"

			elseif l:inp == "\<C-P>" || l:inp ==# 'p'
				let len = len( s:prev_winnrs )
				if len >= 2
					exe 'silent ' . s:prev_winnrs[ len - 2 ] . ' wincmd w '
				else
					exe "silent! wincmd p"
				endif

			else
				exe "silent! wincmd " . l:inp

			endif

			" end: Misc ommands that require switched window highlighting:
			" ----------------------------------------}}}



			" ----------------------------------------
			"  Exchange buffers up/down/left/right {{{
			"
		elseif l:inp =~  "\\(\<C-K>\\|\<C-J>\\|\<C-H>\\|\<C-L>\\)"

			if l:inp ==  "\<C-K>"
				let win1 = winnr()
				let win2 = GetWinAdjacent( "up" )
				if win2 < 1
					silent! wincmd b 
					let win2 = winnr()
				endif
				call s:Exchange_win( win1, win2 ) 
				exe 'silent! ' . win2 . ' wincmd w'

			elseif l:inp ==  "\<C-J>"
				let win1 = winnr()
				let win2 = GetWinAdjacent( "down" )
				if win2 < 1
					silent! wincmd t
					let win2 = winnr()
				endif
				call s:Exchange_win( win1, win2 ) 
				exe 'silent! ' . win2 . ' wincmd w'

			elseif l:inp ==  "\<C-H>"
				let win1 = winnr()
				let win2 = GetWinAdjacent( "left" )
				if win2 < 1
					if g:WinWalker_opts.wrap ==? 'win'
						silent! 99 wincmd l
						let win2 = winnr()
					elseif g:WinWalker_opts.wrap ==? 'tabs'
						if winnr("$") == 1
							call s:New_window( 'new' )
							wincmd p
						endif
						hide
						if tabpagenr("$") == 1
							tabnew
							let where = min( [0, tabpagenr() - 2 ] )
							exe 'tabmove ' . where
						else
							tabprev

							let s:Keep_jumps = 1
							silent 99 wincmd l
							let s:Keep_jumps = 0

							call s:New_window( 'new' )
						endif
						exe 'buf ' . s:this_bufnr
					endif
				else
					call s:Exchange_win( win1, win2 ) 
					exe 'silent! ' . win2 . ' wincmd w'
				endif

			elseif l:inp ==  "\<C-L>"
				let win1 = winnr()
				let win2 = GetWinAdjacent( "right" )
				if win2 < 1
					if g:WinWalker_opts.wrap ==? 'win'
						silent! 99 wincmd h
						let win2 = winnr()
					elseif g:WinWalker_opts.wrap ==? 'tabs'
						if winnr("$") == 1
							call s:New_window( 'new' )
							wincmd p
						endif
						hide
						if tabpagenr("$") == 1
							tabnew
						else
							tabnext

							let s:Keep_jumps = 1
							silent 99 wincmd h
							let s:Keep_jumps = 0

							call s:New_window( 'new' )
						endif
						exe 'silent buf ' . s:this_bufnr
					endif
				else
					call s:Exchange_win( win1, win2 ) 
					exe 'silent! ' . win2 . ' wincmd w'
				endif

			endif
			let s:Need_window_update = 1
			"  End: Exchange buffers up/down/left/right
			" ---------------------------------------- }}}


			" ---------------------------------------- 
			" Do preset      {{{
			"
		elseif l:inp ==# 'c'
			let resiz = 1
			if   exists( 'w:set_fixed_lines' )
			\ && exists( 'w:set_fixed_cols' )
			"\ && winheight( winnr() ) == w:set_fixed_lines
			"\ && winwidth( winnr() ) == w:set_fixed_cols
				unlet! w:set_fixed_lines 
				unlet! w:set_fixed_cols 
				let resiz = 0
			endif

			if   exists( 'b:set_fixed_lines' )
			\ && exists( 'b:set_fixed_cols' )
			"\ && g:WinWalker_opts.preset_lines == b:set_fixed_lines
			"\ && g:WinWalker_opts.preset_cols == b:set_fixed_cols
				unlet! b:set_fixed_lines 
				unlet! b:set_fixed_cols 
				let resiz = 0
			endif

			if resiz
				if	 winheight( winnr() ) < g:WinWalker_opts.preset_lines
				\ || ( exists('w:set_fixed_lines' )
				\      && winheight( winnr() ) < w:set_fixed_lines )
					exe 'silent! resize ' . g:WinWalker_opts.preset_lines
				endif
				let b:set_fixed_lines = g:WinWalker_opts.preset_lines
				let w:set_fixed_lines = g:WinWalker_opts.preset_lines

				exe 'silent! vert resize ' . g:WinWalker_opts.preset_cols
				let b:set_fixed_cols = g:WinWalker_opts.preset_cols
				let w:set_fixed_cols = g:WinWalker_opts.preset_cols
			endif


			"}}}


			" ----------------------------------------
			"  Quit commands {{{
			"

		elseif l:inp ==# 'Z'
			try
				let is_last = ( winnr() == winnr("$") )
				normal! ZZ
				if is_last && bufnr("%") != s:this_bufnr
					call s:Jump_list_backward()
				endif
			catch
				let s:errmsg .= " " . v:errmsg
			endtry

		elseif l:inp ==# 'q'
			try
				let is_last = ( winnr() == winnr("$") )
				if is_last && bufnr("%") != s:this_bufnr
					quit
					call s:Jump_list_backward()
				else
					quit
				endif
			catch
				let s:errmsg .= " " . v:errmsg
			endtry

		elseif l:inp ==# 'Q'
			try
				let is_last = ( winnr() == winnr("$") )
				quit!
				if is_last && bufnr("%") != s:this_bufnr
					call s:Jump_list_backward()
				endif
			catch
				let s:errmsg .= " " . v:errmsg
			endtry

		elseif l:inp ==  "\<C-Q>"
			try
				tabclose!
				call s:Jump_list_backward()
			catch
				let s:errmsg .= " " . v:errmsg
			endtry
			"
			"  Quit commands 
			" ---------------------------------------- }}}



			" ---------------------------------------- 
			" WinWalkerMenu end stuff {{{
			"
		elseif l:inp =~  "^\\( \\|\<C-SPACE>\\|\<CR>\\|\<C-CR>\\)$"

			if s:Unhuman_input
				let Menu_started = 1
				continue
			else
				break
			endif


			"elseif l:inp =~  "[\eq]" || l:inp ==  "\<C-C>"
		elseif l:inp =~ "^\\(\<C-C>\\|\<ESC>\\)$"
			break


		"elseif l:inp =~# "^\\(\<leftmouse>\\)$"
		elseif l:inp =~# "\<leftmouse>"
			exe 'silent normal! ' . l:inp

		else
			"let s:errmsg = 'Invalid cmd (' . l:inp . ')'
			"echomsg 'WinWalker exiting on (' . strtrans( l:inp ) . ')' 
			exe 'silent normal! ' . l:inp

		endif

		if index( g:WinWalker_dropout_keys, l:inp ) > -1
			break
		endif

	endwhile

	if s:Unhuman_input 
	else
		"call WinWalker_clean_empties()
		call Clear_cmd_window()
		echohl Special

		echo "Done"

		echohl None

		let &cmdheight = s:save_cmdheight
		let &switchbuf = s:save_switchbuf
	endif

	call s:Cause_window_update()

	let &lazyredraw = save_lazyredraw

	redraw
	" WinWalkerMenu end stuff }}}


endfunction

" WinWalkerMenu main func                                           }}}
" ----------------------------------------------------------------------



" ----------------------------------------------------------------------
" Misc_menu                                                         {{{

function! s:Misc_menu()

	let l:last_inp = ''

	while 1
		let l = []

		if s:errmsg != ''
			call Char_menu_wrapAdd( l, '%#Error#' . s:errmsg . '%##' )
			let s:errmsg = ''
		endif
		if s:infomsg != ''
			call Char_menu_wrapAdd( l, '%#Special#' . s:infomsg . '%##' )
			let s:infomsg = ''
		endif

		call Char_menu_wrapAdd( l, "{r}c file is (%#Tag#" . g:WinWalker_opts.rc_file . "%##)" )

		call Char_menu_wrapAdd( l, "{g}row is " 
					\ . ( s:grow_mode ? '%#DiffChange#ON%##' : 'OFF' ) )

		call Char_menu_wrapAdd( l, "{w}rap is " 
					\ . '%#DiffChange#' . g:WinWalker_opts.wrap . '%##' )

		call Char_menu_wrapAdd( l, "{menu} is " 
					\ . '%#DiffChange#' . g:WinWalker_opts.menu . '%##' )

		call Char_menu_wrapAdd( l, "{h}ighlight current win " 
					\ . '%#DiffChange#' . g:WinWalker_opts.hi_curr . '%##' )

		call Char_menu_wrapAdd( l, "{e}mpty files == " 
					\ . '%#DiffChange#' . g:WinWalker_opts.empty . '%##' )

		call Char_menu_wrapAdd( l, "{t}imeoutlen (startup) is (%#Tag#" . g:WinWalker_opts.timeoutlen . "ms%##)" )
		call Char_menu_wrapAdd( l, "{mt}imeoutlen (menu) is (%#Tag#" . g:CMu_timeoutlen . "ms%##)" )

		call Char_menu_wrapAdd( l, "{j}ump list"  )
		call Char_menu_wrapAdd( l, "{k}ey remap"  )
		call Char_menu_wrapAdd( l, "{d}ropout keys "  )
		call Char_menu_wrapAdd( l, "{macro} keys "  )
		call Char_menu_wrapAdd( l, "{SPACE}=>main menu :  "  )



		if s:Unhuman_input
			let l:inp = s:Recheck_unhuman()
			if l:inp == 0 | return | endif
		else
			let l:inp = Char_menu( join( l, '' )
						\ , "{<ESC>} {<C-C>} {<DEL>} {;}"
						\ , l:last_inp )
		endif

		let l:last_inp = l:inp




		" --------------------------------------------------
		" Set rc_file
		"
		if l:inp ==# 'r'
			exe 'echohl ' . g:CMu_menu_hl_text
			let rc_file = input( 'Enter file name: ', g:WinWalker_opts.rc_file, 'file' )
			if rc_file != ''
				let g:WinWalker_opts.rc_file = rc_file
			endif

		" --------------------------------------------------
		" Set menu verbose level
		"
		elseif '^menu' =~ l:inp 
			if g:WinWalker_opts.menu ==? 'SHORT' 
				let g:WinWalker_opts.menu = 'FULL'
			else 
				let g:WinWalker_opts.menu = 'SHORT'
			endif



		" --------------------------------------------------
		" Set highlighting
		"
		elseif l:inp ==# 'h'
			if g:WinWalker_opts.hi_curr ==? 'ON' 
				let g:WinWalker_opts.hi_curr = 'OFF'
			else 
				let g:WinWalker_opts.hi_curr = 'ON'
			endif
			call s:Hi_init()

		" --------------------------------------------------
		" Set empty label
		"
		elseif l:inp ==# 'e'
			if g:WinWalker_opts.empty ==? 'LABEL' 
				let g:WinWalker_opts.empty = 'NOLABEL'
			else 
				let g:WinWalker_opts.empty = 'LABEL'
			endif


		" --------------------------------------------------
		" Set startup timeoutlen
		"
		elseif l:inp ==# 't'
			let g:WinWalker_opts.timeoutlen = str2nr( input('Enter startup timeout in millisec: ' ) )


		" --------------------------------------------------
		" Set menu timeoutlen
		"
		elseif l:inp ==# 'mt'
			let g:CMu_timeoutlen = str2nr( input('Enter menu timeout in millisec: ' ) )


		" --------------------------------------------------
		" Set grow
		"
		elseif l:inp ==# 'g'
			if s:grow_mode 
				let s:grow_mode = 0 
			else 
				let s:grow_mode = 1 
			endif

		" --------------------------------------------------
		" Set wrap
		"
		elseif l:inp ==# 'w'
			if g:WinWalker_opts.wrap ==? 'none'
				let g:WinWalker_opts.wrap = 'win' 
			elseif g:WinWalker_opts.wrap ==? 'win'
				let g:WinWalker_opts.wrap = 'tabs' 
			elseif g:WinWalker_opts.wrap ==? 'tabs'
				let g:WinWalker_opts.wrap = 'none' 
			endif

		" --------------------------------------------------
		" Show jump list
		"
		elseif l:inp ==# 'j'
			call WinWalker_show_jump_list()

		" --------------------------------------------------
		" Change key remap  {{{
		"
		elseif l:inp ==# 'k'

			echon "\n"
			for key in keys( g:WinWalker_remap )
				echon '(' . strtrans( key ) . ')->(' 
							\ . strtrans( g:WinWalker_remap[ key ] )
							\ . ")\n"
			endfor

			echon "\n(<ESC> to cancel) Map *FROM* key:"
			let key1 = getchar()
			if nr2char( key1 ) != "" | let key1 = nr2char( key1 ) | endif
			if key1 != "\<ESC>"
				echon "\n(<ESC> to remove '" . key1 . "') Map *TO* key:"
				let key2 = getchar()
				if nr2char( key2 ) != "" | let key2 = nr2char( key2 ) | endif
				if key2 == "\<ESC>"
					call remove( g:WinWalker_remap, key1 )
				else
					let g:WinWalker_remap[ key1 ] = key2
				endif
			else
			endif

			echon "\n"
			for key in keys( g:WinWalker_remap )
				echon '(' . strtrans( key ) . ')->(' 
							\ . strtrans( g:WinWalker_remap[ key ] )
							\ . ")\n"
			endfor
			echon "\nPress any key: "
			call getchar()

			"}}}

		" --------------------------------------------------
		" Change drop out keys     {{{
		" 
		elseif l:inp ==# 'd'
			let save_display = &display
			set display-=uhex
			echon "\n"
			echon strtrans( string ( g:WinWalker_dropout_keys ) ) . ', <ESC>=abort : '

			let key1 = getchar()
			if nr2char( key1 ) != "" | let key1 = nr2char( key1 ) | endif

			if key1 != "\<ESC>"
				if count( g:WinWalker_dropout_keys, key1 ) > 0
					let s:infomsg .= ' Removing (' . strtrans( key1 )
								\ . ') from list'
					call filter( g:WinWalker_dropout_keys, 'v:val !=# key1' )
				else
					let s:infomsg .= ' Adding (' . strtrans( key1 )
								\ . ') to list'
					call add( g:WinWalker_dropout_keys, key1 )
				endif
			endif

			let &display = save_display
			"}}}

		" --------------------------------------------------
		" Change macro keys                             {{{
		"
		elseif '^macro' =~ l:inp 
			for key in sort( keys( g:WinWalker_macros ) )
				if strlen( key ) > 1
					let key = s:special_key_name( key ) 
				endif
			endfor

			echon "\n"
			for key in sort( keys( g:WinWalker_macros ) )
				echon '(' . strtrans( key ) . ')->(' 
							\ . strtrans( g:WinWalker_macros[ key ] )
							\ . ")\n"
			endfor

			echon "\n(<ESC> to cancel) Map *FROM* key:"
			let key1 = getchar()
			if nr2char( key1 ) != "" | let key1 = nr2char( key1 ) | endif
			if key1 != "\<ESC>" && key1 != ' '
				let prompt = "\n(<ESC> to remove '" . key1 . "') Map *TO* key sequence:"
				let default_inp = has_key( g:WinWalker_macros, key1 ) ? g:WinWalker_macros[key1] : ''
				let macro = input( prompt, default_inp  )
				echo 'macro ' . strtrans( macro )
				if macro == ""
					if has_key( g:WinWalker_macros, key1 )
						call remove( g:WinWalker_macros, key1 )
					endif
				else
					let g:WinWalker_macros[ key1 ] = macro
				endif
			else
			endif
			"}}}


		elseif l:inp ==# ' ' || l:inp =~ "\\(\<ESC>\\|\<C-C>\\)"
			break
		else
			let s:errmsg .= " Invalid command (" . l:inp . ") "
		endif

		redraw

	endwhile

	let s:Keep_jumps = 1
	call WinWalker_save_cfg()
	let s:Keep_jumps = 0

endfunction





function! s:Window_status_string()
endfunction



function! s:Buffer_status_string()
	let out = ''

	let has_b_lines = ''
	if ( exists( 'b:set_fixed_lines' ) && b:set_fixed_lines )
		let has_b_lines = 'b:%#Tag#' . b:set_fixed_lines . '%##l'
	endif
	let has_b_cols = ''
	if ( exists( 'b:set_fixed_cols' ) && b:set_fixed_cols )
		let has_b_cols = 'b:%#Tag#' . b:set_fixed_cols . '%##l'
	endif
	let has_w_lines = ''
	if ( exists( 'w:set_fixed_lines' ) && w:set_fixed_lines )
		let has_w_lines = 'w:%#Tag#' . w:set_fixed_lines . '%##l'
	endif
	let has_w_cols = ''
	if ( exists( 'w:set_fixed_cols' ) && w:set_fixed_cols )
		let has_w_cols = 'w:%#Tag#' . w:set_fixed_cols . '%##l'
	endif
	let has_follow = ''
	if ( exists( 'b:set_follow' ) && b:set_follow != '' )
		let has_follow = '%#Tag#' . b:set_follow . '%##l'
	endif


	let l = [ has_b_lines , has_b_cols , has_w_lines , has_w_cols 
				\ , has_follow ]
	let has_any = join( filter( l, 'v:val != ""' ), ',' )

	if has_any != ''
		let out .= ' , lock=' . '(' . has_any . ')'
	endif

	if exists( 'b:attach_to' ) && len( b:attach_to ) < 1
			unlet b:attach_to
	endif
	if exists( 'b:attach_to' )
		let attachments = ''
		for dir in [ 'up', 'down', 'left', 'right' ]
			if has_key( b:attach_to, dir )
				let attachments .= ( attachments == '' ? '' : ',' )
							\ . '%#Tag#' . dir . '->'
							\ . b:attach_to[ dir ] 
							\ . '%##'
							"\ . ( b:attach_to[ dir ] == '' ?
							"\		'[Wall]' : b:attach_to[ dir ] )
			endif
		endfor
		let out .= ' , attach=(' . attachments . ')'
	endif


	return out
endfunction



" Misc_menu                                                         }}}
" ----------------------------------------------------------------------



" ----------------------------------------------------------------------
" Tab_menu                                                         {{{

function! s:Tab_menu()

	let l:last_inp = ''

	while 1
		let l = []

		if s:infomsg != ''
			call Char_menu_wrapAdd( l, '%#Special#' . s:infomsg . '%##' )
			let s:infomsg = ''
		endif
		if s:errmsg != ''
			call Char_menu_wrapAdd( l, '%#Error#' . s:errmsg . '%##' )
			let s:errmsg = ''
		endif
		call Char_menu_wrapAdd( l, "{h}/{l}=go prev/next" )
		call Char_menu_wrapAdd( l, "{H}/{L}=move prev/next" )
		call Char_menu_wrapAdd( l, "{n}/{N}ew tab" )
		call Char_menu_wrapAdd( l, "{t}ab table" )
		call Char_menu_wrapAdd( l, "{q}/{<C-Q>}=tabclose/tabclose!" )

		call Char_menu_wrapAdd( l, "{SPACE}=>main menu :  "  )



		if s:Unhuman_input
			let l:inp = s:Recheck_unhuman()
			if l:inp == 0 | return | endif
		else
			" Almost all the tab commands require a redraw:
			redraw
			let l:inp = Char_menu( join( l, '' )
						\ , "{<ESC>} {<C-C>} {<DEL>} {;}"
						\ , l:last_inp )
		endif

		let l:last_inp = l:inp

		if l:inp ==# 'h'
			tabprev
			"endif
		elseif l:inp ==# 'l'
			tabnext
		elseif l:inp ==# 't'
			redir @">
			tabs
			redir end
			call input( @" . "\<NL>\<NL>Press enter:  " )
		elseif l:inp ==# 'H'
			let t = tabpagenr() - 2
			if t < 0
				tabmove
			else
				exe 'tabmove ' . t
			endif
		elseif l:inp ==# 'L'
			if tabpagenr() == tabpagenr("$")
				tabmove 0
			else
				exe 'tabmove ' . tabpagenr()
			endif
		elseif l:inp =~# '[nN]'
			tabnew
		elseif l:inp ==# "q"
			tabclose
		elseif l:inp ==# "\<C-Q>"
			tabclose!
		elseif l:inp ==# ' ' || l:inp == "\<ESC>" || l:inp == "\<C-C>"
			break
		else
			let s:errmsg .= " Invalid command (" . l:inp . ") "
		endif

	endwhile

endfunction

" Tab_menu                                                         }}}
" ----------------------------------------------------------------------



" ----------------------------------------------------------------------
" Save menu, functions, options                                      {{{

function! s:Save_menu()

	let l:last_inp = ''

	while 1
		let l = []

		if s:infomsg != ''
			call Char_menu_wrapAdd( l, '%#Special#' . s:infomsg . '%##' )
			let s:infomsg = ''
		endif
		if s:errmsg != ''
			call Char_menu_wrapAdd( l, '%#Error#' . s:errmsg . '%##' )
			let s:errmsg = ''
		endif

		call Char_menu_wrapAdd( l, "Save {d}irectory is (%#Tag#" . g:WinWalker_opts.sess_dir . "%##)" )
		call Char_menu_wrapAdd( l, "{s}ave session" )
		call Char_menu_wrapAdd( l, "{t}ab save session" )
		call Char_menu_wrapAdd( l, "{r}estore session" )
		call Char_menu_wrapAdd( l, "{l}ist sessions" )

		call Char_menu_wrapAdd( l, "{SPACE}=>main menu :  "  )



		if s:Unhuman_input
			let l:inp = s:Recheck_unhuman()
			if l:inp == 0 | return | endif
		else
			" Almost all the tab commands require a redraw:
			redraw
			let l:inp = Char_menu( join( l, '' )
						\ , "{<ESC>} {<C-C>} {<DEL>} {;}"
						\ , l:last_inp )
		endif

		let l:last_inp = l:inp


		" ----------------------------------------
		"  Display session files
		"
		if l:inp ==# 'd'
			exe 'echohl ' . g:CMu_menu_hl_text
			let sess_dir = input( 'Enter directory : ', g:WinWalker_opts.sess_dir, 'file' )
			if sess_dir != ''
				let g:WinWalker_opts.sess_dir = sess_dir
			endif


		" ----------------------------------------
		"  Save session            {{{
		"
		elseif l:inp =~# '[st]'
			call Clear_cmd_window()
			redraw

			let save_sessionoptions = &sessionoptions

			exe 'echohl ' . g:CMu_menu_hl_text

			if l:inp ==# 't'
				echon "Save this tab, "
				set sessionoptions-=tabpages
			else
				echon "Save all tabs, "
				set sessionoptions+=tabpages
			endif

			echon 'Session options=' 
			echohl Tag
			echon &sessionoptions 
			echohl NONE

			exe 'echohl ' . g:CMu_menu_hl_text

			let fname = input( 'Save file? (^C, ^D, <TAB> active): ',
						\ g:WinWalker_opts.sess_dir . '/Session.',
						\ 'customlist,Session_file_complete' )


			if fname != ''
				if fname !~? '\.vim$'
					let fname .= '.vim'
				endif
				try
					exe 'mksession! ' . fname
				catch
					let s:errmsg .= ' Save session failed, ' . v:errmsg
					let &sessionoptions = save_sessionoptions
					continue
				endtry
			else
				let s:infomsg .= ' Not saved.'
			endif

			let xfname = substitute( fname, '\.vim$', 'x&', '' )


			" ----------------------------------------
			" Get all the window vars.
			" Do it before we open a new window, and screw up the window
			" numbers:
			let winvar_cmds = {}
			let bufvar_cmds = {}
			for tabnum in range( 1, tabpagenr("$") )
				for winnr in range( 1, tabpagewinnr( tabnum, "$") )

					if !exists( 'winvar_cmds[ tabnum ]' )
						let winvar_cmds[ tabnum ] = []
					endif
					if !exists( 'bufvar_cmds[ tabnum ]' )
						let bufvar_cmds[ tabnum ] = []
					endif
					" If we are only saving one tab, then create
					" settabwinvar(0,var,val) commands so the will properly
					" set the window variables in the restored tab only.
					let tabnum2 = tabnum
					if l:inp ==# 't' 
						if tabnum != tabpagenr()
							continue
						endif
						let tabnum2 = 0
					endif


					let bufnr = winbufnr( winnr )
					let bufname = bufname( bufnr )

					"let lines = gettabwinvar( tabnum, winnr, 'set_fixed_lines' )
					let lines = getbufvar( bufnr, 'set_fixed_lines' )
					if lines
						"let cmd = 'call settabwinvar( '
						let cmd = 'call setbufvar( '
									\ . '"' . bufname . '"' . ','
									\ . '"set_fixed_lines"' . ','
									\ . lines . ')'
						"call add( winvar_cmds[ tabnum ], cmd )
						call add( bufvar_cmds[ tabnum ], cmd )
					endif

					let cols = getbufvar( bufnr, 'set_fixed_cols' )
					if cols
						let cmd = 'call setbufvar( '
									\ . '"' . bufname . '"' . ','
									\ . '"set_fixed_cols"' . ','
									\ . cols . ')'
						call add( bufvar_cmds[ tabnum ], cmd )
					endif

					let follow = getbufvar( bufnr, 'set_follow' )
					if follow != ''
						let cmd = 'call setbufvar( '
									\ . '"' . bufname . '"' . ','
									\ . '"set_follow"' . ','
									\ . '"' . follow . '")'
						call add( bufvar_cmds[ tabnum ], cmd )
					endif

					let attach_to = {}
					if len( getbufvar( bufnr, 'attach_to' ) ) > 0
						let attach_to = getbufvar( bufnr, 'attach_to' )
					endif
					if len( attach_to ) > 0
						let cmd = 'call setbufvar( '
									\ . '"' . bufname . '"' . ','
									\ . '"attach_to"' . ','
									\ . string( attach_to ) . ')'
						call add( bufvar_cmds[ tabnum ], cmd )
					endif

				endfor
			endfor


			" ----------------------------------------
			"  Write out the Session...x.vim file
			"
			try
				exe 'silent 1 new ' . xfname
			catch
				let s:errmsg .= ' Save session x failed, ' . v:errmsg
				let &sessionoptions = save_sessionoptions
				continue
			endtry

			silent %d

			for tabnum in keys( bufvar_cmds )
				call append( line('$'), bufvar_cmds[ tabnum ] )
			endfor

			for tabnum in keys( winvar_cmds )
				call append( line('$'), winvar_cmds[ tabnum ] )
			endfor

			" Cause a BufEnter for each window, which should trigger
			" the resizing:
			"call append( line('$'), 'for winnr in range( 1, winnr("$") )' )
			"call append( line('$'), '   wincmd w ' )
			"call append( line('$'), 'endfor' )

			silent write!
			silent bwipeout

			let &sessionoptions = save_sessionoptions

			" End save session 
			" ---------------------------------------- }}}


		" ----------------------------------------
		"  Restore session                     {{{
		"
		elseif l:inp ==# 'r'
			call Clear_cmd_window()
			redraw

			exe 'echohl ' . g:CMu_menu_hl_text

			let fname = input( 'Restore file? (^C, ^D, <TAB> active): ', 
						\ g:WinWalker_opts.sess_dir . '/Session.',
						\ 'customlist,Session_file_complete' )

			if fname != ''
				if filereadable( fname )
					try
						exe 'source ' . fname
					catch
					endtry
				else
					if fname !~ '\.vim$'
						let fname .= '.vim'
					endif
					if filereadable( fname )
						try
							exe 'source ' . fname
						catch
						endtry
					else
						let s:errmsg .= ' Restore session failed, ' 
									\ . fname . ' not found'
					endif
				endif
			endif

			if filereadable( $HOME . '/' . '.gvimrc' )
				exe 'source ' . $HOME . '/' . '.gvimrc'
			elseif filereadable( $HOME . '/' . '_gvimrc' )
				exe 'source ' . $HOME . '/' . '_gvimrc'
			endif

			if filereadable( $HOME . '/' . '.vimrc' )
				exe 'source ' . $HOME . '/' . '.vimrc'
			elseif filereadable( $HOME . '/' . '_vimrc' )
				exe 'source ' . $HOME . '/' . '_vimrc'
			endif
			" End restore session
			" ---------------------------------------- }}}

		elseif l:inp ==# 'l'
			let files = globpath( g:WinWalker_opts.sess_dir . ',.', 'Session*' )

			echo s:Session_show_details( g:WinWalker_opts.sess_dir . '/Session* Session*' )
			exe 'echohl ' . g:CMu_menu_hl_text
			echon "\nPress any key: "
			call getchar()

		elseif l:inp ==# ' ' || l:inp == "\<ESC>" || l:inp == "\<C-C>"
			break
		else
			let s:errmsg .= " Invalid command (" . l:inp . ") "
		endif

	endwhile

endfunction




function! Session_file_complete( ArgLead, CmdLine, CursorPos )

	"echomsg 'ArgLead, CmdLine, CursorPos =' . a:ArgLead . ',' .  a:CmdLine . ',' .  a:CursorPos 

	let candidates = split( glob( a:ArgLead . '*' ), "\n")

	"echo string( candidates )

	if len( candidates ) == 1

		echo s:Session_show_details( join( candidates, ' ' )  )
		echon "\nPress any key: "
		call getchar()
		call Clear_cmd_window()

		"return s:Session_show_details( candidates[0] . '*' )
		"return
		"return []
	endif
	"return candidates 
	"echomsg 'returning ' . join( candidates, "\n" )
	"return glob( a:ArgLead . '*' )
	"return join( candidates, "\n" )
	return candidates
endfunction




function! s:Session_show_details( ... )
	try
		exe 'silent vimgrep /^\(edit \|tabnew\)/j' . join( a:000, ' ' )
	catch
		let s:errmsg .= ' No session files found.'
		return ''
	endtry

	silent copen
	let matches = getbufline( "%", 1, "$" )
	silent cclose

	let bufs_in_session = {}
	let session_tab_level = {}
	for match in matches
		if match =~ 'tabnew'
			let session_tab_level[ session ] += 1
		else
			let [ junk, session, bufname; trash ] = matchlist( match, '^\([^|]\+\)|[^|]*|\s*edit\s*\(.*\)' )

			if !exists( 'session_tab_level[ session ] ' )
				let session_tab_level[ session ] = 1
			endif
			if !exists('bufs_in_session[session]' )
				let bufs_in_session[ session ] = {}
			endif
			if !exists('bufs_in_session[session][ session_tab_level[ session ] ]' )
				let bufs_in_session[ session ][  session_tab_level[ session ]  ] = []
			endif
			call add( bufs_in_session[ session ][  session_tab_level[ session ]  ], bufname )
		endif
	endfor

	let out = ''
	for session in sort( keys( bufs_in_session ) )
		let out .= session . '  ' . strftime( "%c", getftime( session ) ) . " :\n    "
		for tabnum in keys( bufs_in_session[ session ] )
			let out .= '  Tab#' . tabnum . '=[' . join( bufs_in_session[ session ][ tabnum ], ', ' ) . ']'
		endfor
		let out .= "\n\n"
	endfor
	return out
endfunction



function! WinWalker_save_cfg()

	exe 'silent 1 new ' . g:WinWalker_opts.rc_file

	let s = [ ''
	\ , 'let g:WinWalker_opts = ' . string( g:WinWalker_opts  )
	\ , 'let g:WinWalker_remap = ' . string( g:WinWalker_remap  )
	\ , 'let g:WinWalker_dropout_keys = ' . string( g:WinWalker_dropout_keys )
	\ , 'let g:WinWalker_macros = ' . string( g:WinWalker_macros  )
	\ , 'let g:CMu_timeoutlen = ' . g:CMu_timeoutlen
	\ ]

	silent %d
	call append( 1, s )
	if delete( g:WinWalker_opts.rc_file )
		let s:errmsg .= ' Remove failed for ' . g:WinWalker_opts.rc_file
	endif

	" This causes a weird 2 second delay:
	"exe 'silent write ' . g:WinWalker_opts.rc_file
	silent write!
	silent bwipeout

endfunction


let save_opts = deepcopy( g:WinWalker_opts )

exe 'silent! source ' . g:WinWalker_opts.rc_file

" This is for when options are added in new releases:
for key in keys( save_opts )
	if !exists( 'g:WinWalker_opts[key]' )
		let g:WinWalker_opts[ key ] = save_opts[ key ]
	endif
endfor


" Save menu, functions, options                                    }}}
" ----------------------------------------------------------------------



" ----------------------------------------------------------------------
" Constraints menu:                                                   {{{

function! s:Constraints_menu()

	let l:last_inp = ''

	while 1
		let l = []

		if s:infomsg != ''
			call Char_menu_wrapAdd( l, '%#Special#' . s:infomsg . '%##' )
			let s:infomsg = ''
		endif
		if s:errmsg != ''
			call Char_menu_wrapAdd( l, '%#Error#' . s:errmsg . '%##' )
			let s:errmsg = ''
		endif

		call Char_menu_wrapAdd( l, "{c}=>set preset ("
					\ . "%#Tag#" . g:WinWalker_opts.preset_lines . "%##"
					\ . ","
					\ . "%#Tag#" . g:WinWalker_opts.preset_cols . "%##"
					\ . ")" )

		let has_lines = ( exists( 'b:set_fixed_lines' ) && b:set_fixed_lines )
		let has_cols = ( exists( 'b:set_fixed_cols' ) && b:set_fixed_cols )
		call Char_menu_wrapAdd( l, "{ll}/{lc}=>lock-lines/cols"
					\ . ( has_lines || has_cols ? '(' : '' )
					\ . ( has_lines ? 
						\ '%#Tag#' . b:set_fixed_lines . '%##l' : '' )
					\ . ( has_lines && has_cols ? ',' : '' )
					\ . ( has_cols ? 
						\ '%#Tag#' . b:set_fixed_cols . '%##c' : '' )
					\ . ( has_lines || has_cols ? ')' : '' )
					\   )

		let has_follow = ( exists( 'b:set_follow' ) && b:set_follow != '' )
		call Char_menu_wrapAdd( l, "{f}ollow="
					\ . '%#Tag#' 
					\ . ( has_follow != '' ? b:set_follow : 'OFF' )
					\ . '%##' 
					\ )

		let out = ''
				for dir in [ 'up', 'down', 'left', 'right' ]
					if exists( 'b:attach_to[dir]' )
						let out .= ( out == '' ? '' : ',' )
									\ . '%#Tag#' . dir . '->'
									\ . b:attach_to[ dir ] 
									\ . '%##'
					endif
				endfor
		call Char_menu_wrapAdd( l, "{a}ttach (" . out . ")" )


		call Char_menu_wrapAdd( l, "{SPACE}=>main menu :  "  )



		if s:Unhuman_input
			let l:inp = s:Recheck_unhuman()
			if l:inp == 0 | return | endif
		else
			" Almost all the tab commands require a redraw:
			redraw
			let l:inp = Char_menu( join( l, '' )
						\ , "{<ESC>} {<C-C>} {<DEL>} {;}"
						\ , l:last_inp )
		endif

		let l:last_inp = l:inp

		" --------------------------------------------------
		" Change window size preset
		"
		if l:inp ==# 'c'
			let lines = 0
			let cols = 0
			try
				let [ lines, cols; trash ] = split( input( "Lines, columns? " ), ', *' )
			catch
			finally
				if lines | let g:WinWalker_opts.preset_lines = lines | endif
				if cols | let g:WinWalker_opts.preset_cols = cols | endif
			endtry


		" --------------------------------------------------
		" Set-fixed-sizes         {{{
		"
		elseif l:inp =~# 'l[lc]'

			if l:inp ==# 'll'
				if exists( 'b:set_fixed_lines' ) && b:set_fixed_lines
					unlet b:set_fixed_lines
					unlet w:set_fixed_lines
				else
					let b:set_fixed_lines = winheight(winnr())
					let w:set_fixed_lines = winheight(winnr())
				endif
			elseif l:inp ==# 'lc'
				if exists( 'b:set_fixed_cols' ) && b:set_fixed_cols
					unlet b:set_fixed_cols
					unlet w:set_fixed_cols
				else
					let b:set_fixed_cols = winwidth(winnr())
					let w:set_fixed_cols = winwidth(winnr())
				endif
			endif
			"}}}
		" --------------------------------------------------



		" --------------------------------------------------
		" Set-follow                                  {{{
		"
		elseif l:inp =~# '[f]'

				if exists( 'b:set_follow' ) && b:set_follow != ''
					unlet b:set_follow
				else
					let b:set_follow = 'FOLLOW'
				endif
			"}}}
		" --------------------------------------------------



		" --------------------------------------------------
		" Set attachments                               {{{
		"
		elseif l:inp ==# 'a'

			while 1
				exe 'echohl ' . g:CMu_menu_hl_text
				let out = ''
				for dir in [ 'up', 'down', 'left', 'right' ]
					if exists( 'b:attach_to[dir]' )
						let out .= dir . '->'
									\ . b:attach_to[ dir ] 
									\ . "\n"
					endif
				endfor

				echo out

				echon "\nEnter direction [jkhl] : "
				let dir = getchar()
				if nr2char( dir ) != "" | let dir = nr2char( dir ) | endif
				if dir =~ "\\(\<ESC>\\|\<C-C>\\| \\|\<CR>\\)" | break | endif
				if dir !~ '[jkhl]'
					echohl Error
					echon "Invalid (" . dir ") "
					continue
				endif

				let dir = s:dir_names[ dir ]
				let winnr = GetWinAdjacent( dir )
				if !exists( 'b:attach_to' ) | let b:attach_to = {} | endif
				let what = winnr

				if has_key( b:attach_to, dir )
					call remove( b:attach_to, dir )
					echo "Removed " . dir . '->' . what
					continue
				else
				endif

				if winnr > 0
"					if len( b:attach_to ) > 0
"						echohl Error
"						echo "Attachments to windows must be exclusive"
"						continue
"					endif

					let what = bufname( winbufnr( winnr ) ) 
					if what == ''
						echohl Error
						echo "Can't attach to No Name windows"
						if has_key( b:attach_to, dir ) 
							call remove( b:attach_to, dir )
						endif
						continue
					endif

				else
					let what = '[Wall]'

"					let found_win = 0
"					for d in keys( b:attach_to )
"						if b:attach_to[ d ] > 0
"							let found_win += 1
"						endif
"					endfor
"					if found_win > 0
"						echohl Error
"						echo "Attachments to windows must be exclusive"
"						continue
"					endif
				endif

				let b:attach_to[ dir ] = what

			endwhile
			echohl NONE

			"}}}
		" --------------------------------------------------


		elseif l:inp ==# ' ' || l:inp == "\<ESC>" || l:inp == "\<C-C>"
			break
		else
			let s:errmsg .= " Invalid command (" . l:inp . ") "
		endif

	endwhile

endfunction

" End Constraints_menu                                            }}}




" ----------------------------------------------------------------------
" Misc functions                                                    {{{


function! s:special_key_name( key )
	exe "normal :let special_key_name='" . "\<c-k>" . a:key . "'\<CR>"
	return special_key_name
endfunction


function! s:Recheck_unhuman()
	let s:Unhuman_input = Peek_char_timeout_wait( 100 )
	if !s:Unhuman_input 
		"finish
		return 0
	endif
	let inp = getchar()
	if nr2char( inp ) != "" | let inp = nr2char( inp ) | endif
	return inp
endfunction








function! WinWalker_clean_empties()
	let save_bufnr = bufnr("%")
	let save_eventignore = &eventignore
	set eventignore=all

	for bufnr in range( 1, bufnr("$") )

		if !bufexists( bufnr )  | continue | endif
		if bufname( bufnr ) != '' | continue | endif

		if bufloaded( bufnr )
			if !getbufvar( bufnr, '&hidden' )
				continue
			endif
		endif

		exe 'sbuf ' . bufnr

		if line("$") > 1 || col("$") > 1
				\ || strlen( getline( "$" ) ) > 0 
			hide
		elseif !getbufvar( bufnr, '&modified' )
			exe 'silent! bwipeout! ' . bufnr
		endif

	endfor
	"exe 'buf ' . save_bufnr
	let &eventignore = save_eventignore
endfunction






function! s:Pad( s, len )
	let s = a:s
	let pads = a:len - strlen( s )
	while pads > 0
		let s .= ' '
		let pads -= 1
	endwhile
	return s
endfunction



function! Tab_jump_or_win_move()
	let l = line(".")
	let c = col(".")
	let b = bufnr("%")
	exe "normal! \<esc>\<tab>"
	if l == line(".") && c == col(".") && b == bufnr("%")
		call feedkeys( "\<c-w>\<c-w>", "t" )
	endif
endfunction



" Misc functions                                                    }}}
" ----------------------------------------------------------------------



" ----------------------------------------------------------------------
" Misc window functions                                             {{{




function! s:New_window( orient )
	if a:orient =~? '^v'
		silent vnew
		let tabwin = tabpagenr() . ',' . winnr()
		let s:Win_orient[ tabwin ] = 'vertical'
	else
		silent new
		let tabwin = tabpagenr() . ',' . winnr()
		let s:Win_orient[ tabwin ] = 'horizontal'
	endif
	setlocal bufhidden=wipe
endfunction




let s:Need_window_update = 0
function! s:Cause_window_update()

	if ! s:Need_window_update | return | endif

	let s:Keep_jumps = 1

	let save_bufnr = bufnr("%")
	let save_winnr = winnr()
	let buflist = []

	for winnr in range( 1, winnr("$") )
		call add( buflist, winbufnr( winnr ) )
	endfor

	"let s:need_window_update = 1
	"let safety = 0 

	"while s:need_window_update && safety < 10
		"let s:need_window_update = 0

		for bufnr in buflist
			"let s:infomsg .= " w" . bufwinnr( bufnr )
			exe 'silent ' . bufwinnr( bufnr ) . 'wincmd w'
		endfor

		"let s:infomsg .= " upd#" . safety . ',need=' . s:need_window_update
		"let safety += 1
	"endwhile

	"let s:need_window_update = 0
	"if safety >= 10
		"let s:errmsg .= ' Warning: possible window constraint conflicts.'
	"endif


	"
	" Set up correct previous window for   wincmd p   or  ^P  , etc.:
	"
	let len = len( s:prev_winnrs )
	if len >= 2
		exe 'silent ' . s:prev_winnrs[ len - 2 ] . ' wincmd w '
	endif




	if winbufnr( save_winnr ) == save_bufnr
		" Try to handle case where there are two windows with the same
		" buffer:
		exe 'silent ' . save_winnr . ' wincmd w '
	else
		exe 'silent ' . bufwinnr( save_bufnr ) . ' wincmd w '
	endif
	redraw
	let s:Keep_jumps = 0
endfunction


function! s:bufwinnrs( bufnr )
	let winnrs = []
	for winnr in range( 1, winnr("$" ) )
		if winbufnr( winnr ) == a:bufnr
			call add( winnrs, winnr )
		endif
	endfor
	return winnrs
endfunction



function! s:Exchange_win( win1, win2 )

	if a:win1 < 1 || a:win1 > winnr("$") || a:win2 < 1 || a:win2 > winnr("$")
		return
		" It's ok to give invalid arg so it will prop. errors
	endif

	let bufnr1 = winbufnr( a:win1 )
	let bufnr2 = winbufnr( a:win2 )

"	if getbufvar( bufnr1, '&modified' ) 
"		let s:errmsg = " Cannot exchange, b#" . bufnr1 . " is modified"
"		return
"	endif
"	if getbufvar( bufnr2, '&modified' )
"		let s:errmsg = " Cannot exchange, b#" . bufnr2 . " is modified"
"		return
"	endif

	let save_winnr = winnr()

	"Disabling all events caused windows to lose syntax highlighting.
	" To do:  which events should be disabled, i.e.  not FileType
	"
	let save_eventignore = &eventignore
	set eventignore=BufEnter,BufLeave,WinEnter,WinLeave

	" Some app.s like 'Taglist' will self-destruct if they are fiddled
	" with, so let's start with 'bufhidden', and eventignore.
	let win1_bufhidden = getbufvar( bufnr1, '&bufhidden' ) 
	let win2_bufhidden = getbufvar( bufnr2, '&bufhidden' ) 
	call setbufvar( bufnr1, '&bufhidden', 'hide' ) 
	call setbufvar( bufnr2, '&bufhidden', 'hide' ) 

	exe 'silent! ' . a:win1 . 'wincmd w'
	exe 'silent! buf! ' . bufnr2
	exe 'silent! ' . a:win2 . 'wincmd w'
	exe 'silent! buf! ' . bufnr1

	exe "silent! " . save_winnr . "wincmd w"

	call setbufvar( bufnr1, '&bufhidden', win1_bufhidden ) 
	call setbufvar( bufnr2, '&bufhidden', win2_bufhidden ) 

	let &eventignore = save_eventignore
endfunction




"
" Usage:  GetWinAdjacent( dir ) or ( winnr, dir )
" where 'dir'  is  h, l, j, k, up, down, left, right
" Returns:	winnr==success,
"			0==found no winnr,
"			-1==usage error
"
function! GetWinAdjacent( ... )
	if a:0 == 1
		let test_winnr = winnr()
		let dir = tolower( a:1 )
	elseif a:0 == 2
		let test_winnr = a:1
		let dir = a:2
	else
		return -1
	endif

	let save_winnr = winnr()

	" This test must come first, because 0 == '.' will succeed
	if test_winnr =~ '[.]'
		let test_winnr = winnr()
	elseif test_winnr < 1 || test_winnr > winnr("$")
		return -1
	else
	endif

	if     dir ==# 'up'		| let dir = 'k' 
	elseif dir ==# 'down'	| let dir = 'j' |
	elseif dir ==# 'left'	| let dir = 'h' |
	elseif dir ==# 'right'	| let dir = 'l' |
	elseif dir ==# 'right'	| let dir = 'l' |
	elseif dir !~# '[jkhl]'
		return -1
	endif

	let save_eventignore = &eventignore
	set eventignore=BufEnter,BufLeave,WinEnter,WinLeave

	exe "silent! " . test_winnr . "wincmd w"
	if winnr() != test_winnr
		let &eventignore = save_eventignore
		return 0
	endif

	exe 'silent! wincmd ' . dir
	if winnr() == test_winnr
		let result = 0
	else
		let result = winnr()
	endif
	exe "silent! " . save_winnr . "wincmd w"
	let &eventignore = save_eventignore
	return result
endfunction





" This is a simple way to give the user a visual que which window
" is current when there is no cursor in it.
"
function! s:Empty_fill_fake()
	if g:WinWalker_opts.empty ==? 'NOLABEL' | return | endif
	if s:Unhuman_input | return | endif
	let b:empty = 0
	let label2 = s:empty_label

	let width = winwidth( winnr() ) - 1

	if &number
		let width = width - &numberwidth - &diff - 1
	endif

	if width <= strlen( label2 )
		let label2 = substitute( label2, ' ', '', 'g' )
	endif

	if line("$") == 1 && col("$") == 1
		let b:empty = 1
		exe 'silent! normal! ' . winheight(winnr()) . "a\n\eM"
		exe 'silent! normal! ' . width . "a \e"
		exe "silent! normal! o\e" . width . "a_\e"
		exe 'silent! normal! 0gm' . ( strlen( label2 ) / 2 ) . "h"
		exe "silent! normal! R" . label2 . "\e"
		exe "silent! normal! o\e" . width . "a \e"
		set nomodified
	endif
	" Need redraw here or 'match' command fails elsewhere
	"redraw
endfunction




function! s:Empty_empty()
	if g:WinWalker_opts.empty ==? 'NOLABEL' | return | endif
	if line("$") > 200 | return | endif	" try to short circuit large files

	let look_for = '\(' . s:empty_label . '\|'
				\ . substitute( s:empty_label, ' ', '', 'g' ) . '\)'

	if search( look_for, 'wn' ) > 0
		let s = join( getline(1,"$"), '' )
		if s =~ '\%^\_s*_*' . look_for . '_*\_s*\%$'
			1,$d _
			set nomodified
		endif
	endif
	" Need redraw here or 'match' command fails elsewhere
	"redraw
endfunction





function! WinWalker_push_window( ... )  

	if a:0 == 1
		let l:dir = a:1
	elseif a:0 == 2
		let goto_winnr = a:1
		if goto_winnr < 1 || goto_winnr > winnr("$")
			let s:errmsg .= " Push window: bad window number " . goto_winnr
			return
		endif
		let l:dir = a:2
	else
		let s:errmsg .= " Push window: bad arg number "
		return
	endif

	if !has_key( s:dir_names, tolower( l:dir )  )
		let s:errmsg .= ' Bad call to push window: ' . l:dir
		return
	endif

	let b_return_to = bufnr("%")
	if exists('goto_winnr')
		exe 'silent ' . a:1 . ' wincmd w'
	endif

	let b_old = bufnr("%")
	let w_old = winnr()

	" Some app.s like 'Taglist' will self-destruct if they are fiddled
	" with, so let's start with 'bufhidden', and eventignore.
	let save_bufhidden = getbufvar( b_old, '&bufhidden' )
	set bufhidden=hide

	let save_eventignore = &eventignore
	set eventignore=BufEnter,BufLeave,WinEnter,WinLeave

	if l:dir =~# '^[JKHL]$'
	elseif l:dir =~# '^[jkhl]$'
		let l:dir = toupper( l:dir )
	else
		let l:dir = toupper( s:dir_names[ l:dir ] )
	endif
	if l:dir !~# '^[JKHL]$' | echoerr "I screwed up" | return | endif

	let w_other = GetWinAdjacent( l:dir )
	let b_other = winbufnr( w_other )
	let save_winvars_old = s:Save_window_vars( w_old )
	let lines_old = winheight(winnr())
	let cols_old = winwidth(winnr())

	if exists( 'b:set_fixed_lines' )
		let save_fixed_lines = b:set_fixed_lines
	endif
	if exists( 'b:set_fixed_cols' )
		let save_fixed_cols = b:set_fixed_cols
	endif


	if w_other < 1
		if l:dir ==# 'H' && GetWinAdjacent( 'left' ) < 1
					\ &&  GetWinAdjacent( 'up' ) < 1
					\ &&  GetWinAdjacent( 'down' ) < 1
					\ &&  g:WinWalker_opts.wrap ==? 'tabs'
			" Already maximized
			if tabpagenr("$") == 1
				hide
				tabnew
				let where = min( [0, tabpagenr() - 2 ] )
				exe 'tabmove ' . where
			else
				hide
				tabprev

				let s:Keep_jumps = 1
				silent 99 wincmd l
				let s:Keep_jumps = 0

				call s:New_window( 'new' )
			endif
			exe 'buf ' . b_old
		elseif l:dir ==# 'L' && GetWinAdjacent( 'right' ) < 1
					\ &&  GetWinAdjacent( 'up' ) < 1
					\ &&  GetWinAdjacent( 'down' ) < 1
					\ &&  g:WinWalker_opts.wrap ==? 'tabs'
			" Already maximized
			if tabpagenr("$") == 1
				hide
				tabnew
			else
				hide
				tabnext

				let s:Keep_jumps = 1
				silent 99 wincmd h
				let s:Keep_jumps = 0


			endif
			exe 'silent buf ' . b_old
		else
			" Maximize at wall:
			exe 'silent! wincmd ' . l:dir
			wincmd =
		endif

	else " w_other >= 1

		if l:dir =~# '[JK]'

			if len( getbufvar( b_other, 'attach_to' ) ) > 0
				let attach_to = getbufvar( b_other, 'attach_to' )
				if l:dir ==# 'J' && has_key( attach_to, 'up' ) && attach_to[ 'up' ] == bufname( "%" )
					if GetWinAdjacent( w_other, 'down' )
						call WinWalker_push_window( w_other, 'down' )
					endif
				endif
				if l:dir ==# 'K' && has_key( attach_to, 'down' ) && attach_to[ 'down' ] == bufname( "%" )
					if GetWinAdjacent( w_other, 'up' )
						call WinWalker_push_window( w_other, 'up' )
					endif
				endif
			endif

			let save_splitright = &splitright
			if GetWinAdjacent( 'right' ) < 1
				let &splitright = 1
			elseif GetWinAdjacent( 'left' ) < 1
				let &splitright = 0
			endif

			exe 'wincmd ' . tolower( l:dir )
			call s:New_window( 'vnew' )

			let &splitright = save_splitright

		elseif l:dir =~# '[HL]'

			unlet! other_attach_to
			if len( getbufvar( b_other, 'attach_to' ) ) > 0
				let other_attach_to = getbufvar( b_other, 'attach_to' )
				if l:dir ==# 'H' && has_key( other_attach_to, 'right' ) && other_attach_to[ 'right' ] == bufname( "%" )
					if GetWinAdjacent( w_other, 'left' )
						call WinWalker_push_window( w_other, 'left' )
					endif
				endif
				if l:dir ==# 'L' && has_key( other_attach_to, 'left' ) && other_attach_to[ 'left' ] == bufname( "%" )
					if GetWinAdjacent( w_other, 'right' )
						call WinWalker_push_window( w_other, 'right' )
					endif
				endif
			endif

			let save_splitbelow = &splitbelow
			if GetWinAdjacent( 'up' ) < 1
				let &splitbelow = 0
			elseif GetWinAdjacent( 'down' ) < 1
				let &splitbelow = 1
			endif

			if exists('other_attach_to') && w_other > 0
				if has_key( other_attach_to, 'up' ) && other_attach_to[ 'up' ] == '[WALL]'
					let &splitbelow = 1
				elseif has_key( other_attach_to, 'down' ) && other_attach_to[ 'down' ] == '[WALL]'
					let &splitbelow = 0
				endif
			endif

			exe 'silent wincmd ' . tolower( l:dir )

			call s:New_window( 'new' )

			let &splitbelow = save_splitbelow

		endif

		

		" [v]new invalidates old window numbers

		let b_temp = winbufnr( winnr() )
		let save_winvars_temp = s:Save_window_vars( winnr() )

		let old_winnr = bufwinnr( b_old )
		exe 'silent ' . old_winnr . 'wincmd w'

		silent hide
		"redraw

		exe 'silent ' . bufwinnr( b_temp ) . 'wincmd w'





		exe 'silent buf ' . b_old

		" Assume that if it's currently full height/width, then don't
		" resize and squish everything at the destination:
		let avail_lines = &lines - &cmdheight - 2
		if lines_old > winheight(winnr()) && lines_old < avail_lines
			exe 'silent resize ' . lines_old
		endif
		if cols_old > winwidth(winnr()) && cols_old < &columns
			exe 'silent vert resize ' . cols_old
		endif

		call setbufvar( b_old, '&bufhidden', save_bufhidden )
		let &eventignore = save_eventignore

		if exists( 'save_fixed_lines' )
			let b:set_fixed_lines = save_fixed_lines
			call s:WinWalker_resize_fixed()
		endif
		if exists( 'save_fixed_cols' )
			let b:set_fixed_cols = save_fixed_cols
			call s:WinWalker_resize_fixed()
		endif


		for cmd in save_winvars_old
			exe cmd
		endfor
		for cmd in save_winvars_temp
			exe cmd
		endfor


	endif " [JK] handling,  w_other <?? 1


	let &eventignore = save_eventignore

	if winbufnr( winnr() ) != b_return_to
		exe 'silent ' . bufwinnr( b_return_to ) . 'wincmd w'
	endif

	let s:Need_window_update = 1

endfunction " WinWalker_push_window( dir )




function! s:Save_window_vars( winnr )
	return []
endfunction



function! s:Save_buffer_vars( winnr )
	if a:winnr < 1 
		let s:errmsg .= ' Save_window_vars bad winnr'
		return
	endif
	let out = []
	if exists( 'b:set_fixed_lines' ) 
		"call add( out,  'call setwinvar(' . a:winnr . ','
					"\ . '"set_fixed_lines",'
					"\ . getwinvar( a:winnr, 'set_fixed_lines' )
					"\ . ')'
					"\ )
		call add( out,  'let b:set_fixed_lines = '
					\ . getwinvar( a:winnr, 'set_fixed_lines' )
					\ )
	endif
	if exists( 'b:set_fixed_cols' ) 
		call add( out,  'let b:set_fixed_cols = '
					\ . getwinvar( a:winnr, 'set_fixed_cols' )
					\ )
	endif
	if exists( 'b:attach_to' ) 
		call add( out,  'let b:attach_to = '
					\ . string( getwinvar( a:winnr, 'attach_to' ) ) 
					\ )
	endif
	return out
endfunction


" Misc window functions                                             }}}
" ----------------------------------------------------------------------



" ----------------------------------------------------------------------
" Pick_buf, search/find functions                                   {{{

function! s:Pick_buf( )

	let inp = ''
	let last_inp = ''
	let buflist = []

	let has_window = {}
	for tabnr in range( 1, tabpagenr("$") )
		for bufnr in tabpagebuflist( tabnr )
			let has_window[ bufnr ] = 1
		endfor
	endfor

	while 1
		let l = []
		let bufnr = 1

		if s:errmsg != ''
			call Char_menu_wrapAdd( l, '%#Error#' . s:errmsg . '%##  ' )
			let s:errmsg = ''
		endif

		while bufnr <= bufnr("$")
			if ( s:buf_check == 'loaded' )
			\ && ( !bufloaded( bufnr ) 
			\	|| !has_key( has_window, bufnr ) )
				let bufnr += 1
				continue
			endif
			let elem = {}
			let bufname = bufname( bufnr )
			if bufname == '' | let bufname = '[No Name]' | endif
			let elem.bufnr = bufnr
			let elem.bufname = bufname
			call add( buflist, elem )
			if s:buf_check == '' && bufloaded( bufnr )
				call add( l, '( {' . fnamemodify( bufname, ':t' ) . '} = #{' . bufnr . '} %#Tag#has win%## )' )
			else
				 call add( l, '( {' . fnamemodify( bufname, ':t' ) . '} = #{' . bufnr . '} )' )
			endif
			let bufnr += 1
		endwhile

		let l1 = sort( l )
		let l = []
		for el in l1
			call Char_menu_wrapAdd( l, el )
		endfor


		let s:errmsg = ''

		" So far this is working.  After going through the pick dialog, it
		" will return and exit out of the menu subsystem.
		if s:Unhuman_input
			"let inp = s:Recheck_unhuman()
			"let s:Unhuman_input = 0
			"if inp == 0 | return | endif
		endif
		"else

			call Char_menu_wrapAdd( l, ' Enter first uniq char(s), {?} to toggle hidden: ' )

			let inp = Char_menu( join( l, '' )
						\ , "{<ESC>} {<C-C>} {<DEL>} {;}"
						\ , last_inp )
			let last_inp = inp
		"endif

		if inp == '?'
			if s:buf_check == 'loaded'
			   let s:buf_check = ''
		   else
			   let s:buf_check = 'loaded'
		   endif
		   redraw
		   continue
	   endif

	   " Clear unnecessary chars:
	   while getchar(1)
		   if nr2char( getchar(1) ) =~ "\\(\<esc>\\|\<c-c>\\|\<cr>\\| \\)" 
			   " assumed nonhuman input
			   call getchar()
			   break
		   endif
		   call getchar()
	   endwhile




		" Quick completion:
		"
		let matched = {}
		for elem in buflist
			"if elem['bufname'] != '' && elem['bufname'] =~ '^' . inp
			let i = escape( inp, '.' )
			let n = fnamemodify( elem['bufname'], ':t' )
			if n == '' | continue | endif
			let longest = 0

			if n == inp 
				let matched = elem
				break
			elseif n =~ '^' . i 
			\ && strlen( string( i ) ) > longest
				let matched = elem
				let longest = strlen( string( i ) )
			elseif elem['bufnr'] ==  inp 
			"\ && strlen( string( inp ) ) > longest
				let matched = elem
				break
				"let longest = strlen( string( inp ) )
			elseif elem['bufnr'] =~  '^' . i 
			\ && strlen( string( i ) ) > longest
				let matched = elem
				let longest = strlen( string( i ) )
			endif
		endfor

		if len( matched ) == 0
			if s:Unhuman_input
				break
			endif
			if inp =~ "\\( \\|\<ESC>\\|\<C-C>\\)"
				break
			else
				let s:errmsg = "No match in list for (" . inp . "), try again."
			endif
		else
			break
		endif
		call Clear_cmd_window()
		redraw

	endwhile


	" This sections has the potential to force the cmd window way up the
	" screen.  I don't know what the right way is to lower it back to it's
	" old position, as it usually does automatically.
	"
	" It needs a redraw, but doesn't help to put it here:
	let s = &cmdheight
	set cmdheight=2
	redraw
	let &cmdheight = s

	return matched

endfunction



" Pick_buf, search/find functions                                   }}}
" ----------------------------------------------------------------------



" ----------------------------------------------------------------------
" Help functions                                                   {{{

function! WinWalker_help_extract()

	try
		silent 1 new
		exe "silent read " . s:This_script_fname
		silent 1,/^" Help Start:/ d
		silent /^" Help End:/,$ d
		silent 1,$ s/^"//
	catch
		echomsg 'WinWalker_help_extract() failed read/clean.'
		return
	endtry

	let pathlist = [ fnamemodify( s:This_script_fname, ':h' ) . '/..' ]
	let pathlist += split( &runtimepath, ', *' )

	let cmd = ''
	for docpath in pathlist
		let docpath = docpath . '/' . 'doc'
		if isdirectory( docpath )
			let cmd = 'silent write! ' . docpath . '/WinWalker.txt'
			break
		endif
	endfor

	if cmd != ''
		"if search( 'WinWalker.txt\*\s*\a*' . s:version, 'w' )
			"echomsg 'Skipping extract'
		"else
			echomsg "Extracting : " . docpath
			exe cmd
			let cmd = 'silent helptags ' . docpath
			exe cmd
		"endif
	endif

	silent bwipeout!

endfunction


" Don't re-extract if the version hasn't changed, since it can cause
" debugging problems since it overwrites any user changes to the .txt
" file, see switch inside:

if g:WinWalker_debug =~ 'help_extract'
	call WinWalker_help_extract()
endif




aug WinWalker_help
	au!
	au BufEnter * call WinWalker_help_BufEnter()
	au BufLeave * call WinWalker_help_BufLeave()
aug END



function! WinWalker_help_BufEnter()
	if &filetype != 'help' | return | endif

	if !exists( 'b:save_help_foldenable' )
		call WinWalker_save_help_fold_opts()
	endif

	if !exists( 'b:save_other_foldenable' )
		call WinWalker_save_other_fold_opts()
	endif

	call WinWalker_help_init()

	call WinWalker_restore_help_fold_opts()
endfunction



function! WinWalker_help_BufLeave()
	if &filetype != 'help' | return | endif
	call WinWalker_save_help_fold_opts()
	try
		" Throw away problem with the extract function:
		call WinWalker_restore_other_fold_opts()
	catch
	endtry
endfunction



function! WinWalker_help_init()
	nnoremap <buffer> <silent> <c-]> :call WinWalker_help_tag_trap( "\<c-]>" )<CR>
	" I don't understand this magic:  why does \\<  work for special keys, and
	" \<  doesn't.  There are some other weird side effects, but it seems to
	" work.
	nnoremap <buffer> <silent> <2-leftmouse> :call WinWalker_help_tag_trap( "\\<2-leftmouse>" )<CR>
	nnoremap <buffer> <silent> <c-leftmouse> :call WinWalker_help_tag_trap( "\\<c-leftmouse>" )<CR>
	nnoremap <buffer> <silent> g<leftmouse> :call WinWalker_help_tag_trap( "g\\<leftmouse>" )<CR>
endfunction



function! WinWalker_help_tag_trap( key )
	let tag = expand("<cword>")
	let contents = getline( line(".") )
	let col = col(".")
	let idx = col - 1
	let word = matchstr( contents, '\S\+', idx )
	if word =~ '|\W*'
		let idx = strridx( contents, '|', idx - 1 )
		let word = matchstr( contents, '[^|		]\+', idx )
		let word = substitute( word, '|', '', 'g' )
		if strlen( word ) > 0
			let tag = word
		endif
	endif

	if tag =~ '^call__'
		let func = substitute( tag, '^call__', '', '' )
		call {func}()
	elseif tag == 'Turn_folding_on'
		call WinWalker_set_help_folding()
	elseif tag == 'Turn_folding_off'
		call WinWalker_unset_help_folding()
	else
		"exe 'silent normal! ' . a:key
		try
			exe 'silent tag ' . tag
		catch
			echohl Error
			echomsg v:errmsg
			echohl None
		endtry
	endif
endfunction



function! WinWalker_save_help_fold_opts()
	let b:save_help_foldenable	= &foldenable
	let b:save_help_foldopen	= &foldopen
	let b:save_help_foldclose	= &foldclose
	let b:save_help_foldmarker	= &foldmarker
	let b:save_help_foldmethod	= &foldmethod
endfunction


function! WinWalker_save_other_fold_opts()
	let b:save_other_foldenable	= &foldenable
	let b:save_other_foldopen	= &foldopen
	let b:save_other_foldclose	= &foldclose
	let b:save_other_foldmarker	= &foldmarker
	let b:save_other_foldmethod	= &foldmethod
endfunction



function! WinWalker_restore_help_fold_opts()
	let &foldenable	= b:save_help_foldenable
	let &foldopen	= b:save_help_foldopen
	let &foldclose	= b:save_help_foldclose
	let &foldmarker	= b:save_help_foldmarker
	let &foldmethod	= b:save_help_foldmethod
endfunction


function! WinWalker_restore_other_fold_opts()
	let &foldenable	= b:save_other_foldenable
	let &foldopen	= b:save_other_foldopen
	let &foldclose	= b:save_other_foldclose
	let &foldmarker	= b:save_other_foldmarker
	let &foldmethod	= b:save_other_foldmethod
endfunction



function! WinWalker_set_help_folding()
	set foldenable
	set foldopen=all
	set foldclose=all
	set foldmarker={{{,}}}
	set foldmethod=marker
endfunction



function! WinWalker_unset_help_folding()
	set foldenable!
	set foldopen=
	set foldclose=
	"set foldmarker={{{,}}}
	"set foldmethod=marker
endfunction



"function! WinWalker_help_extract_tmp_buf()
"
"	" Grrrr!  No local mode for these:
"	let s:restore_foldopen = &foldopen
"	let s:restore_foldclose = &foldclose
"
"	" Use new then edit, as a simple way to deal with an existing hidden 
"	" help buffer.
"	silent new
"	silent edit! _WinWalker Help_
"	silent setlocal modifiable
"	silent setlocal noreadonly
"	silent setlocal nofoldenable
"	" Refreshing each time is mostly useful for development, but doesn't hurt
"	" in general:
"	silent 1,$d
"	exe "silent read " . s:This_script_fname
"	silent 1,/^" Help Start:/ d
"	silent /^" Help End:/,$ d
"	silent 1,$ s/^"//
"
"	silent 1
"
"	silent setlocal nomodifiable
"	silent setlocal readonly
"	silent setlocal buftype=nofile
"	silent setlocal filetype=help
"	silent setlocal noswapfile
"	silent setlocal foldenable
"	silent set foldopen=hor,jump,mark,search
"	silent set foldclose=all
"	silent setlocal foldmarker={{{,}}}
"	silent setlocal foldmethod=marker
"	"silent setlocal foldtext=RH_foldtext1()
"	"silent setlocal syntax=rh
"endfunction 


" End Help functions                                                   }}}
" ----------------------------------------------------------------------



" ----------------------------------------------------------------------
" Jump funcs                                                        {{{
"

if !exists( 's:Jump_list' )
	let s:Jump_list = []
endif
if !exists( 's:Jump_list_idx' )
	let s:Jump_list_idx = 0
endif
if !exists( 's:Keep_jumps' )
	let s:Keep_jumps = 0
endif





function! s:Get_curr_location()
	let loc =  {
				\ "tabnr"	: tabpagenr(),
				\ "winnr"	: winnr(),
				\ "bufnr"	: bufnr("%"),
				\ "winid"	: s:winid(),
				\ "line"	: line("."),
				\ "col"		: col("."),
				\ }
	return loc
endfunction





" Create a window id that will be unique across tabs:
"
function! s:winid()
	if !exists( 'w:winid' )
		let w:winid = s:winid_counter
		let s:winid_counter += 1
	endif
	return w:winid
endfunction




function! s:Jump_list_fix()
	if len( s:Jump_list ) < 1
		call add( s:Jump_list, s:Get_curr_location() )
		let s:Jump_list_idx = 0
		return -1
	elseif s:Jump_list_idx > ( len( s:Jump_list )  - 1 ) 
		let s:Jump_list_idx = ( len( s:Jump_list )  - 1 )
	endif
endfunction




function! s:Locations_duplicates( loc1, loc2 )
	if	len( a:loc1 ) < 6 || len( a:loc2 ) < 6
		return 0
	endif
	if	a:loc1.tabnr	== a:loc2.tabnr
	\&&	a:loc1.winnr	== a:loc2.winnr
	\&&	a:loc1.winid	== a:loc2.winid
		return 1
	endif
	return 0
endfunction



" Return idx, or -1 on error
"
function! s:Jump_list_clean_and_go( idx )
	let idx = a:idx

	if len( s:Jump_list ) < 1 
		call s:Jump_list_fix()
		return -1
	elseif len( s:Jump_list ) == 1
		let loc = s:Jump_list[ 0 ]
		if s:Go_to_location( loc ) =~ '\(tabnr\|winnr\|winid\)'
			call s:Jump_list_fix()
			return -1
		endif
	endif

	if idx < 0 || idx > ( len( s:Jump_list ) - 1 )
		return -1
	endif

	let loc = s:Jump_list[ idx ]
	let old_loc = s:Get_curr_location()
	let ret_loc = s:Go_to_location( loc )
	if s:Go_to_location( loc ) =~ '\(tabnr\|winnr\|winid\)'
		call remove( s:Jump_list, idx )
		if idx <= s:Jump_list_idx
			let s:Jump_list_idx -= 1
		endif
		call s:Go_to_location( old_loc )
		if g:WinWalker_debug =~ 'jumplist' 
			echomsg 'Jump_list_clean: mismatched ' . ret_loc . ', removing idx# ' . idx
		endif
		call s:Jump_list_fix()
		return -1
	endif

	return idx

endfunction




" Takes a dict type, with { tab, win, line, col, ... }
"
function! s:Go_to_location( loc )

	if	len( a:loc ) < 6
		return 'bad_loc'
	endif


	if a:loc.tabnr > tabpagenr("$")
		if g:WinWalker_debug =~ 'jumplist' 
			echomsg "Go_to_location : no such tabnr" . a:loc.tabnr
		endif
		return 'tabnr'
	endif
	exe 'silent tabnext ' . a:loc.tabnr

	if a:loc.winnr > winnr("$")
		if g:WinWalker_debug =~ 'jumplist' 
			echomsg "Go_to_location : no such winnr" . a:loc.winnr
		endif
		return 'winnr'
	endif
	exe 'silent ' . a:loc.winnr . 'wincmd w '

	if bufnr("%") != a:loc.bufnr 
		if g:WinWalker_debug =~ 'jumplist' 
			echomsg "Go_to_location : " . bufnr("%") . '!= ??' .  a:loc.bufnr 
		endif
		return 'bufnr'
	endif

	if s:winid() != a:loc.winid
		if g:WinWalker_debug =~ 'jumplist' 
			echomsg "Go_to_location : " . s:winid() . ' != ' .  a:loc.winid 
		endif
		return 'winid'
	endif

	if a:loc.line && a:loc.col
		call cursor( a:loc.line, a:loc.col )
	endif

	return 'success'

endfunction




function! s:Jump_list_forward()
	call s:Jump_list_fix()

	if ( s:Jump_list_idx + 1 ) > ( len( s:Jump_list ) - 1 )
		let s:infomsg .= ' At end of jump list'
		return
	endif

	let s:Keep_jumps = 1

	let idx = s:Jump_list_idx + 1

	" Get the next idx, and clear until an idx matching
	"
	while len( s:Jump_list ) > 0

		let idx = s:Jump_list_clean_and_go( s:Jump_list_idx + 1 )

		if idx > -1 
		\ && s:Locations_duplicates( s:Jump_list[ s:Jump_list_idx ], s:Jump_list[ idx ] )
			call remove( s:Jump_list, idx )
			if idx < ( len( s:Jump_list ) - 1 )
				continue
			endif
		endif

		if idx > ( len( s:Jump_list ) - 1 )
			let s:Jump_list_idx = ( len( s:Jump_list ) - 1 )
			let s:infomsg .= ' At end of jump list'
			let s:Keep_jumps = 0
			call s:Jump_list_fix()
			return -1
		endif

		if idx > -1  | break | endif	" success

	endwhile

	let s:Jump_list_idx = idx
	let s:Keep_jumps = 0

	let loc = s:Jump_list[ s:Jump_list_idx ]
	let s:infomsg .= ' At jump # ' . s:Jump_list_idx
	if g:WinWalker_debug =~ 'jumplist' 
		let s:infomsg .= ', t' . loc.tabnr 
						\ . ', w' . loc.winnr
						\ . ', name=' . fnamemodify( bufname( loc.bufnr ), ':t' )
	endif

	if s:Jump_list_idx == ( len( s:Jump_list ) - 1 )
		let s:infomsg .= ',END '
	endif

endfunction



function! s:Jump_list_backward()

	call s:Jump_list_fix()

	if s:Jump_list_idx == 0
		let s:infomsg .= ' At start of jump list'
		return
	endif

	let s:Keep_jumps = 1

	let idx_old = s:Jump_list_idx

	while len( s:Jump_list ) > 1

		let idx = s:Jump_list_idx - 1

		let ret_idx = s:Jump_list_clean_and_go( idx )
		
		if ret_idx > 0 && ret_idx != ( s:Jump_list_idx - 1 )
			" a jump has been removed, and s:Jump_list_idx has changed
			continue
		endif

		if ret_idx > -1
		\ && s:Locations_duplicates( s:Jump_list[ s:Jump_list_idx ], s:Jump_list[ ret_idx ] )
			call remove( s:Jump_list, ret_idx )
			let s:Jump_list_idx -= 1
			continue
		endif

		if idx == 0 && ret_idx < 0
			let s:infomsg .= ' At start of jump list'
			let s:Keep_jumps = 0
			call s:Jump_list_fix()
			return -1
		endif

		if ret_idx > -1  
			break 
		endif	" success

	endwhile

	call s:Jump_list_fix()

	if exists('ret_idx')
		let s:Jump_list_idx = ret_idx
	endif


	let loc = s:Jump_list[ s:Jump_list_idx ]
	let s:infomsg .= ' At jump # ' . s:Jump_list_idx
	if g:WinWalker_debug =~ 'jumplist' 
		let s:infomsg .= ', t' . loc.tabnr 
						\ . ', w' . loc.winnr
						\ . ', name=' . fnamemodify( bufname( loc.bufnr ), ':t' )
	endif

	let s:Keep_jumps = 0

endfunction







function! WinWalker_show_jump_list()
	echon "\n"
	echon "WinWalker jump list.\n"

	let fmt = '%1s %-5.5s %-5.5s %-5.5s %-5.5s %-5.5s %s' . "\n"

	echohl DiffChange
	echon printf( fmt, '', 'Jump', 'Tab', 'Win', 'WinId', 'Buf', 'Name' )
	echohl NONE

	hi Underlinedx cterm=underline term=underline gui=underline
	let i = len( s:Jump_list ) - 1
	while i >= 0

		if i == s:Jump_list_idx
			echohl Tag
		elseif i % 2 == 0
			echohl Underlinedx
		else
			echohl NONE
		endif
		echon printf( fmt
					\ , i == s:Jump_list_idx ? '>' : ' ' 
					\ , i
					\ , s:Jump_list[i].tabnr
					\ , s:Jump_list[i].winnr
					\ , s:Jump_list[i].winid
					\ , s:Jump_list[i].bufnr
					\ , fnamemodify( bufname(s:Jump_list[i].bufnr), ':t' )
					\ )

		let i -= 1

	endwhile

	echohl NONE

	let inp = input( "\nEnter {#} or {d}elete [{# list/range}] : " )
	let inp = substitute( inp, '\s*-\s*', '-', 'g' )
	let l = split( inp, '[ ,]\+' )

	if len( l ) < 1
		return
	endif

	if l[0] =~ '^d' 
		call remove( l, 0 )
		if len( l ) == 0
			call remove( s:Jump_list, s:Jump_list_idx )
		else
			let del = {}
			for jumpnr in range( len( s:Jump_list ) )
				let del[ jumpnr ] = 0
			endfor
			for jumpnr in l
				let start = jumpnr
				let stop = jumpnr
				let l1 = split( jumpnr, '-' )
				if len( l1 ) == 2
					let start = l1[0]
					let stop = l1[1]
				elseif len( l1 ) > 2
					let s:errmsg .= 'Invalid jump #' . jumpnr
					return
				endif
				for jumpnr1 in range( start, stop )
					try
						let del[ jumpnr1 ] = 1
					catch
					endtry
				endfor
			endfor
			let l = []
			for jumpnr in range( len( s:Jump_list ) )
				if del[ jumpnr ] != 1
					call add( l, s:Jump_list[ jumpnr ] )
				endif
			endfor
			let s:Jump_list = deepcopy( l )
		endif
		call s:Jump_list_fix()

		call WinWalker_show_jump_list()

		call s:Jump_list_fix()
		call s:Go_to_location( s:Jump_list[ s:Jump_list_idx ] )
		return
	else
		let jumpnr = str2nr( inp )
	endif


	if inp == ''
	elseif jumpnr >= 0 && jumpnr <= ( len( s:Jump_list ) - 1 )

		let s:infomsg .= ' Jumping to ' . string( s:Jump_list[ jumpnr ] )

		let s:Keep_jumps = 1

		if s:Jump_list_clean_and_go( jumpnr ) > -1
			let s:Jump_list_idx = jumpnr
			call WinWalker_BufEnter()
		else
			let s:errmsg .= 'Failed to jump to #' . jumpnr
		endif

		let s:Keep_jumps = 0
		redraw
	else
		let s:errmsg .= 'Invalid jump #' . jumpnr
	endif

endfunction






" Jump funcs                                                        }}}
" ----------------------------------------------------------------------



" ----------------------------------------------------------------------
" Autocommand functions                                             {{{
"



aug WinWalker_aug
	au!
	au BufEnter * call WinWalker_BufEnter()
	au BufLeave * call WinWalker_BufLeave()
	au WinEnter * call WinWalker_WinEnter()
	au WinLeave * call WinWalker_WinLeave()
	au TabEnter * call WinWalker_TabEnter()
	au TabLeave * call WinWalker_TabLeave()

	" This interrupts getchar(), and then resets the event, and loops:
	"au CursorHold * match
aug end




function! WinWalker_TabEnter()
	if g:WinWalker_debug =~ 'events' | echomsg 'TabEnter @%=' . @% . ', afile=' . expand("<afile>") | endif

	for bufnr in range( 1, bufnr("$") )
		if getbufvar( bufnr, 'set_follow' ) ==? 'FOLLOW'
					\ && bufwinnr( bufnr ) < 1
			" Tricky:  has to be exactly this way:
			exe 'silent new ' . bufname( bufnr )

			" Jump back to the other window for the case when tabnew
			" will try to open a default buffer in the current window,
			" thus replacing the above desired buffer.
			silent wincmd p
		endif
	endfor
	let s:Need_window_update = 1
endfunction





function! WinWalker_TabLeave()
	if g:WinWalker_debug =~ 'events' | echomsg 'TabLeave @%=' . @% . ', afile=' . expand("<afile>") | endif

	"for winnr in range( 1, winnr("$") )
	for bufnr in range( 1, bufnr("$") )

		if !bufexists( bufnr )  | continue | endif
		if getbufvar( bufnr, 'set_follow' ) != 'FOLLOW' | continue | endif
		let winnr = bufwinnr( bufnr )
		if winnr < 1  | continue | endif

		try
			let s = 'silent ' . winnr . ' wincmd w'
			exe s
			"echomsg 'buf=' . bufname( winbufnr(winnr) ) . ', ' . s
		catch
			echomsg 'TabLeave: ' . v:errmsg
		finally
			if winnr == winnr()
				silent hide
			endif
		endtry

	endfor

endfunction





function! WinWalker_BufEnter()
	if g:WinWalker_debug =~ 'events' 
		let s:infomsg .= " BFE"
		echomsg 'BufEnter @%=' . @% . ', afile=' . expand("<afile>") 
	endif

	call s:WinWalker_resize_fixed()

	if exists( 's:O_temp_win' ) | unlet s:O_temp_win | endif

	if s:Keep_jumps	
		if g:WinWalker_debug =~ 'jumplist' 
			echomsg 'bufenter returning because of keep jumps list '
		endif
		return 
	endif

	let elem = s:Get_curr_location() 

	" Remove build-up of next/prev couples between two windows:
	if len( s:Jump_list ) > 2
		if s:Locations_duplicates( s:Jump_list[ s:Jump_list_idx ], s:Jump_list[ s:Jump_list_idx - 2] )
			call remove( s:Jump_list, s:Jump_list_idx - 2 )
			let s:Jump_list_idx -= 1
		endif
	endif

	if len( s:Jump_list ) < 1

		if g:WinWalker_debug =~ 'jumplist' 
			echomsg 'bufenter jump list empty, adding'
		endif
		call s:Jump_list_fix()

	elseif s:Jump_list_idx >= ( len( s:Jump_list ) - 1 )

		if g:WinWalker_debug =~ 'jumplist' 
			echomsg 'bufenter growing jump list '
		endif
		call add( s:Jump_list, elem )
		let s:Jump_list_idx = ( len( s:Jump_list ) - 1 )

	elseif s:Jump_list_idx < ( len( s:Jump_list ) - 1 )
		"
		" Means that that we're somewhere in the jumplist via ^I or ^O

		if				tabpagenr()	== s:Jump_list[ s:Jump_list_idx ].tabnr
					\ &&	winnr()		== s:Jump_list[ s:Jump_list_idx ].winnr
					\ &&	s:winid()	== s:Jump_list[ s:Jump_list_idx ].winid
			" Haven't deviated from list.
			if g:WinWalker_debug =~ 'jumplist' 
				echomsg 'bufenter keep existing jump # ' . s:Jump_list_idx 
			endif
			return

		endif

		if g:WinWalker_debug =~ 'jumplist' 
			echomsg 'bufenter inserting jump # ' . s:Jump_list_idx 
		endif

		let loc = s:Get_curr_location()

		if s:Locations_duplicates( loc, s:Jump_list[ s:Jump_list_idx ] )
			" is a duplicate
		else
			let s:Jump_list_idx += 1
			call extend( s:Jump_list, [ s:Get_curr_location() ], s:Jump_list_idx )
		endif

	else " s:Jump_list_idx  not in proper condition
		if g:WinWalker_debug =~ 'jumplist' 
			echomsg 'bufenter:  bad case'
		endif
	endif


	if len( s:Jump_list ) > 1000
		call remove( s:Jump_list, 0 )
		let s:Jump_list_idx -= 1
		" Should never happen:
		let s:Jump_list_idx = max( [ s:Jump_list_idx, 0 ] )
	endif

endfunction



function! WinWalker_BufLeave()
	if g:WinWalker_debug =~ 'events' | echomsg 'BufLeave @%=' . @% . ', afile=' . expand("<afile>") | endif
	if exists( 's:O_temp_win' ) | unlet s:O_temp_win | endif

	if s:Keep_jumps	| return | endif

	" Don't know what to do about this yet, if anything:
	"let afile = expand("<afile>")
	"if afile != @%
		"let s:errmsg .= ' Warning, BufLeave, afile('.afile.')!=@%('. @% .')'
	"endif

	if !exists( 'w:Jump_list_valid' )
		let w:Jump_list_valid = []
	endif

	call s:Jump_list_fix()

	let s:Jump_list[ s:Jump_list_idx ] = s:Get_curr_location()

"	let loc = s:Jump_list[ s:Jump_list_idx ]
"
"	if		loc.tabnr == tabpagenr()
"	\ &&	loc.winnr == winnr()
"
"		let loc.bufnr	= bufnr("%")
"		let loc.line	= line(".")
"		let loc.col		= col(".")
"
"		let s:Jump_list[ s:Jump_list_idx ] = loc
"	endif

endfunction




if !exists( 's:winid_counter' )
	let s:winid_counter = 0
endif

function! WinWalker_WinEnter()
	if g:WinWalker_debug =~ 'events' 
		let s:infomsg .= " WE"
		echomsg 'WinEnter @%=' . @% . ', afile=' . expand("<afile>") 
	endif

	call s:winid()

	call s:WinWalker_do_attachments()

	call s:WinWalker_resize_fixed()	" Do this after moving the windows around
endfunction



function! s:WinWalker_resize_fixed()

	let set_lines = 0
	if exists( 'b:set_fixed_lines' ) 
		let w:set_fixed_lines = b:set_fixed_lines
		let set_lines = b:set_fixed_lines
	elseif exists( 'w:set_fixed_lines' ) 
		let b:set_fixed_lines = w:set_fixed_lines
		let set_lines = w:set_fixed_lines
	endif

	let set_cols = 0
	if exists( 'b:set_fixed_cols' ) 
		let w:set_fixed_cols = b:set_fixed_cols
		let set_cols = b:set_fixed_cols
	elseif exists( 'w:set_fixed_cols' ) 
		let b:set_fixed_cols = w:set_fixed_cols
		let set_cols = w:set_fixed_cols
	endif

	if bufname("%") == ''
		unlet! b:set_fixed_lines
		unlet! b:set_fixed_cols
	endif


	if set_lines && winheight( winnr() ) != set_lines
		" only do if something is up or down, since otherwise, it will
		" confuse cmdheight
		if GetWinAdjacent( 'up' ) || GetWinAdjacent( 'down' ) 
			exe 'silent resize ' . set_lines
			let s:need_window_update = 1
		endif
	endif
	if set_cols && winwidth( winnr() ) != set_cols
		exe 'silent vertical resize ' . set_cols
		let s:need_window_update = 1
	endif
endfunction



let s:dir_names = 
			\{
			\ 'k'		: 'up',
			\ 'j'		: 'down', 
			\ 'h'		: 'left', 
			\ 'l'		: 'right', 
			\ 'up'		: 'k', 
			\ 'down'	: 'j', 
			\ 'left'	: 'h', 
			\ 'right'	: 'l'
			\ }



let s:need_window_update = 0

function! s:WinWalker_do_attachments()
	if !exists( 'b:attach_to' ) | return | endif

	let s:Keep_jumps = 1

	let bufnr_starting = bufnr("%")

	"let s:infomsg .=  ', ' . bufname( bufnr_starting ) . '=' . string( b:attach_to )

	let counter = 0
	while counter < 10
		let counter += 1
		let need_retry = 0


		" ----------------------------------------------------------------------
		"
		"  Attachments to other windows
		"
		for dir in keys ( b:attach_to )
			if b:attach_to[ dir ] == '[Wall]' | continue | endif

			let other_winnr = GetWinAdjacent( dir )

			if bufname( winbufnr( other_winnr ) ) != b:attach_to[ dir ]

				let bufnr_attach_to =  bufnr( b:attach_to[ dir ] )
				let winnr_attach_to =  bufwinnr( bufnr_attach_to )
				if winnr_attach_to < 1
					"call remove( b:attach_to, dir )
					break
				endif
				exe 'silent ' . winnr_attach_to . ' wincmd w'

				let save_splitbelow = &splitbelow
				let save_splitright = &splitright
				let save_bufhidden = &bufhidden
				set bufhidden=hide
				let save_eventignore = &eventignore

				if		dir == 'up'
					set splitbelow
					new '_kill_this_'
				elseif	dir == 'down'
					set nosplitbelow
					new '_kill_this_'
				elseif	dir == 'left'
					set splitright
					vnew '_kill_this_'
				elseif	dir == 'right'
					set nosplitright
					vnew '_kill_this_'
				endif

				let &splitbelow = save_splitbelow
				let &splitright = save_splitright

				call s:Exchange_win( winnr(), bufwinnr( bufnr_starting ) ) 

				let winnr_other = bufwinnr( bufnr("_kill_this_" ))
				if winnr_other > 0
					exe 'silent ' . winnr_other . ' wincmd w'
					silent bwipeout
				else
					let s:errmsg .= ' Clean up failed after do_attachment'
				endif

				exe 'silent ' . bufwinnr( bufnr_starting ) . ' wincmd w'

				let &eventignore = save_eventignore
				let &bufhidden = save_bufhidden


			else  " other buffer is already attach_to buffer

				"
				" Copy buffer constraint var vals from the window
				" attached to:
				"

				let bufnr_attach_to =  bufnr( b:attach_to[ dir ] )

				let follow = getbufvar( bufnr_attach_to, 'set_follow' )
				if follow != ''
					call setbufvar( bufnr_starting, 'set_follow', follow )
				endif

				let cols = getbufvar( bufnr_attach_to, 'set_fixed_cols' )
				let lines = getbufvar( bufnr_attach_to, 'set_fixed_lines' )
				if dir == 'up' || dir == 'down'
					if cols != ''
						call setbufvar( bufnr_starting, 'set_fixed_cols', str2nr( cols ) )
					endif
				endif
				if dir == 'left' || dir == 'right'
					if lines != ''
						call setbufvar( bufnr_starting, 'set_fixed_lines', str2nr( lines ) )
					endif
				endif

			endif " check if other buffer is already attach_to buffer

		endfor
		"  End:  Attachments to other windows
		" ----------------------------------------------------------------------



		" ----------------------------------------------------------------------
		"
		"  Attachments to walls
		"
		for dir in keys ( b:attach_to )
			if b:attach_to[ dir ] != '[Wall]' | continue | endif

			"let s:infomsg .= ', dir ' . dir

			let other_winnr = GetWinAdjacent( dir )

			while other_winnr > 0    " End push until hit wall

				if		( dir == 'up' || dir == 'down' )
							\&&	b:attach_to[ dir ] == '[WALL]'

					if has_key( b:attach_to, 'left' )
								\&&	b:attach_to[ 'left' ] == '[WALL]'
						"let s:infomsg .= ', push right'
						call WinWalker_push_window( other_winnr, 'right' )  
					else
						"let s:infomsg .= ', push left'
						call WinWalker_push_window( other_winnr, 'left' )  
					endif

					let need_retry = 1
					let s:need_window_update = 1

				elseif		( dir == 'left' || dir == 'right' )
							\&&	b:attach_to[ dir ] == '[WALL]'

					if has_key( b:attach_to, 'up' )
								\&&	b:attach_to[ 'up' ] == '[WALL]'
						"let s:infomsg .= ', push down'
						call WinWalker_push_window( other_winnr, 'down' )  
					else
						"let s:infomsg .= ', push up'
						call WinWalker_push_window( other_winnr, 'up' )  
					endif

					let need_retry = 1
					let s:need_window_update = 1
				else

					let s = 'push win('. bufname(winbufnr(other_winnr)) . '}( ' . dir . ') toward ' . other_winnr
					"let s:infomsg .= s
					"echomsg s
					call WinWalker_push_window( dir )  
					let need_retry = 1
					let s:need_window_update = 1
				endif

				let other_winnr = GetWinAdjacent( dir )

			endwhile " End push until hit wall

		endfor
		"
		"  End Attachments to walls
		"
		" ----------------------------------------------------------------------



		if !need_retry  | break | endif

	endwhile

	let s:Keep_jumps = 0

endfunction




function! WinWalker_WinLeave()
	if g:WinWalker_debug =~ 'events' | echomsg 'WinLeave @%=' . @% . ', afile=' . expand("<afile>") | endif
	call WinWalker_BufLeave()
endfunction




" Autocommand functions                                             }}}
" ----------------------------------------------------------------------



" vim7:tw=75:ts=4:sw=4:foldenable:foldmarker={{{,}}}:foldmethod=marker:foldopen=hor,tag,jump,mark,search:noexpandtab

