" ~/.config/nvim/init.vim

" Plugins:
call plug#begin()

Plug 'tpope/vim-surround'

Plug 'itchyny/lightline.vim'

Plug 'NLKNguyen/papercolor-theme'

Plug 'ervandew/supertab'

Plug 'skywind3000/asyncrun.vim'

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }

Plug 'christoomey/vim-tmux-navigator'

Plug 'preservim/vimux'

Plug 'vifm/vifm.vim'

" Plug 'junegunn/seoul256.vim'

Plug 'junegunn/rainbow_parentheses.vim'

Plug 'khaveesh/vim-fish-syntax'

" Plug 'easymotion/vim-easymotion'

Plug 'tidalcycles/vim-tidal'

Plug 'vlime/vlime', {'rtp': 'vim/'}

" Plug 'nvim-lua/plenary.nvim'

" Plug 'nvim-telescope/telescope.nvim'

" Plug 'nvim-treesitter/nvim-treesitter'

call plug#end()

" General mappings:
let mapleader = " "

inoremap kj <esc>
vnoremap kj <esc>
cnoremap kj <C-C>

vnoremap <C-c> "+y
inoremap <C-v> <esc>"+pa

nnoremap <silent> <C-s> :update<CR>
nnoremap <silent> <C-q> :quit<CR>

nnoremap zr zt<C-y>

nnoremap <silent> <leader>h :nohlsearch<CR>

noremap <silent> <leader>s :set spell! spelllang=en_gb,de<CR>

nnoremap <silent> <leader>rs :silent ! nautilus '%:p' &<CR>

tnoremap <esc> <C-\><C-n>

set tabstop=4
set shiftwidth=4

set mouse=a

set ignorecase
set smartcase

set wildmenu

set noshowmode

set splitbelow
set splitright

syntax enable

filetype plugin on

" True colour support:
if (has("nvim"))
	let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif

if (has("termguicolors"))
    set termguicolors
endif

" Inspired by tpope/sensible.vim:
set autoread
set scrolloff=1
set autoindent
set complete-=i
set smarttab
set incsearch
set scrolloff=1
runtime! macros/matchit.vim

let comments = [
	\ ['tex', "% "], 
	\ ['plaintex', "% "], 
	\ ['lisp', ";; "], 
	\ ['tidal', "-- "], 
	\ ['python', "# "], 
	\ ['yaml', "# "], 
	\ ['sh', "# "], 
	\ ['readline', "# "], 
	\ ['i3config', "# "], 
	\ ['conf', "# "], 
	\ ['tmux', "# "], 
	\ ['toml', "# "], 
	\ ['fish', "# "], 
	\ ['vim', '" '], 
	\ ['javascript', "// "],
	\ ['rust', "// "]
	\]

function! CommentOut()
	let chars = ""
	for [type, comment] in g:comments
		if &filetype == type
			let l:chars = comment
		endif
	endfor
	let charslength = strlen(l:chars)
	if chars != ""
		if getline('.')[0:(l:charslength - 1)] == l:chars
			call setline('.', getline('.')[l:charslength:])
			nohlsearch
		else
			call setline('.', l:chars . getline('.'))
			nohlsearch
		endif
	endif
endfunction

vnoremap <silent> # :call CommentOut()<CR>

let maparrows = 0

function! MapArrows()
	if g:maparrows == 0
		unsilent echo "j → gj"
		noremap j gj
		noremap gj j
		noremap <down> gj

		noremap k gk
		noremap gk k
		noremap <up> gk

		let g:maparrows = 1
	else
		unsilent echo "j = j"
		unmap j
		unmap gj
		unmap <down>
		unmap k
		unmap gk
		unmap <up>

		let g:maparrows = 0
	endif
endfunction

noremap <silent> <leader>k :call MapArrows()<CR>

function! s:SmartSplit(...)
" 	if (winwidth(0)*0.5)/winheight(0) >= 1.1
	if (winwidth(0)*0.5)/winheight(0) >= 1.3
		execute "vsplit " . a:1
	else
		execute "split " . a:1
	endif
endfunction

command! -nargs=? -complete=file S :silent call s:SmartSplit(<q-args>)

function! QuickfixToggle()
	if &buftype == "quickfix"
		cclose
	else
		copen
		normal G
	endif
endfunction

nnoremap <silent> <leader>\ :call QuickfixToggle()<CR>

let countwords = 0

function! WordcountToggle()
	if g:countwords == 0
		let g:countwords = 1
	else
		let g:countwords = 0
	endif
	call Wordcount()
	if winwidth(0) - strlen(expand('%:t')) < 10
		let g:countwords = 0
		if &modified
			echo "~ " . g:wordcount . " words"
		else
			echo g:wordcount . " words"
		endif
	endif
endfunction

nnoremap <silent> <leader>w :call WordcountToggle()<CR>

function! Wordcount()
	let filename = expand('%:p')
	if g:countwords == 1
		if expand('%:e') == 'tex'
			let g:wordcount = system("detex -e 'array, eqnarray, equation, picture, verbatim' '" . l:filename . "'" . " | tr -cd '[a-zA-Z0-9 \n]._-' | wc -w")
		else
			let g:wordcount = system("cat '" . l:filename . "'" . " | tr -cd '[a-zA-Z0-9 \n]._-' | wc -w")
		endif
		let g:wordcount = substitute(g:wordcount, '\D', "", "g")
	endif
endfunction

function! Dictionary(term)
	unsilent echo system("dict -d wn '". a:term ."'")
endfunction

nnoremap <silent> <leader>bd :silent call Dictionary(expand('<cword>'))<CR>

command! -nargs=1 Dict :silent call Dictionary(<q-args>)

function! Bibline(searchterm)
	let s:svpos = winsaveview()
	let head = expand('%:p:h')
	let path = expand('%:p')
	let cword = a:searchterm
	if system("grep addbibresource '" . l:path . "'") != ""
		let found = 0
		let biblines = systemlist("grep addbibresource '" . l:path . "'")
		for bibline in l:biblines
			let l:thisbibline = substitute(bibline, "^.*{", "", "")
			let bibfile = substitute(l:thisbibline, "}.*", "", "")
			let l:bibfile = l:head . "/" . l:bibfile
			let bibentry = system("sed -n '/" . l:cword . "/I,/}$/p' '" . l:bibfile . "'")
			let author = substitute(l:bibentry, ".*author = {", "", "")
			let l:author = substitute(l:author, "},.*", "", "")
			if l:author != ""
				let l:found = 1
				unsilent echo l:bibentry
			endif
		endfor
		if l:found == 0
			unsilent echo "No entry found."
		endif
		call winrestview(s:svpos)
	endif
endfunction

noremap <silent> <leader>bb :silent call Bibline(expand('<cword>'))<CR>

command! -nargs=1 Ref :silent call Bibline(<q-args>)

" LaTeX:
function! RunLatexmk()
	let filepath = expand('%:p')
	let tail = expand('%:t')
	let root = expand('%:r')
	let longroot = expand('%:p:r')
	if expand('%:e') == 'tex'
		w
		silent execute "! cp '" . l:filepath . "' ~/texbackup/"
		silent call asyncrun#run("", "", "latexmk -xelatex -synctex=1 -cd -recorder- '" . l:filepath . "'")
		silent execute "! cp '" . l:longroot . ".pdf' ~/texbackup/"
	endif
endfunction

function! OpenEvince()
	if expand('%:e') == 'tex'
" 		silent ! evince '%:r.pdf' &
		silent ! rifle '%:r.pdf' &
	endif
endfunction

function! CreateEnvironment()
	if &filetype == 'tex'
" 	if expand('%:e') == 'tex'
		let indentation = indent('.')
		if l:indentation > 0
			s/\s//g
		endif
		let name = getline('.')
		call setline('.', repeat(" ", l:indentation) . "\\begin{" . l:name . "}")
		call append('.', repeat(" ", l:indentation) . "\\end{" . l:name . "}")
	endif
endfunction

function! TeXfuncs()
	setlocal wrap linebreak

	noremap <silent> <leader>q :call CreateEnvironment()<CR>
	noremap <silent> <leader>e :call OpenEvince()<CR>
	noremap <silent> <leader>l :call RunLatexmk()<CR>
endfunction

autocmd FileType tex call TeXfuncs()

" Markdown:
function! MDview()
	if expand('%:e') == 'md'
		w
		silent ! cp '%:p' ~/texbackup/'%:t'
		silent ! mdview '%:p'
	endif
endfunction

function! Bullet()
	if &filetype == 'markdown'
		if getline('.')[0:1] == "- "
			call setline('.', getline('.')[2:])
			nohlsearch
		elseif getline('.') != ""
			call setline('.', '- ' . getline('.'))
			nohlsearch
		endif
	endif
endfunction

function! MDfuncs()
	noremap <silent> <leader>j :call MDview()<CR>
	noremap <silent> <leader>8 :call Bullet()<CR>

	setlocal wrap linebreak
endfunction

autocmd FileType markdown call MDfuncs()

" Python:
function! PyRun()
	if &filetype == "python"
		let filepath = expand('%:p')
		write
		let g:VimuxHeight = "50"
		if exists('$TMUX')
" 			call VimuxRunCommandInDir("python3", 1)
			call VimuxRunCommand("python3 " . expand('%:p'))
		endif
	endif
endfunction

function! PyFuncs()
	noremap <silent> <F5> :call PyRun()<CR>
	noremap <silent> <leader>j :call PyRun()<CR>
endfunction

autocmd FileType python call PyFuncs()

function! LispFuncs()
	RainbowParentheses
endfunction

autocmd FileType lisp call LispFuncs()

function! EnterFuncs()
	if &filetype == "markdown"
		call MDfuncs()
	elseif &filetype == "python"
		call PyFuncs()
	elseif &filetype == "lisp"
		call LispFuncs()
	endif

	if (&buftype == "" && expand('%:p')[:6] != "zipfile")
		call Wordcount()
		cd %:p:h
	endif

	if (exists('$TMUX') && g:tmuxpanes > 1) || winnr('$') > 1
		nnoremap <silent> <C-h> :TmuxNavigateLeft<CR>
		nnoremap <silent> <C-l> :TmuxNavigateRight<CR>
	else
		nnoremap <C-l> gt
		nnoremap <C-h> gT
	endif
endfunction

autocmd TabEnter * call EnterFuncs()

autocmd BufEnter * call EnterFuncs()

autocmd WinEnter * call EnterFuncs()

let g:tmuxpanes = str2nr(system("tmux display-message -p '#{window_panes}'"))

function! FocusFuncs()
	if exists('$TMUX')
		let g:tmuxpanes = str2nr(system("tmux display-message -p '#{window_panes}'"))
	endif
	
	call EnterFuncs()
endfunction

autocmd FocusGained * call FocusFuncs()

autocmd FocusLost * call FocusFuncs()

autocmd BufWritePost * call Wordcount()

autocmd FileType text setlocal wrap linebreak

autocmd FileType bib setlocal wrap linebreak

function! LinePercent()
	return line('.') * 100 / line('$') . '%'
endfunction

" lightline:
let g:lightline = { 
			\ 'colorscheme': 'default',
			\ 'active' : {
			\ 	'left' : [ [ 'mode', 'paste' ],
			\ 				[ 'readonly', 'filename' ] ],
			\ 	'right' : [ [ 'lineinfo' ],
			\ 				[ 'percent' ],
			\ 				[ 'wordcount', 'filetype' ] ]
			\ },
			\ 'component': {
			\ 	'lineinfo': '%3l:%-2v%<',
			\ },
			\ 'component_function': {
			\ 	'filename': 'LightlineFilename',
			\ 	'percent': 'LightlinePercent',
			\ 	'filetype': 'LightlineFiletype',
			\ 	'fileformat': 'LightlineFileformat',
			\ 	'fileencoding': 'LightlineFileencoding',
			\ 	'wordcount': 'LightlineWordcount',
			\ },
			\ }

function! LightlineFilename()
	let filename = expand('%:t') !=# '' ? expand('%:t') : '[No Name]'
	let modified = &modified ? ' +' : ''
	return filename . modified
endfunction

function! LightlinePercent()
	return (winwidth(0) - strlen(expand('%:t'))) > 28 ? LinePercent() : ''
endfunction

function! LightlineFiletype()
	return (winwidth(0) - strlen(expand('%:t'))) > 35 ? (&filetype !=# '' ? &filetype : 'no ft') : ''
endfunction

function! LightlineWordcount()
	if g:countwords == 1
		if &modified
			return "~ " . g:wordcount
		else
			return g:wordcount
		endif
	else
		return ""
	endif
endfunction

" Colour scheme:
let g:PaperColor_Theme_Options = { 'theme': { 'default': { 'allow_italic': 1 } } }

:colorscheme PaperColor

" FZF:
let g:fzf_action = {
			\ 'enter': 'tab split',
			\ 'ctrl-x': 'split',
			\ 'ctrl-s': 'S',
			\ 'ctrl-t': 'edit',
			\ 'ctrl-v': 'vsplit' }

function! FZFpwd()
	let $FZF_DEFAULT_COMMAND='find . \! \( -type d -path ./.git -prune \) \! -type d \! -name ''*.tags'' -printf ''%P\n'''
	FZF
" 	call fzf#run(fzf#wrap({'source': 'find . \! \( -type d -path ./.git -prune \) \! -type d \! -name ''*.tags'' -printf ''%P\n'''}))
endfunction

nnoremap <silent> <leader>F :call FZFpwd()<CR>

function! FZFcd()
	let $FZF_DEFAULT_COMMAND='find * type f'
	cd
	FZF
" 	call fzf#run(fzf#wrap({'source': 'find * type f'}))
	cd -
endfunction

nnoremap <silent> <leader>f :call FZFcd()<CR>

function! FZFhidden()
	let $FZF_DEFAULT_COMMAND='find . \! \( -type d -path ./.git -prune \) \! -type d \! -name ''*.tags'' -printf ''%P\n'''
	cd
	FZF
" 	call fzf#run(fzf#wrap({'source': 'find . \! \( -type d -path ./.git -prune \) \! -type d \! -name ''*.tags'' -printf ''%P\n'''}))
	cd -
endfunction

nnoremap <silent> <leader>H :call FZFhidden()<CR>

function! RecentFiles()
	silent redir => oldfiles
	old
	redir END

	let l:oldfiles = split(l:oldfiles, '\n')
	let recent = []
	for file in l:oldfiles
		let filepath = substitute(file, ".*: ", "", "")
		if filereadable(l:filepath)
			call insert(l:recent, filepath)
		endif
	endfor
	let recent = reverse(recent)
	call fzf#run(fzf#wrap({'source': l:recent}))
endfunction

nnoremap <silent> <leader>re :silent call RecentFiles()<CR>

function! OpenRanger()
	if exists('$TMUX')
		let g:VimuxHeight = "50"
" 		call VimuxRunCommandInDir("ranger", 0)
		call VimuxRunCommand("ranger")
	endif
endfunction

nnoremap <silent> <leader>ra :call OpenRanger()<CR>

" if exists('$TMUX')
" 	let g:tidal_target = "tmux"
" else
" 	let g:tidal_target = "terminal"
" endif

let g:tidal_target = "terminal"

let g:tidal_no_mappings = 1

nnoremap <C-e> <Plug>TidalParagraphSend

" This is a test.
