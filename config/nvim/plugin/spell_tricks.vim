" A few tricks/customizations of vim's spell checker

" Don't consider acronyms/abbreviations at least 3 long as spelling errors.
" Includes a trailing 's' at the end, and any numbers as part of the acronym.
syn match NoSpellAcronym '\<\(\u\|\d\)\{3,}s\?\>' contains=@NoSpell

" Don't consider URL-like things as spelling errors
syn match NoSpellUrl '\w\+:\/\/[^[:space:]]\+' contains=@NoSpell
