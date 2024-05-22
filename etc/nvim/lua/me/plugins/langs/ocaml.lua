-- OCaml language support, and because of that, ReasonML (since reason is simply
-- an alternative syntax injected into ocaml's compiler).

---@type LazySpec[]
return {
  -- ReasonML syntax support
  {
    'reasonml-editor/vim-reason-plus',
    ft = { 'reason', 'merlin' },
  },
}
