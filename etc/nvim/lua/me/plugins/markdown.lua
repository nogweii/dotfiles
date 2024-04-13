---@type LazySpec[]
return {
  {
    'jakewvincent/mkdnflow.nvim',
    opts = {
      modules = {
        maps = false, -- don't do keymaps, ftplugin/markdown.lua has it handled
        cmp = true,
        yaml = true, -- parse the frontmatter as YAML to configure mkdnflow (just bib files for now)
      },
      links = {
        style = 'markdown',
        -- Ran when I explicit choose to make a link (<CR> on selected text)
        ---@param text string The text selected by me
        ---@return string the name of the file in the link
        transform_explicit = function(text)
          text = text:gsub(' ', '-')
          text = text:lower()
          return text
        end,
      },
      perspective = {
        priority = 'root', -- search for files relative to the root of the notebook/wiki
        root_tell = '.marksman.toml', -- this file is where the root is at
        nvim_wd_heel = true, -- change nvim's dir when navigating files
      },
    },
    ft = { 'markdown', 'rmd', 'md' },
  },

  {
    'lukas-reineke/headlines.nvim',
    opts = function()
      local opts = {}
      for _, ft in ipairs({ 'markdown', 'norg', 'rmd', 'org' }) do
        opts[ft] = {
          headline_highlights = {},
        }
        for i = 1, 6 do
          local hl = 'Headline' .. i
          vim.api.nvim_set_hl(0, hl, { link = 'Headline', default = true })
          table.insert(opts[ft].headline_highlights, hl)
        end
      end
      return opts
    end,
    ft = { 'markdown', 'norg', 'rmd', 'org' },
    config = function(_, opts)
      -- PERF: schedule to prevent headlines slowing down opening a file
      vim.schedule(function()
        require('headlines').setup(opts)
        require('headlines').refresh()
      end)
    end,
  },

  -- Some utility key bindings for editng markdown tables
  {
    'allen-mack/nvim-table-md',
    ft = 'markdown',
  },

  {
    'kiran94/edit-markdown-table.nvim',
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
    cmd = 'EditMarkdownTable',
  },

  {
    'HakonHarnes/img-clip.nvim',
    cmd = { 'PasteImage', 'ImgClipDebug', 'ImgClipConfig' },
    opts = {
      default = {
        prompt_for_file_name = false,
        relative_template_path = false,
        drag_and_drop = {
          -- enable drag and drop in insert mode
          insert_mode = true,
          -- copy images instead of using the original file
          copy_images = true,
          -- download images and save them to dir_path instead of using the URL
          download_images = true,
        },
      },
      filetypes = {
        markdown = {
          url_encode_path = false,
          template = '![$CURSOR]($FILE_PATH)',

          drag_and_drop = {
            download_images = true,
          },
        },

        html = {
          url_encode_path = true,
          template = '<img src="$FILE_PATH" alt="$CURSOR">',
        },

        tex = {
          relative_template_path = false,
          template = [[
\begin{figure}[h]
  \centering
  \includegraphics[width=0.8\textwidth]{$FILE_PATH}
  \caption{$CURSOR}
  \label{fig:$LABEL}
\end{figure}
    ]],
        },

        typst = {
          template = [[
#figure(
  image("$FILE_PATH", width: 80%),
  caption: [$CURSOR],
) <fig-$LABEL>
    ]],
        },

        rst = {
          template = [[
.. image:: $FILE_PATH
   :alt: $CURSOR
   :width: 80%
    ]],
        },

        asciidoc = {
          template = 'image::$FILE_PATH[width=80%, alt="$CURSOR"]',
        },

        org = {
          template = [=[
#+BEGIN_FIGURE
[[file:$FILE_PATH]]
#+CAPTION: $CURSOR
#+NAME: fig:$LABEL
#+END_FIGURE
    ]=],
        },
      },
      -- override options for specific files, dirs or custom triggers
      files = {}, -- file specific options (e.g. "main.md" or "/path/to/main.md")
      dirs = {}, -- dir specific options (e.g. "project" or "/home/user/project")
      custom = {}, -- custom options enabled with the trigger option
    },
  },

  {
    'nfrid/markdown-togglecheck',
    dependencies = { 'nfrid/treesitter-utils' },
    ft = { 'markdown' },
  },

  {
    'ellisonleao/glow.nvim',
    opts = {
      pager = false,
    },
    cmd = 'Glow',
  },
}
