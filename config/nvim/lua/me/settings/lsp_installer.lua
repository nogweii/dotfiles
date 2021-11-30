local lsp_installer = require("nvim-lsp-installer")
local servers = require "nvim-lsp-installer.servers"
local server = require "nvim-lsp-installer.server"
local std_installer = require "nvim-lsp-installer.installers.std"
local metadata = require "nvim-lsp-installer._generated.metadata"
local zk_config = require('lspconfig').zk

lsp_installer.settings({
    ui = {
        icons = {
            server_installed = "💾",
            server_pending = "🎁",
            server_uninstalled = "❓"
        }
    }
})

local zk_server = server.Server:new {
    name = "zk",
    root_dir = servers.get_server_install_path("zk"),
    homepage = "https://github.com/mickael-menu/zk/",
    languages = zk_config.document_config.default_config.filetypes,
    installer = std_installer.ensure_executables {
        {
            "zk",
            "zk binary not found in $PATH. Make sure it's installed.",
        },
    },
    default_options = {},
}
servers.register(zk_server)
metadata["zk"] = {
    filetypes = zk_config.document_config.default_config.filetypes
}
