local schemas = require('schemastore').json.schemas()

-- Extend the existing AWS CloudFormation schema to support the naming convetion IRL uses
local aws_cfn_schema_index = schemas.index['AWS CloudFormation']
local aws_cfn_schema = vim.deepcopy(schemas[aws_cfn_schema_index])
table.insert(aws_cfn_schema['fileMatch'], 'cloudformation/*.yml')
-- and then overwrite the table with my modified one
schemas[aws_cfn_schema_index] = aws_cfn_schema

local setup_options = {
  settings = {
    yaml = {
      schemaStore = {
        -- disable the schemas shipped with the LSP to use the schemastore plugin
        -- which can be configured in more ways than a basic on/off
        enable = false,
      },
      schemas = schemas,
      -- AWS CloudFormation tags
      customTags = {
        '!And scalar',
        '!And map',
        '!And sequence',
        '!If scalar',
        '!If map',
        '!If sequence',
        '!Not scalar',
        '!Not map',
        '!Not sequence',
        '!Equals scalar',
        '!Equals map',
        '!Equals sequence',
        '!Or scalar',
        '!Or map',
        '!Or sequence',
        '!FindInMap scalar',
        '!FindInMap mappping',
        '!FindInMap sequence',
        '!Base64 scalar',
        '!Base64 map',
        '!Base64 sequence',
        '!Cidr scalar',
        '!Cidr map',
        '!Cidr sequence',
        '!Ref scalar',
        '!Ref map',
        '!Ref sequence',
        '!Sub scalar',
        '!Sub map',
        '!Sub sequence',
        '!GetAtt scalar',
        '!GetAtt map',
        '!GetAtt sequence',
        '!GetAZs scalar',
        '!GetAZs map',
        '!GetAZs sequence',
        '!ImportValue scalar',
        '!ImportValue map',
        '!ImportValue sequence',
        '!Select scalar',
        '!Select map',
        '!Select sequence',
        '!Split scalar',
        '!Split map',
        '!Split sequence',
        '!Join scalar',
        '!Join map',
        '!Join sequence',
      },
    },
  },
}

return setup_options
