#!/usr/bin/python
# Author: Marcin Szamotulski <mszamot[@]gmail[.]com>
# This script is a part of Automatic TeX Plugin for Vim.
# It can be destributed seprately under General Public Licence ver.3 or higher.

# SYNTAX:
# atp_RevSearch.py <file> <line_nr> [<col_nr>]

# DESRIPTION: 
# This is a python sctipt which implements reverse searching (okular->vim)
# it uses atplib#FindAndOpen() function which finds the vimserver which hosts
# the <file>, then opens it on the <line_nr> and column <col_nr>.
# Column number is an optoinal argument if not set on the command line it is 1.

# HOW TO CONFIGURE OKULAR to get Reverse Search
# Designed to put in okular: 
# 		Settings>Configure Okular>Editor
# Choose: Custom Text Edit
# In the command field type: atp_RevSearch.py '%f' '%l'
# If it is not in your $PATH put the full path of the script.

# DEBUG:
# debug file : /tmp/atp_RevSearch.debug

import subprocess, sys, re

f = open('/tmp/atp_RevSearch.debug', 'w')

# Get list of vim servers.
output = subprocess.Popen(["gvim", "--serverlist"], stdout=subprocess.PIPE)
servers = output.stdout.read().decode()
match=re.match('(.*)(\\\\n)?', servers)
# Get the column (it is an optional argument)
if (len(sys.argv) >= 4 and int(sys.argv[3]) > 0):
	column = str(sys.argv[3])
else:
	column = str(1)

f.write(">>> args "+sys.argv[1]+":"+sys.argv[2]+":"+column+"\n")

if match != None:
	servers=match.group(1)
	server_list=servers.split('\\n')
	server = server_list[0]
	# Call atplib#FindAndOpen()     
	cmd="gvim --servername "+server+" --remote-expr \"atplib#FindAndOpen('"+sys.argv[1]+"','"+sys.argv[2]+"','"+column+"')\""
	subprocess.call(cmd, shell=True)
# Debug:
f.write(">>> output      "+str(servers)+"\n")
if match != None:
	f.write(">>> file        "+sys.argv[1]+"\n>>> line        "+sys.argv[2]+"\n>>> column      "+column+"\n>>> server      "+server+"\n>>> server list "+str(server_list)+"\n>>> cmd         "+cmd+"\n")
else:
	f.write(">>> file        "+sys.argv[1]+"\n>>> line        "+sys.argv[2]+"\n>>> column      "+column+"\n>>> server       not found\n")
f.close()
