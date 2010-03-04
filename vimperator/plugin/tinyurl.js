/*
 * This script adds a tinyurl command to Vimperator
 *
 * You can map this script adding this to your .vimperatorrc
 * :map <silent> your_key :exe ":tinyurl "+getBrowser().contentWindow.location.href<CR>
 * @author: fox (fox91 at anche dot no)
 * @version: 0.1
 *
*/

commands.addUserCommand(['tinyurl'],
    "TinyUrl command",
    function(args) {
        if (args.length == 1) {
            let req = Components.classes["@mozilla.org/xmlextras/xmlhttprequest;1"]
                .createInstance(Components.interfaces.nsIXMLHttpRequest);
            req.open('GET', "http://tinyurl.com/api-create.php?url=" + escape(args[0]), true);
            req.onreadystatechange = function (aEvt) {
                if (req.readyState == 4) {
                    if(req.status == 200) {
                        let clip = Components.classes["@mozilla.org/widget/clipboardhelper;1"].
                            getService(Components.interfaces.nsIClipboardHelper);
                        clip.copyString(req.responseText);
                        liberator.echo("TinyUrl: " + req.responseText);
                    }
                    else
                        liberator.echoerr("Error contacting tinyurl.com!\n");
                }
            };
            req.send(null);
        }
        else {
            liberator.echoerr("Please specify one url");
        }
    });
