/**
 * Plugin to call Ubiquity functions from within vimperator
 *
 *
 * Usage:
 *   :ubiquity <ubiquity-command> <args>       runs the command with arguments as if entered in Ubiquity
 *   :ubiquity! <ubiquity-command> <args>      opens the Ubiquity window and enters the command into the textbox 
 *
 *
 * Example usage:
 *   :ubiquity map this
 *   :ubiquity delete
 *   :ubi! translate this to german
 *
 *
 * @author Felix Riedel (felix.riedel@gmail.com)
 * @version 0.2
 *
 * History:
 *      v0.1
 *          - introduced :ubiquity command
 *          - completion with Ubiquity command description
 *      v0.2
 *          - completion includes Ubiquity command icons
 *          - changed commands.add to commands.addUserCommand
 *          - :ubiquity! (bang) open Ubiquity window with arguments as input
 *
 * TODO:
 *      - allow user-defined completion for Ubiquity commands
 */


commands.addUserCommand(['ubi[quity]'],
  'Run Ubiquity commands from within vimperator.',
   function (args) {
        var cmdMgr = gUbiquity.__cmdManager;
        var ubiquityContext = {
            focusedWindow:  window.document.commandDispatcher.focusedWindow,
            focusedElement: window.document.commandDispatcher.focusedElement
        };
        var cmdLine = args.join(' ');

        if (args.bang) { // :ubiquity! 
            var anchor = window.document.getElementById("content");
            gUbiquity.__textBox.value = cmdLine;
            gUbiquity.openWindow(anchor);
        }else{
            cmdMgr.updateInput(cmdLine, ubiquityContext);
            cmdMgr.execute(ubiquityContext);
        }
    },
    // options
    {
        count: true,
        bang: true,
        argCount: '*',
        completer: function (context, args) {
           var completions = [];
           if (args.length <= 1 ) {
               var cmdMgr = gUbiquity.__cmdManager;
               var ubiCommands = cmdMgr.__cmdSource.getAllCommands();
               for each (c in ubiCommands) {
                   var desc = c.description || "";
                   desc = desc.replace(/<[^>]*>/g,''); // strip HTML tags from description
                   completions.push([c.name, desc, c.icon]);
               }
           }else{
               completions.push( ['this','current selection', ''] )
           }

           context.title = ["Ubiquity Commands"];
           context.completions = completions;
           context.keys = { text: 0, description: 1, icon: 2 };
        }
    },
    true // replace Command if already exists
);
