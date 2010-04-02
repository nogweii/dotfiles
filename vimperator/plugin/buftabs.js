// PLUGIN_INFO {{{
let PLUGIN_INFO =
<VimperatorPlugin>
  <name>Buftabs</name>
  <description>Add buffer tabs to the statusline to save space.</description>
  <version>1.1</version>
  <author mail="lucas@tuple-typed.org" homepage="http://tuple-typed.org/">Lucas de Vries (GGLucas)</author>
  <license>WTFPL version 2 (http://sam.zoy.org/wtfpl/)</license>
  <minVersion>2.2</minVersion>
  <maxVersion>2.2</maxVersion>
  <detail><![CDATA[

  == Usage ==
  When the script is loaded it hijacks the statusline to display a list of tabs,
  you can use the "buftabs" option to toggle it on or off.

  == Styling ==
  Use the BufTab and BufTabSelected highlight groups to style the buftabs.
  Make sure you've called the "loadplugins" command before using the highlight
  groups in your vimperatorrc.

  == Length ==
  You can set the max length of a title before it is cut off with the 
  buftabs_maxlength option. It is set to 25 by default.

  ]]></detail>
</VimperatorPlugin>;
// }}}

buftabs = {
    // Update the tabs
    updateUrl: function (url)
    {
        // Get buftabbar
        var btabs = document.getElementById("liberator-statusline-buftabs");
        var urlWidget = document.getElementById("liberator-statusline-field-url");
        var browsers = tabs.getBrowser().browsers;
        var position=0, selpos;

        // Make sure we have an appropriate amount of labels
        while (btabs.childNodes.length > browsers.length)
        {
            btabs.removeChild(btabs.lastChild);
        }

        while (btabs.childNodes.length < browsers.length)
        {
            var label = document.createElement("label");
            btabs.appendChild(label);

            label.onclick = function (ev)
            {
                if (ev.button == 0)
                    tabs.select(this.tabpos);
                else if (ev.button == 1)
                    tabs.remove(tabs.getTab(this.tabpos), 1, false, 0);
            }
        }

        // Create the new tabs
        for (let i=0; i < browsers.length; i++)
        {
            // Create label
            var browser = browsers[i];
            var label = btabs.childNodes[i];

            // Hook on load
            if (browser.webProgress.isLoadingDocument)
            {
                browser._buftabs_label = label;
                browser.contentDocument.addEventListener("load", function ()
                {
                    buftabs.fillLabel(this._buftabs_label, this);
                }, false);
            }

            // Fill label
            label.tabpos = i;
            buftabs.fillLabel(label, browser);

            if (tabs.index() == label.tabpos)
            {
                selpos = [position, label.clientWidth+position];
            }

            position += label.clientWidth;
        }

        // Scroll
        if (selpos[0] < btabs.scrollLeft || selpos[1] > btabs.scrollLeft+btabs.clientWidth)
            btabs.scrollLeft = selpos[0];

        // Show the entire line if possible
        if (btabs.scrollWidth == btabs.clientWidth)
            btabs.scrollLeft = 0;

        // Empty url label
        urlWidget.value = "";
    },

    // Fill a label with browser content
    fillLabel: function(label, browser)
    {
        var maxlength = options.get("buftabs_maxlength").get();
        var tabvalue;

        // Get title
        if (browser.webProgress.isLoadingDocument)
        {
            tabvalue = "Loading...";
        } else {
            tabvalue = browser.contentTitle || "Untitled";
        }

        // Check length
        if (tabvalue.length > maxlength)
            tabvalue = tabvalue.substr(0, maxlength-3)+"...";

        // Bookmark icon
        if (bookmarks.isBookmarked(browser.contentDocument.location.href))
            tabvalue += "\u2764";

        // Brackets and index
        tabvalue = "["+(label.tabpos+1)+"-"+tabvalue+"]";

        label.setAttribute("value", tabvalue);

        // Set the correct highlight group
        if (tabs.index() == label.tabpos)
            label.setAttributeNS(NS.uri, "highlight", "BufTabSelected");
        else
            label.setAttributeNS(NS.uri, "highlight", "BufTab");


    },

    // Create the horizontal box for adding the tabs to
    createBar: function()
    {
        var statusline = document.getElementById("liberator-statusline");
        var buftabs = document.getElementById("liberator-statusline-buftabs");
        var urlWidget = document.getElementById("liberator-statusline-field-url");

        // Only create if it doesn't exist yet
        if (!buftabs)
        {
            buftabs = document.createElement("hbox");
            buftabs.setAttribute("id", "liberator-statusline-buftabs");
            buftabs.setAttribute("flex", "1");
            buftabs.style.overflow = "hidden"

            statusline.insertBefore(buftabs, urlWidget);
        }
    },

    destroyBar: function()
    {
        var statusline = document.getElementById("liberator-statusline");
        var buftabs = document.getElementById("liberator-statusline-buftabs");

        if (buftabs)
            statusline.removeChild(buftabs);
    }
}

/// Attach to events in order to update the tabline
var tabContainer = tabs.getBrowser().mTabContainer;
buftabs._statusline_updateUrl = statusline.updateUrl;

tabContainer.addEventListener("TabMove", function (event) {
    if (options.get("buftabs").get())
        statusline.updateUrl();
}, false);
tabContainer.addEventListener("TabOpen", function (event) {
    if (options.get("buftabs").get())
        statusline.updateUrl();
}, false);
tabContainer.addEventListener("TabClose", function (event) {
    if (options.get("buftabs").get())
        setTimeout(statusline.updateUrl, 0);
}, false);
tabContainer.addEventListener("TabSelect", function (event) {
    if (options.get("buftabs").get())
        statusline.updateUrl();
}, false);

tabs.getBrowser().addEventListener("load", function (event) {
    if (options.get("buftabs").get())
        statusline.updateUrl();
}, false);

/// Initialise highlight groups
highlight.loadCSS(String(<![CDATA[
    BufTab
    BufTabSelected   font-weight: bold;
]]>));

/// Options
options.add(["buftabs"],
        "Control whether to use buftabs in the statusline",
        "boolean", true, 
        {
            setter: function (value)
            {
                if (value)
                {
                    buftabs.createBar();
                    buftabs.updateUrl(null);

                    statusline.updateUrl = buftabs.updateUrl;
                } else {
                    buftabs.destroyBar();
                    statusline.updateUrl = buftabs._statusline_updateUrl;
                    statusline.update();
                }

                return value;
            },

            completer: function (context)
            [
                [false, "Don't show buftabs, show the url"],
                [true, "Show buftabs"]
            ],

            validator: Option.validateCompleter
        });

options.add(["buftabs_maxlength"],
        "Max length of an entry in the buftabs list",
        "number", "25", 
        {
            setter: function (value)
            {
                buftabs.updateUrl();
                return value;
            }
        });
