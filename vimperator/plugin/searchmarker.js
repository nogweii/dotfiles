/* Search Marker plugin
 * migration of the add-on Search Marker
 * by Mattk(https://addons.mozilla.org/en-US/firefox/addon/1823)
 * 
 * @author  Xie Luyun
 * @version 0.1
 */
CONTAINER_ID = "__searchMarkerContainer"

function ClearMarker() {
    var markerDiv = window._content.document.getElementById(CONTAINER_ID);
    if(markerDiv)
    {
        markerDiv.parentNode.removeChild(markerDiv);
    }
}

function MarkIt() {
    CONTAINER_STYLE = "position:fixed; right:0; top:0; height:100%; width:10px; z-index:999999; background:#eee;";
    
    MARKER_STYLE =  "position:absolute; width:12px; height:1px; " +
                    "border-top:1px solid #69f; border-bottom:1px solid #036; " +
                    "border:1px solid #69f; cursor:pointer; ";

    function getAbsoluteTop(element) {
        return element == null ? 0
            : getAbsoluteTop(element.offsetParent) + element.offsetTop;
    }

    ClearMarker()

    // reinstall them
    try
    {
        var body = window._content.document.body;

        // use container to store markers so it can be styled
        var markerDiv = document.createElement("div");
        markerDiv.setAttribute("id", CONTAINER_ID);
        markerDiv.setAttribute("style", CONTAINER_STYLE);

        // for aesthetics, offset is the space taken by scrollbar buttons
        // we don't want to display a marker where the user can't scroll to
        var scrollBarOffset = self.innerWidth - body.parentNode.clientWidth;

        markerDiv.style.top = scrollBarOffset + "px !important";
        markerDiv.style.bottom = scrollBarOffset + "px !important";
        
        // get selected marker
        var smarker = window._content.getSelection().getRangeAt(0).startContainer.parentNode;

        // find all the entries the doHighlight function leaves behind
        var searchResults = body.getElementsByTagName("span");

        for(var i = 0; i < searchResults.length; ++i)
        {
            if(searchResults[i].getAttribute("highlight") != "Search")
                continue;

            var searchResult = searchResults[i];

            var absoluteTop = getAbsoluteTop(searchResult);
            // get the y location as a percentage
            var markerTop = absoluteTop / (body.parentNode.scrollHeight / 100.0);

            // add marker to container
            var marker = document.createElement("div");
            marker.onclick = function () {
                var str = this.style.top;
                var mtop = str.slice(0, str.length - 2);
                var absTop = mtop * (body.parentNode.scrollHeight / 100.0);
                absTop = Math.round(absTop);
                window._content.scrollTo(0, absTop);
            };
            
            marker.setAttribute("style", MARKER_STYLE + "top:" + markerTop + "% !important");
            if(searchResult == smarker) {
                marker.style.background = "#ff0000";
                marker.style.borderWidth = "3px";
            }

            markerDiv.appendChild(marker);
        }

        if(markerDiv.childNodes.length > 0)
            body.appendChild(markerDiv);
    }
    catch(e)
    {	// apparently it breaks sometimes, who knows why?
    }
}

OldFindAgain = finder.findAgain;
finder.findAgain = function (reverse) {
    OldFindAgain.call(finder, reverse);
    MarkIt()
};

OldClear = finder.clear;
finder.clear = function () {
    OldClear();
    ClearMarker();
};

liberator.registerCallback("submit", modes.SEARCH_FORWARD, function (str) { finder.onSubmit(str); MarkIt(str); });
liberator.registerCallback("submit", modes.SEARCH_BACKWARD, function (str) { finder.onSubmit(str); MarkIt(str); });
