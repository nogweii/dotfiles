(function() {
	try { // Go up one level
		location = location.href.match(/(\w+:\/\/.+?\/)([\w\?\=\+\%\&\-\.]+\/?)$/)[1];
	}
	catch(e) {
		try { // Removing sub-domain
			var s = document.domain.match(/^(?!www\.)\w+\.(.+?)\.([a-z]{2,4})(?:\.([a-z]{2}))?$/);
			var l = s.length;
			location = location.protocol + "//" + s.slice(1, s[l] ? l : l-1).join(".");
		}
		catch(e) {}
	}
})();
