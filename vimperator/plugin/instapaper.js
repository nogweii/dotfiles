// PLUGIN_INFO//{{{
var PLUGIN_INFO =
<VimperatorPlugin>
  <name>Instapaper</name>
  <description>Add pages to instapaper for later reading</description>
  <author mail="vimperator@mobocracy.net" homepage="http://mobocracy.net">Blake Matheny</author>
  <version>0.1</version>
  <license>GPL</license>
  <minVersion>1.2</minVersion>
  <maxVersion>2.1</maxVersion>
  <detail><![CDATA[

== Command ==
Usage:
  :rl
    Add current URL to your read later list at instapaper

  ]]></detail>
</VimperatorPlugin>;
//}}}
//

(
 function() {
 commands.addUserCommand(['readlater', 'rl', 'instapaper'],
	 'Add current page to Instapaper',
	 function (args) {
	 	var url = 'https://www.instapaper.com/api/add';
		var params = [];
		var username = liberator.globalVariables.instapaper_username || false;
		var password = liberator.globalVariables.instapaper_password || '';

		if ( username === false ) {
			liberator.echo("Must set instapaper username. 'let g:instapaper_username = \"username\"' in .vimperratorrc");
			return;
		}
		params.push('username=' + encodeURIComponent(username));

		if ( password.length > 0 ) {
			params.push('password=' + encodeURIComponent(password));
		}

		if ( buffer.URL && buffer.URL.length > 0 && buffer.URL != 'about:blank' ) {
			params.push('url=' + encodeURIComponent(buffer.URL));
		} else {
			liberator.echo("No URL found");
			return;
		}

		if ( buffer.title && buffer.title.length > 0 ) {
			params.push('title=' + encodeURIComponent(buffer.title));
		} else {
			params.push('auto-title=1');
		}

		try {
	 		var descr = buffer.getCurrentWord();
	 		if ( descr && descr.length > 0 ) {
				params.push('selection=' + encodeURIComponent(descr));
	 		}
		} catch (e) {
			// Discard exception when there is no current word
		}

		var post_data = params.join('&');

		var xhr = new XMLHttpRequest();
		xhr.open('POST', url, true);
		xhr.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');

		xhr.onreadystatechange = function() {
			// When request is completed check state
			if ( xhr.readyState === 4 ) {
				var msg = '';
				switch (xhr.status) {
					case 201:
						msg = 'This URL has been successfully added to this Instapaper account.';
						break;
					case 400:
						msg = 'Bad request. Probably missing a required parameter, such as url.';
						break;
					case 403:
						msg = 'Invalid username or password.' + post_data;
						break;
					case 500:
						msg = 'The service encountered an error. Please try again later.';
						break;
					default:
						msg = 'Unknown status code returned (' + xhr.status + ')';
						break;
				}
				liberator.echo(msg);
			}
		}

		xhr.send(post_data);
	 }
	 );
 })();
