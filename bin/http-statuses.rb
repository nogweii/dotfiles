#!/usr/bin/env ruby
# frozen_string_literal: true

require "bundler/inline"

gemfile do
  source "https://rubygems.org"
  gem "tty-table"
  gem "pastel"
  gem "tty-link"
  gem "tty-screen"
end

require "tty-table"
require "pastel"
require "tty-link"
require "tty-screen"

# Data structure to hold statuses
Status = Struct.new(:code, :name, :description) do
  def link
    "https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Status/#{code}"
  end
end

STATUSES = [
  Status.new(100, "Continue", "This interim response indicates that the client should continue the request or ignore the response if the request is already finished."),
  Status.new(101, "Switching Protocols", "This code is sent in response to an Upgrade request header from the client and indicates the protocol the server is switching to."),
  Status.new(102, "Processing", "This code was used in WebDAV contexts to indicate that a request has been received by the server, but no status was available at the time of the response."),
  Status.new(103, "Early Hints", "This status code is primarily intended to be used with the Link header, letting the user agent start preloading resources while the server prepares a response or preconnect to an origin from which the page will need resources."),
  Status.new(200, "OK", "The request succeeded. The result and meaning of \"success\" depends on the HTTP method: GET: The resource has been fetched and transmitted in the message body. HEAD: Representation headers are included in the response without any message body. PUT or POST: The resource describing the result of the action is transmitted in the message body. TRACE: The message body contains the request as received by the server."),
  Status.new(201, "Created", "The request succeeded, and a new resource was created as a result. This is typically the response sent after POST requests, or some PUT requests."),
  Status.new(202, "Accepted", "The request has been received but not yet acted upon. It is noncommittal, since there is no way in HTTP to later send an asynchronous response indicating the outcome of the request. It is intended for cases where another process or server handles the request, or for batch processing."),
  Status.new(203, "Non-Authoritative Information", "This response code means the returned metadata is not exactly the same as is available from the origin server, but is collected from a local or a third-party copy. This is mostly used for mirrors or backups of another resource. Except for that specific case, the 200 OK response is preferred to this status."),
  Status.new(204, "No Content", "There is no content to send for this request, but the headers are useful. The user agent may update its cached headers for this resource with the new ones."),
  Status.new(205, "Reset Content", "Tells the user agent to reset the document which sent this request."),
  Status.new(206, "Partial Content", "This response code is used in response to a range request when the client has requested a part or parts of a resource."),
  Status.new(207, "Multi-Status", "Conveys information about multiple resources, for situations where multiple status codes might be appropriate."),
  Status.new(208, "Already Reported", "Used inside a <dav:propstat> response element to avoid repeatedly enumerating the internal members of multiple bindings to the same collection."),
  Status.new(226, "IM Used", "The server has fulfilled a GET request for the resource, and the response is a representation of the result of one or more instance-manipulations applied to the current instance."),
  Status.new(300, "Multiple Choices", "In agent-driven content negotiation, the request has more than one possible response and the user agent or user should choose one of them. There is no standardized way for clients to automatically choose one of the responses, so this is rarely used."),
  Status.new(301, "Moved Permanently", "The URL of the requested resource has been changed permanently. The new URL is given in the response."),
  Status.new(302, "Found", "This response code means that the URI of requested resource has been changed temporarily. Further changes in the URI might be made in the future, so the same URI should be used by the client in future requests."),
  Status.new(303, "See Other", "The server sent this response to direct the client to get the requested resource at another URI with a GET request."),
  Status.new(304, "Not Modified", "This is used for caching purposes. It tells the client that the response has not been modified, so the client can continue to use the same cached version of the response."),
  Status.new(305, "Use Proxy", "Deprecated Defined in a previous version of the HTTP specification to indicate that a requested response must be accessed by a proxy. It has been deprecated due to security concerns regarding in-band configuration of a proxy."),
  Status.new(306, "unused", "This response code is no longer used; but is reserved. It was used in a previous version of the HTTP/1.1 specification."),
  Status.new(307, "Temporary Redirect", "The server sends this response to direct the client to get the requested resource at another URI with the same method that was used in the prior request. This has the same semantics as the 302 Found response code, with the exception that the user agent must not change the HTTP method used: if a POST was used in the first request, a POST must be used in the redirected request."),
  Status.new(308, "Permanent Redirect", "This means that the resource is now permanently located at another URI, specified by the Location response header. This has the same semantics as the 301 Moved Permanently HTTP response code, with the exception that the user agent must not change the HTTP method used: if a POST was used in the first request, a POST must be used in the second request."),
  Status.new(400, "Bad Request", "The server cannot or will not process the request due to something that is perceived to be a client error (e.g., malformed request syntax, invalid request message framing, or deceptive request routing)."),
  Status.new(401, "Unauthorized", "Although the HTTP standard specifies \"unauthorized\", semantically this response means \"unauthenticated\". That is, the client must authenticate itself to get the requested response."),
  Status.new(402, "Payment Required", "The initial purpose of this code was for digital payment systems, however this status code is rarely used and no standard convention exists."),
  Status.new(403, "Forbidden", "The client does not have access rights to the content; that is, it is unauthorized, so the server is refusing to give the requested resource. Unlike 401 Unauthorized, the client's identity is known to the server."),
  Status.new(404, "Not Found", "The server cannot find the requested resource. In the browser, this means the URL is not recognized. In an API, this can also mean that the endpoint is valid but the resource itself does not exist. Servers may also send this response instead of 403 Forbidden to hide the existence of a resource from an unauthorized client. This response code is probably the most well known due to its frequent occurrence on the web."),
  Status.new(405, "Method Not Allowed", "The request method is known by the server but is not supported by the target resource. For example, an API may not allow DELETE on a resource, or the TRACE method entirely."),
  Status.new(406, "Not Acceptable", "This response is sent when the web server, after performing server-driven content negotiation, doesn't find any content that conforms to the criteria given by the user agent."),
  Status.new(407, "Proxy Authentication Required", "This is similar to 401 Unauthorized but authentication is needed to be done by a proxy."),
  Status.new(408, "Request Timeout", "This response is sent on an idle connection by some servers, even without any previous request by the client. It means that the server would like to shut down this unused connection. This response is used much more since some browsers use HTTP pre-connection mechanisms to speed up browsing. Some servers may shut down a connection without sending this message."),
  Status.new(409, "Conflict", "This response is sent when a request conflicts with the current state of the server. In WebDAV remote web authoring, 409 responses are errors sent to the client so that a user might be able to resolve a conflict and resubmit the request."),
  Status.new(410, "Gone", "This response is sent when the requested content has been permanently deleted from server, with no forwarding address. Clients are expected to remove their caches and links to the resource. The HTTP specification intends this status code to be used for \"limited-time, promotional services\". APIs should not feel compelled to indicate resources that have been deleted with this status code."),
  Status.new(411, "Length Required", "Server rejected the request because the Content-Length header field is not defined and the server requires it."),
  Status.new(412, "Precondition Failed", "In conditional requests, the client has indicated preconditions in its headers which the server does not meet."),
  Status.new(413, "Content Too Large", "The request body is larger than limits defined by server. The server might close the connection or return a Retry-After header field."),
  Status.new(414, "URI Too Long", "The URI requested by the client is longer than the server is willing to interpret."),
  Status.new(415, "Unsupported Media Type", "The media format of the requested data is not supported by the server, so the server is rejecting the request."),
  Status.new(416, "Range Not Satisfiable", "The ranges specified by the Range header field in the request cannot be fulfilled. It's possible that the range is outside the size of the target resource's data."),
  Status.new(417, "Expectation Failed", "This response code means the expectation indicated by the Expect request header field cannot be met by the server."),
  Status.new(418, "I'm a teapot", "The server refuses the attempt to brew coffee with a teapot."),
  Status.new(421, "Misdirected Request", "The request was directed at a server that is not able to produce a response. This can be sent by a server that is not configured to produce responses for the combination of scheme and authority that are included in the request URI."),
  Status.new(422, "Unprocessable Content", "The request was well-formed but was unable to be followed due to semantic errors."),
  Status.new(423, "Locked", "The resource that is being accessed is locked."),
  Status.new(424, "Failed Dependency", "The request failed due to failure of a previous request."),
  Status.new(425, "Too Early", "Indicates that the server is unwilling to risk processing a request that might be replayed."),
  Status.new(426, "Upgrade Required", "The server refuses to perform the request using the current protocol but might be willing to do so after the client upgrades to a different protocol. The server sends an Upgrade header in a 426 response to indicate the required protocol(s)."),
  Status.new(428, "Precondition Required", "The origin server requires the request to be conditional. This response is intended to prevent the 'lost update' problem, where a client GETs a resource's state, modifies it and PUTs it back to the server, when meanwhile a third party has modified the state on the server, leading to a conflict."),
  Status.new(429, "Too Many Requests", "The user has sent too many requests in a given amount of time (rate limiting)."),
  Status.new(431, "Request Header Fields Too Large", "The server is unwilling to process the request because its header fields are too large. The request may be resubmitted after reducing the size of the request header fields."),
  Status.new(451, "Unavailable For Legal Reasons", "The user agent requested a resource that cannot legally be provided, such as a web page censored by a government."),
  Status.new(500, "Internal Server Error", "The server has encountered a situation it does not know how to handle. This error is generic, indicating that the server cannot find a more appropriate 5XX status code to respond with."),
  Status.new(501, "Not Implemented", "The request method is not supported by the server and cannot be handled. The only methods that servers are required to support (and therefore must not return this code) are GET and HEAD."),
  Status.new(502, "Bad Gateway", "This error response means that the server, while working as a gateway to get a response needed to handle the request, got an invalid response."),
  Status.new(503, "Service Unavailable", "The server is not ready to handle the request. Common causes are a server that is down for maintenance or that is overloaded. Note that together with this response, a user-friendly page explaining the problem should be sent. This response should be used for temporary conditions and the Retry-After HTTP header should, if possible, contain the estimated time before the recovery of the service. The webmaster must also take care about the caching-related headers that are sent along with this response, as these temporary condition responses should usually not be cached."),
  Status.new(504, "Gateway Timeout", "This error response is given when the server is acting as a gateway and cannot get a response in time."),
  Status.new(505, "HTTP Version Not Supported", "The HTTP version used in the request is not supported by the server."),
  Status.new(506, "Variant Also Negotiates", "The server has an internal configuration error: during content negotiation, the chosen variant is configured to engage in content negotiation itself, which results in circular references when creating responses."),
  Status.new(507, "Insufficient Storage", "The method could not be performed on the resource because the server is unable to store the representation needed to successfully complete the request."),
  Status.new(508, "Loop Detected", "The server detected an infinite loop while processing the request."),
  Status.new(510, "Not Extended", "The client request declares an HTTP Extension (RFC 2774) that should be used to process the request, but the extension is not supported."),
  Status.new(511, "Network Authentication Required", "Indicates that the client needs to authenticate to gain network access.")
]

def wrap_text(text, width)
  text.gsub(/(.{1,#{width}})(\s+|\Z)/, "\\1\n")
end

width = [TTY::Screen.width, 80].max
pastel = Pastel.new
target_code = ARGV[0]&.to_i
if target_code
  status = STATUSES.find { |s| s.code == target_code }

  if status
    puts pastel.bold.cyan(TTY::Link.link_to("#{status.code} #{status.name}", status.link))
    puts "\n"
    puts wrap_text(status.description, width)
  else
    puts pastel.red("Status code #{target_code} not found.")
    exit 1
  end
else
  # Estimate status column width (longest name is around 35 chars) plus borders/padding
  status_col_width = STATUSES.map { |s| "#{s.code} #{s.name}".length }.max
  desc_width = [width - status_col_width - 8, 20].max

  # Prepare table data
  table_data = []
  STATUSES.each do |s|
    status_text = "#{s.code} #{s.name}"
    wrapped_desc = wrap_text(s.description, desc_width).split("\n")
    # First row with status
    table_data << [status_text, wrapped_desc.shift]
    # Subsequent rows with empty status
    wrapped_desc.each do |line|
      table_data << ["", line]
    end
  end

  header = ["Status", "Description"]
  table = TTY::Table.new(header: header, rows: table_data)
  rendered_table = table.render(:unicode, width: width, resize: true) do |renderer|
    renderer.border.separator = :each_row
    renderer.padding = [0, 1]

    # We let the columns auto-size, but they should adhere to our manual wrapping
    renderer.filter = proc do |val, _row_index, col_index|
      if col_index == 0 # Status column
        if val && !val.empty?
          # Extract code to find the status object for the link
          code = val.to_i
          s = STATUSES.find { |stat| stat.code == code }
          text = val
          if s
            text = TTY::Link.link_to(val, s.link)
          end
          pastel.bold.cyan(text)
        else
          val
        end
      else
        val
      end
    end
  end

  puts rendered_table
end
