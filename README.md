# trivial-nntp
Common lisp tools for connecting to and crawling around NNTP servers.  It uses usocket and cl+ssl, and therefore handles simple or SSL-encrypted NNTP connections.

This is a minimalistic effort; however watch this:
    
     CL-USER> (in-package :tnntp)
     TNNTP> (command "HELP")
     "100 Legal commands^M"
     1
     TNNTP> (rlist)
	 "  ARTICLE [message-ID|number]"
	 "  AUTHINFO USER name|PASS password|GENERIC program [argument ...]"
	 "  BODY [message-ID|number]" "  CAPABILITIES [keyword]"
	 "  COMPRESS DEFLATE" "  DATE" "  GROUP newsgroup"
	 "..."
     "Report problems to <usenet@fleegle.mixmin.net>.^M"
     TNNTP> (disconnect)
    "205 Bye!^M"
    2
    CL-USER>

This simple test connect to news.mixmin.net (see tnntp.lisp).  To connect to your server, create a server just like this (with your own info, of course):
```lisp
(defparameter *server* ;; or keep this in some other place...
  (make-server :name "news.mixmin.net"
	       :port 119
	       :user nil
	       :password nil
	       :ssl nil
	       )
(defparameter *conn* ;; or create an array of connections or whatever
  (make-conn :server *server* :group "alt.whatever")
```

At the core, the server structure (see 'tnntp.lisp') contains information about the URL, port, authentication.  A connection keeps track of the socket/stream state.  The system will transparently reconnect and restore current group on a connection should the server close the connection.

Commands are sent with something like 
    (command connection "commandstring" :expecting 2 )
The expecting parameter, if specified, makes sure that the response in in 200-299 range (only first digit is checked).
For commands with an additional parameter such as "GROUP groupname" the :also parameter avoids building command strings:
    (send-command "GROUP" :also groupname) 

Responses are read with

- (rline) - read one line
- (rlist) - read lines terminated by ".^M" into a list; optionally process with :proc.

Lines are returned unprocessed, with control-M character.  Rationale: you will probably parse the lines anyway, so there is little reason to worry about that.

*WORKFLOW*

1. Create a server structure with your server url, port and authentication info.
2. Create a connection structure with the server.
3. (command ...) "MODE READER" is a good start.  If you send commands that return data, make sure to read everything up to and including the termination line containing a single dot.  See (rlist) for details.  If you don't you will get out of sync and send-command will not get a good response line.
4. (disconnect) when done -- it sends "QUIT" and kills the sockets
5. Write a news transport, a reader, a downloader, or anything that you are discouraged to do [in this article](http://newsreaders.com/misc/twpierce/news/newsreader-manifesto.html)

*A SIMPLE EXAMPLE*

    (load-groups)
    (search-groups "book")

Search the grouplist for anything containing the word 'book'; regex expressions allowed!

References:
* 
* [USOCKET API](http://quickdocs.org/usocket/api)
* [RFC 977: Network News Transfer Protocol (1986)](http://newsreaders.com/misc/twpierce/news/rfc977.html)
* [RFC 1036: Standard for Interchange of USENET Messages (1987)](http://newsreaders.com/misc/twpierce/news/rfc1036.html)
* [Don't Write a Newsreader (1995)](http://newsreaders.com/misc/twpierce/news/newsreader-manifesto.html)
