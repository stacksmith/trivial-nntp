# trivial-nntp
Common lisp tools for connecting to and crawling around NNTP servers.  It uses usocket and cl+ssl, and therefore handles simple or SSL-encrypted NNTP connections.

This is a minimalistic effort; however watch this:
    
     CL-USER> (in-package :tnntp)
     TNNTP> (send-command *conn* "HELP")
     1
     "100 Legal commands^M"
     TNNTP> (read-list *conn*)
     "Report problems to <usenet@fleegle.mixmin.net>.^M"
     "  XPAT header message-ID|range pattern [pattern ...]^M" "  XOVER [range]^M"
     "  XHDR header [message-ID|range]^M" "  XGTITLE [wildmat]^M"
     "  STAT [message-ID|number]^M" "  STARTTLS^M" "  QUIT^M" "  POST^M"
     ...
     "  ARTICLE [message-ID|number]^M")
     TNNTP> (disconnect)
    2
    "205 Bye!^M"
    CL-USER>

At the core, the server structure (see 'tnntp.lisp') contains information about the URL, port, authentication.  A connection keeps track of the socket/stream state.  The system will transparently reconnect and restore current group on a connection should the server close the connection.


Commands are sent with
    (send-command connection "commandstring" :expecting 2 )
The expecting parameter, if specified, makes sure that the response in in 200-299 range (only first digit is checked).
For commands with an additional parameter such as "GROUP groupname" the :also parameter avoids building command strings:
    (send-command "GROUP" :also groupname) 

Responses are read with

- (read-unit) - read one line
- (read-list) - read lines terminated by ".^M" into a list

Lines are returned unprocessed, with control-M character.  Rationale: you will probably parse the lines anyway, so there is little reason to worry about that.

*WORKFLOW*

1. Create a server structure with your server url, port and authentication info.
2. Create a connection structure with the server.
3. (send-command connection ...) "MODE READER" is a good start.  If you send commands that return data, make sure to read everything up to and including the termination line containing a single dot.  See (read-list) for details.  If you don't you will get out of sync and send-command will not get a good response line.
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
