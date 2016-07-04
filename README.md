# trivial-nntp
Common lisp tools for connecting to and crawling around NNTP servers.  It uses usocket.

This is a minimalistic effort; however watch this:
    
     CL-USER> 
     CL-USER> (tnntp:send-command "HELP")
     1
     "100 Legal commands^M"
     CL-USER> (tnntp:read-list)
     "Report problems to <usenet@fleegle.mixmin.net>.^M"
     "  XPAT header message-ID|range pattern [pattern ...]^M" "  XOVER [range]^M"
     "  XHDR header [message-ID|range]^M" "  XGTITLE [wildmat]^M"
     "  STAT [message-ID|number]^M" "  STARTTLS^M" "  QUIT^M" "  POST^M"
     ...
     "  ARTICLE [message-ID|number]^M")
     CL-USER> (tnntp:disconnect)
    2
    "205 Bye!^M"
    CL-USER>

At the core, the server structure (see 'tnntp.lisp') contains information about the URL, port, authentication and connection state to a server.  The system will transparently reconnect and restore current group on a connection should the server close the connection.

Multiple server connections are supported (via arrays of sockets and groups for each server).  A socket is selected by specifying server and socket index.  For defaults, \*server\* and socket index 0 are used; an open nntp server 'news.mixmin.net' is defaulted to.

Commands are sent with
    (send-command "commandstring" :expecting 2 )
The expecting parameter, if specified, makes sure that the response in in 200-299 range (only first digit is checked).
For commands with an additional parameter such as "GROUP groupname" the :also parameter avoids building command strings:
    (send-command "GROUP" :also groupname) 

Responses are read with

- (read-unit) - read one line
- (read-list) - read lines terminated by ".^M" into a list

Lines are returned unprocessed, with control-M character.  Rationale: you will probably parse the lines anyway, so there is little reason to worry about that.

*WORKFLOW*

1. Create a server structure with your server url, port and authentication info.
2. (send-command ...) "MODE READER" is a good start.  If you send commands that return data, make sure to read everything up to and including the termination line containing a single dot.  See (read-list) for details.  If you don't you will get out of sync and send-command will not get a good response line.
3. (disconnect) when done -- it sends "QUIT" and kills the sockets
4. Write an news transport, a reader, a downloader, or anything that you are discouraged to do [in this article](http://newsreaders.com/misc/twpierce/news/newsreader-manifesto.html)

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
