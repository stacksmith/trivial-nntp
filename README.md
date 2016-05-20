# trivial-nntp
Common lisp tools for connecting to and crawling around NNTP servers.  It uses usocket.

This is a minimalistic effort; however watch this:
    
     CL-USER> (tnntp:connect)
     #<USOCKET:STREAM-USOCKET {100E6BDDD3}>
     CL-USER> (tnntp:send-command "HELP")
     1
     "100 Legal commands^M"
     CL-USER> (tnntp:read-list)
     "Report problems to <usenet@fleegle.mixmin.net>.^M"
     "  XPAT header message-ID|range pattern [pattern ...]^M" "  XOVER [range]^M"
     "  XHDR header [message-ID|range]^M" "  XGTITLE [wildmat]^M"
     "  STAT [message-ID|number]^M" "  STARTTLS^M" "  QUIT^M" "  POST^M"
     "  OVER [range]^M" "  NEXT^M" "  NEWNEWS wildmat [yy]yymmdd hhmmss [GMT]^M"
     "  NEWGROUPS [yy]yymmdd hhmmss [GMT]^M" "  MODE READER^M"
     "  LISTGROUP [newsgroup [range]]^M"
     "  LIST [ACTIVE [wildmat]|ACTIVE.TIMES [wildmat]|COUNTS [wildmat]|DISTRIB.PATS|DISTRIBUTIONS|HEADERS [MSGID|RANGE]|MODERATORS|MOTD|NEWSGROUPS [wildmat]|OVERVIEW.FMT|SUBSCRIPTIONS [wildmat]]^M"
     "  LAST^M" "  IHAVE message-ID^M" "  HELP^M" "  HEAD [message-ID|number]^M"
     "  HDR header [message-ID|range]^M" "  GROUP newsgroup^M" "  DATE^M"
     "  CAPABILITIES [keyword]^M" "  BODY [message-ID|number]^M"
     "  AUTHINFO USER name|PASS password|GENERIC program [argument ...]^M"
     "  ARTICLE [message-ID|number]^M")
     CL-USER> (tnntp:disconnect)
    2
    "205 Bye!^M"
    CL-USER>

The system uses several special variables to keep defaults which come in handy in interactive work:

- \*acct*    structure contains server name, address, username and password (defaults to a free server).  Used in (connect) and (login).
- \*socket*  contains the usocket opened by most recent connect.  Most functions use it if the :socket parameter is missing.

Commands are sent with
    (send-command "commandstring" :expecting 2)
The expecting parameter, if specified, makes sure that the response in in 200-299 range (only first digit is checked).

Responses are read with

- (read-unit) - read one line
- (read-list) - read lines terminated by ".^M" into a list

Lines are returned unprocessed, with control-M character.  Rationale: you will probably parse the lines anyway, so there is little reason to worry about that.

*WORKFLOW*

1. Create an acct structure with your server url, port and authentication info.
2. (connect) to your server.
3. (login) with your acct.
4. (send-command ...) "MODE READER" is a good start.  If you send commands that return data, make sure to read everything up to and including the termination line containing a single dot.  See (read-list) for details.  If you don't you will get out of sync and send-command will not get a good response line.
4. (disconnect) when done -- it sends "QUIT" and kills the socket.
5. Write an news transport, a reader, a downloader, or anything that you are discouraged to do [in this article](http://newsreaders.com/misc/twpierce/news/newsreader-manifesto.html)
See:
* 
* [USOCKET API](http://quickdocs.org/usocket/api)