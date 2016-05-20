# trivial-nntp
Common lisp tools for connecting to and crawling around NNTP servers.

This is a minimalistic effort; however watch this:
    
     CL-USER> (tnntp:connect)
     2
     "200 news.mixmin.net InterNetNews NNRP server INN 2.7.0 (20151105 prerelease) ready (posting ok)"
     CL-USER> (tnntp:disconnect)
     2
     "205 Bye!"
     CL-USER> (tnntp:connect)
     2
     "200 news.mixmin.net InterNetNews NNRP server INN 2.7.0 (20151105 prerelease) ready (posting ok)"
     CL-USER> (tnntp:send-command "HELP")
     1
     "100 Legal commands"
     CL-USER> (tnntp:read-list)
     "Report problems to <usenet@fleegle.mixmin.net>."
     "  XPAT header message-ID|range pattern [pattern ...]" "  XOVER [range]"
     "  XHDR header [message-ID|range]" "  XGTITLE [wildmat]"
     "  STAT [message-ID|number]" "  STARTTLS" "  QUIT" "  POST"
     "  OVER [range]" "  NEXT" "  NEWNEWS wildmat [yy]yymmdd hhmmss [GMT]"
     "  NEWGROUPS [yy]yymmdd hhmmss [GMT]" "  MODE READER"
     "  LISTGROUP [newsgroup [range]]"
     "  LIST [ACTIVE [wildmat]|ACTIVE.TIMES [wildmat]|COUNTS [wildmat]|DISTRIB.PATS|DISTRIBUTIONS|HEADERS [MSGID|RANGE]|MODERATORS|MOTD|NEWSGROUPS [wildmat]|OVERVIEW.FMT|SUBSCRIPTIONS [wildmat]]"
     "  LAST" "  IHAVE message-ID" "  HELP" "  HEAD [message-ID|number]"
     "  HDR header [message-ID|range]" "  GROUP newsgroup" "  DATE"
     "  CAPABILITIES [keyword]" "  BODY [message-ID|number]"
     "  AUTHINFO USER name|PASS password|GENERIC program [argument ...]"
     "  ARTICLE [message-ID|number]")
     CL-USER> (tnntp:disconnect)
    2
    "205 Bye!"
    CL-USER>
