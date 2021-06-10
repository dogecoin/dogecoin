var sockjs_url = '/multiplex';
    var sockjs = new SockJS(sockjs_url);

    var multiplexer = new WebSocketMultiplex(sockjs);
    var ann  = multiplexer.channel('ann');
    var bob  = multiplexer.channel('bob');
    var carl = multiplexer.channel('carl');
