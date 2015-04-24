/*global Introflection,log*/

Introflection.WebSocketConnection = function(url) {
	this.socket = null;
	this.scene = null;
	this.url = url;
	this.subprotocol = 'ifproto';
	this.dispatcher = new Introflection.EventDispatcher();

	this.count = 0;
};

Introflection.WebSocketConnection.prototype = {
	constructor: Introflection.WebSocketConnection,

	start: function(scene) {
		this.dispatcher.init(scene);
		var socket = new WebSocket(this.url, this.subprotocol);
		this.setHandlers(socket);
	},

	setHandlers: function(socket) {
		this.setOnopen(socket);
		this.setOnclose(socket);
		this.setOnerror(socket);

		this.setOnmessage(socket);
	},

	setOnopen: function(socket) {
		socket.onopen = function() {
			log.info("Established a new websocket connection on " + this.url);
		};
	},

	setOnclose: function(socket) {
		socket.onclose = function(event) {
			var msg = "Closed the websocket connection on " + this.url + " ";
			if (event.wasClean) {
				log.info(msg + "without errors");
			} else {
				var errmsg = msg + "with errors. Error (" + event.code + "): " +
						event.reason;
				log.info(errmsg);
			}
		};
	},

	setOnerror: function(socket) {
		socket.onerror = function(error) {
			log.warn("An error occurred: " + error.message);
		};
	},

	setOnmessage: function(socket) {
		var that = this;
		socket.onmessage = function(event) {
			that.count++;
			log.debug(that.count);

			that.dispatcher.dispatch(event);
		};
	}
};
