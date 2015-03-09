/*global log*/

var Introflection = { REVISION: '1' };

Introflection.start = function() {
	var wsconnection, scene;

	log.enableAll();

	wsconnection = new Introflection.WebSocketConnection('ws://localhost:8080/ws');
	wsconnection.start();

	scene = new Introflection.Scene(wsconnection);
	scene.init();
};
