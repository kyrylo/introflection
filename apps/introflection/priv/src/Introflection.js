/*global log*/

var Introflection = { REVISION: '1' };

Introflection.start = function() {
	var wsconnection, scene;

	log.enableAll();

	wsconnection = new Introflection.WebSocketConnection('ws://localhost:8080/ws');

	scene = new Introflection.Scene(wsconnection);
	log.info("Initialising the scene...");
	scene.init();
};
