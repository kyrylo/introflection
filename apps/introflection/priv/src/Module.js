/*global Introflection,log,THREE*/

Introflection.Module = function(event) {
	this.moduleAddedEvent = new Introflection.ModuleAddedEvent();
	this.moduleAddedEvent.handle(event.data);
};

Introflection.Module.prototype = {
	constructor: Introflection.Module,

	display: function(scene, count) {
		var cube = new THREE.Mesh(new THREE.BoxGeometry(1, 1, 1), new THREE.MeshLambertMaterial({color: 'blue'}));
		cube.position.x = 5*count + 5;
		scene.add(cube);
	}
};
