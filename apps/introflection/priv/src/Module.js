/*global Introflection,log,THREE*/

Introflection.Module = function(event) {
	var data = event.data;

	this.object_id = data.object_id;
	this.name = String.fromCharCode.apply(this, data.name);
	this.nesting = data.nesting;
	this.parent = data.parent;
};

Introflection.Module.prototype = {
	constructor: Introflection.Module,

	display: function(scene, count) {
		log.debug(this);
		var cube = new THREE.Mesh(new THREE.BoxGeometry(1, 1, 1), new THREE.MeshLambertMaterial({color: 'blue'}));
		cube.position.x = 5*count + 5;
		scene.add(cube);
	}
};
