/*global Introflection,log,THREE,THREEx*/

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
		var dynamicTexture	= new THREEx.DynamicTexture(512,512);
		dynamicTexture.clear('cyan');
		dynamicTexture.drawText(this.name, 30, 50, 'black', "bold 55px Arial");


		var cube = new THREE.Mesh(
			new THREE.BoxGeometry(1, 1, 1),
			new THREE.MeshLambertMaterial({map: dynamicTexture.texture})
		);
		cube.position.x = 5*count;
		cube.position.y = 5*this.nesting;
		scene.add(cube);
	}
};
