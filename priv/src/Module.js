/*global Introflection,log,THREE,THREEx*/

Introflection.Module = function(data) {
	this.object_id = data.object_id;
	this.name = String.fromCharCode.apply(this, data.name);
	this.nesting = data.nesting;
	this.parent = data.parent;

	this.geometry = null;
	this.mesh = null;

	this.height = 1;
};

Introflection.Module.prototype = {
	constructor: Introflection.Module,

	init: function() {
		this.geometry = new THREE.BoxGeometry(1, this.height, 1);

		var dynamicTexture	= new THREEx.DynamicTexture(512, 512);
		dynamicTexture.clear('cyan');
		dynamicTexture.drawText(this.name, 30, 50, 'black', "bold 55px Arial");

		this.mesh = new THREE.Mesh(
			this.geometry,
			new THREE.MeshLambertMaterial({map: dynamicTexture.texture})
		);

		this.align('x');
		this.align('y');
		this.align('z');
	},

	display: function(scene) {
		scene.add(this.mesh);
	},

	expand: function() {
		this.increaseWidth(1);
		this.increaseDepth(1);
	},

	align: function(axis) {
		this.mesh.position[axis] = this.size()[axis] / 2;
	},

	increaseWidth: function(pos) {
		this.mesh.scale.x += pos;
		this.align('x');
	},

	increaseDepth: function(pos) {
		this.mesh.scale.z += pos;
		this.align('z');
	},

	size: function() {
		return new THREE.Box3().setFromObject(this.mesh).size();
	},

	elevate: function() {
		this.mesh.position.y = this.nesting * this.height + this.height / 2;
	},

	isParentToSelf: function() {
		return this.parent === this.object_id;
	}
};
