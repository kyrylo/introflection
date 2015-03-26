/*global Introflection,log,THREE*/

Introflection.Camera = function() {
	this.camera = new THREE.PerspectiveCamera(75, this.getAspect(), 1, 1000);
	this.cameraControls = new THREE.OrbitControls(this.camera);
};

Introflection.Camera.prototype = {
	constructor: Introflection.Camera,

	start: function(renderFunc) {
		this.camera.position.z = 10;
		this.camera.position.y = 10;

		this.cameraControls.damping = 0.2;
		this.cameraControls.addEventListener('change', renderFunc);
	},

	update: function() {
		this.cameraControls.update();
	},

	resize: function() {
		this.setAspect();
		this.camera.updateProjectionMatrix();
	},

	getAspect: function() {
		return window.innerWidth / window.innerHeight;
	},

	setAspect: function() {
		this.camera.aspect = this.getAspect();
	}
};
