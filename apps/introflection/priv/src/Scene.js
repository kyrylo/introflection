/*global Introflection,log,THREE*/

Introflection.Scene = function(ws) {
	var aspect = window.innerWidth / window.innerHeight;

	this.ws = ws;

	this.scene = new THREE.Scene();

	this.container = document.getElementById('container');
	this.camera = new THREE.PerspectiveCamera(75, aspect, 1, 1000);
	this.renderer = new THREE.WebGLRenderer();
	this.cameraControls = new THREE.OrbitControls(this.camera);

	this.ambientLight = new THREE.AmbientLight(0x000044);
	this.directionalLight = new THREE.DirectionalLight(0xffffff);

	this.stats = new Introflection.Stats();
	this.gui = new Introflection.GUI();

	this.grid = new Introflection.Grid({
		scene: this.scene,
		visible: false,
		enabled: ['XZ', 'axes']
	});
};

Introflection.Scene.prototype = {
	constructor: Introflection.Scene,

	init: function() {
		this.ws.start(this.scene);

		this.setOnResize();
		this.setRendererSize();
		this.renderer.setSize(window.innerWidth, window.innerHeight);
		this.renderer.setClearColor(0xAAAAAA, 1);
		this.container.appendChild(this.renderer.domElement);


		this.scene.add(this.ambientLight);
		this.directionalLight.position.set(1, 1, 1).normalize();
		this.scene.add(this.directionalLight);

		this.camera.position.z = 10;
		this.camera.position.y = 10;

		this.cameraControls.damping = 0.2;
		this.cameraControls.addEventListener('change', this.render.bind(this));

		this.stats.start();
		this.gui.start();

		this.animate.apply(this);
	},

	setRendererSize: function() {
		this.renderer.setSize(window.innerWidth, window.innerHeight);
	},

	setOnResize: function() {
		window.addEventListener('resize', this.resizeFunc.bind(this), false);
	},

	resizeFunc: function() {
		var windowHalfX, windowHalfY;

		windowHalfX = window.innerWidth / 2;
		windowHalfY = window.innerHeight / 2;

		this.camera.aspect = window.innerWidth / window.innerHeight;
		this.camera.updateProjectionMatrix();

		this.setRendererSize();
	},

	animate: function() {
		window.requestAnimationFrame(Introflection.Scene.prototype.animate.bind(this));
		this.render();
		this.cameraControls.update();
		this.stats.update();
	},

	render: function() {
		var changed;

		changed = this.gui.changedItems();

		if (!changed.empty) {
			this.gui.updateOptions();
			this.drawHelpers(changed);
		}

		this.renderer.render(this.scene, this.camera);
	},

	drawHelpers: function(changed) {
		if (changed.x) { this.grid.toggle('XZ'); }
		if (changed.y) { this.grid.toggle('YZ'); }
		if (changed.z) { this.grid.toggle('XY'); }
		if (changed.a) { this.grid.toggle('axes'); }
	}
};
