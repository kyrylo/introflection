/*global Introflection,log*/

Introflection.Stats = function() {
	this.stats = new Stats();
	this.modes = {
		fps: 0,
		ms: 1
	};
};

Introflection.Stats.prototype = {
	constructor: Introflection.Stats,

	start: function() {
		this.stats.setMode(this.modes.ms);

		this.stats.domElement.style.position = 'absolute';
		this.stats.domElement.style.left = '0px';
		this.stats.domElement.style.top = '0px';

		document.body.appendChild(this.stats.domElement);
	},

	update: function() {
		this.stats.update();
	}
};
