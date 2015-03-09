/*global Introflection,log,dat*/

Introflection.GUI = function() {
	this.gui = new dat.GUI();

	this.options = {};
	this.optionsController = {
		gridX: true,
		gridY: false,
		gridZ: false,
		axes: true
	};
};

Introflection.GUI.prototype = {
	constructor: Introflection.GUI,

	start: function() {
		var menuItems = [
			['gridX', 'Show XZ grid'],
			['gridY', 'Show YZ grid'],
			['gridZ', 'Show XY grid'],
			['axes', 'Show axes']
		];
		menuItems.forEach(function(menuItem) {
			this.addMenuItem(menuItem[0], menuItem[1]);
		}, this);
	},

	addMenuItem: function(option, description) {
		this.gui.add(this.optionsController, option).name(description);
	},

	changedItems: function() {
		var changed, x, y, z, g, a;

		x = this.optionsController.gridX !== this.options.gridX;
		y = this.optionsController.gridY !== this.options.gridY;
		z = this.optionsController.gridZ !== this.options.gridZ;
		a = this.optionsController.axes !== this.options.axes;

		changed = {x: x, y: y, z: z, a: a, empty: true};

		if (x || y || z || a) {
			changed.empty = false;
		}

		return changed;
	},

	updateOptions: function() {
		this.options.gridX = this.optionsController.gridX;
		this.options.gridY = this.optionsController.gridY;
		this.options.gridZ = this.optionsController.gridZ;
		this.options.axes = this.optionsController.axes;
	}
};
