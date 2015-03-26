/*global Introflection,log,THREE*/

/*
 * Thanks, man: http://danni-three.blogspot.com/2013/09/threejs-helpers.html
 */

Introflection.Grid = function(params) {
	if (typeof params !== 'undefined') {
		this.size = params.size !== undefined ? params.size : 100;
		this.step = params.step !== undefined ? params.step : 2;
		this.axisSize = params.axisSize !== undefined ? params.axisSize : 20;
		this.enabled = params.enabled !== undefined ? params.enabled : [];
	}
	this.scene = params.scene;
	this.isVisible = params.visible;

	this.gridXZ = null;
	this.gridYZ = null;
	this.gridXY = null;
	this.axes = null;

	this.applyDefaults();
};

Introflection.Grid.prototype = {
	constructor: Introflection.Grid,

	applyDefaults: function() {
		if (this.enabled.length !== 0) {
			this.enabled.forEach(function(opt) {
				this.toggle(opt);
			}, this);
		}
	},

	toggle: function(opt) {
		if (opt === 'axes') {
			this.toggleAxes();
		} else {
			this.toggleGrid(opt);
		}
	},

	toggleGrid: function(axes) {
		var gridName = 'grid' + axes;

		if (this[gridName] !== null && this[gridName].visible) {
			this.hideGrid(gridName);
		} else {
			this.showGrid(gridName);
		}
	},

	showGrid: function(gridName) {
		if (this[gridName] !== null) {
			this[gridName].visible = true;
		} else {
			this[gridName] = new THREE.GridHelper(this.size, this.step);

			if (!this.visible) {
				this.hideGrid(gridName);
			}

			switch (gridName) {
			case 'gridYZ':
				this[gridName].rotation.z = Math.PI / 2;
				break;
			case 'gridXY':
				this[gridName].rotation.x = Math.PI / 2;
				break;
			}

			this.scene.add(this[gridName]);
		}
	},

	hideGrid: function(gridName) {
		if (this[gridName] !== null) {
			this[gridName].visible = false;
		}
	},

	toggleAxes: function() {
		if (this.axes !== null && this.axes.visible) {
			this.axes.visible = false;
		} else {
			if (this.axes !== null) {
				this.axes.visible = true;
			} else {
				this.axes = new THREE.AxisHelper(this.axisSize);

				if (!this.visible) {
					this.axes.visible = false;
				}

				this.scene.add(this.axes);
			}
		}
	}
};
