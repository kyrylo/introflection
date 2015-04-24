/*global Introflection,log*/

Introflection.ModuleSpace = function(scene) {
	this.scene = scene;
	this.modules = [];
	window.modules = this.modules;
};

Introflection.ModuleSpace.prototype = {
	constructor: Introflection.ModuleSpace,

	addModuleChain: function(chain) {
		this.sortChain(chain);

		chain.forEach(function(module, idx, array) {
			var parent, current;

			// Initialize current layer.
			if (this.modules[module.nesting] === undefined) {
				this.modules[module.nesting] = [];
			}

			// Initialize the layer above.
			if (this.modules[module.nesting + 1] === undefined &&
				array.length - 1 < idx)
			{
				this.modules[module.nesting + 1] = [];
			}

			current = this.find(module.object_id);

			if (current) {
				current.expand();
				current.elevate();

				while ((parent = this.find(current.parent)) &&
					   !parent.isParentToSelf()) {
					parent.expand();
					parent.align('x');
					parent.align('z');
					current = parent;
				}
			} else {
				this.modules[module.nesting].push(module);

				module.init();
				module.elevate();

				parent = this.find(module.parent);
				while (parent && !parent.isParentToSelf()) {
					parent.expand();
					parent.elevate();
					parent = this.find(parent.parent);
				}

				module.display(this.scene);
			}
		}, this);
	},

	find: function(moduleId) {
		var i, j;

		for (i = 0; i < this.modules.length; i++) {
			for (j = 0; j < this.modules[i].length; j++) {
				if (this.modules[i][j].object_id === moduleId) {
					return this.modules[i][j];
				}
			}
		}
		return null;
	},

	sortChain: function(modules) {
		modules.sort(function(modA, modB) {
			if (modA.nesting > modB.nesting) {
				return 1;
			}

			if (modA.nesting < modB.nesting) {
				return -1;
			}

			throw new Error('Adjacent chain links are the same');
		});
	}
};
