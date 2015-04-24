/*global Introflection,log*/

Introflection.EventDispatcher = function() {
	this.scene = null;
	this.moduleSpace = null;
};

Introflection.EventDispatcher.prototype = {
	constructor: Introflection.EventDispatcher,

	init: function(scene) {
		this.scene = scene;
		this.moduleSpace = new Introflection.ModuleSpace(this.scene);
	},

	dispatch: function(rawEvent) {
		var event, module, modules;

		event = JSON.parse(rawEvent.data);

		switch (event.event) {
		case 0:
			module = new Introflection.Module(event.data);
			this.moduleSpace.attachModule(module);
			break;
		case 1:
			modules = event.data.map(function(datum) {
				return new Introflection.Module(datum);
			});
			this.moduleSpace.addModuleChain(modules);
			break;
		}
	}
};
