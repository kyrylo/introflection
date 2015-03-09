/*global Introflection,log*/

Introflection.EventDispatcher = function() {
	this.scene = null;
	this.count = 0;
};

Introflection.EventDispatcher.prototype = {
	constructor: Introflection.EventDispatcher,

	dispatch: function(rawEvent) {
		var event, module;

		event = JSON.parse(rawEvent.data);

		switch (event.event) {
		case 'module_added':
			module = new Introflection.Module(event);
			module.display(this.scene, this.count);
			this.count++;
			break;
		}
	}
};
