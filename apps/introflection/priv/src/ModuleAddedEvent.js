/*global Introflection,log*/

Introflection.ModuleAddedEvent = function() {
	this.includes = [];
	this.inherits_from = [];
	this.name = '';
	this.nested_under = [];
	this.nesting_level = -1;
	this.origin = '';
	this.type = '';
};

Introflection.ModuleAddedEvent.prototype = {
	constructor: Introflection.ModuleAddedEvent,

	handle: function(data) {
		this.type = data.type;
		this.origin = data.origin;
		this.nesting_level = data.nesting_level;

		data.nested_under.forEach(function(mod) {
			this.nested_under.push(String.fromCharCode.apply(this, mod));
		}, this);

		this.name = String.fromCharCode.apply(this, data.name);

		data.includes.forEach(function(mod) {
			this.includes.push(String.fromCharCode.apply(this, mod));
		}, this);
	}
};
