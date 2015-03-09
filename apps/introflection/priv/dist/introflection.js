'use strict';
// Source: lib/three.js/OrbitControls.js
/**
 * @author qiao / https://github.com/qiao
 * @author mrdoob / http://mrdoob.com
 * @author alteredq / http://alteredqualia.com/
 * @author WestLangley / http://github.com/WestLangley
 * @author erich666 / http://erichaines.com
 */
/*global THREE, console */

// This set of controls performs orbiting, dollying (zooming), and panning. It maintains
// the "up" direction as +Y, unlike the TrackballControls. Touch on tablet and phones is
// supported.
//
//    Orbit - left mouse / touch: one finger move
//    Zoom - middle mouse, or mousewheel / touch: two finger spread or squish
//    Pan - right mouse, or arrow keys / touch: three finter swipe
//
// This is a drop-in replacement for (most) TrackballControls used in examples.
// That is, include this js file and wherever you see:
//    	controls = new THREE.TrackballControls( camera );
//      controls.target.z = 150;
// Simple substitute "OrbitControls" and the control should work as-is.

THREE.OrbitControls = function ( object, domElement ) {

	this.object = object;
	this.domElement = ( domElement !== undefined ) ? domElement : document;

	// API

	// Set to false to disable this control
	this.enabled = true;

	// "target" sets the location of focus, where the control orbits around
	// and where it pans with respect to.
	this.target = new THREE.Vector3();

	// center is old, deprecated; use "target" instead
	this.center = this.target;

	// This option actually enables dollying in and out; left as "zoom" for
	// backwards compatibility
	this.noZoom = false;
	this.zoomSpeed = 1.0;

	// Limits to how far you can dolly in and out
	this.minDistance = 0;
	this.maxDistance = Infinity;

	// Set to true to disable this control
	this.noRotate = false;
	this.rotateSpeed = 1.0;

	// Set to true to disable this control
	this.noPan = false;
	this.keyPanSpeed = 7.0;	// pixels moved per arrow key push

	// Set to true to automatically rotate around the target
	this.autoRotate = false;
	this.autoRotateSpeed = 2.0; // 30 seconds per round when fps is 60

	// How far you can orbit vertically, upper and lower limits.
	// Range is 0 to Math.PI radians.
	this.minPolarAngle = 0; // radians
	this.maxPolarAngle = Math.PI; // radians

	// How far you can orbit horizontally, upper and lower limits.
	// If set, must be a sub-interval of the interval [ - Math.PI, Math.PI ].
	this.minAzimuthAngle = - Infinity; // radians
	this.maxAzimuthAngle = Infinity; // radians

	// Set to true to disable use of the keys
	this.noKeys = false;

	// The four arrow keys
	this.keys = { LEFT: 37, UP: 38, RIGHT: 39, BOTTOM: 40 };

	// Mouse buttons
	this.mouseButtons = { ORBIT: THREE.MOUSE.LEFT, ZOOM: THREE.MOUSE.MIDDLE, PAN: THREE.MOUSE.RIGHT };

	////////////
	// internals

	var scope = this;

	var EPS = 0.000001;

	var rotateStart = new THREE.Vector2();
	var rotateEnd = new THREE.Vector2();
	var rotateDelta = new THREE.Vector2();

	var panStart = new THREE.Vector2();
	var panEnd = new THREE.Vector2();
	var panDelta = new THREE.Vector2();
	var panOffset = new THREE.Vector3();

	var offset = new THREE.Vector3();

	var dollyStart = new THREE.Vector2();
	var dollyEnd = new THREE.Vector2();
	var dollyDelta = new THREE.Vector2();

	var theta;
	var phi;
	var phiDelta = 0;
	var thetaDelta = 0;
	var scale = 1;
	var pan = new THREE.Vector3();

	var lastPosition = new THREE.Vector3();
	var lastQuaternion = new THREE.Quaternion();

	var STATE = { NONE : -1, ROTATE : 0, DOLLY : 1, PAN : 2, TOUCH_ROTATE : 3, TOUCH_DOLLY : 4, TOUCH_PAN : 5 };

	var state = STATE.NONE;

	// for reset

	this.target0 = this.target.clone();
	this.position0 = this.object.position.clone();

	// so camera.up is the orbit axis

	var quat = new THREE.Quaternion().setFromUnitVectors( object.up, new THREE.Vector3( 0, 1, 0 ) );
	var quatInverse = quat.clone().inverse();

	// events

	var changeEvent = { type: 'change' };
	var startEvent = { type: 'start'};
	var endEvent = { type: 'end'};

	this.rotateLeft = function ( angle ) {

		if ( angle === undefined ) {

			angle = getAutoRotationAngle();

		}

		thetaDelta -= angle;

	};

	this.rotateUp = function ( angle ) {

		if ( angle === undefined ) {

			angle = getAutoRotationAngle();

		}

		phiDelta -= angle;

	};

	// pass in distance in world space to move left
	this.panLeft = function ( distance ) {

		var te = this.object.matrix.elements;

		// get X column of matrix
		panOffset.set( te[ 0 ], te[ 1 ], te[ 2 ] );
		panOffset.multiplyScalar( - distance );

		pan.add( panOffset );

	};

	// pass in distance in world space to move up
	this.panUp = function ( distance ) {

		var te = this.object.matrix.elements;

		// get Y column of matrix
		panOffset.set( te[ 4 ], te[ 5 ], te[ 6 ] );
		panOffset.multiplyScalar( distance );

		pan.add( panOffset );

	};

	// pass in x,y of change desired in pixel space,
	// right and down are positive
	this.pan = function ( deltaX, deltaY ) {

		var element = scope.domElement === document ? scope.domElement.body : scope.domElement;

		if ( scope.object.fov !== undefined ) {

			// perspective
			var position = scope.object.position;
			var offset = position.clone().sub( scope.target );
			var targetDistance = offset.length();

			// half of the fov is center to top of screen
			targetDistance *= Math.tan( ( scope.object.fov / 2 ) * Math.PI / 180.0 );

			// we actually don't use screenWidth, since perspective camera is fixed to screen height
			scope.panLeft( 2 * deltaX * targetDistance / element.clientHeight );
			scope.panUp( 2 * deltaY * targetDistance / element.clientHeight );

		} else if ( scope.object.top !== undefined ) {

			// orthographic
			scope.panLeft( deltaX * (scope.object.right - scope.object.left) / element.clientWidth );
			scope.panUp( deltaY * (scope.object.top - scope.object.bottom) / element.clientHeight );

		} else {

			// camera neither orthographic or perspective
			console.warn( 'WARNING: OrbitControls.js encountered an unknown camera type - pan disabled.' );

		}

	};

	this.dollyIn = function ( dollyScale ) {

		if ( dollyScale === undefined ) {

			dollyScale = getZoomScale();

		}

		scale /= dollyScale;

	};

	this.dollyOut = function ( dollyScale ) {

		if ( dollyScale === undefined ) {

			dollyScale = getZoomScale();

		}

		scale *= dollyScale;

	};

	this.update = function () {

		var position = this.object.position;

		offset.copy( position ).sub( this.target );

		// rotate offset to "y-axis-is-up" space
		offset.applyQuaternion( quat );

		// angle from z-axis around y-axis

		theta = Math.atan2( offset.x, offset.z );

		// angle from y-axis

		phi = Math.atan2( Math.sqrt( offset.x * offset.x + offset.z * offset.z ), offset.y );

		if ( this.autoRotate && state === STATE.NONE ) {

			this.rotateLeft( getAutoRotationAngle() );

		}

		theta += thetaDelta;
		phi += phiDelta;

		// restrict theta to be between desired limits
		theta = Math.max( this.minAzimuthAngle, Math.min( this.maxAzimuthAngle, theta ) );

		// restrict phi to be between desired limits
		phi = Math.max( this.minPolarAngle, Math.min( this.maxPolarAngle, phi ) );

		// restrict phi to be betwee EPS and PI-EPS
		phi = Math.max( EPS, Math.min( Math.PI - EPS, phi ) );

		var radius = offset.length() * scale;

		// restrict radius to be between desired limits
		radius = Math.max( this.minDistance, Math.min( this.maxDistance, radius ) );

		// move target to panned location
		this.target.add( pan );

		offset.x = radius * Math.sin( phi ) * Math.sin( theta );
		offset.y = radius * Math.cos( phi );
		offset.z = radius * Math.sin( phi ) * Math.cos( theta );

		// rotate offset back to "camera-up-vector-is-up" space
		offset.applyQuaternion( quatInverse );

		position.copy( this.target ).add( offset );

		this.object.lookAt( this.target );

		thetaDelta = 0;
		phiDelta = 0;
		scale = 1;
		pan.set( 0, 0, 0 );

		// update condition is:
		// min(camera displacement, camera rotation in radians)^2 > EPS
		// using small-angle approximation cos(x/2) = 1 - x^2 / 8

		if ( lastPosition.distanceToSquared( this.object.position ) > EPS
		    || 8 * (1 - lastQuaternion.dot(this.object.quaternion)) > EPS ) {

			this.dispatchEvent( changeEvent );

			lastPosition.copy( this.object.position );
			lastQuaternion.copy (this.object.quaternion );

		}

	};


	this.reset = function () {

		state = STATE.NONE;

		this.target.copy( this.target0 );
		this.object.position.copy( this.position0 );

		this.update();

	};

	this.getPolarAngle = function () {

		return phi;

	};

	this.getAzimuthalAngle = function () {

		return theta

	};

	function getAutoRotationAngle() {

		return 2 * Math.PI / 60 / 60 * scope.autoRotateSpeed;

	}

	function getZoomScale() {

		return Math.pow( 0.95, scope.zoomSpeed );

	}

	function onMouseDown( event ) {

		if ( scope.enabled === false ) return;
		event.preventDefault();

		if ( event.button === scope.mouseButtons.ORBIT ) {
			if ( scope.noRotate === true ) return;

			state = STATE.ROTATE;

			rotateStart.set( event.clientX, event.clientY );

		} else if ( event.button === scope.mouseButtons.ZOOM ) {
			if ( scope.noZoom === true ) return;

			state = STATE.DOLLY;

			dollyStart.set( event.clientX, event.clientY );

		} else if ( event.button === scope.mouseButtons.PAN ) {
			if ( scope.noPan === true ) return;

			state = STATE.PAN;

			panStart.set( event.clientX, event.clientY );

		}

		if ( state !== STATE.NONE ) {
			document.addEventListener( 'mousemove', onMouseMove, false );
			document.addEventListener( 'mouseup', onMouseUp, false );
			scope.dispatchEvent( startEvent );
		}

	}

	function onMouseMove( event ) {

		if ( scope.enabled === false ) return;

		event.preventDefault();

		var element = scope.domElement === document ? scope.domElement.body : scope.domElement;

		if ( state === STATE.ROTATE ) {

			if ( scope.noRotate === true ) return;

			rotateEnd.set( event.clientX, event.clientY );
			rotateDelta.subVectors( rotateEnd, rotateStart );

			// rotating across whole screen goes 360 degrees around
			scope.rotateLeft( 2 * Math.PI * rotateDelta.x / element.clientWidth * scope.rotateSpeed );

			// rotating up and down along whole screen attempts to go 360, but limited to 180
			scope.rotateUp( 2 * Math.PI * rotateDelta.y / element.clientHeight * scope.rotateSpeed );

			rotateStart.copy( rotateEnd );

		} else if ( state === STATE.DOLLY ) {

			if ( scope.noZoom === true ) return;

			dollyEnd.set( event.clientX, event.clientY );
			dollyDelta.subVectors( dollyEnd, dollyStart );

			if ( dollyDelta.y > 0 ) {

				scope.dollyIn();

			} else {

				scope.dollyOut();

			}

			dollyStart.copy( dollyEnd );

		} else if ( state === STATE.PAN ) {

			if ( scope.noPan === true ) return;

			panEnd.set( event.clientX, event.clientY );
			panDelta.subVectors( panEnd, panStart );

			scope.pan( panDelta.x, panDelta.y );

			panStart.copy( panEnd );

		}

		if ( state !== STATE.NONE ) scope.update();

	}

	function onMouseUp( /* event */ ) {

		if ( scope.enabled === false ) return;

		document.removeEventListener( 'mousemove', onMouseMove, false );
		document.removeEventListener( 'mouseup', onMouseUp, false );
		scope.dispatchEvent( endEvent );
		state = STATE.NONE;

	}

	function onMouseWheel( event ) {

		if ( scope.enabled === false || scope.noZoom === true || state !== STATE.NONE ) return;

		event.preventDefault();
		event.stopPropagation();

		var delta = 0;

		if ( event.wheelDelta !== undefined ) { // WebKit / Opera / Explorer 9

			delta = event.wheelDelta;

		} else if ( event.detail !== undefined ) { // Firefox

			delta = - event.detail;

		}

		if ( delta > 0 ) {

			scope.dollyOut();

		} else {

			scope.dollyIn();

		}

		scope.update();
		scope.dispatchEvent( startEvent );
		scope.dispatchEvent( endEvent );

	}

	function onKeyDown( event ) {

		if ( scope.enabled === false || scope.noKeys === true || scope.noPan === true ) return;

		switch ( event.keyCode ) {

			case scope.keys.UP:
				scope.pan( 0, scope.keyPanSpeed );
				scope.update();
				break;

			case scope.keys.BOTTOM:
				scope.pan( 0, - scope.keyPanSpeed );
				scope.update();
				break;

			case scope.keys.LEFT:
				scope.pan( scope.keyPanSpeed, 0 );
				scope.update();
				break;

			case scope.keys.RIGHT:
				scope.pan( - scope.keyPanSpeed, 0 );
				scope.update();
				break;

		}

	}

	function touchstart( event ) {

		if ( scope.enabled === false ) return;

		switch ( event.touches.length ) {

			case 1:	// one-fingered touch: rotate

				if ( scope.noRotate === true ) return;

				state = STATE.TOUCH_ROTATE;

				rotateStart.set( event.touches[ 0 ].pageX, event.touches[ 0 ].pageY );
				break;

			case 2:	// two-fingered touch: dolly

				if ( scope.noZoom === true ) return;

				state = STATE.TOUCH_DOLLY;

				var dx = event.touches[ 0 ].pageX - event.touches[ 1 ].pageX;
				var dy = event.touches[ 0 ].pageY - event.touches[ 1 ].pageY;
				var distance = Math.sqrt( dx * dx + dy * dy );
				dollyStart.set( 0, distance );
				break;

			case 3: // three-fingered touch: pan

				if ( scope.noPan === true ) return;

				state = STATE.TOUCH_PAN;

				panStart.set( event.touches[ 0 ].pageX, event.touches[ 0 ].pageY );
				break;

			default:

				state = STATE.NONE;

		}

		if ( state !== STATE.NONE ) scope.dispatchEvent( startEvent );

	}

	function touchmove( event ) {

		if ( scope.enabled === false ) return;

		event.preventDefault();
		event.stopPropagation();

		var element = scope.domElement === document ? scope.domElement.body : scope.domElement;

		switch ( event.touches.length ) {

			case 1: // one-fingered touch: rotate

				if ( scope.noRotate === true ) return;
				if ( state !== STATE.TOUCH_ROTATE ) return;

				rotateEnd.set( event.touches[ 0 ].pageX, event.touches[ 0 ].pageY );
				rotateDelta.subVectors( rotateEnd, rotateStart );

				// rotating across whole screen goes 360 degrees around
				scope.rotateLeft( 2 * Math.PI * rotateDelta.x / element.clientWidth * scope.rotateSpeed );
				// rotating up and down along whole screen attempts to go 360, but limited to 180
				scope.rotateUp( 2 * Math.PI * rotateDelta.y / element.clientHeight * scope.rotateSpeed );

				rotateStart.copy( rotateEnd );

				scope.update();
				break;

			case 2: // two-fingered touch: dolly

				if ( scope.noZoom === true ) return;
				if ( state !== STATE.TOUCH_DOLLY ) return;

				var dx = event.touches[ 0 ].pageX - event.touches[ 1 ].pageX;
				var dy = event.touches[ 0 ].pageY - event.touches[ 1 ].pageY;
				var distance = Math.sqrt( dx * dx + dy * dy );

				dollyEnd.set( 0, distance );
				dollyDelta.subVectors( dollyEnd, dollyStart );

				if ( dollyDelta.y > 0 ) {

					scope.dollyOut();

				} else {

					scope.dollyIn();

				}

				dollyStart.copy( dollyEnd );

				scope.update();
				break;

			case 3: // three-fingered touch: pan

				if ( scope.noPan === true ) return;
				if ( state !== STATE.TOUCH_PAN ) return;

				panEnd.set( event.touches[ 0 ].pageX, event.touches[ 0 ].pageY );
				panDelta.subVectors( panEnd, panStart );

				scope.pan( panDelta.x, panDelta.y );

				panStart.copy( panEnd );

				scope.update();
				break;

			default:

				state = STATE.NONE;

		}

	}

	function touchend( /* event */ ) {

		if ( scope.enabled === false ) return;

		scope.dispatchEvent( endEvent );
		state = STATE.NONE;

	}

	this.domElement.addEventListener( 'contextmenu', function ( event ) { event.preventDefault(); }, false );
	this.domElement.addEventListener( 'mousedown', onMouseDown, false );
	this.domElement.addEventListener( 'mousewheel', onMouseWheel, false );
	this.domElement.addEventListener( 'DOMMouseScroll', onMouseWheel, false ); // firefox

	this.domElement.addEventListener( 'touchstart', touchstart, false );
	this.domElement.addEventListener( 'touchend', touchend, false );
	this.domElement.addEventListener( 'touchmove', touchmove, false );

	window.addEventListener( 'keydown', onKeyDown, false );

	// force an update at start
	this.update();

};

THREE.OrbitControls.prototype = Object.create( THREE.EventDispatcher.prototype );
THREE.OrbitControls.prototype.constructor = THREE.OrbitControls;

// Source: src/Introflection.js
/*global log*/

var Introflection = { REVISION: '1' };

Introflection.start = function() {
	var wsconnection, scene;

	log.enableAll();

	wsconnection = new Introflection.WebSocketConnection('ws://localhost:8080/ws');
	wsconnection.start();

	scene = new Introflection.Scene(wsconnection);
	scene.init();
};

// Source: src/Grid.js
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

// Source: src/ModuleAddedEvent.js
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

// Source: src/Module.js
/*global Introflection,log,THREE*/

Introflection.Module = function(event) {
	this.moduleAddedEvent = new Introflection.ModuleAddedEvent();
	this.moduleAddedEvent.handle(event.data);
};

Introflection.Module.prototype = {
	constructor: Introflection.Module,

	display: function(scene, count) {
		var cube = new THREE.Mesh(new THREE.BoxGeometry(1, 1, 1), new THREE.MeshLambertMaterial({color: 'blue'}));
		cube.position.x = 5*count + 5;
		scene.add(cube);
	}
};

// Source: src/EventDispatcher.js
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

// Source: src/WebSocketConnection.js
/*global Introflection,log*/

Introflection.WebSocketConnection = function(url) {
	this.socket = null;
	this.scene = null;
	this.url = url;
	this.subprotocol = 'ifproto';
	this.dispatcher = new Introflection.EventDispatcher();
};

Introflection.WebSocketConnection.prototype = {
	constructor: Introflection.WebSocketConnection,

	start: function(scene) {
		this.dispatcher.scene = scene;
		var socket = new WebSocket(this.url, this.subprotocol);
		this.setHandlers(socket);
	},

	setHandlers: function(socket) {
		this.setOnopen(socket);
		this.setOnclose(socket);
		this.setOnerror(socket);

		this.setOnmessage(socket);
	},

	setOnopen: function(socket) {
		socket.onopen = function() {
			log.info("Established a new websocket connection on " + this.url);
		};
	},

	setOnclose: function(socket) {
		socket.onclose = function(event) {
			var msg = "Closed the websocket connection on " + this.url + " ";
			if (event.wasClean) {
				log.info(msg + "without errors");
			} else {
				var errmsg = msg + "with errors. Error (" + event.code + "): " +
						event.reason;
				log.info(errmsg);
			}
		};
	},

	setOnerror: function(socket) {
		socket.onerror = function(error) {
			log.warn("An error occurred: " + error.message);
		};
	},

	setOnmessage: function(socket) {
		var that = this;
		socket.onmessage = function(event) {
			that.dispatcher.dispatch(event);
		};
	}
};

// Source: src/Stats.js
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

// Source: src/GUI.js
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

// Source: src/Camera.js
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

// Source: src/Scene.js
/*global Introflection,log,THREE*/

Introflection.Scene = function(ws) {


	this.ws = ws;

	this.scene = new THREE.Scene();

	this.container = document.getElementById('container');
	this.camera = new Introflection.Camera();
	this.renderer = new THREE.WebGLRenderer();


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

		this.stats.start();
		this.gui.start();
		this.camera.start(this.renderFunc());

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

		this.camera.resize();
		this.setRendererSize();
	},

	animate: function() {
		window.requestAnimationFrame(Introflection.Scene.prototype.animate.bind(this));
		this.render();
		this.camera.update();
		this.stats.update();
	},

	render: function() {
		var changed;

		changed = this.gui.changedItems();

		if (!changed.empty) {
			this.gui.updateOptions();
			this.drawHelpers(changed);
		}

		this.renderer.render(this.scene, this.camera.camera);
	},

	renderFunc: function() {
		var func = function() { this.render(); };
		return func.bind(this);
	},

	drawHelpers: function(changed) {
		if (changed.x) { this.grid.toggle('XZ'); }
		if (changed.y) { this.grid.toggle('YZ'); }
		if (changed.z) { this.grid.toggle('XY'); }
		if (changed.a) { this.grid.toggle('axes'); }
	}
};

// Source: src/App.js
document.addEventListener("DOMContentLoaded", function(_event) {
	Introflection.start();
});
