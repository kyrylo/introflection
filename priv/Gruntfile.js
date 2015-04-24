module.exports = function(grunt) {
	grunt.initConfig({
		pkg: grunt.file.readJSON("package.json"),

		watch: {
			files: 'src/**/*.js',
			tasks: ['concat']
		},

		concat: {
			options: {
				banner: "'use strict';\n",
				process: function(src, filepath) {
					return '// Source: ' + filepath + '\n' +
						src.replace(/(^|\n)[ \t]*('use strict'|"use strict");?\s*/g, '$1');
				}
			},
			dist: {
				src: [
					'lib/three.js/OrbitControls.js',
					'src/Introflection.js',
					'src/Grid.js',
					'src/Module.js',
					'src/ModuleSpace.js',
					'src/EventDispatcher.js',
					'src/WebSocketConnection.js',
					'src/Stats.js',
					'src/GUI.js',
					'src/Camera.js',
					'src/Scene.js',
					'src/App.js'
				],
				dest: 'dist/<%= pkg.name %>.js'
			}
		},

		uglify: {
			options: {
				banner: '/*! <%= pkg.name %> <%= grunt.template.today("dd-mm-yyyy") %> */\n'
			},
			dist: {
				files: {
					'dist/<%= pkg.name %>.min.js': ['<%= concat.dist.dest %>']
				}
			}
		}
	});

	grunt.loadNpmTasks('grunt-contrib-concat');
	grunt.loadNpmTasks('grunt-contrib-uglify');
	grunt.loadNpmTasks('grunt-contrib-watch');

	grunt.registerTask('default', ['concat', 'uglify']);
};
