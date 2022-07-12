/* global QUnit */
QUnit.config.autostart = false;

sap.ui.getCore().attachInit(function () {
	"use strict";

	sap.ui.require([
		"cmh/managesupplier8d/test/unit/AllTests"
	], function () {
		QUnit.start();
	});
});