_global = {};
// setup for switch between
// local JSON model (comment out both _global settings below)
// Mockserver-based OData model (uncomment both _global settings below)
_global.mockserver = true;

sap.ui.define([
	"sap/ui/test/Opa5",
    "cmh/managecomplaint/test/integration/pages/MainListReport",
    "sap/base/util/UriParameters"
], function(Opa5, MainListReport, UriParameters) {
	"use strict";

	Opa5.extendConfig({
		arrangements: new MainListReport(),
		autoWait: true
    });
    
    _global.mockserverParameters = UriParameters.fromURL();

});