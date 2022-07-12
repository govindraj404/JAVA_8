sap.ui.define(["cmh/managecomplaint/ext/controller/OPExtend.controller",
"sap/ui/base/ManagedObject", 
"sap/ui/model/json/JSONModel",
"sap/ui/core/Component",
"sap/ui/thirdparty/sinon",
"sap/ui/thirdparty/sinon-qunit"
], function (ObjectPageExtController, ManagedObject, JSONModel, CoreComponent) {
	"use strict";	

	QUnit.module("Complaint OPExtend Controller", function (hooks) {
		
        hooks.beforeEach(function () {
			// empty by intention			
			this._oApplicationModel = new JSONModel();
			this._oAppController = new ObjectPageExtController();
		});

		hooks.afterEach(function () {
			// empty by intention
            this._oApplicationModel.destroy();
			this._oAppController.destroy();
        });         
        
        QUnit.test("Test", function(assert){
            assert.ok(true, "Test successfully completed");
        });
	
		
	});

});