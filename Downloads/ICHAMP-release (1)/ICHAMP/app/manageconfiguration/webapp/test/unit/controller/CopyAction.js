sap.ui.define([
   "cmh/manageconfiguration/ext/controller/CopyAction",
   "sap/ui/thirdparty/sinon",
   "sap/ui/thirdparty/sinon-qunit"
], function(CopyAction) {
    "use strict";

  QUnit.module("Copy Action", {
		beforeEach: function () {
			this._CopyAction = CopyAction;
		},
		afterEach: function () {
			
		}
	});
     
   

    QUnit.test("Test Enabled functionality of Copy Button", function (assert) {

        var oBindingContext = {'dummy':1}, aSelectedContexts=[{'dummy':1}];
        assert.strictEqual(this._CopyAction.setEnabled(oBindingContext, aSelectedContexts), true, "It should return true");
        
        oBindingContext = {}, aSelectedContexts=[];
        assert.strictEqual(this._CopyAction.setEnabled(oBindingContext, aSelectedContexts), false, "It should return false");
	});    
    
    
          
});