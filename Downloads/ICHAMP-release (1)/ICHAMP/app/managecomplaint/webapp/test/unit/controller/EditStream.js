sap.ui.define([
   "cmh/managecomplaint/ext/controller/EditStream",
   "sap/ui/thirdparty/sinon",
   "sap/ui/thirdparty/sinon-qunit"
], function(EditStream) {
    "use strict";

  QUnit.module("Edit Stream", {
		beforeEach: function () {
			this._EditStream = EditStream;
		},
		afterEach: function () {
			
		}
	});
     
   

    QUnit.test("Test Enabled functionality of Stream Table", function (assert) {

        var oBindingContext = {'dummy':1}, aSelectedContexts=[{'dummy':1}];
        assert.strictEqual(this._EditStream.setEnabled(oBindingContext, aSelectedContexts), true, "It should return true");
        
        oBindingContext = {}, aSelectedContexts=[];
        assert.strictEqual(this._EditStream.setEnabled(oBindingContext, aSelectedContexts), false, "It should return false");
	});    
    
    
          
});