sap.ui.define([
   "cmh/managecostcollector/ext/controller/EditCost",
   "sap/ui/thirdparty/sinon",
   "sap/ui/thirdparty/sinon-qunit"
], function(EditCost) {
    "use strict";

  QUnit.module("Edit Cost", {
		beforeEach: function () {
			this._EditCost = EditCost;
		},
		afterEach: function () {
			
		}
	});
     
   

    QUnit.test("Test Enabled functionality of Edit Button in Cost Collector table", function (assert) {

        var oBindingContext = {'dummy':1}, aSelectedContexts=[{'dummy':1}];
        assert.strictEqual(this._EditCost.setEnabled(oBindingContext, aSelectedContexts), true, "It should return true");
        
        oBindingContext = {}, aSelectedContexts=[];
        assert.strictEqual(this._EditCost.setEnabled(oBindingContext, aSelectedContexts), false, "It should return false");
	});    
    
    
          
});