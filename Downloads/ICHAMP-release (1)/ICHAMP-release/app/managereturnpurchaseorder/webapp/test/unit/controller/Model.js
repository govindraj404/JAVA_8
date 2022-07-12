sap.ui.define([
    "sap/ui/model/resource/ResourceModel",
	"sap/ui/thirdparty/sinon",
    "sap/ui/thirdparty/sinon-qunit"
], function(ResourceModel) {
    "use strict";

  QUnit.module("Resource Model", {
		beforeEach: function () {
			this._oResourceModel = new ResourceModel({
				bundleUrl: sap.ui.require.toUrl("cmh/managereturnpurchaseorder") + "/i18n/i18n.properties"
			});
		},
		afterEach: function () {
			this._oResourceModel.destroy();
		}
	});
     
   

    QUnit.test("Should return the translated texts", function (assert) {

		var oModel = this.stub();
		oModel.withArgs("i18n").returns(this._oResourceModel);
		var oViewStub = {
			getModel: oModel
		};
		var oControllerStub = {
			getView: this.stub().returns(oViewStub)
		};

		// // System under test
		var i18nModel = oControllerStub.getView().getModel('i18n').getResourceBundle();
		// Assert
		assert.strictEqual(i18nModel.getText("APPLICATION_TITLE"), "Return Purchase Order", "Returns Application Title");
        assert.strictEqual(i18nModel.getText("OBJECT_DETAILS"), "Object Details", "Object Details button used for object page header");

	});    
    
    
          
});