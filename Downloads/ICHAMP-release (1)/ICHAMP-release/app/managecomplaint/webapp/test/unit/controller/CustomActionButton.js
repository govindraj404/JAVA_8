sap.ui.define([
    "cmh/managecomplaint/ext/controller/CustomActionButton",
    "sap/ui/thirdparty/sinon",
	"sap/ui/thirdparty/sinon-qunit"
    
], function(CustomActionButton) {
    "use strict";

    QUnit.module("Custom Action Button");

	
        QUnit.test("Should test the ActionText function ", function (assert) {
			var done = assert.async();
			var oFormatterStub = sinon.stub();

            // System under test
             var fnGetactionText = CustomActionButton.actionText.bind(oFormatterStub);

			// Assert
			setTimeout(function () {
                assert.strictEqual(fnGetactionText(null, []), undefined, "Should test the actionText function");
				
				done();
			});
        });
        
        QUnit.test("Should test the ShowLink function ", function (assert) {
			var done = assert.async();
			var oFormatterStub = sinon.stub();
            // System under test
            var fnGetShowLink = CustomActionButton.showLink.bind(oFormatterStub);

			// Assert
			setTimeout(function () {
                assert.strictEqual(fnGetShowLink(null), false, "return value is false");
                assert.strictEqual(fnGetShowLink('null'), true, "return value is true");
				
				done();
			});
		});
});