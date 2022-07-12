sap.ui.define([
    "sap/ui/core/mvc/ControllerExtension"
],
function (ControllerExtension){
	"use strict";
	return ControllerExtension.extend("cmh.managecomplaint.LRExtend", {
		override: {
            /** SAPi START of Life-cycle methods SAPi **/
            /**
             * Life-cycle method onInit. Called once when Controller is instantiated.
             * @public
             */
                
			onInit: function () {
				this.base.getAppComponent()._setGlobalModel();
			}
		}
	});
});