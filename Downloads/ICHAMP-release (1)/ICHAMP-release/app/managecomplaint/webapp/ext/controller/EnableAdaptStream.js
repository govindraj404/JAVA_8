sap.ui.define([], function () {
    "use strict";

    return {
        enableEditStreamButton: function (oBindingContext) {
            var bEnabled = false,
                oCurrentObject = oBindingContext && oBindingContext.getObject();

            if (oCurrentObject && oCurrentObject.hasOwnProperty("complaintStatus_code") && oCurrentObject.hasOwnProperty("IsActiveEntity")) {
                bEnabled =
                    !(oCurrentObject.complaintStatus_code === "CLSD" || oCurrentObject.complaintStatus_code === "DISCD") &&
                    oCurrentObject.IsActiveEntity;
            }
            return bEnabled;
        }
    }
});