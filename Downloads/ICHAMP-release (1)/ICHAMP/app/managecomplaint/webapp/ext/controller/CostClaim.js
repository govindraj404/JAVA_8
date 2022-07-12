sap.ui.define([],
function () {
    "use strict";

    return {
        
         /** 
         * This event is called whenever user Clicks on Add Costs Button of Cost Claim Dialog
         * User will be navigated to Cost Collector App 
         * @public
         */

        onAddCost: function () {
            var oView = this.extension.cmh.managecomplaint.OPExtend.getView(),
                sComplaint = oView.getBindingContext().getObject().ID,
                oParams = {
                    ID: sComplaint
                };
            this.intentBasedNavigation.navigateOutbound("costcollector", oParams);
        },

        /** 
         * This event is called whenever user Clicks on "Transfer to Claim" Button of Cost Claim Dialog
         * User selection will be updated in the transferToCliam field of CostCollectors entity
         * If Row is selected transferToCliam field is true else false
         * All changes are captured in a batch call
         * User will be navigated to Claim creation After successfull call 
         * @public
         */

        onSaveCostDialog: function () {
            var oView = this.extension.cmh.managecomplaint.OPExtend.getView(),
                oModel = oView.getModel(),
                sComplaint = oView.getBindingContext().getObject().ID,
                oParams = {
                    preferredMode: "create",
                    complaint_ID: sComplaint
                };

            var fnSuccess = function (oSucess) {
                this.oCostToClaimDialog.then(function (oDialog) {
                    oDialog.close();
                });
            
                this.intentBasedNavigation.navigateOutbound("claim", oParams);

            }.bind(this);

            var fnError = function (oError) {
                // MessageToast.show(oError.message);
            }.bind(this);

            if (oModel.hasPendingChanges()) {
                oModel.submitBatch("updateCostCollectors").then(fnSuccess, fnError);
            } else {
                this.oCostToClaimDialog.then(function (oDialog) {
                    oDialog.close();
                });
                this.intentBasedNavigation.navigateOutbound("claim", oParams);
            }

        },     
        
        /** 
         * This event is called whenever user Clicks on Cancel Button of Cost Claim Dialog
         * User input will be reset and Dialog will be closed
         * @public
         */

        onCancelCostDialog: function () {
            var oView = this.extension.cmh.managecomplaint.OPExtend.getView(),
                oModel = oView.getModel();
            if (oModel.hasPendingChanges()) {
                oModel.resetChanges("updateCostCollectors");
            }
            this.oCostToClaimDialog.then(function (oDialog) {
                oDialog.close();
            });
        },
        
    }
});