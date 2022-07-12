sap.ui.define([
        "sap/ui/core/Fragment"
    ],
    function (Fragment) {
        "use strict";

        return {

            /** 
             * This event is called whenever user Clicks on Buttons or LInk on attachment cloumn  of Stream Table
             * If The stream type is Claim Processing(CLP), open the Cost To claim Dialog
             * If the stream type is Quality, Nav to Quality App
             * @public
             * @param {sap.ui.base.Event} oEvent Event triggered by the button
             */
            onClickCreate: function (oEvent) {
                var oObject = oEvent.getSource().getBindingContext().getObject(),
                    oView= this.extension.cmh.managecomplaint.OPExtend.getView(),
                    sComplaint = oView.getBindingContext().getObject().ID,
                    sbusinessObjectID = oObject.businessObjectID_ID,
                    sType = oObject.businessObjectType_code,
                   
                    sControllName = oEvent.getSource().getMetadata().getName(),
                    oParams = {};
                if(sControllName === "sap.m.Button"){
                    oParams.preferredMode = "create";
                    oParams.complaint_ID = sComplaint;
                   
                }else{
                    oParams.ID = sbusinessObjectID;
                    oParams.IsActiveEntity = true;
                }

                switch (sType) {
                    case 'CLM':
                        if(sControllName === "sap.m.Button"){
                            if (!this.oCostToClaimDialog) {
                                this.oCostToClaimDialog = Fragment.load({
                                    id: oView.getId(),
                                    name: "cmh.managecomplaint.ext.fragment.CostClaim",
                                    controller: this
                                }).then(function (oDialog) {
                                    oView.addDependent(oDialog);
                                    return oDialog;
                                });
                            } 
                            this.oCostToClaimDialog.then(function (oDialog) {
                                oDialog.open();
                            });
                        }else{
                            this.intentBasedNavigation.navigateOutbound("claim", oParams);
                        }
                    break;
                    case 'QN':
                        this.intentBasedNavigation.navigateOutbound("quality", oParams);
                    break;
                    case 'RPO':
                        this.intentBasedNavigation.navigateOutbound("returnpo", oParams);
                    break;
                    case 'S8D':
                        this.intentBasedNavigation.navigateOutbound("supplier", oParams);
                    break;
                }
            },

            /** 
             * This event is called to show the text of the button of the action column in Stream table
             * 
             * @public
             */

            actionText: function (sCode, aActionList) {
                var sText;
                aActionList.forEach((oActionElement) => {
                    if (sCode === oActionElement.businessObjectType_code) {
                        sText = oActionElement.name;
                    }
                });
                return sText;
            },

          

            /** 
             * This event is called when to show the link of the action column in Stream table
             * 
             * @public
             */

            showLink: function (sCode) {
                return sCode !== null;
            }
        }
    });