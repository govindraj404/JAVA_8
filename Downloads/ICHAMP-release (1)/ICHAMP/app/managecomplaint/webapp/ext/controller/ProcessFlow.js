sap.ui.define(["sap/ui/core/Fragment",
    "sap/ui/model/json/JSONModel"
],
function (Fragment, JSONModel) {
    "use strict";
    return {

        /* Function to Reset Document Flow*/

        onReset: function () {
            this.byId("sDocumentProcessFlow").setZoomLevel(sap.suite.ui.commons.ProcessFlowZoomLevel.Three);
        },

        /* Function to Zoom In Document Flow*/

        onZoomIn: function () {
            this.byId("sDocumentProcessFlow").zoomIn();
        },
        /* Function to Zoom Out Document Flow*/

        onZoomOut: function () {
            this.byId("sDocumentProcessFlow").zoomOut();
        },

        onNodePress: function (oEvent) {
            var sPathToBeRead,sFieldsToBeSelected,
                sExpandQuery,
                oNode = oEvent.getParameters(),
                sBOType = oEvent.getParameters().mProperties.texts[2],
                oView = this.extension.cmh.managecomplaint.OPExtend.getView(),
                sComplaintID = oView.getBindingContext().getObject().ID,
                oDataModel = this.getModel();

            if (!this.oBusinessObjectQuickView) {
                this.oBusinessObjectQuickView = Fragment.load({
                    id: oView.getId(),
                    name: "cmh.managecomplaint.ext.fragment.BusinessObjectQuickView",
                    controller: this
                }).then(function (oBusinessObjectQuickView) {
                    oView.addDependent(oBusinessObjectQuickView);
                    return oBusinessObjectQuickView;
                });
            }

            switch (sBOType) {
                case 'QN':
                    sPathToBeRead = "/QualityNotification";
                    sFieldsToBeSelected = "identifier,number,navigation,company_ID,purchaseOrderItem,purchaseOrderNumber";
                    sExpandQuery = "contactPerson($select=businessPartnerName1),defect($select=ID,identifier;$expand=defectGroup($select=description),defectCode($select=description)),personResponsible($select=businessPartnerName1)";
                    break;
                case 'CLM':
                    sPathToBeRead = "/Claim";
                    sFieldsToBeSelected = "identifier,number,navigation,company_ID";
                    sExpandQuery = "contactPerson($select=businessPartnerName1),personResponsible($select=businessPartnerName1)";
                    break;
                case 'S8D':
                    sPathToBeRead = "/Supplier8DProcess";
                    sFieldsToBeSelected = "identifier,number,navigation,company_ID,requestStartDate,requestEndDate";
                    sExpandQuery = "contactPerson($select=businessPartnerName1),defect($select=identifier;$expand=parent($select=identifier)),personResponsible($select=businessPartnerName1)";
                    break;
                case 'RPO':
                    sPathToBeRead = "/ReturnPurchaseOrder";
                    sFieldsToBeSelected = "identifier,number,navigation,company_ID";
                    sExpandQuery = "contactPerson($select=businessPartnerName1),personResponsible($select=businessPartnerName1),reason,purchasingGroup";
                    break;
                default:
                    break;
            }
            if (sPathToBeRead.length && sExpandQuery.length) {
                var oBinding = oDataModel.bindList(sPathToBeRead, undefined, undefined, undefined, {
                    $select: sFieldsToBeSelected,
                    $filter: "complaint_ID eq " + sComplaintID,
                    $expand: sExpandQuery
                });
                oBinding.attachEventOnce("dataReceived", function (oResponse) {
                    if (oResponse && oResponse.getSource() && oResponse.getSource().getContexts() && oResponse.getSource().getContexts().length) {
                        var oResponseData = oResponse.getSource().getContexts()[0].getObject();
                        oResponseData.BOType = sBOType;
                        oResponseData.oViewContext = oView;
                    }
                    this.oBusinessObjectQuickView.then(function (oBusinessObjectQuickView) {
                        if (!oBusinessObjectQuickView.getModel("dataModel")) {
                            oBusinessObjectQuickView.setModel(new JSONModel(oResponseData), "dataModel");
                        } else {
                            oBusinessObjectQuickView.getModel("dataModel").setData(oResponseData, false);
                        }
                        oBusinessObjectQuickView.openBy(oNode);
                    });
                }.bind(this));
                oBinding.getContexts();
            }
        },
        
        getNavigationURL: function (sBOType, sIdentifier,sNavigationPath,oViewContext) {
            var sSemanticObject, sAction = "managewd",sFormattedURL;
            switch (sBOType) {
                case 'QN':
                    sSemanticObject = "quality";
                    sFormattedURL = ["#quality-managewd",sNavigationPath,"?RIWO00-QMNUM=",sIdentifier].join("");
                    break;
                case 'CLM':
                    sSemanticObject = "claim";
                    sFormattedURL = ["#claim-managewd",sNavigationPath,"?WTY_PNH_DYNPRO-CLMNO=",sIdentifier].join("");
                    break;
                case 'S8D':
                    sSemanticObject = "StepBasedSupplierIssueProcess";
                    sAction = "manage";
                    sFormattedURL = ["#StepBasedSupplierIssueProcess-manage?suplrIssProcId=",sIdentifier].join("");
                    break;
                case 'RPO':
                    sSemanticObject = "returnpo";
                    sFormattedURL = ["#returnpo-managewd",sNavigationPath,"?MEPO_TOPLINE-EBELN=",sIdentifier].join("");
                    break;
                default:
                    break;
            }

            sap.ushell.Container.getService("CrossApplicationNavigation").isNavigationSupported(
                [{
                    target : {
                      semanticObject : sSemanticObject,
                      action: (sBOType === "S8D") ? sAction : [sAction,sNavigationPath].join(""),
                    }
                }]).done(function (aResponses) {
                    oViewContext.byId("sIdentifier").setType((aResponses.length &&  aResponses[0].supported) ? "link" :  "text");  
                }).fail(function() {
                    oViewContext.byId("sIdentifier").setType("text");  
                });  
           return sFormattedURL;
        }

    }
});