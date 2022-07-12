sap.ui.define(
    ["sap/ui/core/mvc/ControllerExtension","cmh/managecomplaint/ext/model/formatter"],
    function (ControllerExtension, formatter) {
        "use strict";
        return ControllerExtension.extend("cmh.managecomplaint.OPExtend", {
            override: {

                /** SAPi START of Life-cycle methods SAPi **/
                
            /**
             * Life-cycle method onInit. Called once when Controller is instantiated.
             * @public
             */
            onInit: function () {
                this.base.getAppComponent()._setGlobalModel();
            },

            onAfterRendering: function () {
                var sPath = ["/bHideButton"],
                    sProperty = ["visible"],
                    sModel = "globalModel";
                ["cmh.managecomplaint::ComplaintsObjectPage--fe::table::businessObjects::LineItem::StandardAction::Create",
                    "cmh.managecomplaint::ComplaintsObjectPage--fe::table::businessObjects::LineItem::StandardAction::Delete",
                    "cmh.managecomplaint::ComplaintsObjectPage--fe::table::draftBusinessObjects::LineItem::StandardAction::Create",
                    "cmh.managecomplaint::ComplaintsObjectPage--fe::table::draftBusinessObjects::LineItem::StandardAction::Delete"
                ].map(function (sId, iIndex) {
                    sProperty.map(function (sControlProperty, iPropertyIndex) {
                        if (this.base.byId(sId) && sPath[iPropertyIndex]) {
                            this.base.byId(sId).bindProperty(sControlProperty, {
                                path: sPath[iPropertyIndex],
                                model: sModel,
                                formatter: jQuery.proxy(function (oValue) {
                                    return oValue;
                                })
                            });
                        }
                    }, this);
                }, this);
            },

            /**
             * Life-cycle method onPageReady. Called once when data loaded for the view.
             * @param mParameters to get the last focused id
             * 
             * @public
             */

            onPageReady: function (mParameters) {
                var sPath = ["/sTableMode"],
                    sProperty = ["mode"],
                    sModel = "globalModel";
                [
                    "cmh.managecomplaint::ComplaintsObjectPage--fe::table::businessObjects::LineItem-innerTable",
                    "cmh.managecomplaint::ComplaintsObjectPage--fe::table::draftBusinessObjects::LineItem-innerTable"
                ].map(function (sId, iIndex) {
                    sProperty.map(function (sControlProperty, iPropertyIndex) {
                        if (this.base.byId(sId) && sPath[iPropertyIndex]) {
                            this.base.byId(sId).bindProperty(sControlProperty, {
                                path: sPath[iPropertyIndex],
                                model: sModel,
                                formatter: jQuery.proxy(function (oValue) {
                                    return oValue;
                                })
                            });
                        }
                    }, this);
                }, this);
            },

              /** 
                 * This event is called To get the node and lane details to plot document flow
                 * @param {object} oSettings :  is for ajax call 
                 * @param {object} oOPEViewModel : to set the data globally
                 * @public
                 */

            routing: {
                onAfterBinding: function(oContextInfo) {
                    var i18n = this.base.getAppComponent().getModel("i18n").getResourceBundle(),
                        oGlobalModel = this.getView().getModel("globalModel");
            
                    if(oContextInfo){
                        var sComplaint = oContextInfo.sPath.substring(oContextInfo.sPath.indexOf("ID=")+3, oContextInfo.sPath.indexOf(",")),
                        oDataModel = this.base.getAppComponent().getModel(),
                        oDocumentFlowBinding = oDataModel.bindList("/ProcessFlow",undefined,undefined,undefined,{
                            $filter:"complaintId eq "+sComplaint ,
                            $orderby:"targetcreatedAt",
                            $expand: "businessObjectStatuses($orderby=createdAt desc;$expand=businessObjectStatus),targetBusinessObjectUUID($expand=businessObjectStatuses($orderby=modifiedAt desc;$expand=businessObjectStatus))"
                       });
                        oDocumentFlowBinding.attachEventOnce("dataReceived", function(oDocFlowEvent){
                            var aDocumentFlowData = [],
                                aDocumentFlowContexts = oDocFlowEvent.getSource().getContexts();

                            aDocumentFlowContexts.forEach((oDocFlowContext) => {
                                aDocumentFlowData.push(oDocFlowContext.getObject());
                            }); 
                            var aData = aDocumentFlowData, aLane =[], aNode = [], sValueState = sap.suite.ui.commons.ProcessFlowNodeState, lanePosition = 0;
                            if(aData.length > 0){
                                aData.forEach((lanelement, index) => { 
                                    //dummy node & lane at possition 0
                                    if(index === 0 && lanelement.businessObjectStatuses){
                                        var oSourceNode = {};
                                        oSourceNode.BOTypeCode = lanelement.sourceBOTypeCode;
                                        oSourceNode.nodeID = lanelement.sourceBusinessObjectUUID;
                                        oSourceNode.laneID = lanelement.sourceStreamTypeCode;
                                        oSourceNode.state = sValueState.Positive;
                                        oSourceNode.stateText = lanelement.businessObjectStatuses[0].businessObjectStatus.name;
                                        oSourceNode.parentNode = "";
                                        oSourceNode.nodeTitle = lanelement.sourceBOTypeName;
                                        oSourceNode.titleAbbreviation = lanelement.sourceIdentifier;
                                        oSourceNode.child = [];
                                        oSourceNode.text = []; 
                                        oSourceNode.text.push(i18n.getText("CREATED_ON", [formatter.showFormattedDate(lanelement.sourcecreatedAt)]));
                                        oSourceNode.text.push(i18n.getText("CREATED_BY", [lanelement.sourcecreatedBy]));
                                        oSourceNode.text.push(lanelement.sourceBOTypeCode);
                                        oSourceNode.BOTypeCode= lanelement.sourceBOTypeCode;
                                        aNode.push(oSourceNode);

                                        var oSourceStreamLane = {};
                                        oSourceStreamLane.laneID = lanelement.sourceStreamTypeCode;
                                        oSourceStreamLane.label = lanelement.sourceStreamTypeName;
                                        oSourceStreamLane.lanePosition = lanePosition;
                                        oSourceStreamLane.laneIconSrc = "sap-icon://monitor-payments";

                                        lanePosition = lanePosition+1;
                                        aLane.push(oSourceStreamLane);
                                        
                                    }
                                    //end of dummy node & lane
                                    var oLane = {}, bLaneExist = false;
                                    
                                    oLane.laneID = lanelement.targetStreamTypeCode;
                                    oLane.label = lanelement.targetStreamTypeName;
                                    switch (lanelement.targetBOTypeCode) {
                                        case 'QN':
                                            oLane.laneIconSrc = "sap-icon://monitor-payments";
                                        break;
                                        case 'RPO':
                                            oLane.laneIconSrc = "sap-icon://shipping-status";
                                        break;
                                        case 'S8D':
                                            oLane.laneIconSrc = "sap-icon://monitor-payments";
                                        break;
                                        case 'CLM':
                                            oLane.laneIconSrc = "sap-icon://sales-document";
                                        break;
                                    }
                                    aLane.filter(function(laneElement) {
                                        if(laneElement.laneID === oLane.laneID){
                                            bLaneExist=true;
                                        }
                                    });

                                    if(!bLaneExist){
                                        oLane.lanePosition = lanePosition;
                                        lanePosition = lanePosition+1;
                                        aLane.push(oLane);
                                    }

                                    var oNode = {};
                                    oNode.BOTypeCode = lanelement.targetBOTypeCode;
                                    oNode.nodeID = lanelement.targetBusinessObjectUUID_ID;
                                    oNode.laneID = lanelement.targetStreamTypeCode;
                                    oNode.state = sValueState.Positive;
                                    oNode.stateText = lanelement.targetBusinessObjectUUID.businessObjectStatuses ? lanelement.targetBusinessObjectUUID.businessObjectStatuses[0].businessObjectStatus.name : "";
                                    oNode.parentNode = lanelement.sourceBusinessObjectUUID;
                                    oNode.nodeTitle = lanelement.targetBOTypeName;
                                    oNode.titleAbbreviation = lanelement.targetIdentifier;
                                    oNode.child = [];
                                    oNode.text = [];
                                    oNode.text.push(i18n.getText("CREATED_ON", [formatter.showFormattedDate(lanelement.targetcreatedAt)])); 
                                    oNode.text.push(i18n.getText("CREATED_BY", [lanelement.targetcreatedBy]));
                                    oNode.text.push(lanelement.targetBOTypeCode);
                                    oNode.BOTypeCode = lanelement.targetBOTypeCode;
                                    aNode.push(oNode);
                                });
                                //add parent child condition
                                aNode.forEach((parentElement)=>{
                                    aNode.some(function (nodeElement) { 
                                        if(nodeElement.nodeID == parentElement.parentNode){
                                            nodeElement.child.push(parentElement.nodeID);
                                        } 
                                    })
                                });
                                oGlobalModel.setProperty("/Nodes",aNode);
                                oGlobalModel.setProperty("/Lanes",aLane);
                                oGlobalModel.setProperty("/showDocumentFlow",true);
                                oGlobalModel.setProperty("/showDocumentFlowNoData",false)
                                oGlobalModel.refresh(true);
                            }else{
                                oGlobalModel.setProperty("/showDocumentFlow",false);
                                oGlobalModel.setProperty("/showDocumentFlowNoData",true);
                                oGlobalModel.refresh(true);
                            }
                        }.bind(this));
                        oDocumentFlowBinding.getContexts();
                    }                                            
                }
            }

        }
    });
});