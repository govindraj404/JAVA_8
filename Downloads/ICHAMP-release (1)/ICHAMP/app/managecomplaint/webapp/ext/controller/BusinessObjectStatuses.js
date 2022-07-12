sap.ui.define([
    "sap/suite/ui/commons/MicroProcessFlowItem",
    "sap/base/strings/formatMessage"    
],
function (MicroProcessFlowItem,formatMessage) {
    "use strict";

    return {
        /** 
         * This event is called before rendering the micro process flow of a row in Stream Table
         * If Complaint status is Created(CRTD) button will be enabled else disabled
         * @public
         * @param {object} oControlEvent :  event of the binded control
         */
        formatMessage :formatMessage,
        renderStatuses: function (oControlEvent) {

            var oContext = oControlEvent.getSource().getBindingContext(),
                oView = this.extension.cmh.managecomplaint.OPExtend.getView(),
                oGlobalModel = oView.getModel("globalModel"),
                oDataModel = oView.getModel();

           if (oContext.getProperty("businessObjectType_code")) {

                var aAllBusinessObject = oView.getModel("globalModel").getData().businessObject,
                    sStatusCode = oContext.getProperty("businessObjectType_code"),
                    aFilteredBusinessObject,
                    sValueState = sap.ui.core.ValueState,
                    aFinalMicroProcessFlow = [];
                    if(aAllBusinessObject) {
                        aFilteredBusinessObject =  aAllBusinessObject.filter(obj => sStatusCode === obj.businessObjectType);
                    }
                
                    if (aFilteredBusinessObject) {
                        aFilteredBusinessObject.forEach((oBusinessObjectElement) => {
                            var oBusinessObject = {};
                            oBusinessObject.state = sValueState.None;
                            oBusinessObject.name = oBusinessObjectElement.name;
                            oBusinessObject.code = oBusinessObjectElement.code;
                            aFinalMicroProcessFlow.push(oBusinessObject);
                        });
                    }
                    //Plot all posible status 
                    oGlobalModel.setProperty("/" + oContext.getObject().ID, aFinalMicroProcessFlow);
                    var oItemTemplate = new MicroProcessFlowItem({
                        state: "{globalModel>state}",
                        title: { parts: ['i18n>MICRO_PROCESS_FLOW_STATUS','globalModel>name','globalModel>state'], 
                            formatter: function(i18ntext,sName,sStatus){
                                if(sStatus === sValueState.None){
                                    return formatMessage(i18ntext,sName);
                                }else{
                                    return sName;
                                }        
                            }
                        },
                       icon: "{globalModel>icon}"
                    });
                    oControlEvent.getSource().bindAggregation("content", "globalModel>/" + oContext.getObject().ID, oItemTemplate);
                    oGlobalModel.refresh(true);

                    //Read the created BO, and color 
                    var sPath = oView.getBindingContext().sPath + "/businessObjects",   
                    oBusinessObjectBinding = oDataModel.bindList(sPath ,undefined,undefined,undefined,{
                    $select:"ID,businessObjectType_code",
                    $expand:"businessObjectID($expand=businessObjectStatuses($select=businessObjectStatus_code))"
                    });
                    oBusinessObjectBinding.attachEventOnce("dataReceived", function(oBOEvent){
                        var aBusinessObjectData = [],
                            aBusinessObjectContexts = oBOEvent.getSource().getContexts();
                            aBusinessObjectContexts.forEach((oBOContext) => {
                                aBusinessObjectData.push(oBOContext.getObject());
                            });  
                    
                        for (var i = 0; i < aBusinessObjectData.length; i++) {
                            var aFilteredBusinessObject = oGlobalModel.getData()[aBusinessObjectData[i].ID];
                            if (aBusinessObjectData[i].businessObjectID) {
                                var afilteredCreatedBusinessObject = aBusinessObjectData[i].businessObjectID.businessObjectStatuses, ilastIndexOfCreated = 0;
                                aFilteredBusinessObject.forEach((oBusinessObjectElement, index) => {
                                    var bBusinessObjectFlag = afilteredCreatedBusinessObject.some(function (o1) {
                                        return oBusinessObjectElement.code === o1.businessObjectStatus_code; // return the ones with equal id
                                    });
                                    if (bBusinessObjectFlag) {
                                        oBusinessObjectElement.state = sValueState.Success;
                                        ilastIndexOfCreated = index;
                                    }
                                });
                                for(var j= 0; j < ilastIndexOfCreated; j++){
                                    if(aFilteredBusinessObject[j].state !== sValueState.Success ){
                                        aFilteredBusinessObject[j].state = sValueState.Warning;
                                        aFilteredBusinessObject[j].icon = "sap-icon://warning";
                                    }
                                }
                            }
                        }
                        oGlobalModel.refresh(true);
                    }.bind(this));
                    oBusinessObjectBinding.getContexts();
            }

        },
    }
});