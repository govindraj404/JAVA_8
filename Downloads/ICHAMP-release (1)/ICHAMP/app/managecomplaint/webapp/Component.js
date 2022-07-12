sap.ui.define([
        'sap/fe/core/AppComponent',
        'sap/ui/Device', 
        'sap/ui/model/json/JSONModel'        
        ], function(AppComponent,Device,JSONModel) {
    'use strict';

    return AppComponent.extend("cmh.managecomplaint.Component", {
        metadata: {
            manifest: "json"
        },
        
         /** 
         * This event is called at the load of the application
         * This function sets required device and global model
         * Global model holds master data of actions and business objects
         * @private
         */

         _setGlobalModel: function(){
            if(!this.getModel("device")){
                //device model
                var oDeviceModel = new JSONModel(Device);
                oDeviceModel.setDefaultBindingMode("OneWay");
                this.setModel(oDeviceModel, "device");
            }
            if(!this.getModel("globalModel")){
                //Global model to store Actions and BusinessObjectSate
                var oGlobalModel = new JSONModel({
                    bHideButton: false,
                    sTableMode :"None",
                    Nodes : [],
                    Lanes : [],
                    showDocumentFlow : false,
                    showDocumentFlowNoData : false
                }),
                oDataModel = this.getModel(),
                oActionBinding = oDataModel.bindList("/Actions");
                this.setModel(oGlobalModel, "globalModel");
               
                oActionBinding.attachEventOnce("dataReceived", function(oActionEvent){
                    var aActionData = [],
                        aActionContexts = oActionEvent.getSource().getContexts();
                        aActionContexts.forEach((oActionContext) => {
                            aActionData.push(oActionContext.getObject());
                        });  
                    oGlobalModel.setProperty("/action", aActionData);
                }.bind(this));
                oActionBinding.getContexts();

                var oBusinessObjectBinding = oDataModel.bindList("/CombineBusinessObjectStatuses",undefined,undefined,undefined,{
                   $orderby:"sequenceNumber"
                });
                oBusinessObjectBinding.attachEventOnce("dataReceived", function(oBusinessObjectEvent){
                    var aBusinessObjectData = [],
                        aBusinessObjectContexts = oBusinessObjectEvent.getSource().getContexts();
                        aBusinessObjectContexts.forEach((oBusinessObjectContext) => {
                            if(oBusinessObjectContext.getObject().sequenceNumber !== 1) {
                                aBusinessObjectData.push(oBusinessObjectContext.getObject());
                            }   
                        });  
                    oGlobalModel.setProperty("/businessObject", aBusinessObjectData);
                });
                oBusinessObjectBinding.getContexts();
            }
         }
    });  
});
