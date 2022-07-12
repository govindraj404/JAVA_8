sap.ui.define([
    "sap/ui/core/Fragment",
    "sap/ui/model/json/JSONModel",
    "sap/m/MessageToast"
],
function (Fragment, JSONModel, MessageToast) {
    "use strict";

    return {

        /** Start of Edit Stream functionality **/
        /** 
         * This event is called whenever user Clicks on Custom Edit Stream Button
         * User is presented with a tree list of hierarchical of Streams and document
         * As there is no support of OData V4 to sap.m.Tree with direct data binding
         * We are using JSON Model to repesent data, data is fatched with AJAX call
         * @public
         * @param {object} oContext :  is the binding context of the current entity 
         * @param {array} aSelectedContexts : contains an array of binding contexts corresponding to selected items in case of table action
         */
       openEditStreamDialog: function (oContext, aSelectedContexts) {
            var oView = this.extension.cmh.managecomplaint.OPExtend.getView(),
                oModel = new JSONModel(),
                i18n = oView.getModel("i18n").getResourceBundle();

             // create dialog lazily
            if (!this.oEditStreamDialog) {
                this.oEditStreamDialog = Fragment.load({
                    id: oView.getId(),
                    name: "cmh.managecomplaint.ext.fragment.EditStream",
                    controller: this
                }).then(function (oDialog) {
                    oView.addDependent(oDialog);
                    return oDialog;
                });
            }
          
            if(oView.getBindingContext()){
                var sComplaint = oView.getBindingContext().getObject().ID,
                   oDataModel = this.getModel(),
                   bIsActiveEntity = !this.getModel("ui").getData().isEditable,
                   oStreamBinding = oDataModel.bindList("/Streams",undefined,undefined,undefined,{
                   $filter:"parentID_ID eq "+sComplaint +" and IsActiveEntity eq "+bIsActiveEntity ,
                   $orderby: "sequenceNumber",
                   $expand:"businessObjects($expand=businessObjectType;$orderby=businessObjectType_code),streamType"
                 });
                oStreamBinding.attachEventOnce("dataReceived", function(oStreamEvent){
                    var aStreamData = [],
                        aStreamContexts = oStreamEvent.getSource().getContexts();

                    aStreamContexts.forEach((oStreamContext) => {
                        aStreamData.push(oStreamContext.getObject());
                    }); 

                     var aData = {
                        "AllSelect": true,
                        "value": []
                    };
                  if(aStreamData.length >0){
                        aStreamData.forEach((oStreamElement) => {
                            var object = {
                                "value": []
                            };
                            object.ID = oStreamElement.ID;
                            object.IsActiveEntity = oStreamElement.IsActiveEntity;
                            if (oStreamElement.streamType) {
                                object.code = oStreamElement.streamType.code;
                                object.description = oStreamElement.streamType.name;
                                object.isRelevant = oStreamElement.isRelevant;
                            }

                            if (oStreamElement.isRelevant === false || oStreamElement.isRelevant === null) {
                                aData.AllSelect = false;
                                // return;
                            }
                            oStreamElement.businessObjects.forEach((oAdaptStreamElement) => {
                                var oAdaptStreamObject = {};
                                oAdaptStreamObject.ID = oAdaptStreamElement.ID;
                                oAdaptStreamObject.IsActiveEntity = oAdaptStreamElement.IsActiveEntity;
                                oAdaptStreamObject.businessObjectID_ID = oAdaptStreamElement.businessObjectID_ID;
                                oAdaptStreamObject.businessObjectType_code = oAdaptStreamElement.businessObjectType_code;
                                if (oAdaptStreamElement.businessObjectType) {
                                    oAdaptStreamObject.description = oAdaptStreamElement.businessObjectType.name;
                                    oAdaptStreamObject.isRelevant = oAdaptStreamElement.isRelevant;
                                }

                                object.value.push(oAdaptStreamObject);
                                if (oAdaptStreamElement.isRelevant === false || oAdaptStreamElement.isRelevant === null) {
                                    aData.AllSelect = false;
                                    // return;
                                }
                            });
                            aData.value.push(object);
                        });
                        oModel.setData(aData);
                        this.oEditStreamDialog.then(function (oDialog) {
                            oDialog.open();
                            oDialog.setModel(oModel, "oEditStreams");
                            oDialog.getContent()[1].expandToLevel(1);
                        });
                    }else{
                        //no Strem or BO available to flag
                        var msg = i18n.getText("ERROR_NO_STREAM_BO");
                        MessageToast.show(msg);
                    }
                    
                }.bind(this));
                oStreamBinding.getContexts();
            }else{
                //no Strem or BO available to flag
                var msg = i18n.getText("ERROR_NO_STREAM_BO");
                MessageToast.show(msg);
            }
        },


        /** 
         * This event is called whenever user Clicks on "Save" Button of Edit Stream Dialog
         * User selection will be updated in the flag(yet to implement) of Stream(yet to implement) entity
         * All changes are captured in a batch call
         * Stream Table will be updated After successfull call 
         * @public
         */
        onSaveDialog: function (oEvent) {
            
            var oModel = this.getModel(),
                oView = this.extension.cmh.managecomplaint.OPExtend.getView(),
                exAPI = oView.oController.extensionAPI,
                aData = oEvent.getSource().getModel("oEditStreams").getData().value;                

            aData.forEach((oStreamElement) => {

                
                oStreamElement.value.forEach((documentElement) => {

                    var oDocumentContextBinding = oModel.bindContext(
                        "/BusinessObjects(ID=" + documentElement.ID + ",IsActiveEntity=" + documentElement.IsActiveEntity + ")",
                        /*oContext*/
                        undefined, {
                            $$updateGroupId: "myGroup"
                        }
                    );
                    oDocumentContextBinding.getBoundContext().setProperty("isRelevant", documentElement.isRelevant);
                });

                var oContextBinding = oModel.bindContext(
                    "/Streams(ID=" + oStreamElement.ID + ",IsActiveEntity=" + oStreamElement.IsActiveEntity + ")",
                    /*oContext*/
                    undefined, {
                        $$updateGroupId: "myGroup"
                    }
                );
                oContextBinding.getBoundContext().setProperty("isRelevant", oStreamElement.isRelevant);

            });
            oModel.submitBatch("myGroup").finally(function () {
                //to do show the error pop over button of framwork 
                this.oEditStreamDialog.then(function (oDialog) {
                    exAPI.refresh();
                    oDialog.close();
                });
            }.bind(this));
        },

        /** 
         * This event is called whenever user Clicks on Cancel Button of Edit Stream Dialog
         * Dialog will be closed
         * @public
         */

        onCloseDialog: function () {
            this.oEditStreamDialog.then(function (oDialog) {
                oDialog.close();
            });
        },

        /** 
         * This event is called whenever user Clicks on any Checkbox on Edit Stream Dialog
         * Change the Selected Item Flag according to the user selection
         * Change the Child Items Flag according to the user selection of a Parent
         * Change the Parent Item Flag, if all child nodes are selected or de-selected
         * Change Select all checkbox, if all items in the list are selected or de-selected
         * @public
         */

        onSelect: function (oEvent) {
            var bSelect = oEvent.getParameter("listItem").isSelected(),
                oSelectedObject = oEvent.getParameter("listItem").getBindingContext("oEditStreams").getObject(),
                oView = this.extension.cmh.managecomplaint.OPExtend.getView(),
                i18n = oView.getModel("i18n").getResourceBundle();
            

            if(oSelectedObject.businessObjectID_ID || oSelectedObject.businessObjectType_code){
                if(oSelectedObject.businessObjectID_ID !== null || oSelectedObject.businessObjectType_code === 'QN' ){
                    oSelectedObject.isRelevant = !bSelect;
                    MessageToast.show(i18n.getText(oSelectedObject.businessObjectType_code === 'QN' ? "QUALITY_NOTIFICATION_MANADATORY": "BUSINESS_OBJECT_ALREADY_CREATED"));
                   return;
                }
            }

            // check and change the Child Items
            var bStreamFlag = true;
            if (oSelectedObject.value && oSelectedObject.code !== 'QLTY') {
                oSelectedObject.value.forEach((oChildElement) => {
                    if(oChildElement.businessObjectID_ID || oChildElement.businessObjectType_code){
                        if(oChildElement.businessObjectID_ID !== null || oChildElement.businessObjectType_code === 'QN'){
                            bStreamFlag = false;
                            return;
                        }
                    }
                    oChildElement.isRelevant = bSelect;
                });
            }

            // change the Selected Item
            if(bStreamFlag && oSelectedObject.code !== 'QLTY'){
                oSelectedObject.isRelevant = bSelect;
            }else{
                MessageToast.show(i18n.getText("SOME_BUSINESS_OBJECT_ALREADY_CREATED"));
                oSelectedObject.isRelevant = !bSelect;
            }
            

            //change the Parent Items
            if (oEvent.getParameter("listItem").getParentNode()) {
                var oParent = oEvent.getParameter("listItem").getParentNode(),
                    aDocuments = oParent.getBindingContext("oEditStreams").getObject().value;
                if (aDocuments.some(Document => Document.isRelevant === true)) {
                    oParent.getBindingContext("oEditStreams").getObject().isRelevant = true;
                }else {
                    oParent.getBindingContext("oEditStreams").getObject().isRelevant = false;
                }
            }

            //check all checkbox now 
            var allData = oEvent.getSource().getModel("oEditStreams").getData(),
                aData = allData.value;
                allData.AllSelect = true;
                aData.forEach((oStreamElement) => {
                    if (oStreamElement.isRelevant === false) {
                        allData.AllSelect = false;
                        return;
                    }
                    if(oStreamElement.value){
                        oStreamElement.value.forEach((oAdaptStreamElement) => {
                            if (oAdaptStreamElement.isRelevant === false) {
                                allData.AllSelect = false;
                                return;
                            }
                        });
                    }
                    
                });
        },

        /** 
         * This event is called whenever user Clicks on select all Checkbox on Edit Stream Dialog
         * All the items will be selected once the select all box will be clicked
         * @public
         */

        onSelectAllCheckbox: function (oEvent) {
            var bSelect = oEvent.getParameter("selected"),
                aList = this.byId("sEditStreamTreeList").getItems(),
                oModel = this.byId("sEditStreamTreeList").getModel("oEditStreams");

            for (var i = 0; i < aList.length; i++) {
                var oObject = aList[i].getBindingContext("oEditStreams").getObject();
                if(oObject.businessObjectID_ID || oObject.businessObjectType_code){
                    if(oObject.businessObjectID_ID !== null || oObject.businessObjectType_code === 'QN'){
                        oModel.setProperty(aList[i].getBindingContext("oEditStreams").getPath() + "/isRelevant", oObject.isRelevant);
                        continue;
                    }
                }
                oModel.setProperty(aList[i].getBindingContext("oEditStreams").getPath() + "/isRelevant", bSelect);
            }

            // check parent node for each 
            for (var i = 0; i < aList.length; i++) {
                if (aList[i].getParentNode()) {
                    var oParent = aList[i].getParentNode(),
                        aDocuments = oParent.getBindingContext("oEditStreams").getObject().value;
                    if (aDocuments.some(Document => Document.isRelevant === true)) {
                        oParent.getBindingContext("oEditStreams").getObject().isRelevant = true;
                    }else {
                        oParent.getBindingContext("oEditStreams").getObject().isRelevant = false;
                    }
                }
            }

            oModel.refresh(true);
        },

        /** End of Edit Stream functionality **/

        setEnabled: function (oBindingContext, aSelectedContexts)
            {
               return (aSelectedContexts && aSelectedContexts.length === 1);
            
            }

    }
});