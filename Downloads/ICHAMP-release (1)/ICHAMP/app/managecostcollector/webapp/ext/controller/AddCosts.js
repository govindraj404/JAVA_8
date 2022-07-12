sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "sap/m/MessageToast"
],
    function (JSONModel,MessageToast) {
        "use strict";
        return {

            /** 
            * This event is called whenever user Clicks on "Add Costs" Button of Cost Claim Dialog
            * User will be navigated to Cost Collector Application 
            * @public
            * @param {object} oContext :  is the binding context of the current entity 
            * @param {array} aSelectedContexts : contains an array of binding contexts corresponding to selected items in case of table action
            */

            openAddCostsDialogFR: function (oContext, aSelectedContexts) {
                this.editFlow
                    .invokeAction("CostCollectorService.AddFR", oContext, { label: "{i18n>ADD}" })
                    .then(jQuery.proxy(function (oresponse) {
                    
                    }, this), this)
                    .catch(jQuery.proxy(function (oresponse) {
                      
                    }, this), this);

            },
             /** 
              * This event is called whenever user Clicks on "Add Costs" Button of Cost Claim Dialog
              * User will be navigated to Cost Collector Application 
              * @public
              * @param {object} oContext :  is the binding context of the current entity 
              * @param {array} aSelectedContexts : contains an array of binding contexts corresponding to selected items in case of table action
              */

             openAddCostsDialogSABL: function (oContext, aSelectedContexts) {
                 this.editFlow
                     .invokeAction("CostCollectorService.AddSUBL", oContext, {
                         label: "{i18n>ADD}"
                     })
                     .then(jQuery.proxy(function (oresponse) {

                     }, this), this)
                     .catch(jQuery.proxy(function (oresponse) {

                     }, this), this);

             }
        };
    });
