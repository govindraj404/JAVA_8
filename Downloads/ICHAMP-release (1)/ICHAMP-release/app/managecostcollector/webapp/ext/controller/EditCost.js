sap.ui.define([
],
    function () {
        "use strict";
        return {

            /** 
            * This event is called whenever user Clicks on "Edit Costs" Button of Cost Cost Collector Table
            * User will open the cost collector edit dialog box 
            * @public
            * @param {object} oContext :  is the binding context of the current entity 
            * @param {array} aSelectedContexts : contains an array of binding contexts corresponding to selected items in case of table action
            */

            openEditCostsDialog: function (oContext, aSelectedContexts) {
              var oView = this.extension.cmh.managecostcollector.OPExtend.getView();
              this.editFlow
                  .invokeAction(((oView.byId("cmh.managecostcollector::ComplaintsObjectPage--fe::table::costCollector::LineItem-innerTable").getSelectedItem().getBindingContext().getObject().itemType_code) === "SUBL") ? "CostCollectorService.EditSUBL" : "CostCollectorService.EditFR", aSelectedContexts, {
                      label: "{i18n>EDIT}"
                  })
                  .then(jQuery.proxy(function (oresponse) {

                  }, this), this)
                  .catch(jQuery.proxy(function (oresponse) {

                  }, this), this);

            },
             /** 
              * This event is called to check on enabling the edit button based on the selection of the table rows
              * User will open the cost collector edit dialog box 
              * @public
              * @param {object} oBindingContext :  is the binding context of the current entity 
              * @param {array} aSelectedContexts : contains an array of binding contexts corresponding to selected items in case of table action
              */
            setEnabled: function (oBindingContext, aSelectedContexts)
            {
               return (aSelectedContexts && aSelectedContexts.length === 1);
            
            }
        };
    });
