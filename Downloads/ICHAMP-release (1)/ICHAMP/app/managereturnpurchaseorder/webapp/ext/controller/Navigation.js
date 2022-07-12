sap.ui.define([
        "sap/m/Dialog",
        "sap/m/DialogType",
        "sap/m/Button",
        "sap/m/ButtonType",
        "sap/m/Text"
    ],
    function (Dialog, DialogType, Button, ButtonType, Text) {
        "use strict";
        return {
            onNavigationButtonPress: function (oContext, aSelectedContexts) {
                var oView = this.extension.cmh.managereturnpurchaseorder.OPExtend.getView(),
                    i18n = oView.getModel("i18n").getResourceBundle(),
                    aFilters = [new sap.ui.model.Filter("ID", "EQ", oContext.getObject().hasOwnProperty("ID") && oContext.getObject().ID)],
                    oReturnPurchaseOrdersBinding = this.getModel().bindList("/ReturnPurchaseOrders", undefined, undefined, aFilters, {});
                oReturnPurchaseOrdersBinding.attachEventOnce("dataReceived", function (oReturnPurchaseOrdersEvent) {
                    var sSemanticObject = "returnpo",
                        sAction = "managewd",
                        oObject = oReturnPurchaseOrdersEvent && oReturnPurchaseOrdersEvent.getSource() &&
                        oReturnPurchaseOrdersEvent.getSource().getContexts() &&
                            oReturnPurchaseOrdersEvent.getSource().getContexts()[0] && oReturnPurchaseOrdersEvent.getSource().getContexts()[0].getObject(),
                        sNavigation = oObject && oObject.sNavigation,
                        sNumber = oObject && oObject.number;
                    if (sNavigation && sNavigation.length) {
                        sap.ushell.Container.getService("CrossApplicationNavigation").isNavigationSupported(
                            [{
                                target: {
                                    semanticObject: sSemanticObject,
                                    action: [sAction, sNavigation].join("")
                                }
                            }]).done(function (aResponses) {
                            if (aResponses.length && aResponses[0].supported) {
                                sap.ushell.Container.getService("CrossApplicationNavigation").toExternal({
                                    target: {
                                        semanticObject: sSemanticObject,
                                        action: [sAction, sNavigation].join("")
                                    },
                                    params: {
                                        "MEPO_TOPLINE-EBELN": sNumber
                                    }
                                });
                            } else {
                                this.oDefaultMessageDialog = new Dialog({
                                    type: DialogType.Message,
                                    title: i18n.getText("ERROR"),
                                    content: new Text({
                                        text: i18n.getText("MSG_NAVIGATION_FAILED")
                                    }),
                                    beginButton: new Button({
                                        type: ButtonType.Emphasized,
                                        text: i18n.getText("OK"),
                                        press: function () {
                                            this.oDefaultMessageDialog.close();
                                        }.bind(this)
                                    })
                                });
                                this.oDefaultMessageDialog.open();
                            }
                        }).fail(function () {});
                    } else {
                        this.oDefaultMessageDialog = new Dialog({
                            type: DialogType.Message,
                            title: i18n.getText("ERROR"),
                            content: new Text({
                                text: i18n.getText("MSG_NAVIGATION_FAILED")
                            }),
                            beginButton: new Button({
                                type: ButtonType.Emphasized,
                                text: i18n.getText("OK"),
                                press: function () {
                                    this.oDefaultMessageDialog.close();
                                }.bind(this)
                            })
                        });
                        this.oDefaultMessageDialog.open();
                    }
                }.bind(this));
                oReturnPurchaseOrdersBinding.getContexts();
            }
        }
    }
);