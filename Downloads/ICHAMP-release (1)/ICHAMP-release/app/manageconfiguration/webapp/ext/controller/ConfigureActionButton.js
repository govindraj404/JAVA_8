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
            onConfigure: function (oEvent) {
                var sCode = oEvent.getSource().getBindingContext().getObject().code, sOutbound = "";
                    switch (sCode) {
                        case 'COMPCHNL': sOutbound = "complaintchannelconfiguration";
                            break;
                        case 'CONTYPE': sOutbound = "conditiontypeconfiguration";
                            break;
                        case 'COMTYPE': sOutbound = "complainttypeconfiguration";
                            break;
                        case 'COMTYPEITEMCAT': sOutbound = "comptypeitemcatconfiguration";
                            break;
                        case 'TARGETREF': sOutbound = "targetreftypemapconfiguration";
                            break;
                        case 'ITMCAT': sOutbound = "itemcategoriesconfiguration";
                            break;
                        case 'REFTYPE': sOutbound = "referencetypesconfiguration";
                            break;
                        case 'TARGETTYPE': sOutbound = "targettypeconfiguration";
                            break;
                        case 'COMITMRESN': sOutbound = "complaintrsnconfiguration";
                            break;
                        case 'SERMAT': sOutbound = "servicematerialconfiguration";
                            break;
                        case 'DESCONFIG': sOutbound = "destinationconfiguration";
                            break;
                        case 'BUSOBJCONFIG': sOutbound = "businessobjectconfiguration";
                            break;
                        case 'CLMSTSMAP': sOutbound = "claimstatusconfiguration";
                            break;
                        case 'COMRSNMAP': sOutbound = "complaintrsnmapconfiguration";
                            break;
                        case 'SOURCEREF': sOutbound = "sourcereftypemapconfiguration";
                            break;
                        default:
                            break;
                    }
                    this.intentBasedNavigation.navigateOutbound(sOutbound);
            }
        }
    });