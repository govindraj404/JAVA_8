sap.ui.define(["sap/ui/core/format/DateFormat"],
function(DateFormat){
    "use strict";
    var oFormatter = {
        _oDFMT: DateFormat.getDateInstance({
            style: "medium"
        }),
        showFormattedDate: function (oDate) {
            var sFormattedDate;
            if (oDate) {
                sFormattedDate =  oFormatter._oDFMT.format(new Date(oDate));
            }
            return sFormattedDate;
        }
    };
    return oFormatter;
},true);