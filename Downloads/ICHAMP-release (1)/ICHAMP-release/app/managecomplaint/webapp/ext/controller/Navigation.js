sap.ui.define([],
	function () {
		"use strict";
		return {
			onNavigationButtonPress: function (oContext, aSelectedContexts) {
				var sIdentifier = oContext.getObject().hasOwnProperty("ID") && oContext.getObject().ID;
                this.intentBasedNavigation.navigateOutbound("costcollector", {"ID" : sIdentifier});
			}
		}
	}
);