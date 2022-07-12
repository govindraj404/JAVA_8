sap.ui.define([],
	function () {
		"use strict";
		return {
			onNavigationButtonPress: function (oContext, aSelectedContexts) {
				var sIdentifier = oContext.getObject().hasOwnProperty("identifier") && oContext.getObject().identifier;
                this.intentBasedNavigation.navigateOutbound("supplierIssueProcess", {"suplrIssProcId" : sIdentifier});
			}
		}
	}
);