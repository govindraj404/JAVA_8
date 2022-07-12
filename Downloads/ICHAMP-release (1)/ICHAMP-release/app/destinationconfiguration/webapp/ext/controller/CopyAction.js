sap.ui.define([],
	function () {
		"use strict";
		return {
			/** 
			 * This event is called to check on enabling the Copy button based on the selection of the table rows
			 * @public
			 * @param {object} oBindingContext :  is the binding context of the current entity 
			 * @param {array} aSelectedContexts : contains an array of binding contexts corresponding to selected items 
			 */
			setEnabled: function (oBindingContext, aSelectedContexts) {
				return (aSelectedContexts && aSelectedContexts.length === 1);
			}
		};
	}
);