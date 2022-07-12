sap.ui.define(["sap/m/MessageToast"],
	function (MessageToast) {
		"use strict";
		return {
			handleOtherFileType: function () {
				MessageToast.show(
					this.getModel("i18n").getResourceBundle().getText("FILE_NOT_SUPPORTED"));
					this.refresh("attachments");
			},

			handleFileSizeExceeded: function () {
				//MessageToast.show(this.getModel("i18n").getResourceBundle().getText("FILE_SIZE_EXCEEDED"));
				//	this.refresh("attachments");
            },
            
			downloadAttachment: function () {
				var oUploadSet = this.byId("sUploadSet");
				oUploadSet.getItems().forEach(function (oItem) {
					if (oItem.getListItem().getSelected()) {
						oItem.download(true);
					}
				});
			},

			createAttachment: function (oEvent) {
				var sURL = "/objectstorage/api/attachment/upload",
				 aItems = oEvent.getParameters("item"),
				 oFileToBeUploaded = aItems.item._oFileObject,
				 aFormData = new FormData();
                 aFormData.append("file", oFileToBeUploaded);
                 aFormData.append("complaintId", oEvent.getSource().getBindingContext().getProperty("ID"));
                 aFormData.append("attachmentName", oFileToBeUploaded.name);
				$.ajax({
					url: sURL,
					method: "POST",
					contentType: false,
					processData: false,
					data: aFormData,
                    success: function () {
						// this.byId("sTitle").setText(this.getModel("i18n").getResourceBundle().getText("ITEMS_COUNT", this.byId("sUploadSet").getItems().length));
						this.refresh("attachments");
					}.bind(this),
                    error: function (oResponse) {
                        MessageToast.show(
                            oResponse.responseText);
					}.bind(this),
				});

			},
		
			deleteAttachment: function (oEvent) {
				oEvent.getParameter("item").getBindingContext().delete("$auto").then(function () {
					MessageToast.show(
						this.getModel("i18n").getResourceBundle().getText("ATTACHMENT_DELETE_TOAST_SINGULAR"));
				}.bind(this), function (oError) {
					MessageToast.show(oError.responseText);	
				});
			},

			formatURL: function (sID) {
				return ["/objectstorage/api/attachment/", sID].join("");
            }

		}
    }
);