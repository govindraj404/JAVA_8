namespace com.sap.ic.cmh.returnPurchaseOrderStatus;

type ReturnPurchaseOrderStatus : Association to one ReturnPurchaseOrderStatuses;

entity ReturnPurchaseOrderStatuses {
    key code : String (40);
        name : localized String(60);
        sequenceNumber : Integer;
}