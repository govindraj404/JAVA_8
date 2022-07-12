namespace com.sap.ic.cmh.returnPurchaseOrderStatusMapping;

using {com.sap.ic.cmh.returnPurchaseOrderStatus.ReturnPurchaseOrderStatus} from './index';

type ReturnPurchaseOrderStatusMapping : Association to one ReturnPurchaseOrderStatusMappings;

entity ReturnPurchaseOrderStatusMappings {
    key code : String (40);
        name : String(40);
        status : ReturnPurchaseOrderStatus;
}