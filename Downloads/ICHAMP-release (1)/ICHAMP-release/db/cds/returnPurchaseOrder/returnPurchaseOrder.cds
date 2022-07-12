namespace com.sap.ic.cmh.returnPurchaseOrder;

using {cuid,managed} from '@sap/cds/common';

using {com.sap.ic.cmh.businessObjectStatus.BusinessObjectStatuses} from '../common/index';
using com.sap.ic.cmh.businessPartner.BusinessPartner from '../common/index';
using {com.sap.ic.cmh.plant.Plant} from '../common/index';
using {com.sap.ic.cmh.purchaseOrganization.PurchaseOrganization} from '../common/index';
using {com.sap.ic.cmh.reason.Reason} from '../common/index';
using {com.sap.ic.cmh.purchasingGroup.PurchasingGroup} from '../common/index';
using {com.sap.ic.cmh.returnPurchaseOrder.dataType as DataType} from './index';
using {com.sap.ic.cmh.materialMasterGeneralData.MaterialMasterGeneralData} from '../common/index';
using {com.sap.ic.cmh.complaint.Complaint} from '../complaint/index';
using {com.sap.ic.cmh.companyCode.CompanyCode} from '../common/index';
using {com.sap.ic.cmh.returnPurchaseOrderStatus.ReturnPurchaseOrderStatus} from './index';
using {com.sap.ic.cmh.businessObjectRelation.BusinessObjectRelations} from '../common/index';

type ReturnPurchaseOrder : Association to one ReturnPurchaseOrders;

entity ReturnPurchaseOrders : cuid, managed {
    identifier                : DataType.Identifier;
    returnPurchaseType        : DataType.Type;
    reason                    : Reason;
    itemNumber                : DataType.ItemNumber;
    supplier                  : BusinessPartner;
    material                  : MaterialMasterGeneralData;
    quantity                  : DataType.Quantity;
    unit                      : DataType.Unit;
    plant                     : Plant;
    purchasingOrganization    : PurchaseOrganization;
    purchasingGroup           : PurchasingGroup;
    goodsIssueDate            : Date;
    personResponsible         : BusinessPartner; //TODO after Master Data of responsible person is finailised
    contactPerson             : BusinessPartner;
    movementType              : DataType.MovementType;
    complaint                 : Complaint;
    company                   : CompanyCode;
    status                    : ReturnPurchaseOrderStatus;
    businessObjectStatuses    : Composition of many BusinessObjectStatuses on businessObjectStatuses.parent = ID;
    businessObjectRelations : Association to many BusinessObjectRelations on businessObjectRelations.sourceBusinessObjectUUID = ID;
}