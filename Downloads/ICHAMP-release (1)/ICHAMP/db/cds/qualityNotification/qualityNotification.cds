namespace com.sap.ic.cmh.qualityNotification;

using {cuid,managed} from '@sap/cds/common';
using {com.sap.ic.cmh.businessObjectStatus.BusinessObjectStatuses} from '../common/index';
using com.sap.ic.cmh.businessPartner.BusinessPartner from '../common/index';
using {com.sap.ic.cmh.companyCode.CompanyCode} from '../common/index';
using {com.sap.ic.cmh.defect.Defects} from './index';
using {com.sap.ic.cmh.plant.Plant} from '../common/index';
using {com.sap.ic.cmh.purchaseOrganization.PurchaseOrganization} from '../common/index';
using {com.sap.ic.cmh.qualityNotification.dataType as DataType} from './index';
using {com.sap.ic.cmh.materialMasterGeneralData.MaterialMasterGeneralData} from '../common/index';
using {com.sap.ic.cmh.complaint.Complaint} from '../complaint/index';
using {com.sap.ic.cmh.complaint.dataType as CDataType} from '../complaint/index';
using {com.sap.ic.cmh.qualityNotificationStatus.QualityNotificationStatus} from './index';
using {com.sap.ic.cmh.businessObjectRelation.BusinessObjectRelations} from '../common/index';

type QualityNotification : Association to one QualityNotifications;

entity QualityNotifications : cuid, managed {
    identifier             : DataType.Identifier;
    qnType                 : DataType.Type;
    supplier               : BusinessPartner;
    material               : MaterialMasterGeneralData;
    quantity               : DataType.Quantity;
    unit                   : DataType.Unit;//Add the association
    plant                  : Plant;
    purchasingOrganization : PurchaseOrganization;
    company                : CompanyCode;
    personResponsible      : BusinessPartner;//TODO after Master Data of responsible person is finailised
    contactPerson          : BusinessPartner;
    inspectionResult       : DataType.InspectionResult;
    defect                 : Composition of one Defects on defect.parent = $self;
    purchaseOrderNumber    : DataType.Number;
    purchaseOrderItem      : DataType.ItemNumber;
    complaint              : Complaint;
    status                 : QualityNotificationStatus;
    supplierRole            : DataType.Role;
    personResponsibleRole   : DataType.Role;
    referenceNumber        : CDataType.Number;
    businessObjectStatuses : Composition of many BusinessObjectStatuses on businessObjectStatuses.parent = ID;
    businessObjectRelations : Association to many BusinessObjectRelations on businessObjectRelations.sourceBusinessObjectUUID = ID;
}